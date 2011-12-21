(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_object;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
{$if defined(USE_SIMPLE_CLASSES)}
  vsagps_classes,
{$else}
  Classes,
{$ifend}
  vsagps_public_base,
  vsagps_public_types,
  vsagps_public_unit_info,
  vsagps_public_device,
  vsagps_tools,
  vsagps_queue,
  vsagps_units,
  vsagps_device_base;
  
type
  Tvsagps_Packet_Thread = class(Tvsagps_Thread)
  protected
    procedure Execute; override;
  end;

  Tvsagps_object = class(TVSAGPS_UNITS)
  private
    FPacketThread: Tvsagps_Packet_Thread; // single thread: parse packets
    FPacketQueue: Tvsagps_queue; // single queue object for all utits
  protected
    procedure InternalThreadRoutine_for_Packets(const AThread: Tvsagps_Packet_Thread); register;
    procedure InternalOnPacketThreadTerminate(Sender: TObject);
    procedure InternalCreateRuntimeObjects; override;
    procedure InternalTerminateRuntimeObjects; override;
    procedure InternalWaitRuntimeObjects; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GPSConnect(const AGPSDevType: DWORD;
                        const AGPSDevName: String;
                        const AFileSource: PWideChar;
                        const AALLDevParams: PVSAGPS_ALL_DEVICE_PARAMS;
                        const ANewDevParams: PVSAGPS_SINGLE_DEVICE_PARAMS;
                        const AUNIT_INFO_Changed: TVSAGPS_UNIT_INFO_DLL_Proc;
                        const AUnitIndexOut: PByte;
                        const AReserved: PDWORD): Boolean;

    // dump packet to string representation (for logging)
    function SerializePacket(const AUnitIndex: Byte;
                             const APacket: Pointer): PChar;

    // send packet to device object
    function SendPacket_ToUnit(const AUnitIndex: Byte;
                               const APacketBuffer: Pointer;
                               const APacketSize: DWORD;
                               const AFlags: DWORD;
                               const AReserved: PDWORD): LongBool;

    // get current status of device object
    function GetDeviceStatus_ForUnit(const AUnitIndex: Byte;
                                     const AGPSDevTypePtr: PDWORD;
                                     const AStatePtr: Pvsagps_GPSState): LongBool;
  end;

implementation

uses
  vsagps_memory,
  vsagps_device_usb_garmin,
  vsagps_device_com_nmea,
  vsagps_track_reader,
  vsagps_runtime;

{ Tvsagps_object }

constructor Tvsagps_object.Create;
begin
  inherited Create;
  FDeviceThread:=nil;
  FPacketThread:=nil;
  FPacketQueue:=Tvsagps_queue.Create;
end;

destructor Tvsagps_object.Destroy;
begin
  // disconnect with wait
  Lock_CS_State;
  try
    InternalGPSDisconnect(FALSE, TRUE);
  finally
    Unlock_CS_State;
  end;
  // kill object
  FreeAndNil(FPacketThread);
  FreeAndNil(FPacketQueue);
  inherited Destroy;
end;

procedure Tvsagps_object.InternalTerminateRuntimeObjects;
begin
  inherited;
  if (nil<>FPacketThread) then begin
    FPacketThread.Terminate;
  end;
  if (nil<>FPacketQueue) then begin
    FPacketQueue.FreeAllPackets;
  end;
end;

procedure Tvsagps_object.InternalThreadRoutine_for_Packets(const AThread: Tvsagps_Packet_Thread);
var
  queued_pointer: Pointer;
  queued_unit_index: Byte;
  dwNowTicks, dwWaitMSec, dwDelay: DWORD;
  dwDevType: DWORD;
  ptrUserPtr: Pointer;
  bCallDevice: Boolean;
  pUnit: PVSAGPS_UNIT;

  function _CheckTerminated: Boolean;
  begin
    Result := ((nil<>AThread) and (AThread.Terminated))
  end;

  procedure _OnErrIndex;
  begin
    bCallDevice:=FALSE;
    dwDevType:=0;
    ptrUserPtr:=FALLDeviceUserPointer;
  end;

begin
  dwDelay := GetDeviceWorkerThreadTimeoutMSec(FALLDeviceParams, nil);

  repeat
  try
    // check exit
    if _CheckTerminated then
      break;

    dwNowTicks:=GetTickCount;

    // read packets
    while FPacketQueue.ExtractGPSPacket(queued_pointer, queued_unit_index) do
    if (nil<>queued_pointer) then
    try
      pUnit:=nil;
      // set timestamp and get params
      if (queued_unit_index<=cUnitIndex_Max) then begin
        // correct index - save params
        Lock_CS_State;
        try
          // check exit
          if _CheckTerminated then
            break;

          pUnit:=GPSUnits[queued_unit_index];
          if (nil<>pUnit) then begin
            // ok
            pUnit^.dwLastPacketTicks:=dwNowTicks;

            // check device state
            bCallDevice := (nil<>pUnit^.objDevice) and
                           (pUnit^.eDevState in [gs_ProcessConnecting,gs_DoneConnected]) and
                           (0<>pUnit^.objDevice.GPSDeviceHandle);

            // devtype
            if (nil<>pUnit^.objDevice) then
              dwDevType := pUnit^.objDevice.GPSDeviceType
            else
              dwDevType := 0;

            // user defined pointer
            if (nil<>pUnit^.pDevParams) then
              ptrUserPtr := pUnit^.pDevParams^.pUserPointer
            else
              ptrUserPtr := FALLDeviceUserPointer;
          end else begin
            // error
            _OnErrIndex;
          end;
        finally
          Unlock_CS_State;
        end;
      end else begin
        // incorrect index
        _OnErrIndex;
      end;

      // check exit
      if _CheckTerminated then
        break;

      // low-level external parser
      if (nil<>FALLDeviceParams) then
      if Assigned(FALLDeviceParams^.pLowLevelHandler) then begin
          FALLDeviceParams^.pLowLevelHandler(ptrUserPtr, queued_unit_index, dwDevType, queued_pointer);
      end;

      // check exit
      if _CheckTerminated then
        break;

      // default internal parser
      if bCallDevice and (nil<>pUnit) then begin
        Lock_CS_State;
        try
          // check exit
          if _CheckTerminated then
            break;
          // call device object
          if Assigned(pUnit^.objDevice) then
            pUnit^.objDevice.ParsePacket(queued_pointer);
        finally
          Unlock_CS_State;
        end;
      end;
    finally
      VSAGPS_FreeMem(queued_pointer);
    end;

    // check exit
    if _CheckTerminated then
      break;

    // check timeouts (and external gps commands)
    for queued_unit_index := 0 to cUnitIndex_Max do begin
      // check exit
      if _CheckTerminated then
        break;
      // check unit
      pUnit:=GPSUnits[queued_unit_index];
      if (nil<>pUnit) and (gs_DoneConnected=pUnit^.eDevState) then begin
        // timeout
        dwWaitMSec:=GetReceiveGPSTimeoutSec(FALLDeviceParams, pUnit^.pDevParams);
        dwWaitMSec:=dwWaitMSec*1000;
        // check
        if (dwNowTicks > pUnit^.dwLastPacketTicks+dwWaitMSec) then begin
          // timeout - no data in time - notify ANOTHER thread about disconnect
          //InternalStartDisconnectingForUnit(queued_unit_index);
          if Assigned(OnGPSTimeout) then
            OnGPSTimeout(InternalGetUserPointer(queued_unit_index), queued_unit_index, pUnit^.objDevice.GPSDeviceType, nil);
          pUnit^.eDevState:=gs_PendingDisconnecting;
        end else begin
          // execute external gps commands
          if pUnit^.reqGPSCommand_ResetDGPS then begin
            pUnit^.reqGPSCommand_ResetDGPS:=FALSE;
            if (nil<>pUnit^.objDevice) then
              pUnit^.objDevice.ExecuteGPSCommand(gpsc_Reset_DGPS, nil);
          end;
        end;
      end;
    end;
  except
  end;
  Sleep(dwDelay);
  until FALSE;

  // kill thread
  if (nil<>AThread) then begin
    AThread.GPSRunning:=FALSE;
    AThread.Terminate;
  end;
end;

procedure Tvsagps_object.InternalWaitRuntimeObjects;
begin
  inherited;
  InternalWaitRuntimeThread(@FPacketThread);
end;

function Tvsagps_object.SendPacket_ToUnit(const AUnitIndex: Byte;
                                          const APacketBuffer: Pointer;
                                          const APacketSize: DWORD;
                                          const AFlags: DWORD;
                                          const AReserved: PDWORD): LongBool;
var p: PVSAGPS_UNIT;
begin
  Result:=FALSE;
  if (AUnitIndex<=cUnitIndex_Max) then begin
    p:=GPSUnits[AUnitIndex];
    if (nil<>p) and (nil<>p^.objDevice) then
      Result:=p^.objDevice.SendPacket(APacketBuffer, APacketSize, AFlags);
  end;
end;

function Tvsagps_object.SerializePacket(const AUnitIndex: Byte;
                                        const APacket: Pointer): PChar;
var p: PVSAGPS_UNIT;
begin
  Result:=nil;
  if (AUnitIndex<=cUnitIndex_Max) then begin
    p:=GPSUnits[AUnitIndex];
    if (nil<>p) and (nil<>p^.objDevice) then
      Result:=p^.objDevice.SerializePacket(APacket);
  end;
end;

function Tvsagps_object.GetDeviceStatus_ForUnit(const AUnitIndex: Byte;
                                                const AGPSDevTypePtr: PDWORD;
                                                const AStatePtr: Pvsagps_GPSState): LongBool;
var p: PVSAGPS_UNIT;
begin
  Result:=FALSE;
  if (AUnitIndex<=cUnitIndex_Max) then begin
    p:=GPSUnits[AUnitIndex];
    if (nil<>p) then begin
      // state
      if (nil<>AStatePtr) then
        AStatePtr^:=p^.eDevState;
      // devtype
      if (not (FALLState in [gs_DoneDisconnected,gs_ProcessDisconnecting])) then
      if (nil<>AGPSDevTypePtr) then
      if (not (p^.eDevState in [gs_DoneDisconnected,gs_PendingDisconnecting,gs_ProcessDisconnecting])) then begin
        Lock_CS_State;
        try
          if (nil<>p^.objDevice) then
            AGPSDevTypePtr^:=p^.objDevice.GPSDeviceType;
        finally
          Unlock_CS_State;
        end;
      end;
      Result:=TRUE;
    end;
  end;
end;

function Tvsagps_object.GPSConnect(
  const AGPSDevType: DWORD;
  const AGPSDevName: String;
  const AFileSource: PWideChar;
  const AALLDevParams: PVSAGPS_ALL_DEVICE_PARAMS;
  const ANewDevParams: PVSAGPS_SINGLE_DEVICE_PARAMS;
  const AUNIT_INFO_Changed: TVSAGPS_UNIT_INFO_DLL_Proc;
  const AUnitIndexOut: PByte;
  const AReserved: PDWORD): Boolean;
var
  VOldState: Tvsagps_GPSState;
  p: PVSAGPS_UNIT;
begin
  Result:=FALSE;
  if (0=(AGPSDevType and
          (gdt_USB_Garmin or
           gdt_FILE_Track or
           gdt_COM_NMEA0183))) then
    Exit;
  if (nil=AUnitIndexOut) then
    Exit;
  if (nil=AALLDevParams) then
    Exit;
  if (sizeof(AALLDevParams^)<>AALLDevParams^.wSize) then
    Exit;
  if (nil<>FDeviceThread) and (FDeviceThread.Terminated) then
    Exit;
  if (nil<>FPacketThread) and (FPacketThread.Terminated) then
    Exit;
  if (gdt_FILE_Track=(AGPSDevType and gdt_FILE_Track)) and (nil=AFileSource) then
    Exit;
  
  Lock_CS_State;
  try
    // current state
    VOldState:=FALLState;

    AUnitIndexOut^:=InternalFindNewUnitIndex;
    if (cUnitIndex_ALL = AUnitIndexOut^) then
      Exit;
    p:=GPSUnits[AUnitIndexOut^];
    if (nil=p) then
      Exit;

    if (gs_DoneDisconnected=VOldState) then begin
      // only first time
      FALLDeviceParams:=AALLDevParams;
      if (nil<>FPacketQueue) then
        FPacketQueue.FreeAllPackets;
    end;  

    // create threads
    InternalCreateRuntimeObjects;

    // prepare struct
    InternalPrepareItem(AUnitIndexOut^);

    p^.pDevParams:=ANewDevParams;

    // create object
    if (gdt_FILE_Track=(AGPSDevType and gdt_FILE_Track)) then begin
      // fly-on-track mode
      p^.objDevice:=Tvsagps_track_reader.Create(AFileSource);
    end else if (gdt_USB_Garmin=(AGPSDevType and gdt_USB_Garmin)) then begin
      // usb garmin
      p^.objDevice:=Tvsagps_device_usb_garmin.Create;
    end else if (gdt_COM_NMEA0183=(AGPSDevType and gdt_COM_NMEA0183)) then begin
      // com nmea
      p^.objDevice:=Tvsagps_device_com_nmea.Create;
    end;

    if Assigned(p^.objDevice) then begin
      // set params
      p^.objDevice.SetBaseParams(AUnitIndexOut^,
                                 AGPSDevType,
                                 FPacketQueue,
                                 AGPSDevName,
                                 AALLDevParams,
                                 ANewDevParams,
                                 FALLDeviceUserPointer,
                                 AUNIT_INFO_Changed,
                                 @FCS_CloseHandle);

      // set connecting marker (for device thread)
      p^.eDevState:=gs_PendingConnecting;
      
      // raise event and change state
      if (dpdfi_ConnectingFromConnect = (AALLDevParams^.dwDeviceFlagsIn and dpdfi_ConnectingFromConnect)) then
        InternalChangeALLGPSState(gs_PendingConnecting, FALSE);
      // ok
      Result:=TRUE;
    end else begin
      // error
      InternalCleanupItem(AUnitIndexOut^);
    end;
  finally
    Unlock_CS_State;
  end;
end;

procedure Tvsagps_object.InternalCreateRuntimeObjects;
begin
  // thread for packets
  if (nil=FPacketThread) then begin
    FPacketThread:=Tvsagps_Packet_Thread.Create(TRUE);
    FPacketThread.GPSObject:=Self;
    FPacketThread.GPSRunning:=TRUE;
    FPacketThread.FreeOnTerminate:=TRUE;
    FPacketThread.OnTerminate:=InternalOnPacketThreadTerminate;
    FPacketThread.Resume;
  end;
  inherited;
end;

procedure Tvsagps_object.InternalOnPacketThreadTerminate(Sender: TObject);
begin
  FPacketThread:=nil;
end;

{ Tvsagps_Packet_Thread }

procedure Tvsagps_Packet_Thread.Execute;
begin
  //inherited;
  if (FGPSObject=nil) then
    Exit;
  Tvsagps_object(FGPSObject).InternalThreadRoutine_for_Packets(Self);
end;

end.

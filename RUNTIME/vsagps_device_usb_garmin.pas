(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_device_usb_garmin;
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
  vsagps_public_device,
  vsagps_public_garmin,
  vsagps_device_base;


type
  Tvsagps_device_usb_garmin = class(Tvsagps_device_base)
  private
    // API version (used by device)
    FAPIVersion: DWORD;
    FDevicePacketSize: DWORD; // packet size (info from device - used internally)
    // Special workarounds
    FRepeatReadOnCrazyDataSize: Boolean;
    FExitOnLastProtocolPacket: Boolean;
  private
    // Low-level routines
    function ZwCallIoctl4(AIoControlCode: ULONG; iosb: PIO_STATUS_BLOCK; pData: PDWORD): LongBool;
    function ZwSendPacket(pPacket: PGarminUSB_Custom_Packet; iosb: PIO_STATUS_BLOCK): LongBool;
    function ZwRecvIoctlAsync(iosb: PIO_STATUS_BLOCK; AOutputBufferSize: DWORD): PGarminUSB_Custom_Packet;
    function ZwRecvReadPacket(iosb: PIO_STATUS_BLOCK): PGarminUSB_Custom_Packet;
  private
    procedure Internal_Parse_Protocol_Array(AData_Size: DWORD;
                                            pData: PProtocol_Array_Type);
    procedure Internal_Parse_Product_Data(const AData_Size: DWORD;
                                          const pData: PProduct_Data_Type);
    procedure Internal_Parse_Ext_Product_Data(const AData_Size: DWORD;
                                              const pData: PExt_Product_Data_Type);
  protected
    procedure InternalResetDeviceConnectionParams; override;
    procedure Internal_Before_Open_Device; override;
    // start session - operations to establish connection
    function WorkingThread_StartSession: Boolean; override;
    // send query packet to gps
    function WorkingThread_SendPacket: Boolean; override;
    // receive packets from device to queue
    function WorkingThread_Receive_Packets(const AWorkingThreadPacketFilter: DWORD): Boolean; override;
  public
    procedure ExecuteGPSCommand(const ACommand: LongInt;
                                const APointer: Pointer); override;
    function SerializePacket(const APacket: Pointer): PChar; override;
    function ParsePacket(const ABuffer: Pointer): DWORD; override;

    function SendPacket(const APacketBuffer: Pointer;
                        const APacketSize: DWORD;
                        const AFlags: DWORD): LongBool; override;
  end;


implementation

uses
  vsagps_tools,
  vsagps_runtime,
  vsagps_memory,
  vsagps_public_unit_info;

{ Tvsagps_device_usb_garmin }

procedure Tvsagps_device_usb_garmin.ExecuteGPSCommand(const ACommand: LongInt;
                                                      const APointer: Pointer);
begin
  inherited;
  // gpsc_Refresh_DGPS - not supported
end;

procedure Tvsagps_device_usb_garmin.InternalResetDeviceConnectionParams;
begin
  inherited;
  FDevicePacketSize:=0;
  FAPIVersion:=0;
  FRepeatReadOnCrazyDataSize:=FALSE;
  FExitOnLastProtocolPacket:=FALSE;
end;

procedure Tvsagps_device_usb_garmin.Internal_Before_Open_Device;
var
  dwSize: DWORD;
  slUSBs: TStringList;
begin
  inherited;

  dwSize:=Length(FDeviceNameByUser);
  if (0=dwSize) then begin
    // no name from user (commonly) - autodetect garmin usb device(s)
    dwSize:=0;
    slUSBs:=TStringList.Create;
    try
      VSAGPS_Autodetect_Get_SetupDi_Devices(@GUID_DEVINTERFACE_GRMNUSB, @dwSize, TRUE, slUSBs);
      if (0<slUSBs.Count) then begin
        VSAGPS_FreeAndNil_PChar(FGPSDeviceInfo_NameToConnectInternalA);
        FGPSDeviceInfo_NameToConnectInternalA:=VSAGPS_AllocPCharByString(slUSBs[0], TRUE);
      end;
    finally
      FreeAndNil(slUSBs);
    end;
  end else begin
    // name from user - crazy people (to get name for usb device)!
    // prayed
    VSAGPS_FreeAndNil_PChar(FGPSDeviceInfo_NameToConnectInternalA);
    FGPSDeviceInfo_NameToConnectInternalA:=VSAGPS_AllocPCharByString(FDeviceNameByUser, TRUE);
  end;
end;

procedure Tvsagps_device_usb_garmin.Internal_Parse_Ext_Product_Data(const AData_Size: DWORD; const pData: PExt_Product_Data_Type);
begin
  parse_nullstrings_to_strings(AData_Size, @(pData^.product_description), FDeviceInfo, TRUE, FALSE);
end;

procedure Tvsagps_device_usb_garmin.Internal_Parse_Product_Data(const AData_Size: DWORD; const pData: PProduct_Data_Type);
begin
  InternalSetUnitInfo(guik_Product_ID, IntToStr(pData^.product_ID));
  InternalSetUnitInfo(guik_Software_Version, IntToStr(pData^.software_version));

  // parse and save model (first line from list) - 3 is the size of  starting fields
  parse_nullstrings_to_strings(AData_Size-3, @(pData^.product_description), FDeviceInfo, TRUE, FALSE);
  if (nil<>FDeviceInfo) and (0<FDeviceInfo.Count) then begin
    InternalSetUnitInfo(guik_Model_Description_Full, FDeviceInfo[0]);
    FDeviceInfo.Delete(0);
  end;
end;

procedure Tvsagps_device_usb_garmin.Internal_Parse_Protocol_Array(AData_Size: DWORD; pData: PProtocol_Array_Type);
var
  cur: PProtocol_Data_Type;
  s: String;
begin
  if (FSupportedProtocols.Count>0) then
    Exit;
  cur:=@(pData^[0]);
  while (AData_Size>0) do begin
    // parse
    s:=IntToStr(cur^.data);
    while Length(s)<3 do
      s:='0'+s;
    s:=Chr(cur^.tag)+s;
    FSupportedProtocols.Append(s);
    // goto next
    cur:=PProtocol_Data_Type(Pointer(DWORD(Pointer(cur)) + sizeof(Protocol_Data_Type)));
    if (AData_Size<3) then
      AData_Size:=0
    else
      AData_Size:=AData_Size-3;
  end;
end;

function Tvsagps_device_usb_garmin.ParsePacket(const ABuffer: Pointer): DWORD;
begin
  // inherited;
  Result:=Parse_GarminPVT_Packets(ABuffer, vgpt_Allow_Stats)
end;

function Tvsagps_device_usb_garmin.SendPacket(const APacketBuffer: Pointer;
                                              const APacketSize: DWORD;
                                              const AFlags: DWORD): LongBool;
var
  iosb: IO_STATUS_BLOCK;
  pFullPacket: PGarminUSB_Custom_Packet;
begin
  // full packet or data only?
  if (gspf_FullPacket = (AFlags and gspf_FullPacket)) then begin
    // full packet
    Result := ZwSendPacket(APacketBuffer, @iosb);
  end else begin
    // make full packet
    pFullPacket := VSAGPS_GetMem(GarminUSB_Custom_Packet_Base_Size+APacketSize);
    try
      // by default transfer position (safe)
      InitializeUSBGarminPacket(pFullPacket, PT_Application_Layer, A010_Cmnd_Transfer_Posn, APacketSize);
      if (0 < APacketSize) then
        CopyMemory(@(pFullPacket^.Data), APacketBuffer, APacketSize);
      Result := ZwSendPacket(pFullPacket, @iosb);
    finally
      VSAGPS_FreeMem(pFullPacket);
    end;
  end;
end;

function Tvsagps_device_usb_garmin.SerializePacket(const APacket: Pointer): PChar;
var dwLen: DWORD;
begin
  dwLen:=PGarminUSB_Custom_Packet(APacket)^.Data_Size;
  if (MAX_BUFFER_SIZE<=dwLen) then
    dwLen:=0;
  dwLen:=GarminUSB_Custom_Packet_Base_Size+dwLen;
  Result:=VSAGPS_AllocPCharByPByte(APacket, dwLen);
end;

function Tvsagps_device_usb_garmin.WorkingThread_Receive_Packets(const AWorkingThreadPacketFilter: DWORD): Boolean;
var
  pPacket: PGarminUSB_Custom_Packet;
  iosb: IO_STATUS_BLOCK;

  function _NoIOCTL: Boolean;
  begin
    Result:=(wtro_IOCTL_Disabled=(FWT_Params.dwRunningOptions and wtro_IOCTL_Disabled));
  end;

  procedure _CheckInternalPacketFilter(var bExit: Boolean);
  var uniq_id: Int64;
  begin
    if (nil=pPacket) then
      Exit;

    if (pPacket^.Data_Size>MAX_BUFFER_SIZE) then
      if (iosb.Information=0) then
        Exit;

    // parse device info packets
    if (PT_Application_Layer=pPacket^.Packet_Type) and (Pid_Protocol_Array=pPacket^.Packet_ID) then begin
      // Pid_Protocol_Array packets
      Internal_Parse_Protocol_Array(pPacket^.Data_Size, PProtocol_Array_Type(@(pPacket^.Data)));
      // Protocol
      if (not bExit) and (0<>AWorkingThreadPacketFilter) then
        if (wtfp_Protocol=(AWorkingThreadPacketFilter and wtfp_Protocol)) then
          bExit:=TRUE;// stop processing packets
    end else if (PT_Application_Layer=pPacket^.Packet_Type) and (Pid_Product_Data=pPacket^.Packet_ID) then begin
      // Pid_Product_Data packets
      Internal_Parse_Product_Data(pPacket^.Data_Size, PProduct_Data_Type(@(pPacket^.Data)));
    end else if (PT_Application_Layer=pPacket^.Packet_Type) and (Pid_Ext_Product_Data=pPacket^.Packet_ID) then begin
      // Pid_Ext_Product_Data packets
      Internal_Parse_Ext_Product_Data(pPacket^.Data_Size, PExt_Product_Data_Type(@(pPacket^.Data)));
    end else if (PT_Application_Layer=pPacket^.Packet_Type) and (L001_Pid_Xfer_Cmplt=pPacket^.Packet_ID) then begin
      // Complete (note L002_Pid_Xfer_Cmplt=L001_Pid_Xfer_Cmplt = 12)
      if (not bExit) and (0<>AWorkingThreadPacketFilter) then
        if (wtfp_Complete=(AWorkingThreadPacketFilter and wtfp_Complete)) then
          bExit:=TRUE; // stop processing packets
    end else if (PT_Protocol_Layer=pPacket^.Packet_Type) and (A010_Cmnd_Abort_Transfer=pPacket^.Packet_ID) then begin
      // Aborted (note A011_Cmnd_Abort_Transfer=A010_Cmnd_Abort_Transfer = 0)
      if (not bExit) and (0<>AWorkingThreadPacketFilter) then
        if (wtfp_Aborted=(AWorkingThreadPacketFilter and wtfp_Aborted)) then
          bExit:=TRUE; // stop processing packets
    end else if (PT_Protocol_Layer=pPacket^.Packet_Type) and (Pid_Session_Started=pPacket^.Packet_ID) then begin
      // Session_Started - save unit number
      uniq_id:=PDWORD(@(pPacket^.Data))^; // be aware of sign
      InternalSetUnitInfo(guik_Unique_Unit_Number, IntToStr(uniq_id));
      if (not bExit) and (0<>AWorkingThreadPacketFilter) then
        if (wtfp_Session=(AWorkingThreadPacketFilter and wtfp_Session)) then
          bExit:=TRUE; // stop processing packets
    end else begin
      // others
      // Internal_Read_Packets_Handler(pPacket);
    end;
  end;

begin
  // read packets and push them all to queue
  Result:=FALSE;
  pPacket:=nil;

  // Read async data until the driver returns less than the
  // max async data size, which signifies the end of a packet
  if (not _NoIOCTL) then
  repeat
    Sleep(0);
    // get async packet
    pPacket:=ZwRecvIoctlAsync(@iosb,ASYNC_DATA_SIZE);
    // check Xfer_Complete and others
    _CheckInternalPacketFilter(Result);
    // add to queue
    FExternal_Queue.AppendGPSPacket(pPacket, FUnitIndex);
    // check returned data size
    if (ASYNC_DATA_SIZE<>iosb.Information) then
      break;
  until FALSE;

  // If this was a small "signal" packet, read a real packet using ReadFile
  if (_NoIOCTL) or
     (nil=pPacket) or
     ((PT_Protocol_Layer=pPacket^.Packet_Type) and (Pid_Data_Available=pPacket^.Packet_ID)) then begin
    // A full implementation would keep reading (and queueing)
    // packets until the driver returns a 0 size buffer.
    repeat
      Sleep(0);

      // get packet
      pPacket:=ZwRecvReadPacket(@iosb);

      // workaround for some bugs in some devices
      if FRepeatReadOnCrazyDataSize and (0<>AWorkingThreadPacketFilter) then
      if (nil<>pPacket) and (pPacket^.Data_Size>=MAX_BUFFER_SIZE) then begin
        FExternal_Queue.AppendGPSPacket(pPacket, FUnitIndex);
        pPacket:=ZwRecvReadPacket(@iosb);
      end;

      // check Xfer_Complete and others
      _CheckInternalPacketFilter(Result);

      // add to queue
      FExternal_Queue.AppendGPSPacket(pPacket, FUnitIndex);

      // check returned data size
      if (0=iosb.Information) then
        break;

      // workaround for some bugs in some devices
      if FExitOnLastProtocolPacket and Result then
        if (wtfp_Protocol=(AWorkingThreadPacketFilter and wtfp_Protocol)) then
          break;
    until FALSE;
  end;
end;

function Tvsagps_device_usb_garmin.WorkingThread_SendPacket: Boolean;
var
  buf14: TGarminUSB_Custom_Packet;
  iosb: IO_STATUS_BLOCK;
  cmd_Packet_ID: Word;
begin
  // send start pvt packet
  // if L002 then use L002_Pid_Command_Data (very rare!) else use L001_Pid_Command_Data (usually)
  cmd_Packet_ID:=L001_Pid_Command_Data;
  if (0<FSupportedProtocols.Count) and (0<=FSupportedProtocols.IndexOf('L002')) then
    cmd_Packet_ID:=L002_Pid_Command_Data;
  // send packet
  InitializeUSBGarminPacket(@buf14, PT_Application_Layer, cmd_Packet_ID, 2);
  buf14.Data[0]:=A010_Cmnd_Start_Pvt_Data; // no analog in A011 protocol
  Result:=ZwSendPacket(@buf14, @iosb);
end;

function Tvsagps_device_usb_garmin.WorkingThread_StartSession: Boolean;
var
  dwData: DWORD;
  buf12: TGarminUSB_Custom_Packet;
  iosb: IO_STATUS_BLOCK;
begin
  Result:=FALSE;

  // get packet size
  dwData:=0;
  if (not ZwCallIoctl4(IOCTL_USB_PACKET_SIZE, @iosb, @dwData)) then
    Exit;
  FDevicePacketSize:=dwData;

  // get api version
  dwData:=0;
  if (not ZwCallIoctl4(IOCTL_API_VERSION, @iosb, @dwData)) then
    Exit;
  FAPIVersion:=dwData;

  // send start_session packet
  InitializeUSBGarminPacket(@buf12, PT_Protocol_Layer, Pid_Start_Session); // 00 00 00 00 05 00 00 00 00 00 00 00
  if (not ZwSendPacket(@buf12, @iosb)) then
    Exit;

  // wait for recv session_started packet
  WorkingThread_Process_Packets(wtfp_Session);

  // send Product_Rqst - query supported protocols
  InitializeUSBGarminPacket(@buf12, PT_Application_Layer, Pid_Product_Rqst);
  if (not ZwSendPacket(@buf12, @iosb)) then
    Exit;

  // wait for recv session_started packet
  WorkingThread_Process_Packets(wtfp_Protocol);

  // running ok
  Result:=TRUE;
end;

function Tvsagps_device_usb_garmin.ZwCallIoctl4(AIoControlCode: ULONG; iosb: PIO_STATUS_BLOCK; pData: PDWORD): LongBool;
{$if defined(USE_NATIVE_NT_API)}
var
  hEvent: THandle;
  ns: NTSTATUS;
  li: LARGE_INTEGER;
{$ifend}
begin
{$if defined(USE_NATIVE_NT_API)}
  Result:=FALSE;
  if (STATUS_SUCCESS=VSAGPS_CreateEvent(@hEvent)) then
  try
    // ioctl
    ns:=NtDeviceIoControlFile(FGPSDeviceHandle,
                              hEvent,
                              nil,
                              iosb,
                              iosb,
                              AIoControlCode,
                              nil,
                              0,
                              pData,
                              sizeof(DWORD));
    // wait
    if (STATUS_PENDING=ns) then begin
      li.QuadPart:=-12000000; // 1.2 msec max
      ns:=NtWaitForSingleObject(hEvent, FALSE, @li);
    end;
    // check
    if (STATUS_SUCCESS=ns) then begin
      Result:=TRUE;
    end;
  finally
    NtClose(hEvent);
  end;
{$else}
  Result:=DeviceIoControl(FGPSDeviceHandle,
                          AIoControlCode,
                          nil,
                          0,
                          pData,
                          sizeof(DWORD),
                          iosb^.Information,
                          nil);
  if Result then
    iosb^.Status:=STATUS_SUCCESS
  else
    iosb^.Status:=GetLastError;
{$ifend}
end;

function Tvsagps_device_usb_garmin.ZwRecvIoctlAsync(iosb: PIO_STATUS_BLOCK; AOutputBufferSize: DWORD): PGarminUSB_Custom_Packet;
var
  p: PGarminUSB_Custom_Packet;
{$if defined(USE_NATIVE_NT_API)}
  hEvent: THandle;
  ns: NTSTATUS;
  li: LARGE_INTEGER;
{$ifend}
begin
  Result:=nil;
  p:=nil;
{$if defined(USE_NATIVE_NT_API)}
  if (STATUS_SUCCESS=VSAGPS_CreateEvent(@hEvent)) then
{$ifend}
  try
    // ioctl
    p:=VSAGPS_GetMem(AOutputBufferSize);
{$if defined(USE_NATIVE_NT_API)}
    ns:=NtDeviceIoControlFile(FGPSDeviceHandle,
                              hEvent,
                              nil,
                              iosb,
                              iosb,
                              IOCTL_ASYNC_IN,
                              nil,
                              0,
                              p,
                              AOutputBufferSize);
    // wait
    if (STATUS_PENDING=ns) then begin
      li.QuadPart:=-12000000; // 1.2 msec max
      ns:=NtWaitForSingleObject(hEvent, FALSE, @li);
    end;
    // check
    if (STATUS_SUCCESS=ns) then begin
      Result:=p;
      p:=nil;
    end;
{$else}
    if DeviceIoControl(FGPSDeviceHandle, IOCTL_ASYNC_IN, nil, 0, p, AOutputBufferSize, iosb^.Information, nil) then begin
      // ok
      iosb^.Status:=STATUS_SUCCESS;
      Result:=p;
      p:=nil;
    end else begin
      // failed
      iosb^.Status:=GetLastError;
    end;
{$ifend}
  finally
{$if defined(USE_NATIVE_NT_API)}
    NtClose(hEvent);
{$ifend}
    VSAGPS_FreeMem(p);
  end;
end;

function Tvsagps_device_usb_garmin.ZwRecvReadPacket(iosb: PIO_STATUS_BLOCK): PGarminUSB_Custom_Packet;
var
  p: PGarminUSB_Custom_Packet;
{$if defined(USE_NATIVE_NT_API)}
  hEvent: THandle;
  ns: NTSTATUS;
  li: LARGE_INTEGER;
{$ifend}
begin
  Result:=nil;
  p:=nil;
{$if defined(USE_NATIVE_NT_API)}
  if (STATUS_SUCCESS=VSAGPS_CreateEvent(@hEvent)) then
{$ifend}
  try
    p:=VSAGPS_GetMem(MAX_BUFFER_SIZE);
{$if defined(USE_NATIVE_NT_API)}
    li.QuadPart:=0;
    ns:=NtReadFile(FGPSDeviceHandle,
                   hEvent,
                   nil,
                   iosb,
                   iosb,
                   p,
                   MAX_BUFFER_SIZE,
                   @li,
                   nil);
    // wait
    if (STATUS_PENDING=ns) then begin
      li.QuadPart:=-12000000; // 1.2 msec max
      ns:=NtWaitForSingleObject(hEvent, FALSE, @li);
    end;
    // check
    if (STATUS_SUCCESS=ns) then begin
      Result:=p;
      p:=nil;
    end;
{$else}
    if ReadFile(FGPSDeviceHandle, p^, MAX_BUFFER_SIZE, iosb^.Information, nil) then begin
      // ok
      iosb^.Status:=STATUS_SUCCESS;
      Result:=p;
      p:=nil;
    end else begin
      // failed
      iosb^.Status:=GetLastError;
    end;
{$ifend}
  finally
{$if defined(USE_NATIVE_NT_API)}
    NtClose(hEvent);
{$ifend}
    VSAGPS_FreeMem(p);
  end;
end;

function Tvsagps_device_usb_garmin.ZwSendPacket(pPacket: PGarminUSB_Custom_Packet; iosb: PIO_STATUS_BLOCK): LongBool;
var
  dwWriteBytes: DWORD;
{$if defined(USE_NATIVE_NT_API)}
  hEvent: THandle;
  ns: NTSTATUS;
  li: LARGE_INTEGER;
{$else}
  dwDummy: DWORD;
{$ifend}
begin
{$if defined(USE_NATIVE_NT_API)}
  Result:=FALSE;
{$ifend}
  // create event
{$if defined(USE_NATIVE_NT_API)}
  if (STATUS_SUCCESS=VSAGPS_CreateEvent(@hEvent)) then
  try
{$ifend}
    // write buffer
    dwWriteBytes:=GarminUSB_Custom_Packet_Base_Size+pPacket^.Data_Size;
{$if defined(USE_NATIVE_NT_API)}
    li.QuadPart:=0;
    ns:=NtWriteFile(FGPSDeviceHandle,
                    hEvent,
                    nil,
                    iosb,
                    iosb,
                    pPacket,
                    dwWriteBytes,
                    @li,
                    nil);

    // wait
    if (STATUS_PENDING=ns) then begin
      ns:=NtWaitForSingleObject(hEvent, FALSE, nil);
    end;
    if (STATUS_SUCCESS=ns) then begin
      Result:=TRUE;
    end;
{$else}
    Result:=WriteFile(FGPSDeviceHandle, pPacket^, dwWriteBytes, iosb^.Information, nil);
    if Result then
      iosb^.Status:=STATUS_SUCCESS
    else
      iosb^.Status:=GetLastError;
{$ifend}

    if Result then begin
      // If the packet size was an exact multiple of the USB packet
      // size, we must make a final write call with no data
      if (0<FDevicePacketSize) then
      if (0=(iosb^.Information mod FDevicePacketSize)) then begin
        // no event
        // no result
{$if defined(USE_NATIVE_NT_API)}
        li.QuadPart:=0;
        NtWriteFile(FGPSDeviceHandle,0,nil,iosb,iosb,nil,0,@li,nil);
{$else}
        WriteFile(FGPSDeviceHandle, nil^, 0, dwDummy, nil);
{$ifend}
      end;
    end;
{$if defined(USE_NATIVE_NT_API)}
  finally
    NtClose(hEvent);
  end;
{$ifend}
end;

end.










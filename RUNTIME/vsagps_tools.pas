(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_tools;
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
  vsagps_public_types;

const
  // params for simple waiting internal working thread
  // on disconnect
  cWorkingThread_SimpleWait_Count = 8;
  cWorkingThread_SimpleWait_Msec  = 128;
  cWorkingThread_InLoopWait_Msec  = 128;

  // internal working thread running options
  wtro_One_Packet_Loop       = $00000001;
  wtro_IOCTL_Disabled        = $00000002;

  // internal working thread finish packets
  wtfp_Aborted   = $00000001;
  wtfp_Complete  = $00000002;
  wtfp_Session   = $00000004;
  wtfp_Protocol  = $00000008;
  wtfp_AnyPacket = $00000010;

  STATUS_SUCCESS = $00000000;
  STATUS_BUFFER_TOO_SMALL = $C0000023;

  OBJ_CASE_INSENSITIVE = $00000040;

const
  GUID_DEVINTERFACE_GRMNUSB: TGUID = '{2C9C45C2-8E7D-4C08-A12D-816BBAE722C0}';
  GUID_BTHPORT_DEVICE_INTERFACE: TGUID = '{0850302A-B344-4fda-9BE9-90576B8D46F0}';

const
  cNmea_NT_COM_Global = '\GLOBAL??\';


// return number of added/inserted lines
function parse_nullstrings_to_strings(AData_Size: DWORD;
                                      ABuffer: PAnsiChar;
                                      AStrings: TStrings;
                                      const ATrim: Boolean;
                                      const ASourceIsWide: Boolean): Byte;



function VSAGPS_StringList_To_PCharList(sl: TStrings): PVSAGPS_PCHAR_LIST;

implementation

uses
  vsagps_memory;

function parse_nullstrings_to_strings(AData_Size: DWORD;
                                      ABuffer: PAnsiChar;
                                      AStrings: TStrings;
                                      const ATrim: Boolean;
                                      const ASourceIsWide: Boolean): Byte;

var
  sym_size: Byte;

  procedure _Add_to_Obj(var s: AnsiString; var res: Byte);
  begin
    if ATrim and (Length(s)>0) then begin
      //
      s:=Trim(s);
      s:=StringReplace(s,#13,'',[rfReplaceAll]);
      s:=StringReplace(s,#10,'',[rfReplaceAll]);
    end;

    // just append - skip empty lines
    if (Length(s)>0) then begin
      AStrings.Append(s);
    end;

    Inc(res);
  end;

var
  s: AnsiString;
  c: AnsiChar;
  wc: WideChar;
  bz: Boolean;
begin
  Result:=0;
  s:='';
  // symbol size
  sym_size:=Ord(ASourceIsWide);
  Inc(sym_size);

  while (AData_Size>0) do begin
    // parse
    if (ASourceIsWide) then begin
      wc:=PWideChar(ABuffer)^;
      bz:=(#0=wc);
      if (not bz) then
        s:=s+wc;
    end else begin
      c:=ABuffer^;
      bz:=(#0=c);
      if (not bz) then
        s:=s+c;
    end;
    if bz then begin
      // end of single line
      _Add_to_Obj(s, Result);
      s:='';
    end;

    // next
    ABuffer:=PAnsiChar(Pointer(DWORD(Pointer(ABuffer))+sym_size));
    AData_Size:=AData_Size-sym_size;
  end;

  if Length(s)>0 then
    _Add_to_Obj(s, Result);
end;


function VSAGPS_StringList_To_PCharList(sl: TStrings): PVSAGPS_PCHAR_LIST;
var
{$if defined(USE_SIMPLE_CLASSES)}
  EnumPtr: Pvsagps_list_item;
  PayloadPtr: Pointer;
{$ifend}
  i: Integer;
  iSize: DWORD;
  pLines: PAnsiChar; // pointer to text of first item
  s: AnsiString;
begin
  Result:=nil;
  if (nil<>sl) then
  if (0<sl.Count) then begin
    // calc required buffer size (total size)
    iSize:=sizeof(TVSAGPS_PCHAR_LIST); // reserved one pointer - for internal use

{$if defined(USE_SIMPLE_CLASSES)}
    EnumPtr:=nil;
    while sl.EnumItems(EnumPtr, PayloadPtr) do begin
      iSize:=sizeof(PAnsiChar)+
             sizeof(AnsiChar)+
             StrLen(PAnsiChar(PayloadPtr))+
             iSize;
    end;
{$else}
    for i := 0 to sl.Count-1 do
      iSize:=sizeof(PAnsiChar)+ // pointer to array
             sizeof(AnsiChar)+ // null-terminated ansichar
             DWORD(Length(sl[i]))+ // real string length
             iSize;
{$ifend}

    // make buffer
    Result:=VSAGPS_GetMem(iSize);
    if (nil<>Result) then
    try
      // count of lines
      Result^.dwCount:=sl.Count;
      // starting position
      pLines:=PAnsiChar(Pointer(@(Result.szItems[Result.dwCount])));
      // add lines
{$if defined(USE_SIMPLE_CLASSES)}
      EnumPtr:=nil;
      i:=0;
      while sl.EnumItems(EnumPtr, PayloadPtr) do begin
        CopyMemory(pLines, PAnsiChar(PayloadPtr), StrLen(PAnsiChar(PayloadPtr))+1);
        Result^.szItems[i]:=pLines;
        pLines:=PAnsiChar(Pointer(DWORD(Pointer(pLines))+DWORD(Length(s))+1));
      end;
{$else}
      for i := 0 to sl.Count-1 do begin
        s:=sl[i];
        CopyMemory(pLines, PAnsiChar(s), Length(s));
        pLines[Length(s)]:=#0;
        Result^.szItems[i]:=pLines;
        pLines:=PAnsiChar(Pointer(DWORD(Pointer(pLines))+DWORD(Length(s))+1));
      end;
{$ifend}
    except
      VSAGPS_FreeMem(Result);
      Result:=nil;
    end;
  end;
end;

end.



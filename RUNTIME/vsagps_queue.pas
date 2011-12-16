(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_queue;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$if defined(USE_SIMPLE_CLASSES)}
  vsagps_classes,
{$else}
  Classes,
{$ifend}
  SysUtils;

type
{$if not defined(USE_SIMPLE_CLASSES)}
  Tvsagps_stdlist_item = packed record
    p: Pointer;
    u: Pointer;
  end;
  Pvsagps_stdlist_item = ^Tvsagps_stdlist_item;
{$ifend}

  // queue for packets
  Tvsagps_queue = class
{$if defined(USE_SIMPLE_CLASSES)}
  (Tvsagps_List)
{$else}
  (TList)
{$ifend}
  private
    FCS: TRTLCriticalSection;
    FDenyAdd: Boolean;
{$if not defined(USE_SIMPLE_CLASSES)}
    // if defined - use routine from base class
    function InternalExtractGPSPacket(var x: Pvsagps_stdlist_item): Boolean;
{$ifend}
  public
    constructor Create;
{$if defined(USE_SIMPLE_CLASSES)}
    override;
{$ifend}
    destructor Destroy; override;
    // empty queue (before free)
    procedure FreeAllPackets;
    // insert data
    procedure AppendGPSPacket(const p: Pointer; const uindex: Byte);
    // pull data
    function ExtractGPSPacket(var p: Pointer; var uindex: Byte): Boolean;
  end;

implementation

{$if not defined(USE_SIMPLE_CLASSES)}
uses
  vsagps_memory;
{$ifend}

{ Tvsagps_queue }

procedure Tvsagps_queue.AppendGPSPacket(const p: Pointer; const uindex: Byte);
var
  dw: DWORD;
{$if not defined(USE_SIMPLE_CLASSES)}
  x: Pvsagps_stdlist_item;
{$ifend}
begin
  if FDenyAdd then
    Exit;
  dw:=uindex;
  EnterCriticalSection(FCS);
  try
{$if defined(USE_SIMPLE_CLASSES)}
    InternalAppendItem(p, Pointer(dw));
{$else}
    New(x);
    x^.p:=p;
    x^.u:=Pointer(dw);
    Self.Add(x);
{$ifend}
  finally
    LeaveCriticalSection(FCS);
  end;
end;

constructor Tvsagps_queue.Create;
begin
{$if defined(USE_SIMPLE_CLASSES)}
  inherited Create;
{$ifend}
  FDenyAdd:=FALSE;
  InitializeCriticalSection(FCS);
end;

destructor Tvsagps_queue.Destroy;
begin
  FDenyAdd:=TRUE;
  FreeAllPackets;
  DeleteCriticalSection(FCS);
  inherited;
end;

function Tvsagps_queue.ExtractGPSPacket(var p: Pointer; var uindex: Byte): Boolean;
var
{$if defined(USE_SIMPLE_CLASSES)}
  u: Pointer;
{$else}
  x: Pvsagps_stdlist_item;
{$ifend}
begin
  Result:=FALSE;
  p:=nil;
  if (nil=Self) then
    Exit;
  EnterCriticalSection(FCS);
  try
{$if defined(USE_SIMPLE_CLASSES)}
    Result:=InternalExtractItem(p, u);
    uindex:=LoByte(LoWord(DWORD(u)));
{$else}
    Result:=InternalExtractGPSPacket(x);
    if Result then begin
      p:=x^.p;
      uindex:=LoByte(LoWord(DWORD(x^.u)));
      Dispose(x);
    end;
{$ifend}
  finally
    LeaveCriticalSection(FCS);
  end;
end;

procedure Tvsagps_queue.FreeAllPackets;
{$if not defined(USE_SIMPLE_CLASSES)}
var x: Pvsagps_stdlist_item;
{$ifend}
begin
  if (nil=Self) then
    Exit;
  EnterCriticalSection(FCS);
  try
{$if defined(USE_SIMPLE_CLASSES)}
    InternalFreeAllItems;
{$else}
    while InternalExtractGPSPacket(x) do begin
      VSAGPS_FreeMem(x^.p);
      Dispose(x);
    end;
{$ifend}
  finally
    LeaveCriticalSection(FCS);
  end;
end;

{$if not defined(USE_SIMPLE_CLASSES)}
function Tvsagps_queue.InternalExtractGPSPacket(var x: Pvsagps_stdlist_item): Boolean;
begin
  Result:=FALSE;
  if Self.Count>0 then begin
    x:=Self.Items[0];
    Self.Delete(0);
    Result:=TRUE;
  end;
end;
{$ifend}

end.
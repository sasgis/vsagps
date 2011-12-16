(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_memory;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

function VSAGPS_GetMem(const dwBytes: DWORD): Pointer; stdcall;

function VSAGPS_GetMemZ(const dwBytes: DWORD): Pointer;

procedure VSAGPS_FreeMem(p: Pointer); stdcall;

procedure VSAGPS_FreeAndNil_PChar(var p: PChar);
procedure VSAGPS_FreeAndNil_PWideChar(var p: PWideChar);

function VSAGPS_AllocPCharByString(const s: String; const aNILforEmpty: Boolean): PChar;

function VSAGPS_AllocPCharByPChar(const pSrc: PChar; const aNILforEmpty: Boolean): PChar;

// deserialize packet from string
function VSAGPS_AllocPByteByString(const s: String; const dwMinSize: DWORD): PByte;
// serialize packet to string and allocate pchar
function VSAGPS_AllocPCharByPByte(const pSrc: PByte; const dwLen: DWORD): PChar;

implementation

function VSAGPS_GetMem(const dwBytes: DWORD): Pointer;
begin
  Result:=HeapAlloc(GetProcessHeap, HEAP_GENERATE_EXCEPTIONS, dwBytes);
end;

function VSAGPS_GetMemZ(const dwBytes: DWORD): Pointer;
begin
  Result:=HeapAlloc(GetProcessHeap, (HEAP_GENERATE_EXCEPTIONS or HEAP_ZERO_MEMORY), dwBytes);
end;

procedure VSAGPS_FreeMem(p: Pointer);
begin
  if (nil<>p) then
    HeapFree(GetProcessHeap, 0, p);
end;

procedure VSAGPS_FreeAndNil_PChar(var p: PChar);
begin
  if (nil<>p) then begin
    VSAGPS_FreeMem(p);
    p:=nil;
  end;
end;

procedure VSAGPS_FreeAndNil_PWideChar(var p: PWideChar);
begin
  if (nil<>p) then begin
    VSAGPS_FreeMem(p);
    p:=nil;
  end;
end;

function VSAGPS_AllocPCharByString(const s: String; const aNILforEmpty: Boolean): PChar;
var d: Integer;
begin
  Result:=nil;
  d:=Length(s);

  if aNILforEmpty and (0=d) then
    Exit;

  Result:=VSAGPS_GetMem(d+1);
  if (0<d) then
    CopyMemory(Result, PChar(s), d);
  Result[d]:=#0;
end;

function VSAGPS_AllocPCharByPChar(const pSrc: PChar; const aNILforEmpty: Boolean): PChar;
var d: Integer;
begin
  Result:=nil;

  if (nil=pSrc) then
    Exit;

  d:=StrLen(pSrc);

  if aNILforEmpty and (0=d) then
    Exit;

  Result:=VSAGPS_GetMem(d+1);
  CopyMemory(Result, pSrc, d+1);
end;

function VSAGPS_AllocPByteByString(const s: String; const dwMinSize: DWORD): PByte;
var
  v: Integer;
  cur: PByte;
  i,siz,buf_len: DWORD;
begin
  Result:=nil;
  siz := (Length(s) div 2);
  if (0<siz) and (siz * 2 = DWORD(Length(s))) then begin
    buf_len:=siz;
    if (0<dwMinSize) and (siz<dwMinSize) then
      siz:=dwMinSize;
    Result:=VSAGPS_GetMem(siz);
    try
      cur:=Result;
      for i := 0 to siz-1 do begin
        if (i<buf_len) then begin
          v:=StrToint('0x'+s[2*i+1]+s[2*i+2]);
          cur^:=LoByte(LoWord(v));
        end else begin
          cur^:=0;
        end;
        Inc(cur);
      end;
    except
      VSAGPS_FreeMem(Result);
      Result:=nil;
    end;
  end;
end;

function VSAGPS_AllocPCharByPByte(const pSrc: PByte; const dwLen: DWORD): PChar;
var
  i: DWORD;
  cur: PByte;
  s: String;
begin
  Result:=nil;
  if (nil<>pSrc) and (0<dwLen) then
  try
    s:='';
    cur:=pSrc;
    for i := 0 to dwLen-1 do begin
      s:=s+IntToHex(cur^,2);
      Inc(cur);
    end;
    s:=s+#13#10;
    Result:=VSAGPS_AllocPCharByString(s, TRUE);
  except
    VSAGPS_FreeAndNil_PChar(Result);
  end;
end;

end.
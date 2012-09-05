(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_public_sysutils;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

type
  TVSAGPS_DivideStringToLines_Proc = procedure (const ALine: AnsiString; const AUserPtr: Pointer) of object;
  TVSAGPS_DivideWideStringToLines_Proc = procedure (const ALine: WideString; const AUserPtr: Pointer) of object;

function StrLenW(Src: PWideChar): DWORD;

function VSAGPS_CreateFileW(const phFile: PHandle;
                            const pSrcFileName: PWideChar;
                            const bOpenExisting: Boolean): Boolean;

function VSAGPS_GetFileSize(const hFile: THandle; var iSize: Int64): Boolean;

procedure SafeSetStringP(var S: AnsiString; Buffer: PAnsiChar);
procedure SafeSetStringL(var S: AnsiString; Buffer: PAnsiChar; Length: Integer);

procedure SafeSetWideStringP(var WS: WideString; WBuffer: PWideChar);
procedure SafeSetWideStringL(var WS: WideString; WBuffer: PWideChar; Length: Integer);

procedure VSAGPS_DividePCharToLines(const ASource: PAnsiChar;
                                    const AProc: TVSAGPS_DivideStringToLines_Proc;
                                    const AUserPtr: Pointer;
                                    const ADivOnSpacesToo: Boolean;
                                    ApExitIfSet: PBoolean);

procedure VSAGPS_DividePWideCharToLines(const APWideChar: PWideChar;
                                        const AProc: TVSAGPS_DivideWideStringToLines_Proc;
                                        const AUserPtr: Pointer;
                                        const ADivOnSpacesToo: Boolean;
                                        ApExitIfSet: PBoolean);

function VSAGPS_WideString_To_Double(const src: WideString;
                                     var dbl: Double;
                                     const fs: TFormatSettings): Boolean;

function VSAGPS_WideString_To_Byte(const src: WideString;
                                   var v: Byte): Boolean;

function VSAGPS_WideString_to_ISO8601_Time(const src: WideString;
                                           const pdt: PDateTime): Boolean;

function ExtractBeforeSpaceDelimiter(var ASourceString: WideString): WideString;

implementation

function StrLenW(Src: PWideChar): DWORD;
begin
  Result:=0;
  if (nil<>Src) then
  while (Src^<>#0) do begin
    Inc(Result);
    Inc(Src);
  end;
end;

function VSAGPS_CreateFileW(const phFile: PHandle;
                            const pSrcFileName: PWideChar;
                            const bOpenExisting: Boolean): Boolean;
var
  dwDesiredAccess, dwShareMode, dwCreationDisposition: DWORD;
begin
  if bOpenExisting then begin
    // open existing file
    dwDesiredAccess:=GENERIC_READ;
    dwShareMode:=(FILE_SHARE_READ or FILE_SHARE_WRITE);
    dwCreationDisposition:=OPEN_EXISTING;
  end else begin
    // create new file
    dwDesiredAccess:=(GENERIC_READ or GENERIC_WRITE);
    dwShareMode:=FILE_SHARE_READ;
    dwCreationDisposition:=CREATE_ALWAYS;
  end;

  phFile^:=CreateFileW(pSrcFileName,
                       dwDesiredAccess,
                       dwShareMode,
                       nil,
                       dwCreationDisposition,
                       FILE_ATTRIBUTE_NORMAL,
                       0);

  if (INVALID_HANDLE_VALUE=phFile^) then
    phFile^:=0;
  Result:=(0<>phFile^);
end;

function VSAGPS_GetFileSize(const hFile: THandle; var iSize: Int64): Boolean;
var dwLO, dwHI: DWORD;
begin
  Result:=FALSE;
  dwLO:=GetFileSize(hFile, @dwHI);
  if (INVALID_FILE_SIZE<>dwLO) or ((INVALID_FILE_SIZE=dwLO) and (NO_ERROR=GetLastError)) then begin
    Result:=TRUE;
    with Int64Rec(iSize) do begin
      Lo:=dwLO;
      Hi:=dwHI;
    end;
  end;
end;

procedure SafeSetStringP(var S: AnsiString; Buffer: PAnsiChar);
var L: Integer;
begin
  if (nil=Buffer) then
    S:=''
  else begin
    L:=StrLen(Buffer);
    if (0=L) then
      S:=''
    else
      SetString(S, Buffer, L);
  end;
end;

procedure SafeSetStringL(var S: AnsiString; Buffer: PAnsiChar; Length: Integer);
begin
  if (nil=Buffer) or (0=Length) then
    S:=''
  else
    SetString(S, Buffer, Length);
end;

procedure SafeSetWideStringP(var WS: WideString; WBuffer: PWideChar);
var L: Integer;
begin
  if (nil=WBuffer) then
    WS:=''
  else begin
    L:=StrLenW(WBuffer);
    if (0=L) then
      WS:=''
    else
      SetString(WS, WBuffer, L);
  end;
end;

procedure SafeSetWideStringL(var WS: WideString; WBuffer: PWideChar; Length: Integer);
begin
  if (nil=WBuffer) or (0=Length) then
    WS:=''
  else
    SetString(WS, WBuffer, Length);
end;

procedure VSAGPS_DividePCharToLines(const ASource: PAnsiChar;
                                    const AProc: TVSAGPS_DivideStringToLines_Proc;
                                    const AUserPtr: Pointer;
                                    const ADivOnSpacesToo: Boolean;
                                    ApExitIfSet: PBoolean);
var
  p,pStart: PAnsiChar;
  str_line: AnsiString;
begin
  if (nil=ASource) then
    Exit;
  p:=ASource;
  // find lines
  while (#0<>p^) do begin
    // check if aborted by user
    if (nil<>ApExitIfSet) then
      if (ApExitIfSet^) then
        break;
    // work
    pStart:=p;
    while TRUE do begin
      if (p^ in [#0, #10, #13]) then
        break;
      if ADivOnSpacesToo and (p^ in [#32, #160]) then
        break;
      Inc(p);
    end;
    SafeSetStringL(str_line, pStart, p-pStart);
    AProc(str_line, AUserPtr);
    if (#13=p^) then
      Inc(p);
    if (#10=p^) then
      Inc(p);
    if ADivOnSpacesToo and (p^ in [#32, #160]) then
      Inc(p);
  end;
end;

procedure VSAGPS_DividePWideCharToLines(const APWideChar: PWideChar;
                                        const AProc: TVSAGPS_DivideWideStringToLines_Proc;
                                        const AUserPtr: Pointer;
                                        const ADivOnSpacesToo: Boolean;
                                        ApExitIfSet: PBoolean);
var
  p,pStart: PWideChar;
  str_line: WideString;
begin
  if (nil=APWideChar) then
    Exit;
  p:=APWideChar;
  // find lines
  while (#0<>p^) do begin
    // check if aborted by user
    if (nil<>ApExitIfSet) then
      if (ApExitIfSet^) then
        break;
    // work
    pStart:=p;
    while TRUE do begin
      if (#0=p^) or (#10=p^) or (#13=p^) then
        break;
      if ADivOnSpacesToo and ((#32=p^) or (#160=p^)) then
        break;
      Inc(p);
    end;
    SafeSetWideStringL(str_line, pStart, p-pStart);
    AProc(str_line, AUserPtr);
    if (#13=p^) then
      Inc(p);
    if (#10=p^) then
      Inc(p);
    if ADivOnSpacesToo and ((#32=p^) or (#160=p^)) then
      Inc(p);
  end;
end;


function VSAGPS_WideString_To_Double(const src: WideString;
                                     var dbl: Double;
                                     const fs: TFormatSettings): Boolean;
begin
  try
    Result:=TryStrToFloat(src, dbl, fs);
  except
    Result:=FALSE;
  end;
end;

function VSAGPS_WideString_To_Byte(const src: WideString;
                                   var v: Byte): Boolean;
var i: Integer;
begin
  Result:=TryStrToInt(src, i) and (i>0) and (i<=$FF);
  if Result then
    v:=LoByte(LoWord(i));
end;

function VSAGPS_Digit(const wc: WideChar): Boolean;
begin
  Result:=('0'=wc) or (('1'<=wc) and ('9'>=wc));
end;

function VSAGPS_ExtractNum(const src: WideString;
                           var i, V: Integer;
                           const check_minmax: Boolean;
                           const minvalue,maxvalue: SmallInt;
                           const check_len: Boolean;
                           const lenvalue: Byte): Boolean;
var
  cnt: Byte;
  L: Integer;
begin
  Result:=FALSE;
  V:=0;
  cnt:=0;
  L:=Length(src);

  while (i<=L) and VSAGPS_Digit(src[i]) do begin
    V:=V*10+StrToInt(src[i]);
    Inc(cnt);
    // check break
    if (check_minmax) and (maxvalue<V) then
      Exit;
    if (check_len) and (lenvalue<cnt) then
      Exit;
    // next
    Inc(i);
  end;

  if check_len then begin
    // expand
    while (lenvalue>cnt) do begin
      V:=V*10;
      Inc(cnt);
    end;
    Result:=TRUE;
  end;

  if check_minmax then
    Result:=((maxvalue>=V) and (minvalue<=V));
end;

function VSAGPS_WideString_to_ISO8601_Time(const src: WideString;
                                           const pdt: PDateTime): Boolean;
const
  parser_maxyear = 2048;
var
  i,L,V: Integer;
  st: TSystemTime;
  //dt: TDateTime;
  sep: WideChar;

  function _ExtractFirstInt(const minvalue,maxvalue: SmallInt): Boolean;
  begin
    Result:=VSAGPS_ExtractNum(src,i,V,TRUE,minvalue,maxvalue,FALSE,0);
  end;

  function _ExtractFrac(const numlen: Byte): Boolean;
  begin
    Result:=VSAGPS_ExtractNum(src,i,V,FALSE,0,0,TRUE,numlen);
  end;

  function _CheckSeparator: Boolean;
  begin
    if (i>L) then
      Result:=FALSE
    else if (#0=sep) then begin
      // save (except T and Z and points)
      sep:=src[i];
      Result:=(('T'<>sep) and ('Z'<>sep) and ('.'<>sep) and (','<>sep));
    end else begin
      // check
      Result:=(sep=src[i]);
    end;
    if Result then
      Inc(i);
  end;

  function _CheckWChar(const wc1, wc2: WideChar): Boolean;
  begin
    if (i>L) then
      Result:=FALSE
    else
      Result:=(wc1=src[i]) or (wc2=src[i]);
    if Result then begin
      sep:=#0;
      Inc(i);
    end;
  end;
begin
  // 2011-11-30T21:13:05.973Z (ISO 8601 - not fully implemented)
  // 2011-12-08T11:23:57+04:00Z
  Result:=FALSE;
  sep:=#0;
  Zeromemory(@st, sizeof(st));

  L:=Length(src);
  i:=1;

  // Year
  if _ExtractFirstInt(0, parser_maxyear) then
    st.wYear:=V
  else
    Exit;
  // separator
  if not _CheckSeparator then
    Exit;
  // Month
  if _ExtractFirstInt(1,12) then
    st.wMonth:=V
  else
    Exit;
  // separator
  if not _CheckSeparator then
    Exit;
  // Day
  if _ExtractFirstInt(1,31) then
    st.wDay:=V
  else
    Exit;
  // T (reset separator)
  if not _CheckWChar('T',#0) then
    Exit;
  // Hour
  if _ExtractFirstInt(0,23) then
    st.wHour:=V
  else
    Exit;
  // separator
  if not _CheckSeparator then
    Exit;
  // Minute
  if _ExtractFirstInt(0,59) then
    st.wMinute:=V
  else
    Exit;
  // separator
  if not _CheckSeparator then
    Exit;
  // Second
  if _ExtractFirstInt(0,59) then
    st.wSecond:=V
  else
    Exit;
  // Z or ., or +- for timeshift
  if _CheckWChar('.',',') then begin
    // Milliseconds
    if _ExtractFrac(3) then
      st.wMilliseconds:=V
    else
      Exit;
  end;
  // convert to p
  try
    pdt^:=SystemTimeToDateTime(st);
  except
    Exit;
  end;
  // ok
  Result:=TRUE;
end;

function ExtractBeforeSpaceDelimiter(var ASourceString: WideString): WideString;
begin
  Result := '';

  // remove starting delimiters
  while Length(ASourceString)>0 do begin
    case ASourceString[1] of
      #10,#13,#0,#32,#160:
        System.Delete(ASourceString,1,1);
      else
        break;
    end;
  end;

  // get before
  while Length(ASourceString)>0 do begin
    case ASourceString[1] of
      #10,#13,#0,#32,#160:
        break;
      else begin
        Result := Result + ASourceString[1];
        System.Delete(ASourceString,1,1);
      end;
    end;
  end;
end;

end.
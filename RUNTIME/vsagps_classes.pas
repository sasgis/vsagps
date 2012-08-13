(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_classes;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  vsagps_public_base,
  vsagps_public_sysutils;

type
  Pvsagps_list_item = ^Tvsagps_list_item;
  Tvsagps_list_item = record
    next: Pvsagps_list_item;
    data: Pointer;
    uptr: Pointer;
  end;

  Tvsagps_List = class(TObject)
  private
    pData: Pvsagps_list_item;
    pLast: Pvsagps_list_item;
  protected
    procedure InternalFreeAllItems;
    procedure InternalAppendItem(const p, u: Pointer);
    function InternalExtractItem(var p, u: Pointer): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function EnumItems(var EnumPtr: Pvsagps_list_item; var p: Pointer): Boolean;
  end;

  Tvsagps_Indexed_List = class(Tvsagps_List)
  private
    // cache for index operations
    FCachedItem: Pvsagps_list_item;
    FCachedIndex: Integer;
  protected
    function GetListObjects(const AIndex: Integer): Pointer;
    procedure SetListObjects(const AIndex: Integer; const Value: Pointer);
    function InternalLocateItem(const AIndex: Integer; out pItem: Pvsagps_list_item): Boolean;
  public
    constructor Create; override;
    function Count: Integer;
    procedure Delete(const AIndex: Integer);
    property Objects[const AIndex: Integer]: Pointer read GetListObjects write SetListObjects;
  end;

  THandleStream = class(TObject)
  protected
    FHandle: THandle;
  public
    constructor Create(const AHandle: THandle); virtual;
    function Read(var ABuffer; const ACount: LongWord): Longint;
    function Size: Int64; virtual; abstract;
  end;

  TStrings = class(Tvsagps_Indexed_List)
  private
    function GetListItem(const AIndex: Integer): AnsiString;
    function GetTextStr: AnsiString;
    procedure SetTextStr(const S: AnsiString);
  public
    procedure AppendWithPtr(const ALine: AnsiString; const AUserPtr: Pointer);
    procedure Append(const S: AnsiString);
    procedure AppendPChar(const S: PAnsiChar);
    function AddObject(const S: AnsiString; AObject: TObject): Integer;
    procedure Assign(src: TStrings);
    procedure Clear;
    procedure LoadFromStream(AStream: THandleStream);
    function IndexOf(const S: AnsiString): Integer;
    property Items[const AIndex: Integer]: AnsiString read GetListItem; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
  end;
  TStringList = TStrings;

  TFileStream = class(THandleStream)
  private
    procedure InternalCloseFile;
  public
    constructor Create(const AFileName: AnsiString; const Mode: Word); reintroduce;
    destructor Destroy; override;
    function Size: Int64; override;
  end;

  EFOpenError = class(Exception);
  EThread = class(Exception);

  TNotifyEvent = procedure(Sender: TObject) of object;

  TThread = class(TObject)
  private
    FHandle: THandle;
    FThreadID: DWORD;
    FTerminated: Boolean;
    FFinished: Boolean;
    FFreeOnTerminate: Boolean;
    FOnTerminate: TNotifyEvent;
  protected
    procedure InternalCloseHandle;
    procedure Execute; virtual; abstract;
    procedure DoTerminate; //virtual;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    procedure Resume;
    procedure Terminate;
    function WaitFor: LongWord;

    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Handle: THandle read FHandle;
    property ThreadID: DWORD read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  TRegistry = class(TObject)
  private
    FRootKey: HKEY;
    FCurrentKey: HKEY;
    procedure SetRootKey(const Value: HKEY);
  protected
    function InternalRegOpenKeyEx(const AKey: AnsiString; const AAccess: DWORD): Boolean;
    procedure InternalCloseCurrentKey;
  public
    constructor Create;
    destructor Destroy; override;

    function OpenKeyReadOnly(const AKey: AnsiString): Boolean;
    procedure GetValueNames(AStrings: TStrings);
    function ReadString(const AName: AnsiString): AnsiString;

    property RootKey: HKEY read FRootKey write SetRootKey;
  end;

implementation

uses
  RTLConsts,
  vsagps_memory;

function ALLThreadProc(lpParameter: Pointer): DWORD; stdcall;
begin
  Result:=0;
  if (nil=lpParameter) then
    Exit;

  try
    if not TThread(lpParameter).Terminated then
    try
      TThread(lpParameter).Execute;
    except
    end;
  finally
    TThread(lpParameter).Terminate;
    TThread(lpParameter).DoTerminate;
    TThread(lpParameter).FFinished:=TRUE;
    if TThread(lpParameter).FFreeOnTerminate then
      TThread(lpParameter).Free;
    ExitThread(Result);
  end;
end;  

{ Tvsagps_List }

constructor Tvsagps_List.Create;
begin
  pData:=nil;
  pLast:=nil;
end;

destructor Tvsagps_List.Destroy;
begin
  InternalFreeAllItems;
  inherited;
end;

function Tvsagps_List.EnumItems(var EnumPtr: Pvsagps_list_item; var p: Pointer): Boolean;
begin
  if (nil=EnumPtr) then begin
    // first run
    EnumPtr:=pData;
    Result:=(nil<>pData);
    if Result then
      p:=pData.data
    else
      p:=nil;
  end else begin
    // others
    Result:=(nil<>Pvsagps_list_item(EnumPtr).next);
    if Result then begin
      EnumPtr:=EnumPtr.next;
      p:=EnumPtr.data;
    end;
  end;
end;

procedure Tvsagps_List.InternalAppendItem(const p, u: Pointer);
begin
  if (nil=pData) then begin
    // very first packet
    New(pData);
    pData.next:=nil;
    pData.data:=p;
    pData.uptr:=u;
    pLast:=pData;
  end else begin
    // add to tail
    New(pLast.next);
    pLast:=pLast.next;
    pLast.next:=nil;
    pLast.data:=p;
    pLast.uptr:=u;
  end;
end;

function Tvsagps_List.InternalExtractItem(var p, u: Pointer): Boolean;
var q: Pvsagps_list_item;
begin
  Result:=FALSE;
  if (nil<>pData) then begin
    q:=pData;
    pData:=pData.next;
    p:=q.data;
    u:=q.uptr;
    Dispose(q);
    if (nil=pData) then
      pLast:=nil;
    Result:=TRUE;
  end;
end;

procedure Tvsagps_List.InternalFreeAllItems;
var p, u: Pointer;
begin
  if (nil=Self) then
    Exit;
  while InternalExtractItem(p, u) do
    VSAGPS_FreeMem(p);
end;

{ TStrings }

function TStrings.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  InternalAppendItem(VSAGPS_AllocPCharByString(S, FALSE), Pointer(AObject));
  Result:=0;
end;

procedure TStrings.Append(const S: AnsiString);
begin
  AppendWithPtr(S, nil);
end;

procedure TStrings.AppendPChar(const S: PAnsiChar);
begin
  InternalAppendItem(VSAGPS_AllocPCharByPChar(S, FALSE), nil);
end;

procedure TStrings.AppendWithPtr(const ALine: AnsiString; const AUserPtr: Pointer);
begin
  InternalAppendItem(VSAGPS_AllocPCharByString(ALine, FALSE), AUserPtr);
end;

procedure TStrings.Assign(src: TStrings);
var
  EnumPtr: Pvsagps_list_item;
  PayloadPtr: Pointer;
  S: AnsiString;
begin
  Clear;
  if (nil=src) then
    Exit;
  EnumPtr:=nil;
  while src.EnumItems(EnumPtr, PayloadPtr) do begin
    SafeSetStringP(S, PAnsiChar(PayloadPtr));
    Append(S);
  end;
end;

procedure TStrings.Clear;
begin
  InternalFreeAllItems;
end;

function TStrings.GetListItem(const AIndex: Integer): AnsiString;
var p: Pvsagps_list_item;
begin
  if InternalLocateItem(AIndex, p) then begin
    if (p<>nil) and (p^.data<>nil) then
      SetString(Result, PAnsiChar(p^.data), StrLen(PAnsiChar(p^.data)))
    else
      Result:='';
  end else
    Result:='';
end;

function TStrings.GetTextStr: AnsiString;
var
  EnumPtr: Pvsagps_list_item;
  PayloadPtr: Pointer;
  S: AnsiString;
  line_counter: DWORD;
begin
  Result:='';
  line_counter:=0;
  EnumPtr:=nil;
  while EnumItems(EnumPtr, PayloadPtr) do begin
    SafeSetStringP(S, PAnsiChar(PayloadPtr));
    if (0<line_counter) then
      Result:=Result+System.sLineBreak;
    Result:=Result+S;
    Inc(line_counter);
  end;
end;

function TStrings.IndexOf(const S: AnsiString): Integer;
var
  EnumPtr: Pvsagps_list_item;
  PayloadPtr: Pointer;
  L: DWORD;
begin
  Result:=0;
  EnumPtr:=nil;
  L:=Length(S);
  while EnumItems(EnumPtr, PayloadPtr) do begin
    if (StrLen(PAnsiChar(PayloadPtr))=L) then
    if (0=AnsiStrLIComp(PAnsiChar(PayloadPtr), PAnsiChar(S), L)) then
      Exit;
    Inc(Result);
  end;
  Result:=-1;
end;

procedure TStrings.LoadFromStream(AStream: THandleStream);
var
  L: Integer;
  s: AnsiString;
begin
  L:=AStream.Size;
  SetLength(s, L);
  AStream.Read(Pointer(s)^, L);
  SetTextStr(s);
end;

procedure TStrings.SetTextStr(const S: AnsiString);
begin
  Clear;
  VSAGPS_DividePCharToLines(PAnsiChar(S), Self.AppendWithPtr, nil, FALSE, nil);
end;

{ TFileStream }

constructor TFileStream.Create(const AFileName: AnsiString; const Mode: Word);
var F: Integer;
begin
  F:=FileOpen(AFileName, Mode);
  if (F<0) then
    raise EFOpenError.CreateResFmt(Integer(@SFOpenErrorEx), [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  inherited Create(F);
end;

destructor TFileStream.Destroy;
begin
  InternalCloseFile;
  inherited;
end;

procedure TFileStream.InternalCloseFile;
begin
  if (0<>FHandle) then begin
    FileClose(FHandle);
    FHandle:=0;
  end;
end;

function TFileStream.Size: Int64;
begin
  if not VSAGPS_GetFileSize(FHandle, Result) then
    Result:=0;
end;

{ THandleStream }

constructor THandleStream.Create(const AHandle: THandle);
begin
  FHandle:=AHandle;
end;

function THandleStream.Read(var ABuffer; const ACount: LongWord): Longint;
begin
  Result := FileRead(FHandle, ABuffer, ACount);
  if Result < 0 then
    Result := 0;
end;

{ TThread }

constructor TThread.Create(CreateSuspended: Boolean);
begin
  Assert(CreateSuspended, 'TThread.Create not CreateSuspended');
  FHandle:=0;
  FThreadID:=0;
  FFinished:=FALSE;
  FTerminated:=FALSE;
  FFreeOnTerminate:=FALSE;
  FOnTerminate:=nil;
end;

destructor TThread.Destroy;
begin
  if (0<>FThreadID) and (not FFinished) then
  begin
    Terminate;
    WaitFor;
  end;
  InternalCloseHandle;
  inherited;
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TThread.InternalCloseHandle;
begin
  if (0<>FHandle) then begin
    CloseHandle(FHandle);
    FHandle:=0;
  end;
end;

procedure TThread.Resume;
begin
  if (0=FHandle) then begin
    // create thread
    FHandle := CreateThread(nil, 0, @ALLThreadProc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
    if (0=FHandle) then
      raise EThread.CreateResFmt(Integer(@SThreadCreateError), [SysErrorMessage(GetLastError)]);
  end;
  ResumeThread(FHandle);
end;

procedure TThread.Terminate;
begin
  FTerminated:=TRUE;
end;

function TThread.WaitFor: LongWord;
var dwTicks: DWORD;
begin
  if (0<>FHandle) then
  repeat
    Terminate;
    dwTicks:=GetTickCount;
    if (0=FHandle) then begin
      // killed
      Result:=0;
      Exit;
    end else if GetExitCodeThread(FHandle, Result) then begin
      if (STILL_ACTIVE=Result) then begin
        // running
        Sleep(100);
        Sleep(0);
        // check
        if GetTickCount>(dwTicks+$2000) then // 8192
          if (FHandle<>0) then begin
            TerminateThread(FHandle, 0);
            Exit;
          end;
      end else begin
        // finished
        Exit;
      end;
    end else begin
      // error
      Result:=0;
      Exit;
    end;
  until FALSE;
end;

{ TRegistry }

function TRegistry.OpenKeyReadOnly(const AKey: AnsiString): Boolean;
begin
  Result:=InternalRegOpenKeyEx(AKey, KEY_READ);
  if (not Result) then
    Result:=InternalRegOpenKeyEx(AKey, (STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS));
  if (not Result) then
    Result:=InternalRegOpenKeyEx(AKey, KEY_QUERY_VALUE);
end;

function TRegistry.ReadString(const AName: AnsiString): AnsiString;
var
  res: LongInt;
  pBuf: PAnsiChar;
  dwType, dwSize, dwLen: DWORD;
  S,SS: AnsiString;
begin
  Result:='';
  SS:=AName+#0;
  dwLen:=256;
  pBuf:=VSAGPS_GetMemZ(dwLen);
  try
    repeat
      dwType:=REG_NONE;
      dwSize:=dwLen;
      res:=RegQueryValueEx(FCurrentKey, PAnsiChar(SS), nil, @dwType, PByte(pBuf), @dwSize);
      if (ERROR_SUCCESS=res) then begin
        // ok
        SafeSetStringL(Result, pBuf, dwLen);
        Exit;
      end else if (ERROR_MORE_DATA=res) then begin
        // buffer too small
        dwLen:=dwLen*2;
        VSAGPS_FreeMem(pBuf);
        pBuf:=VSAGPS_GetMemZ(dwLen);
        SetLength(S, dwLen);
      end else begin
        // error
        // raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [AName]);
        Exit;
      end;
    until FALSE;
  finally
    VSAGPS_FreeMem(pBuf);
  end;
end;

constructor TRegistry.Create;
begin
  FRootKey:=HKEY_CURRENT_USER;
end;

procedure TRegistry.SetRootKey(const Value: HKEY);
begin
  if (FRootKey <> Value) then begin
    InternalCloseCurrentKey;
    FRootKey := Value;
  end;
end;

destructor TRegistry.Destroy;
begin
  InternalCloseCurrentKey;
  inherited;
end;

procedure TRegistry.GetValueNames(AStrings: TStrings);
var
  dwIndex, dwLen, dwSize: DWORD;
  res: Longint;
  S: AnsiString;
begin
  AStrings.Clear;
  dwIndex:=0;
  dwLen:=256;
  SetLength(S, dwLen);
  repeat
    dwSize:=dwLen;
    res:=RegEnumValue(FCurrentKey, dwIndex, PAnsiChar(S), dwSize, nil, nil, nil, nil);
    if (ERROR_SUCCESS=res) then begin
      // ok
      AStrings.AppendPChar(PAnsiChar(S));
      Inc(dwIndex);
    end else if (ERROR_NO_MORE_ITEMS=res) then begin
      // finished
      Exit;
    end else if (ERROR_MORE_DATA=res) then begin
      // buffer too small
      dwLen:=dwLen*2;
      SetLength(S, dwLen);
    end else begin
      // error
      Exit;
    end;
  until FALSE;
end;

procedure TRegistry.InternalCloseCurrentKey;
begin
  if (0<>FCurrentKey) then begin
    RegCloseKey(FCurrentKey);
    FCurrentKey:=0;
  end;
end;

function TRegistry.InternalRegOpenKeyEx(const AKey: AnsiString; const AAccess: DWORD): Boolean;
var
  hTempKey: HKEY;
begin
  Result := (RegOpenKeyExA(FRootKey, PAnsiChar(AKey), 0, AAccess, hTempKey) = ERROR_SUCCESS);
  if Result then
    FCurrentKey:=hTempKey;
end;

{ Tvsagps_Indexed_List }

function Tvsagps_Indexed_List.Count: Integer;
var
  EnumPtr: Pvsagps_list_item;
  p: Pointer;
begin
  Result:=0;
  EnumPtr:=nil;
  while EnumItems(EnumPtr, p) do begin
    Inc(Result);
  end;
end;

constructor Tvsagps_Indexed_List.Create;
begin
  inherited;
  FCachedItem:=nil;
  FCachedIndex:=0;
end;

procedure Tvsagps_Indexed_List.Delete(const AIndex: Integer);
var
  p, u: Pointer;
  pPrev, pNext: Pvsagps_list_item;
begin
  if (0>AIndex) then
    Exit;

  if (0=AIndex) then begin
    // extract and kill very first item
    if InternalExtractItem(p, u) then
      VSAGPS_FreeMem(p);
  end else if InternalLocateItem(AIndex-1, pPrev) then begin
    // got prev item
    if (pPrev^.next<>nil) then begin
      // next item exists - free it
      pNext:=pPrev^.next;
      pPrev^.next:=pNext^.next;
      if (pNext=pLast) then
        pLast:=pPrev;
      if (pNext^.data<>nil) then
        VSAGPS_FreeMem(pNext^.data);
      Dispose(pNext);
    end;
  end;
end;

function Tvsagps_Indexed_List.GetListObjects(const AIndex: Integer): Pointer;
var p: Pvsagps_list_item;
begin
  if InternalLocateItem(AIndex, p) then
    Result:=p^.uptr
  else
    Result:=nil;
end;

function Tvsagps_Indexed_List.InternalLocateItem(const AIndex: Integer; out pItem: Pvsagps_list_item): Boolean;
var p: Pointer;
begin
  Result:=FALSE;
  pItem:=nil;

  // check
  if (AIndex<0) or (nil=pData) then begin
    // failed
    Exit;
  end;

  // if after cached item - lookup from cached item
  // if before - start from the very beginning
  if (nil=FCachedItem) or (AIndex<FCachedIndex) then begin
    // set at start
    FCachedItem:=pData;
    FCachedIndex:=0;
  end;

  repeat
    // cached item
    if (AIndex=FCachedIndex) and (nil<>FCachedItem) then begin
      pItem:=FCachedItem;
      Result:=TRUE;
      Exit;
    end;

    // get next
    if EnumItems(FCachedItem, p) then begin
      // fetched successfully
      Inc(FCachedIndex);
    end else begin
      // end of list - reset cache
      FCachedItem:=nil;
      // do not reset FCachedIndex!!!
      Exit;
    end;
  until FALSE;
end;

procedure Tvsagps_Indexed_List.SetListObjects(const AIndex: Integer; const Value: Pointer);
var p: Pvsagps_list_item;
begin
  if InternalLocateItem(AIndex, p) then
    p^.uptr:=Value;
end;

end.

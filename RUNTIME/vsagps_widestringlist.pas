(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_widestringlist;
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
  vsagps_public_unicode;

type
  TWideStringList = class
{$if defined(USE_SIMPLE_CLASSES)}
  (Tvsagps_Indexed_List)
{$else}
  (TList)
{$ifend}
  protected
{$if not defined(USE_SIMPLE_CLASSES)}
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
{$ifend}
    function GetListItem(const AIndex: Integer): WideString;
    function GetTextStr: WideString;
    procedure SetTextStr(const AValue: WideString);
  public
    procedure Append(const S: String);
    procedure AppendPChar(const S: PChar);

    property Text: WideString read GetTextStr write SetTextStr;
    property Items[const AIndex: Integer]: WideString read GetListItem; default;
  end;

implementation

{ TWideStringList }

function TWideStringList.GetListItem(const AIndex: Integer): WideString;
{$if defined(USE_SIMPLE_CLASSES)}
var p: Pvsagps_list_item;
{$ifend}
begin
{$if defined(USE_SIMPLE_CLASSES)}
  if InternalLocateItem(AIndex, p) then begin
    if (p<>nil) and (p^.data<>nil) then
      SetString(Result, PWideChar(p^.data), StrLenW(PWideChar(p^.data)))
    else
      Result:='';
  end else
    Result:='';
{$else}

{$ifend}
end;

function TWideStringList.GetTextStr: WideString;
{$if defined(USE_SIMPLE_CLASSES)}
var
  EnumPtr: Pvsagps_list_item;
  PayloadPtr: Pointer;
  S: WideString;
  line_counter: DWORD;
{$ifend}
begin
  Result:='';
{$if defined(USE_SIMPLE_CLASSES)}
  line_counter:=0;
  EnumPtr:=nil;
  while EnumItems(EnumPtr, PayloadPtr) do begin
    SafeSetStringP(S, PWideChar(PayloadPtr));
    if (0<line_counter) then
      Result:=Result+System.sLineBreak;
    Result:=Result+S;
    Inc(line_counter);
  end;
{$else}

{$ifend}
end;

procedure TWideStringList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;

end;

procedure TWideStringList.SetTextStr(const AValue: WideString);
var
  p,pStart: PWideChar;
  str_line: WideString;
begin
  Clear;
  if (0=Length(AValue)) then
    Exit;
  p:=PWideChar(AValue);
  // find lines
  while (#0<>p^) do begin
    pStart:=p;
    while (p^<>#0) and (p^<>#10) and (p^<>#13) do
      Inc(p);
    SafeSetWideStringL(str_line, pStart, p-pStart);
    Self.Append(str_line);
    if (#13=p^) then
      Inc(p);
    if (#10=p^) then
      Inc(p);
  end;
end;

end.
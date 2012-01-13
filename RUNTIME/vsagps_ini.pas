(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_ini;
(*
*)

{$I vsagps_defines.inc}

interface

uses
  Windows,
  SysUtils,
{$if defined(USE_SIMPLE_CLASSES)}
  vsagps_classes,
{$else}
  Classes,
{$ifend}
  vsagps_public_nmea;

const
  vsagps_nmea_ini_filename = 'vsagps_nmea.ini';
  vsagps_garmin_ini_filename = 'vsagps_garmin.ini';
  vsagps_ini_common = 'COMMON';
  vsagps_ini_others = ''; // empty
  vsagps_ini_start = 'start';
  vsagps_ini_resetdgps = 'resetdgps';
  vsagps_ini_fromsection = 'fromsection';
  vsagps_ini_comment = '*'; // like nmea finisher
  vsagps_ini_sectiona = '[';
  vsagps_ini_sectionb = ']';
  vsagps_ini_eq = '=';
  vsagps_ini_subcodes = '/';
  // for garmin workarounds
  vsagps_ini_BreakOnLastProtocolPacket = 'BreakOnLastProtocolPacket';
  vsagps_ini_CheckByNumericProductID = 'CheckByNumericProductID';
  vsagps_ini_CheckByModelDescription = 'CheckByModelDescription';

// load entire ini from file
procedure VSAGPS_ini_LoadFromFile(sl: TStrings; const AIniFileName: String);

// get all user defined sentances to send on connect
procedure VSAGPS_ini_GetStartSent(sl_Ini, sl_Sent: TStrings);

// get all user defined sentances to send on "refresh dgps" command and user defined commands
procedure VSAGPS_ini_GetSent_ForGPSCommand(sl_Ini, sl_Sent, sl_Prop: TStrings; const AParamName: String);

// get section header position (comments allowed)
function VSAGPS_ini_GetSectionHeader(sl_Ini: TStrings;
                                     const SectionNameUppercased: String;
                                     out StartSectionIndex: Integer): Boolean;

// check param name and extract tail
function VSAGPS_ini_StartPromParam(const ASrc: String; const AParam: String; out ATail: String): Boolean;

function Append_Gpms_Subcodes(const ACode: Tgpms_Code; const ASubCodes: Word; sl_prop: TStrings): Integer;

implementation

procedure VSAGPS_ini_LoadFromFile(sl: TStrings; const AIniFileName: String);
var fs: TFileStream;
begin
  if (not FileExists(AIniFileName)) then
    Exit;
  fs := TFileStream.Create(AIniFileName, fmOpenRead);
  try
    sl.LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

function VSAGPS_ini_DelComment(const AString: String): String;
var p: Integer;
begin
  Result:=AString;
  p:=System.Pos(vsagps_ini_comment,Result);
  if (0<p) then begin
    SetLength(Result,p-1);
    Result:=Trim(Result);
  end else begin
    Result:=Trim(Result);
  end;
end;

function VSAGPS_ini_GetAfterEq(const AString: String): String;
var p: Integer;
begin
  p:=System.Pos(vsagps_ini_eq, AString);
  if (0<p) then begin
    Result:=System.Copy(AString, p+1, Length(AString));
  end else begin
    // mandatory
    Result:='';
  end;
end;

function VSAGPS_ini_GetSectionHeader(sl_Ini: TStrings;
                                     const SectionNameUppercased: String;
                                     out StartSectionIndex: Integer): Boolean;
var s: String;
begin
  Result:=FALSE;
  StartSectionIndex:=0;
  while StartSectionIndex<sl_Ini.Count do begin
    s:=sl_Ini[StartSectionIndex];
    if (0<Length(s)) then
    if (vsagps_ini_sectiona=s[1]) then begin
      // start of some section
      s:=VSAGPS_ini_DelComment(s);
      if SameText(s, vsagps_ini_sectiona+SectionNameUppercased+vsagps_ini_sectionb) then begin
        Result:=TRUE;
        Exit;
      end;
    end;
    Inc(StartSectionIndex);
  end;
end;

function VSAGPS_ini_StartPromParam(const ASrc: String; const AParam: String; out ATail: String): Boolean;
begin
  Result:=FALSE;
  ATail:='';
  if SameText(AParam, System.Copy(ASrc, 1, Length(AParam))) then begin
    Result:=TRUE;
    ATail:=System.Copy(ASrc, Length(AParam)+1, Length(ASrc));
  end;
end;

function VSAGPS_ini_ForeachSection(sl_Ini: TStrings;
                                   sl_Result: TStrings;
                                   const SectionNameUppercased: String;
                                   const ParamName: String;
                                   const ParseLinks: Boolean;
                                   const ParseOthers: Boolean): Boolean;
var
  i: Integer;
  s,s_tail: String;
begin
  Result:=FALSE;
  if VSAGPS_ini_GetSectionHeader(sl_Ini, SectionNameUppercased, i) then begin
    Inc(i);
    while (i<sl_Ini.Count) do begin
      // check lines before next section
      s:=sl_Ini[i];
      if (0<Length(s)) then begin
        // not empty line
        if (vsagps_ini_sectiona=s[1]) then begin
          // next section started - exit
          Exit;
        end else if VSAGPS_ini_StartPromParam(s, ParamName, s_tail) then begin
          // startN or startfromsectionN
          Result:=TRUE;
          s_tail:=VSAGPS_ini_DelComment(s_tail);
          if VSAGPS_ini_StartPromParam(s_tail, vsagps_ini_fromsection, s) then begin
            // startfromsectionN
            if ParseLinks then begin
              // only if available
              s:=Uppercase(Trim(VSAGPS_ini_GetAfterEq(s)));
              VSAGPS_ini_ForeachSection(sl_Ini, sl_Result, s, ParamName, FALSE, ParseOthers);
            end;
          end else begin
            // startN
            s_tail:=VSAGPS_ini_GetAfterEq(s_tail);
            if (0<Length(s_tail)) then
              sl_Result.Append(s_tail);
          end;
        end;
      end;
      // next
      Inc(i);
    end;
  end;
  // section not found - look for /others
  if ParseOthers and (not Result) then begin
    i:=System.Pos(vsagps_ini_subcodes, SectionNameUppercased);
    if (0<i) then begin
      s:=System.Copy(SectionNameUppercased, 1, i) + Uppercase(vsagps_ini_others);
      if (not SameText(s, SectionNameUppercased)) then
        Result:=VSAGPS_ini_ForeachSection(sl_Ini, sl_Result, s, ParamName, ParseLinks, FALSE);
    end;
  end;
end;

procedure VSAGPS_ini_GetStartSent(sl_Ini, sl_Sent: TStrings);
begin
  // preamble
  if (sl_Sent=nil) then
    Exit;
  sl_Sent.Clear;
  if (sl_Ini=nil) then
    Exit;
  if (sl_Ini.Count=0) then
    Exit;

  // lookup from COMMON section: a) startN, b) startfromsectionN, c) startN from sections
  // NO PROPRIETARIES!
  VSAGPS_ini_ForeachSection(sl_Ini, sl_Sent, vsagps_ini_common, vsagps_ini_start, TRUE, FALSE);
end;

procedure VSAGPS_ini_GetSent_ForGPSCommand(sl_Ini, sl_Sent, sl_Prop: TStrings; const AParamName: String);
var
  i: Integer;
begin
  // preamble
  if (nil=sl_Sent) then
    Exit;
  sl_Sent.Clear;
  if (nil=sl_Ini) then
    Exit;
  if (0=sl_Ini.Count) then
    Exit;
  if (0=Length(AParamName)) then
    Exit;

  if (sl_Prop<>nil) and (sl_Prop.Count>0) then begin
    // look through proprietaries
    for i := 0 to sl_Prop.Count-1 do begin
      VSAGPS_ini_ForeachSection(sl_Ini, sl_Sent, sl_Prop[i], AParamName, TRUE, TRUE);
    end;
  end else begin
    // no proprietaries - common behavior
    VSAGPS_ini_ForeachSection(sl_Ini, sl_Sent, vsagps_ini_common, AParamName, TRUE, TRUE);
  end;
end;

function Append_Gpms_Subcodes(const ACode: Tgpms_Code; const ASubCodes: Word; sl_prop: TStrings): Integer;
var s: String;
begin
  Result:=0;
  s:=Gpms_Code_to_String(ACode);
  if (gpms_SRF=ACode) then begin
    if (gpms_SRF_sub_U_Blox = (ASubCodes or gpms_SRF_sub_U_Blox)) then begin
      sl_prop.Append(s+vsagps_ini_subcodes+'U-BLOX');
      Inc(Result);
    end;
    if (gpms_SRF_sub_GlobalSat = (ASubCodes or gpms_SRF_sub_GlobalSat)) then begin
      sl_prop.Append(s+vsagps_ini_subcodes+'GLOBALSAT');
      Inc(Result);
    end;
  end;
end;

end.
(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_parser_nmea;
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
  vsagps_public_nmea,
  vsagps_public_parser,
  vsagps_public_device;

const
  cNmea_Starter     = '$';
  cNmea_Query       = 'Q';
  cNmea_Proprietary = 'P';
  cNmea_Separator   = ',';
  cNmea_Point       = '.';
  cNmea_Finisher    = '*';
  cNmea_Tail        = #13#10;  

type
  TNmeaParserProc = function (const AData, ASubCommand: String): DWORD of object;

  TNmeaProprietary = record
    ParserProc: TNmeaParserProc;
    SubCode: Word;
    Supported: Boolean;
  end;
  PNmeaProprietary = ^TNmeaProprietary;

  TOnGGAProc = function (const p: PNMEA_GGA): DWORD of object;
  TOnGLLProc = function (const p: PNMEA_GLL): DWORD of object;
  TOnGSAProc = function (const p: PNMEA_GSA): DWORD of object;
  TOnGSVProc = function (const p: PNMEA_GSV): DWORD of object;
  TOnRMCProc = function (const p: PNMEA_RMC): DWORD of object;
{$if defined(USE_NMEA_VTG)}
  TOnVTGProc = function (const p: PNMEA_VTG): DWORD of object;
{$ifend}

  TOnApplyUTCDateTimeProc = procedure (const ADate: PNMEA_Date; const ATime: PNMEA_Time) of object;

  Tvsagps_parser_nmea = class(TStringList)
  public
    FOnApplyUTCDateTime: TOnApplyUTCDateTimeProc;
    FGSVNmeaCounterGP: Byte;
    FGSVNmeaCounterGL: Byte;

    FOnGGA: TOnGGAProc;
    FOnGLL: TOnGLLProc;
    FOnGSA: TOnGSAProc;
    FOnGSV: TOnGSVProc;
    FOnRMC: TOnRMCProc;
{$if defined(USE_NMEA_VTG)}
    FOnVTG: TOnVTGProc;
{$ifend}

    FProprietaries: array [Tgpms_Code] of TNmeaProprietary;
  protected
    function Internal_Parse_NmeaComm_Base_Sentence(const ANmeaBaseString: String;
                                                   const AProprietaryWithoutFinisher: Boolean): DWORD;
    function Internal_Parse_NmeaComm_Talker_Sentence(const ATalkerID, ACommandID, AData: String): DWORD;
    function Internal_Parse_NmeaComm_Proprietary_Sentence(const ACommandID, AData: String): DWORD;
  protected
    // helper routines
    procedure Parse_NMEA_Char(const AIndex: Byte; const AChar: PChar);
    procedure Parse_NMEA_Coord(const AIndexCoord, AIndexSymbol: Byte; const ACoord: PNMEA_Coord);
    procedure Parse_NMEA_Date(const AIndex: Byte; const ADate: PNMEA_Date);
    procedure Parse_NMEA_Float32(const AIndex: Byte; const AFloat32: PFloat32);
    procedure Parse_NMEA_GSV_INFO(const AIndex: Byte; const AGSV_INFO: PNMEA_GSV_INFO);
    procedure Parse_NMEA_SatID(const AIndex: Byte; var ASatID: SInt8; var ASatIdNormalizedFlag: Byte);
    procedure Parse_NMEA_SInt16(const AIndex: Byte; const ASInt16: PSInt16);
    procedure Parse_NMEA_SInt8(const AIndex: Byte; const ASInt8: PSInt8);
    procedure Parse_NMEA_Time(const AIndex: Byte; const ATime: PNMEA_Time);
    // supported talker sentences
    function Parse_and_Send_GGA_Data(const AData, ATalkerID: String): DWORD;
    function Parse_and_Send_GLL_Data(const AData, ATalkerID: String): DWORD;
    function Parse_and_Send_GSA_Data(const AData, ATalkerID: String): DWORD;
    function Parse_and_Send_GSV_Data(const AData, ATalkerID: String): DWORD;
    function Parse_and_Send_RMC_Data(const AData, ATalkerID: String): DWORD;
    function Parse_and_Send_VTG_Data(const AData, ATalkerID: String): DWORD;
  public
    function Get_NMEAPart_By_Index(const AIndex: Byte): String;
    function Parse_Sentence_Without_Starter(const ANmeaFullStringWithoutStarter: String): DWORD;
    // enable subcode
    procedure Enable_Proprietaries_SubCode(const ACode: Tgpms_Code; const ASubCode: Word);
    // check subcode enabled
    function Proprietaries_SubCode_Enabled(const ACode: Tgpms_Code; const ASubCode: Word): Boolean;
    function SomeProprietariesSupported: Boolean;
  end;

// calc checksum
function CalcNmeaChecksum(const ANmeaBaseString: String): Byte;

// calc checksum and add $ and the tail
function MakeNmeaFullString(const ANmeaBaseString: String): String;

// create nmea query sentence
function MakeNmeaBaseQuerySentence(const ANmeaTalkerIdSrc, ANmeaTalkerIdDst: String; const ANmeaSentenceId: String): String;

// extract nmea data and check checksum (if exists)
function ExtractNmeaBaseString(const ANmeaFullString: String;
                               const ACheckNmeaStarter: Boolean;
                               var ANmeaBaseString: String;
                               var AChecksumFound: Boolean;
                               var AChecksumMatched: Boolean;
                               var AProprietaryWithoutFinisher: Boolean): Boolean;

// parse nmea string coordinates (without symbol)
procedure NMEA_Parse_Coord(const ACoordValue: String; const ACoord: PNMEA_Coord);

implementation

function CalcNmeaChecksum(const ANmeaBaseString: String): Byte;
var i: DWORD;
begin
  Result:=0;
  if (0<=Length(ANmeaBaseString)) then
  for i:=1 to Length(ANmeaBaseString) do
  Result:=(Result xor Ord(ANmeaBaseString[i]));
end;

function MakeNmeaFullString(const ANmeaBaseString: String): String;
var chksum: Byte;
begin
  chksum:=CalcNmeaChecksum(ANmeaBaseString);
  Result:=cNmea_Starter+ANmeaBaseString+cNmea_Finisher+IntToHex(chksum,2)+cNmea_Tail;
end;

function MakeNmeaBaseQuerySentence(const ANmeaTalkerIdSrc, ANmeaTalkerIdDst: String; const ANmeaSentenceId: String): String;
begin
  Result:=ANmeaTalkerIdSrc+ANmeaTalkerIdDst+cNmea_Query+cNmea_Separator+ANmeaSentenceId;
end;

function ExtractNmeaBaseString(const ANmeaFullString: String;
                               const ACheckNmeaStarter: Boolean;
                               var ANmeaBaseString: String;
                               var AChecksumFound: Boolean;
                               var AChecksumMatched: Boolean;
                               var AProprietaryWithoutFinisher: Boolean): Boolean;
  function _NoString: Boolean;
  begin
    Result:=(0=Length(ANmeaBaseString))
  end;

  function _SameChecksums(const cs1: String; const cs2: Byte): Boolean;
  var n,m: Integer;
  begin
    if TryStrToInt('0x'+cs1, n) then begin
      m:=cs2;
      Result:=(m=n);
    end else
      Result:=FALSE;
  end;

var
  test_str: String;
  i: Integer;
begin
  Result:=FALSE;
  AChecksumFound:=FALSE;
  AChecksumMatched:=FALSE;
  AProprietaryWithoutFinisher:=FALSE;

  ANmeaBaseString:=ANmeaFullString;
  if (_NoString) then
    Exit;

  // extract nmea starter
  if ACheckNmeaStarter then begin
    i:=System.Pos(cNmea_Starter, ANmeaBaseString);
    if (i>1) then
      System.Delete(ANmeaBaseString,1,i-1);
    test_str:=System.Copy(ANmeaBaseString, 1, Length(cNmea_Starter));
    if (not SameText(test_str, cNmea_Starter)) then
      Exit;
    System.Delete(ANmeaBaseString, 1, Length(cNmea_Starter));
    if (_NoString) then
      Exit;
  end;

  // extract nmea tail
  if (0<Length(cNmea_Tail)) then
  while (0<Length(ANmeaBaseString)) do begin
    if (System.Pos(ANmeaBaseString[Length(ANmeaBaseString)], cNmea_Tail)>0) then
      SetLength(ANmeaBaseString, Length(ANmeaBaseString)-1)
    else
      break;
  end;
  if (_NoString) then
    Exit;

  // extract nmea-finisher and checksum
  i:=System.Pos(cNmea_Finisher, ANmeaBaseString);
  if (i>0) then begin
    // nmea-finisher found - try to get checksum
    test_str:=System.Copy(ANmeaBaseString, i+Length(cNmea_Finisher), Length(ANmeaBaseString));
    // note:test_str has only checksum!
    // remove nmea-finisher and checksum - leave only "base" nmea string
    SetLength(ANmeaBaseString, i-1);
    if (_NoString) then
      Exit;
    // check finisher
    if (0=Length(test_str)) then begin
      // no checksum after nmea-finisher
      // allow for dummies
      Result:=TRUE;
    end else begin
      // checksum found
      AChecksumFound:=TRUE;
      // calc new checksum and compare values
      if _SameChecksums(test_str, CalcNmeaChecksum(ANmeaBaseString)) then begin
        // checksum ok
        AChecksumMatched:=TRUE;
        Result:=TRUE;
      end; // else checksum failed
    end;
  end else begin
    // no nmea-finisher (and of course no checksum) - allowed only for proprietary sentences
    if SameText(System.Copy(ANmeaBaseString, 1, Length(cNmea_Proprietary)), cNmea_Proprietary) then begin
      AProprietaryWithoutFinisher:=TRUE;
      Result:=TRUE;
    end;
  end;
end;

procedure NMEA_Parse_Coord(const ACoordValue: String; const ACoord: PNMEA_Coord);
  procedure _SetNoData;
  begin
    ACoord^.deg:=$FF;
  end;
var
  g: String;
  p: Integer;

  procedure _MinutesToFloat(const s: String);
  var
    dc: Char;
  begin
    dc:=DecimalSeparator;
    try
      DecimalSeparator:=cNmea_Point;
      ACoord^.min:=StrToFloat(s);
    finally
      DecimalSeparator:=dc;
    end;
  end;
begin
  // 04321.0123
  try
    if (0=Length(ACoordValue)) then begin
      // no data
      _SetNoData;
    end else begin
      // with data
      p:=System.Pos(cNmea_Point,ACoordValue);
      if (p>0) then begin
        // with point
        if (5<=p) then begin
          // 4 o more symbols before point - leave 2 symbols for minutes
          g:=System.Copy(ACoordValue,1,p-3);
          // g='043' s='21.0123'
          _MinutesToFloat(System.Copy(ACoordValue, Length(g)+1, Length(ACoordValue)));
          ACoord^.deg:=StrToInt(g);
        end else begin
          // error
          _SetNoData;
        end;
      end else begin
        // no point - may be only int part
        if (4<=Length(ACoordValue)) then begin
          // simple
          ACoord^.deg:=StrToInt(ACoordValue[Length(ACoordValue)-3]+ACoordValue[Length(ACoordValue)-2]);
          ACoord^.min:=StrToInt(ACoordValue[Length(ACoordValue)-1]+ACoordValue[Length(ACoordValue)]);
        end else begin
          // error
          _SetNoData;
        end;
      end;
    end;
  except
    _SetNoData;
  end;
end;

{ Tvsagps_parser_nmea }

procedure Tvsagps_parser_nmea.Enable_Proprietaries_SubCode(const ACode: Tgpms_Code; const ASubCode: Word);
begin
  FProprietaries[ACode].SubCode := (FProprietaries[ACode].SubCode or ASubCode);
end;

function Tvsagps_parser_nmea.Get_NMEAPart_By_Index(const AIndex: Byte): String;
begin
  if (AIndex>=Self.Count) then
    Result:=''
  else
    Result:=Self[AIndex];
end;

function Tvsagps_parser_nmea.Internal_Parse_NmeaComm_Base_Sentence(const ANmeaBaseString: String; const AProprietaryWithoutFinisher: Boolean): DWORD;
var
  pComma: Integer;
  Vcommand, Vdata: String;

  procedure _DoProprietary;
  begin
    // nmea proprietary sentence
    System.Delete(Vcommand, 1, 1); // delete starting 'P'
    Result:=Internal_Parse_NmeaComm_Proprietary_Sentence(Vcommand, Vdata);
  end;

  procedure _DoQuery;
  begin
    // nmea query sentence
  end;

  procedure _DoTalker;
  var Vtalkerid: String;
  begin
    // nmea talker sentence
    if (4<Length(Vcommand)) then begin
      // correct talker sentence
      Vtalkerid:=System.Copy(Vcommand,1,2); // usually 'GP'
      System.Delete(Vcommand,1,2);
      Result:=Internal_Parse_NmeaComm_Talker_Sentence(Vtalkerid, Vcommand, Vdata);
    end;
  end;
begin
  pComma:=System.Pos(cNmea_Separator,ANmeaBaseString);
  if (pComma>0) then begin
    // extract command
    Vcommand:=System.Copy(ANmeaBaseString, 1, pComma-1);
    Vdata:=System.Copy(ANmeaBaseString, pComma+1, Length(ANmeaBaseString));

    // check command type
    if (0<Length(Vcommand)) then begin
      // command exists
      if (cNmea_Proprietary=Vcommand[1]) then
        _DoProprietary
      else
      if (cNmea_Query=Vcommand[Length(Vcommand)]) then
        _DoQuery
      else
        _DoTalker;
    end else begin
      // no command
    end;
  end else begin
    // no comma
  end;
end;

function Tvsagps_parser_nmea.Internal_Parse_NmeaComm_Proprietary_Sentence(const ACommandID, AData: String): DWORD;
var
  VMajorCmd, VMinorCmd: String;
  Vgpms: Tgpms_Code;
begin
  Result:=0;

  // check supported proprietary commands
  VMajorCmd:=System.Copy(ACommandID, 1, 3);
  VMinorCmd:=System.Copy(ACommandID, 4, Length(ACommandID));

  // find handler and params
  Vgpms:=String_to_Gpms_Code(VMajorCmd);

  // also set "unknown"
  FProprietaries[Vgpms].Supported:=TRUE;

  // set marker that some of known proprietaries are supported
  if (gpms_Unknown<>Vgpms) and (gpms_Some<>Vgpms) then
    FProprietaries[gpms_Some].Supported:=TRUE;

  // run handler to parse sentence
  if Assigned(FProprietaries[Vgpms].ParserProc) then begin
    // parse data to stringlist
    parse_string_to_strings(AData, cNmea_Separator, Self);
    // call handler
    FProprietaries[Vgpms].ParserProc(AData, VMinorCmd);
  end;
end;

function Tvsagps_parser_nmea.Internal_Parse_NmeaComm_Talker_Sentence(const ATalkerID, ACommandID, AData: String): DWORD;
var p: TNmeaParserProc;
begin
  Result:=0;
  p:=nil;

  // check supported commands
  if (nmea_si_RMC=ACommandID) then begin
    // RMC
    if Assigned(FOnRMC) then
      p:=Parse_and_Send_RMC_Data;
  end else if (nmea_si_GSA=ACommandID) then begin
    // GSA
    if Assigned(FOnGSA) then
      p:=Parse_and_Send_GSA_Data;
  end else if (nmea_si_GGA=ACommandID) then begin
    // GGA
    if Assigned(FOnGGA) then
      p:=Parse_and_Send_GGA_Data;
  end else if (nmea_si_GLL=ACommandID) then begin
    // GLL
    if Assigned(FOnGLL) then
      p:=Parse_and_Send_GLL_Data;
  end else if (nmea_si_GSV=ACommandID) then begin
    // GSV
    if Assigned(FOnGSV) then
      p:=Parse_and_Send_GSV_Data;
{$if defined(USE_NMEA_VTG)}
  end else if (nmea_si_VTG=ACommandID) then begin
    // VTG
    if Assigned(FOnVTG) then
      p:=Parse_and_Send_VTG_Data;
{$ifend}
  end else ;

  if Assigned(p) then begin
    // parse data to stringlist
    parse_string_to_strings(AData, cNmea_Separator, Self);
    // call handler
    p(AData, ATalkerID);
  end;
end;

function Tvsagps_parser_nmea.Parse_and_Send_GGA_Data(const AData, ATalkerID: String): DWORD;
var h: TNMEA_GGA;
begin
  Result:=0;
  // fill data
  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  // 1) Time (UTC) (as 194723.000)
  Parse_NMEA_Time(0, @(h.time));

  // if ask to apply gps time to computer time
  if Assigned(FOnApplyUTCDateTime) then
    FOnApplyUTCDateTime(nil, @(h.time));

  // 2) Latitude
  // 3) N or S (North or South)
  Parse_NMEA_Coord(1, 2, @(h.lat));
  // 4) Longitude
  // 5) E or W (East or West)
  Parse_NMEA_Coord(3, 4, @(h.lon));
  // 6) GPS Quality Indicator
  Parse_NMEA_SInt8(5, @(h.quality));
  // 7) Number of satellites in view, 00 - 12
  Parse_NMEA_SInt8(6, @(h.sats_in_view));
  // 8) Horizontal Dilution of precision
  Parse_NMEA_Float32(7, @(h.hdop));
  // 9) Antenna Altitude above/below mean-sea-level (geoid)
  Parse_NMEA_Float32(8, @(h.alt_from_msl));
  // 10) Units of antenna altitude, meters
  Parse_NMEA_Char(9, @(h.alt_unit));
  // 11) Geoidal separation, the difference between the WGS-84 earth ellipsoid and mean-sea-level (geoid), "-" means mean-sea-level below ellipsoid
  Parse_NMEA_Float32(10, @(h.msl_above_ellipsoid));
  // 12) Units of geoidal separation, meters
  Parse_NMEA_Char(11, @(h.ele_unit));
  // 13) Age of differential GPS data, time in seconds since last SC104 type 1 or 9 update, null field when DGPS is not used
  Parse_NMEA_Float32(12, @(h.dgps_age_second));
  // 14) Differential reference station ID, 0000-1023
  Parse_NMEA_SInt16(13, @(h.dgps_station_id));

  // call handler
  if Assigned(FOnGGA) then
  try
    Result:=FOnGGA(@h);
  except
  end;
end;

function Tvsagps_parser_nmea.Parse_and_Send_GLL_Data(const AData, ATalkerID: String): DWORD;
var h: TNMEA_GLL;
begin
  Result:=0;
  // fill data
  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  // 1) Latitude
  // 2) N or S (North or South)
  Parse_NMEA_Coord(0,1,@(h.lat));
  // 3) Longitude
  // 4) E or W (East or West)
  Parse_NMEA_Coord(2,3,@(h.lon));
  // 5) Time (UTC)
  Parse_NMEA_Time(4, @(h.time));
  // 6) Status A - Data Valid, V - Data Invalid
  Parse_NMEA_Char(5, @(h.status));

  // if ask to apply gps time to computer time
  if ('A'=h.status) then
  if Assigned(FOnApplyUTCDateTime) then
    FOnApplyUTCDateTime(nil, @(h.time));

  // 7) Mode // NMEA version 2.3 (and later)
  Parse_NMEA_Char(6, @(h.nmea23_mode));

  // call handler
  if Assigned(FOnGLL) then
  try
    Result:=FOnGLL(@h);
  except
  end;
end;

function Tvsagps_parser_nmea.Parse_and_Send_GSA_Data(const AData, ATalkerID: String): DWORD;
var
  h: TNMEA_GSA;
  i,k: Byte;
begin
  Result:=0;
  // fill data
  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  // 1) Selection mode
  Parse_NMEA_Char(0, @(h.sel_mode));
  // 2) Mode
  Parse_NMEA_SInt8(1, @(h.fix_mode));
  // 3) ID of 1st satellite used for fix
  // 4) ID of 2nd satellite used for fix
  // ...
  // 14) ID of 12th satellite used for fix
  // count of sats in fix may be greater then 12
  k:=Self.Count-5;
  h.sat_fix.all_count:=k;

  if (k<cNmea_min_fix_sat_count) then
    k:=cNmea_min_fix_sat_count
  else if (k>cNmea_max_sat_count) then
    k:=cNmea_max_sat_count;
  h.sat_fix.fix_count:=k;

  for i := 0 to k-1 do begin
    // i=0 - index=2 // i=11 - index=13
    Parse_NMEA_SatID(2+i, h.sat_fix.sats[i].svid, h.sat_fix.sats[i].normalized_flag);
  end;

  // 15) PDOP in meters
  Parse_NMEA_Float32(Self.Count-3, @(h.pdop));
  // 16) HDOP in meters
  Parse_NMEA_Float32(Self.Count-2, @(h.hdop));
  // 17) VDOP in meters
  Parse_NMEA_Float32(Self.Count-1, @(h.vdop));

  // call handler
  if Assigned(FOnGSA) then
  try
    Result:=FOnGSA(@h);
  except
  end;
end;

function Tvsagps_parser_nmea.Parse_and_Send_GSV_Data(const AData, ATalkerID: String): DWORD;
var
  h: TNMEA_GSV;
  i,j: Byte;
  pCounter: PByte;
begin
  Result:=0;

  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  if SameText(ATalkerID, nmea_ti_GLONASS) then
    pCounter:=@FGSVNmeaCounterGL
  else
    pCounter:=@FGSVNmeaCounterGP;

  // 1) total number of messages
  Parse_NMEA_SInt8(0, @(h.msg_total));
  // 2) message number
  Parse_NMEA_SInt8(1, @(h.msg_cur));
  // 3) satellites in view
  Parse_NMEA_SInt8(2, @(h.sats_in_view));

  // if first sentence - reset counter
  if (1=h.msg_cur) then begin
    pCounter^:=0;
  end;

  // LOOP
  // 4) satellite number
  // 5) elevation in degrees
  // 6) azimuth in degrees to true
  // 7) SNR in dB
  // more satellite infos like 4)-7)
  i:=0;
  repeat
    j:=3+4*i; // params starting index in list: 0-3, 1-7, 2-11, 3-15, 4-19
    if (j>=Self.Count) then
      break;
    // parse info
    Parse_NMEA_GSV_INFO(j, @(h.info));
    h.global_index:=pCounter^;
    pCounter^:=pCounter^+1;
    // call handler
    if Assigned(FOnGSV) then
    try
      FOnGSV(@h);
    except
    end;
    // next
    Inc(i);
  until FALSE;
end;

function Tvsagps_parser_nmea.Parse_and_Send_RMC_Data(const AData, ATalkerID: String): DWORD;
var h: TNMEA_RMC;
begin
  Result:=0;
  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  // 1) Time (UTC)
  Parse_NMEA_Time(0, @(h.time));
  // 2) Status, V = Navigation receiver warning
  Parse_NMEA_Char(1, @(h.status));
  // 3) Latitude
  // 4) N or S
  Parse_NMEA_Coord(2,3,@(h.lat));
  // 5) Longitude // 0xFF - no data
  // 6) E or W // #0 no data
  Parse_NMEA_Coord(4,5,@(h.lon));
  // 7) Speed over ground, knots
  Parse_NMEA_Float32(6, @(h.speed_in_knots));
  // 8) Track made good, degrees true
  Parse_NMEA_Float32(7, @(h.course_in_degrees));
  // 9) Date, ddmmyy
  Parse_NMEA_Date(8, @(h.date));
  // 10) Magnetic Variation, degrees
  Parse_NMEA_Float32(9, @(h.magvar_deg));
  // 11) E or W
  Parse_NMEA_Char(10, @(h.magvar_sym));
  // 12) Mode for NMEA version 2.3 (and later)
  Parse_NMEA_Char(11, @(h.nmea23_mode));

  // if ask to apply gps time to computer time
  if Assigned(FOnApplyUTCDateTime) then
    FOnApplyUTCDateTime(@h.date, @h.time);

  // call handler
  if Assigned(FOnRMC) then
  try
    Result:=FOnRMC(@h);
  except
  end;
end;

function Tvsagps_parser_nmea.Parse_and_Send_VTG_Data(const AData, ATalkerID: String): DWORD;
{$if defined(USE_NMEA_VTG)}
var h: TNMEA_VTG;
{$ifend}
begin
  Result:=0;
{$if defined(USE_NMEA_VTG)}
  h.dwSize:=sizeof(h);
  Parse_NMEA_TalkerID(ATalkerID, @(h.chTalkerID));

  // 1) Track Degrees
  Parse_NMEA_Float32(0, @(h.trk_deg));
  // 2) T = True
  Parse_NMEA_Char(1, @(h.trk_sym));
  // 3) Track Degrees
  Parse_NMEA_Float32(2, @(h.mag_deg));
  // 4) M = Magnetic
  Parse_NMEA_Char(3, @(h.mag_sym));
  // 5) Speed Knots
  Parse_NMEA_Float32(4, @(h.knots_speed));
  // 6) N = Knots
  Parse_NMEA_Char(5, @(h.knots_sym));
  // 7) Speed Kilometers Per Hour
  Parse_NMEA_Float32(6, @(h.kmph_speed));
  // 8) K = Kilometres Per Hour
  Parse_NMEA_Char(7, @(h.kmph_sym));
  // 9) Mode for NMEA version 2.3 (and later)
  Parse_NMEA_Char(8, @(h.nmea23_mode));

  // call handler
  if Assigned(FOnVTG) then
  try
    Result:=FOnVTG(@h);
  except
  end;
{$ifend}
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_Char(const AIndex: Byte; const AChar: PChar);
var s: String;
begin
  s:=Get_NMEAPart_By_Index(AIndex);
  if (0<Length(s)) then
    AChar^:=s[1]
  else
    AChar^:=#0;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_Coord(const AIndexCoord, AIndexSymbol: Byte; const ACoord: PNMEA_Coord);
begin
  // 5432.1098 or 04321.0123
  // symbol
  Parse_NMEA_Char(AIndexSymbol, @(ACoord^.sym));
  // coord
  NMEA_Parse_Coord(Get_NMEAPart_By_Index(AIndexCoord), ACoord);
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_Date(const AIndex: Byte; const ADate: PNMEA_Date);
  procedure _SetNoData;
  begin
    ADate^.day:=0;
  end;
var
  s: String;
begin
  s:=Get_NMEAPart_By_Index(AIndex);
  if (6=Length(s)) then
  try
    // ok - treat as ddmmyy
    ADate^.day:=StrToInt(s[1]+s[2]);
    ADate^.month:=StrToInt(s[3]+s[4]);
    ADate^.year:=StrToInt(s[5]+s[6]);
  except
    _SetNoData;
  end else begin
    // no data or error
    _SetNoData;
  end;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_Float32(const AIndex: Byte; const AFloat32: PFloat32);
  procedure _SetNoData;
  begin
    AFloat32^:=cGps_Float32_no_data;
  end;
var
  dc: Char;
  s: String;
begin
  try
    s:=Get_NMEAPart_By_Index(AIndex);
    if (0=Length(s)) then begin
      _SetNoData;
    end else begin
      dc:=DecimalSeparator;
      try
        DecimalSeparator:=cNmea_Point;
        AFloat32^:=StrToFloat(s);
      finally
        DecimalSeparator:=dc;
      end;
    end;
  except
    _SetNoData;
  end;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_GSV_INFO(const AIndex: Byte; const AGSV_INFO: PNMEA_GSV_INFO);
begin
  // 4 = AIndex+0) satellite number
  Parse_NMEA_SatID(AIndex, AGSV_INFO^.sat_info.svid, AGSV_INFO^.sat_info.normalized_flag);
  // 5 = AIndex+1) elevation in degrees
  Parse_NMEA_SInt16(AIndex+1, @(AGSV_INFO^.sat_ele));
  // 6 = AIndex+2) azimuth in degrees to true
  Parse_NMEA_SInt16(AIndex+2, @(AGSV_INFO^.azimuth));
  // 7 = AIndex+3) SNR in dB
  Parse_NMEA_SInt16(AIndex+3, @(AGSV_INFO^.snr));
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_SatID(const AIndex: Byte; var ASatID: SInt8; var ASatIdNormalizedFlag: Byte);
  procedure _SetNoData;
  begin
    ASatID:=-1;
    ASatIdNormalizedFlag:=cGPS_SatID_Not_Normalized;
  end;
var
  s: String;
begin
  s:=Get_NMEAPart_By_Index(AIndex);
  if (0<Length(s)) then
  try
    ASatID:=NormalizeSatelliteID(StrToInt(s), ASatIdNormalizedFlag);
  except
    _SetNoData;
  end else begin
    // no data
    _SetNoData;
  end;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_SInt16(const AIndex: Byte; const ASInt16: PSInt16);
  procedure _SetNoData;
  begin
    ASInt16^:=-1;
  end;
var
  s: String;
begin
  s:=Get_NMEAPart_By_Index(AIndex);
  if (0<Length(s)) then
  try
    ASInt16^:=StrToInt(s);
  except
    _SetNoData;
  end else begin
    // no data
    _SetNoData;
  end;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_SInt8(const AIndex: Byte; const ASInt8: PSInt8);
  procedure _SetNoData;
  begin
    ASInt8^:=-1;
  end;
var
  s: String;
begin
  s:=Get_NMEAPart_By_Index(AIndex);
  if (0<Length(s)) then
  try
    ASInt8^:=StrToInt(s);
  except
    _SetNoData;
  end else begin
    // no data
    _SetNoData;
  end;
end;

procedure Tvsagps_parser_nmea.Parse_NMEA_Time(const AIndex: Byte; const ATime: PNMEA_Time);
  procedure _SetNoData;
  begin
    //pTime^.msec:=-1;
    Dec(ATime^.msec);
  end;

var
  s: String;
  p: Integer;

  procedure _SetHHMMSS;
  begin
    ATime^.hour:=StrToInt(s[1]+s[2]);
    ATime^.min :=StrToInt(s[3]+s[4]);
    ATime^.sec :=StrToInt(s[5]+s[6]);
  end;
begin
  ATime^.msec:=0;
  try
    // if msec<0 - no data (=194726.000)
    s:=Get_NMEAPart_By_Index(AIndex);
    if (0=Length(s)) then begin
      // no data
      _SetNoData;
    end else begin
      // data
      p:=System.Pos(cNmea_Point,s);
      if (0<p) then begin
        // has point
        if (7=p) then begin
          // ok
          _SetHHMMSS;
          System.Delete(s,1,p);
          if (0<Length(s)) then begin
            // with msec
            while (Length(s)<3) do
              s:=s+'0';
            ATime^.msec:=StrToInt(s);
          end;
        end else begin
          // error
          _SetNoData;
        end;
      end else begin
        // no point
        if (6<=Length(s)) then begin
          // ok
          _SetHHMMSS;
        end else begin
          // error
          _SetNoData;
        end;
      end;
    end;
  except
    _SetNoData;
  end;
end;

function Tvsagps_parser_nmea.Parse_Sentence_Without_Starter(const ANmeaFullStringWithoutStarter: String): DWORD;
var
  VNmeaBaseString: String;
  VChecksumFound: Boolean;
  VChecksumMatched: Boolean;
  AProprietaryWithoutFinisher: Boolean;
begin
  Result:=0;
  if (0=Length(ANmeaFullStringWithoutStarter)) then
    Exit;
  if ExtractNmeaBaseString(ANmeaFullStringWithoutStarter,
                           FALSE,
                           VNmeaBaseString,
                           VChecksumFound,
                           VChecksumMatched,
                           AProprietaryWithoutFinisher) then begin
    // right sentence
    Result:=Internal_Parse_NmeaComm_Base_Sentence(VNmeaBaseString, AProprietaryWithoutFinisher);
  end;
end;

function Tvsagps_parser_nmea.Proprietaries_SubCode_Enabled(const ACode: Tgpms_Code; const ASubCode: Word): Boolean;
begin
  Result := ((FProprietaries[ACode].SubCode and ASubCode) <> 0)
end;

function Tvsagps_parser_nmea.SomeProprietariesSupported: Boolean;
begin
  Result:=FProprietaries[gpms_Some].Supported;
  //Result:=FProprietaries[gpms_Unknown].Supported; // same check for supporting unknown (some others) proprietary sentences
end;

end.
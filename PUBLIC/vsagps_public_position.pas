(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_public_position;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  vsagps_public_base;

type
  // packed params with size of Double(=Float64) for working with sensors
  TSingleDGPSData = packed record
    Nmea23_Mode: Char; // E for Dead Reckoning Mode, D for DGPS Mode, A for Autonomous P for PPS (+N+R)
    Dimentions: Byte; // 2 or 3 (if ok)
    DGPS_Station_ID: SmallInt; // from 0000 to 1023
    DGPS_Age_Second: Single;
  end;
  PSingleDGPSData = ^TSingleDGPSData;

  // packed params with size of Double(=Float64) for working with sensors
  TSingleMagneticData = packed record
    variation_degree: Single;
    variation_symbol: Char; // E or W
    rezerved1: Byte;
    rezerved2: Word;
  end;
  PSingleMagneticData = ^TSingleMagneticData;

  TSingleGPSData = packed record
    PositionLon: Double; // X
    PositionLat: Double; // Y
    Altitude: Double;
    GeoidHeight: Double;
    Speed_KMH: Double;   // in km/h
    VSpeed_MS: Double;   // in m/s
    Heading: Double;     // true
    UTCDate: TDateTime;
    UTCTime: TDateTime;
    HDOP: Double;
    VDOP: Double;
    PDOP: Double;
    DGPS: TSingleDGPSData;
    MagVar: TSingleMagneticData;
    FixStatus: Byte; // 0 - 1 - 2
    NavMode: Char; // A or V (or #0 if no data)
    PositionOK: Boolean;
    UTCDateOK: Boolean;
    UTCTimeOK: Boolean;
    VSpeedOK: Boolean;
    AllowCalcStats: Boolean; // do not update statistics in fly-on-track mode
  end;
  PSingleGPSData = ^TSingleGPSData;

  TSingleSatsInfoEntry = packed record
    single_fix: TSingleSatFixibilityData;
    single_sky: TSingleSatSkyData;
  end;
  PSingleSatsInfoEntry = ^TSingleSatsInfoEntry;

  TSingleSatsInfoData = packed record
    entries: packed array [0..cNmea_max_sat_count-1] of TSingleSatsInfoEntry;
  end;
  PSingleSatsInfoData = ^TSingleSatsInfoData;

  TFullSatsInfoData = packed record
    gp: TSingleSatsInfoData; // gps
    gl: TSingleSatsInfoData; // glonass
  end;
  PFullSatsInfoData = ^TFullSatsInfoData;

  TSingleTrackPointData = packed record
   gps_data: TSingleGPSData;
   gpx_sats_count: Byte;
   // aligned to 8
   full_data_size: SmallInt;
   w_reserved: Word;
  end;
  PSingleTrackPointData = ^TSingleTrackPointData;

  TFullTrackPointData = packed record
    single_item: TSingleTrackPointData;
    fix_all: TVSAGPS_FIX_ALL;
    sky_fix: TFullSatsInfoData;
  end;
  PFullTrackPointData = ^TFullTrackPointData;

  TExecuteGPSCmd_WaypointData = packed record
    sz_sasx_file_name: PChar;
    sz_cmt: PChar;
    sz_desc: PChar;
    sz_sym: PChar;
  end;
  PExecuteGPSCmd_WaypointData = ^TExecuteGPSCmd_WaypointData;

procedure InitSingleGPSData(p: PSingleGPSData); inline;

function SingleGPSDataNotEmpty(const p: PSingleGPSData): Boolean;

function GetKMLCoordinate(const p: PSingleGPSData; const fs: TFormatSettings): String;

implementation

procedure InitSingleGPSData(p: PSingleGPSData);
begin
  ZeroMemory(p, sizeof(p^));
end;

function SingleGPSDataNotEmpty(const p: PSingleGPSData): Boolean;
var
  b: PChar;
  w: SmallInt;
begin
  b := Pointer(p);
  w := sizeof(p^);
  while (0<w) do
  begin
    if (#0<>b^) then
    begin
      Result := TRUE;
      Exit;
    end;
    Inc(b);
    Dec(w);
  end;
  Result := FALSE;
end;

function GetKMLCoordinate(const p: PSingleGPSData; const fs: TFormatSettings): String;
begin
  Result:='';
  if (nil<>p) then
  if p^.PositionOK then begin
    Result:=FloatToStrF(p^.PositionLon, ffFixed, 18, 14, fs)+','+FloatToStrF(p^.PositionLat, ffFixed, 18, 14, fs);
    // altitude
    if not NoData_Float64(p^.Altitude) then
      Result:=Result+','+FloatToStrF(p^.Altitude, ffFixed, 18, 10, fs);
  end;
end;

end.

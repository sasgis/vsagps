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

  TSingleTrackPointData = packed record
   gps_data: TSingleGPSData;
   gpx_sats_count: Byte;
   // aligned to 8
   // reserved
   ptr_reserved: Pointer;
   dw_reserved: DWORD;
   // sat_info
   fix_data: TSingleSatFixibilityData;
   sky_data: TSingleSatSkyData;
  end;
  PSingleTrackPointData = ^TSingleTrackPointData;

procedure InitSingleGPSData(p: PSingleGPSData); inline;

function SingleGPSDataNotEmpty(const p: PSingleGPSData): Boolean;

function GetKMLCoordinate(const p: PSingleGPSData; const fs: TFormatSettings): String;

function DeserializeSatsInfo(const pInfo: PWideChar; pSTP: PSingleTrackPointData): Boolean;

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

function DeserializeSatsInfo(const pInfo: PWideChar; pSTP: PSingleTrackPointData): Boolean;
begin
  Result:=FALSE;
  if (nil<>pInfo) then begin
    //
  end;
end;

end.
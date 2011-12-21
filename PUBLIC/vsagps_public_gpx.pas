(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_public_gpx;
(*
*)

{$I vsagps_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  vsagps_public_sysutils,
  vsagps_public_position;
  
type
  // tags and params

  // main gpx tags
  Tvsagps_GPX_main_tag = (
    // keep first
    gpx_gpx,
    // abc order
    //gpx_extensions,
    gpx_link,
    gpx_metadata,
    gpx_rte,
    gpx_rtept,
    gpx_trk,
    gpx_trkpt,
    gpx_trkseg,
    gpx_wpt
  );

  // params for trkpt and rtept and wpt
  Tvsagps_GPX_wpt_param = (
    wpt_latlon, // attributes
    wpt_ele,
    wpt_time,
    wpt_magvar,
    wpt_geoidheight,
    wpt_fix,
    wpt_sat,
    wpt_hdop,
    wpt_vdop,
    wpt_pdop,
    wpt_ageofdgpsdata,
    wpt_dgpsid,
    wpt_speed,
    wpt_course,
    wpt_vspeed_ms,
    wpt_extensions);
  Tvsagps_GPX_wpt_params = set of Tvsagps_GPX_wpt_param;

  // simple string tags for trkpt and rtept and wpt
  Tvsagps_GPX_wpt_str = (
    wpt_name,
    wpt_cmt,
    wpt_desc,
    wpt_src,
    //wpt_link,
    wpt_sym,
    wpt_type);
  Tvsagps_GPX_wpt_strs = set of Tvsagps_GPX_wpt_str;

  // string tags for sasx extension
  Tvsagps_GPX_ext_sasx_str = (
    sasx_file_name,
    sasx_sats_info,
    sasx_src
  );
  Tvsagps_GPX_ext_sasx_strs = set of Tvsagps_GPX_ext_sasx_str;

  // params for sasx extension
  Tvsagps_GPX_ext_sasx_param = (
    sasx_localtime,
    sasx_systemtime,
    sasx_timeshift);
  Tvsagps_GPX_ext_sasx_params = set of Tvsagps_GPX_ext_sasx_param;

  // string tags for trk and rte
  Tvsagps_GPX_trk_str = (
    trk_name,
    trk_cmt,
    trk_desc,
    trk_src,
    //trk_link,
    trk_number,
    trk_type);
  Tvsagps_GPX_trk_strs = set of Tvsagps_GPX_trk_str;

  // data

  // all for sasx
  Tvsagps_GPX_ext_sasx_data = packed record
    sasx_localtime: TDateTime;
    sasx_systemtime: TDateTime;
    sasx_timeshift: Double;
    sasx_strs: array [Tvsagps_GPX_ext_sasx_str] of PWideChar;
    fAvail_params: Tvsagps_GPX_ext_sasx_params;
    fAvail_strs: Tvsagps_GPX_ext_sasx_strs;
  end;
  Pvsagps_GPX_ext_sasx_data = ^Tvsagps_GPX_ext_sasx_data;

  // all for metadata
  Tvsagps_GPX_metadata_data = packed record
  
  end;
  Pvsagps_GPX_metadata_data = ^Tvsagps_GPX_metadata_data;
  
  // all for wpt and rtept and trkpt
  Tvsagps_GPX_wpt_data = packed record
    fPos: TSingleGPSData;
    fStrs: array [Tvsagps_GPX_wpt_str] of PWideChar;
    fSatFixCount: Byte;
    // availability
    fAvail_wpt_params: Tvsagps_GPX_wpt_params;
    fAvail_wpt_strs: Tvsagps_GPX_wpt_strs;
  end;
  Pvsagps_GPX_wpt_data = ^Tvsagps_GPX_wpt_data;

  // all for trk and rte
  Tvsagps_GPX_trk_data = packed record
    fStrs: array [Tvsagps_GPX_trk_str] of PWideChar;
    // availability
    fAvail_trk_strs: Tvsagps_GPX_trk_strs;
  end;
  Pvsagps_GPX_trk_data = ^Tvsagps_GPX_trk_data;
  
  // input parser options for gpx
  Tvsagps_GPX_ParserOptions = packed record
    // parse gpx parts
    bParse_trk: WordBool; // for trk, trkseg and trkpt
    bParse_rte: WordBool; // for rte and rtept
    bParse_wpt: WordBool; // only for gpx/wpt
    bParse_metadata: WordBool; // only for gpx/metadata
    // extensions
    bParse_trk_extensions: WordBool; // only for trk
    bParse_rte_extensions: WordBool; // only for rte
    bParse_wpt_extensions: WordBool; // for wpt, rtept and trkpt
    // params
    bParse_wpt_params: WordBool; // for wpt, rtept and trkpt
    bParse_links: WordBool; // links for all tags
    // aux
    //bCalc_counters: WordBool; // increment counters
  end;
  Pvsagps_GPX_ParserOptions = ^Tvsagps_GPX_ParserOptions;

  // all these counters are 1-based
  Tvsagps_GPX_Counters = packed record
    {
    trk_index: Integer; // for trk in gpx
    trkseg_index: Integer; // for trkseg in trk (in gpx)
    trkpt_index: Integer; // for trkpt in trkseg (in trk in gpx)
    wpt_index: Integer; // for wpt in gpx
    rte_index: Integer; // for rte in gpx
    rtept_index: Integer; // for rtept in rte (in gpx)
    }
  end;
  Pvsagps_GPX_Counters = ^Tvsagps_GPX_Counters;

  Tvsagps_GPX_ParserData = packed record
    // aux
    current_tag: Tvsagps_GPX_main_tag;
    subitem_tag: Tvsagps_GPX_main_tag;
    //current_counters: Tvsagps_GPX_Counters;
    extensions_data: Tvsagps_GPX_ext_sasx_data;
    // switch for base data
    case Byte of
    0:(trk_data: Tvsagps_GPX_trk_data);
    1:(wpt_data: Tvsagps_GPX_wpt_data);
    2:(metadata_data: Tvsagps_GPX_metadata_data);
  end;
  Pvsagps_GPX_ParserData = ^Tvsagps_GPX_ParserData;

  // internal helpers (buffers for tag values)
  
  Tvsagps_GPX_sasx_WideStrings = array [Tvsagps_GPX_ext_sasx_str] of WideString;
  Pvsagps_GPX_sasx_WideStrings = ^Tvsagps_GPX_sasx_WideStrings;

  Tvsagps_GPX_trk_WideStrings = record
    trk_buffers: array [Tvsagps_GPX_trk_str] of WideString;
  end;
  Pvsagps_GPX_trk_WideStrings = ^Tvsagps_GPX_trk_WideStrings;

  Tvsagps_GPX_wpt_WideStrings = record
    wpt_buffers: array [Tvsagps_GPX_wpt_str] of WideString;
  end;
  Pvsagps_GPX_wpt_WideStrings = ^Tvsagps_GPX_wpt_WideStrings;

  Tvsagps_GPX_ext_WideStrings = record
    sasx_buffers: Tvsagps_GPX_sasx_WideStrings;
  end;
  Pvsagps_GPX_ext_WideStrings = ^Tvsagps_GPX_ext_WideStrings;


const
  c_GPX_main_tag: array [Tvsagps_GPX_main_tag] of WideString =(
    // keep first
    'gpx',
    // abc order
    //'extensions',
    'link',
    'metadata',
    'rte',
    'rtept',
    'trk',
    'trkpt',
    'trkseg',
    'wpt'
  );

function GPX_get_main_tag_type(const ATagName: WideString; var t: Tvsagps_GPX_main_tag): Boolean;

function GPX_trk_str_subtag(const AName: WideString; var t: Tvsagps_GPX_trk_str): Boolean;
function GPX_wpt_str_subtag(const AName: WideString; var t: Tvsagps_GPX_wpt_str): Boolean;
function GPX_wpt_param_subtag(const AName: WideString; var t: Tvsagps_GPX_wpt_param): Boolean;

function GPX_sasx_str_subtag(const AName: WideString; var t: Tvsagps_GPX_ext_sasx_str): Boolean;
  
implementation

function GPX_get_main_tag_type(const ATagName: WideString; var t: Tvsagps_GPX_main_tag): Boolean;
var i: Tvsagps_GPX_main_tag;
begin
  for i := Low(Tvsagps_GPX_main_tag) to High(Tvsagps_GPX_main_tag) do
  if WideSameText(ATagName, c_GPX_main_tag[i]) then begin
    t:=i;
    Result:=TRUE;
    Exit;
  end;
  Result:=FALSE;
end;

function GPX_trk_str_subtag(const AName: WideString; var t: Tvsagps_GPX_trk_str): Boolean;
begin
  Result:=TRUE;
  if WideSameText(AName, 'name') then
    t:=trk_name
  else if WideSameText(AName, 'cmt') then
    t:=trk_cmt
  else if WideSameText(AName, 'desc') then
    t:=trk_desc
  else if WideSameText(AName, 'src') then
    t:=trk_src
  //else if WideSameText(AName, 'link') then
    //t:=trk_link
  else if WideSameText(AName, 'number') then
    t:=trk_number
  else if WideSameText(AName, 'type') then
    t:=trk_type
  else
    Result:=FALSE;
end;

function GPX_wpt_str_subtag(const AName: WideString; var t: Tvsagps_GPX_wpt_str): Boolean;
begin
  Result:=TRUE;
  if WideSameText(AName, 'name') then
    t:=wpt_name
  else if WideSameText(AName, 'cmt') then
    t:=wpt_cmt
  else if WideSameText(AName, 'desc') then
    t:=wpt_desc
  else if WideSameText(AName, 'src') then
    t:=wpt_src
  //else if WideSameText(AName, 'link') then
    //t:=wpt_link
  else if WideSameText(AName, 'sym') then
    t:=wpt_sym
  else if WideSameText(AName, 'type') then
    t:=wpt_type
  else
    Result:=FALSE;
end;

function GPX_wpt_param_subtag(const AName: WideString; var t: Tvsagps_GPX_wpt_param): Boolean;
begin
  Result:=TRUE;
  // wpt_latlon - as attributes
  // wpt_vspeed_ms - in extensions
  if WideSameText(AName, 'ele') then
    t:=wpt_ele
  else if WideSameText(AName, 'time') then
    t:=wpt_time
  else if WideSameText(AName, 'magvar') then
    t:=wpt_magvar
  else if WideSameText(AName, 'geoidheight') then
    t:=wpt_geoidheight
  else if WideSameText(AName, 'fix') then
    t:=wpt_fix
  else if WideSameText(AName, 'sat') then
    t:=wpt_sat
  else if WideSameText(AName, 'hdop') then
    t:=wpt_hdop
  else if WideSameText(AName, 'vdop') then
    t:=wpt_vdop
  else if WideSameText(AName, 'pdop') then
    t:=wpt_pdop
  else if WideSameText(AName, 'ageofdgpsdata') then
    t:=wpt_ageofdgpsdata
  else if WideSameText(AName, 'dgpsid') then
    t:=wpt_dgpsid
  else if WideSameText(AName, 'speed') then
    t:=wpt_speed
  else if WideSameText(AName, 'course') then
    t:=wpt_course
  else if WideSameText(AName, 'extensions') then
    t:=wpt_extensions
  else
    Result:=FALSE;
end;

function GPX_sasx_str_subtag(const AName: WideString; var t: Tvsagps_GPX_ext_sasx_str): Boolean;
begin
  Result:=TRUE;
  if WideSameText(AName, 'file_name') then
    t:=sasx_file_name
  else if WideSameText(AName, 'sats_info') then
    t:=sasx_sats_info
  else if WideSameText(AName, 'src') then
    t:=sasx_src
  else
    Result:=FALSE;
end;

end.

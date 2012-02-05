(*
  VSAGPS Library. Copyright (C) 2011, Sergey Vasketsov
  Please read <info_*.txt> file for License details and conditions (GNU GPLv3)
*)
unit vsagps_public_xml_dom;
(*
*)

{$I vsagps_defines.inc}

interface

uses
  SysUtils,
{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
  xmldom,
{$ifend}
{$if defined(VSAGPS_USE_XERCES_XML_IMPORT)}
  xercesxmldom,
{$ifend}
{$if defined(VSAGPS_USE_MSXML_IMPORT)}
  msxmldom,
{$ifend}
  XMLConst,
  Classes;

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
type
  EVSAGPS_Base_XML_Exception = class(Exception);
  EVSAGPS_No_XML_DOM_Vendors = class(EVSAGPS_Base_XML_Exception);
  EVSAGPS_Error_Loading_XML = class(EVSAGPS_Base_XML_Exception);
{$ifend}

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Create_DOMDocument(var ADOMDocument: IDOMDocument; const ARaiseErrorIfFALSE: Boolean; const ASetValidationMode: Byte=0): Boolean;
{$ifend}

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Load_DOMDocument_FromStream(const ADOMDocument: IDOMDocument; const AStream: TStream; const ARaiseErrorIfFALSE: Boolean): Boolean;
{$ifend}

implementation

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Create_DOMDocument(var ADOMDocument: IDOMDocument; const ARaiseErrorIfFALSE: Boolean; const ASetValidationMode: Byte): Boolean;

  procedure _CheckValidationMode;
  var VDOMParseOptions: IDOMParseOptions;
  begin
    {ASetValidationMode: 0 - nothing, 1 - force set FALSE, 2 - force set TRUE}
    if (0<>ASetValidationMode) then
    if Assigned(ADOMDocument) then
    try
      VDOMParseOptions := ADOMDocument as IDOMParseOptions;
      if Assigned(VDOMParseOptions) then begin
        VDOMParseOptions.validate:=((ASetValidationMode-1)<>0);
      end;
    except
    end;
  end;
begin
  Result:=FALSE;

{$if defined(VSAGPS_USE_XERCES_XML_IMPORT)}
  if (not Assigned(ADOMDocument)) then
  try
    ADOMDocument:=XercesDOM.DOMImplementation.createDocument('','',nil);
    Inc(Result);
  except
  end;
{$ifend}

{$if defined(VSAGPS_USE_MSXML_IMPORT)}
  if (not Assigned(ADOMDocument)) then
  try
    ADOMDocument:=MSXML_DOM.DOMImplementation.createDocument('','',nil);
    // reset validation mode only for msxml
    _CheckValidationMode;
    Inc(Result);
  except
  end;
{$ifend}

  if ARaiseErrorIfFALSE and (not Result) then
    raise EVSAGPS_No_XML_DOM_Vendors.Create('');
end;
{$ifend}

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Load_DOMDocument_FromStream(const ADOMDocument: IDOMDocument; const AStream: TStream; const ARaiseErrorIfFALSE: Boolean): Boolean;
var
  VDOMPersist: IDOMPersist;
  VParseError: IDOMParseError;
  VMsg: String;
begin
  Result:=FALSE;
  VDOMPersist:=nil;

  if Assigned(ADOMDocument) and Assigned(AStream) then
  try
    // make loader
    VDOMPersist := ADOMDocument as IDOMPersist;
    // load
    if Assigned(VDOMPersist) then
      if (VDOMPersist.loadFromStream(AStream)<>FALSE) then
        Result:=TRUE;
  finally
    VDOMPersist:=nil;
  end;

  if ARaiseErrorIfFALSE and (not Result) then begin
    VMsg:='';
    VParseError:=nil;
    try
      // see TXMLDocument.LoadData
      VParseError := ADOMDocument as IDOMParseError;
      with VParseError do
        VMsg := Format('%s%s%s: %d%s%s', [Reason, SLineBreak, SLine, Line, SLineBreak, Copy(SrcText, 1, 40)]);
    except
    end;
    // raise
    {if (0<Length(VMsg)) and Assigned(VParseError) then
      raise EDOMParseError.Create(VParseError, VMsg)
    else}
      raise EVSAGPS_Error_Loading_XML.Create(VMsg);
  end;
end;
{$ifend}

end.
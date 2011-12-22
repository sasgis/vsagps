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
  Classes;

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
type
  EVSAGPS_Base_XML_Exception = class(Exception);
  EVSAGPS_No_XML_DOM_Vendors = class(EVSAGPS_Base_XML_Exception);
  EVSAGPS_Error_Loading_XML = class(EVSAGPS_Base_XML_Exception);
{$ifend}

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Create_DOMDocument(var ADOMDocument: IDOMDocument; const ARaiseErrorIfFALSE: Boolean): Boolean;
{$ifend}

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Load_DOMDocument_FromStream(const ADOMDocument: IDOMDocument; const AStream: TStream; const ARaiseErrorIfFALSE: Boolean): Boolean;
{$ifend}

implementation

{$if defined(VSAGPS_USE_SOME_KIND_OF_XML_IMPORT)}
function VSAGPS_Create_DOMDocument(var ADOMDocument: IDOMDocument; const ARaiseErrorIfFALSE: Boolean): Boolean;
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
var VDOMPersist: IDOMPersist;
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

  if ARaiseErrorIfFALSE and (not Result) then
    raise EVSAGPS_Error_Loading_XML.Create('');
end;
{$ifend}

end.
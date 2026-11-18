program nmea_client;

uses
  Vcl.Forms,
  Vcl.XPMan,
{$IFDEF MADEXECT}
  u_MadExcept in '..\madexcept\u_MadExcept.pas',
{$ENDIF}
  nmea_client_main in 'nmea_client_main.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'NMEA Client (TCP/UDP)';
  Application.CreateForm(TfrmNmeaClient, frmNmeaClient);
  Application.Run;
end.

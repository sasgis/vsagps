program nmea_server;

uses
  Vcl.Forms,
  Vcl.XPMan,
{$IFDEF MADEXECT}
  u_MadExcept in '..\madexcept\u_MadExcept.pas',
{$ENDIF}
  nmea_server_main in 'nmea_server_main.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'NMEA Server (TCP/UDP)';
  Application.CreateForm(TfrmNmeaServer, frmNmeaServer);
  Application.Run;
end.

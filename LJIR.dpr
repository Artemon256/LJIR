program LJIR;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {fmMain},
  Fix in 'Fix.pas',
  LJAPI in 'LJAPI.pas',
  FlickrAPI in 'FlickrAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, MainForm.fmMain);
  Application.Run;
end.

program LJIR;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {fmMain},
  LJAPI in 'LJAPI.pas',
  FlickrAPI in 'FlickrAPI.pas',
  XMLObject in 'XMLObject.pas',
  Fix in 'Fix.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.NetEncoding, Vcl.StdCtrls, idHash,
  ShellAPI, Vcl.ComCtrls, Fix, ClipBrd, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
  Vcl.Samples.Gauges, System.Actions, Vcl.ActnList, FileCtrl;

type
  TfmMain = class(TForm)
    pcMain: TPageControl;
    tsLJLogin: TTabSheet;
    gbParams: TGroupBox;
    gbExplain: TGroupBox;
    Label1: TLabel;
    edLogin: TEdit;
    btLJLogin: TButton;
    edPass: TEdit;
    tsFlickr: TTabSheet;
    gbExplain2: TGroupBox;
    gbParams2: TGroupBox;
    btUnderstand: TButton;
    tsFlickrAuth: TTabSheet;
    gbExplain3: TGroupBox;
    Label3: TLabel;
    gbParams3: TGroupBox;
    btFlickrAuth: TButton;
    Image1: TImage;
    tsProxy: TTabSheet;
    gbExplain4: TGroupBox;
    gbParams4: TGroupBox;
    Label4: TLabel;
    edHost: TEdit;
    edProxyLogin: TEdit;
    edProxyPass: TEdit;
    edPort: TEdit;
    tsMain: TTabSheet;
    moPosts: TMemo;
    moDomains: TMemo;
    btStart: TButton;
    Label2: TLabel;
    btOK: TButton;
    pbCurrent: TGauge;
    pbTotal: TGauge;
    pcReuploader: TPageControl;
    tsSetting: TTabSheet;
    tsProgress: TTabSheet;
    moProgress: TMemo;
    btStop: TButton;
    btPause: TButton;
    lbHint: TLabel;
    alMain: TActionList;
    acSelectAll: TAction;
    tsChooseDir: TTabSheet;
    gbExplain5: TGroupBox;
    Label5: TLabel;
    gbParams5: TGroupBox;
    btChooseImageFolder: TButton;
    edPathToFolder: TEdit;
    btOk2: TButton;
    procedure btLJLoginClick(Sender: TObject);
    procedure btUnderstandClick(Sender: TObject);
    procedure btFlickrAuthClick(Sender: TObject);
    procedure edProxyPassClick(Sender: TObject);
    procedure edProxyLoginClick(Sender: TObject);
    procedure edHostClick(Sender: TObject);
    procedure edPortClick(Sender: TObject);
    procedure edPassClick(Sender: TObject);
    procedure edLoginClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure moPostsMouseEnter(Sender: TObject);
    procedure moPostsMouseLeave(Sender: TObject);
    procedure moDomainsMouseEnter(Sender: TObject);
    procedure moDomainsMouseLeave(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure btChooseImageFolderClick(Sender: TObject);
    procedure edPathToFolderExit(Sender: TObject);
    procedure btOk2Click(Sender: TObject);
  private
    ImgDir: String;
  public
    procedure PBCB(value: Integer; Total: Boolean);
    procedure EndCallback;
    procedure ErrCB;
  end;

var
  fmMain: TfmMain;
  Fix: TFix;
  PostIndex: Integer;
  ErrPosts: TStringList;
  Pause: Boolean;

implementation

{$R *.dfm}

procedure TfmMain.ErrCB;
begin
  moProgress.Lines[moProgress.Lines.Count-1] := Fix.LastPost + ' - ошибка!';
  ErrPosts.Add(Fix.LastPost);
  ErrPosts.SaveToFile('errposts.txt');
end;

procedure TfmMain.PBCB(value: Integer; Total: Boolean);
begin
  if Total then
  begin
    pbCurrent.MaxValue := value;
    pbCurrent.MinValue := 0;
    moProgress.Lines.Add(Fix.LastPost + ' - обрабатывается');
  end
  else
    pbCurrent.Progress := value;
  pbCurrent.Update;
end;

procedure TfmMain.acSelectAllExecute(Sender: TObject);
begin
  if ActiveControl is TMemo then
    (ActiveControl as TMemo).SelectAll;
end;

procedure TfmMain.btChooseImageFolderClick(Sender: TObject);
var
  Folder: String;
begin
  if SelectDirectory('Выберите папку с файлами','Desktop',Folder) then
  begin
    edPathToFolder.Text := Folder;
  end;
end;

procedure TfmMain.btFlickrAuthClick(Sender: TObject);
begin
  Fix.FlickrAuth(Clipboard.AsText);
  pcMain.ActivePage := tsChooseDir;
end;

procedure TfmMain.btLJLoginClick(Sender: TObject);
begin
  Fix := TFix.Create(edLogin.Text,edPass.Text);
  pcMain.ActivePage := tsFlickr;
end;

procedure TfmMain.btOk2Click(Sender: TObject);
begin
  if not DirectoryExists(edPathToFolder.Text) then
  begin
    ShowMessage('Заданный путь недействителен');
  end
  else
  begin
    ImgDir := edPathToFolder.Text;
    pcMain.ActivePage := tsMain;
    pcReuploader.ActivePage := tsSetting;
  end;
end;

procedure TfmMain.btOKClick(Sender: TObject);
begin
  if (edHost.Text<>'Хост') and (edHost.Text<>'') then
    if (edProxyLogin.Text<>'Логин') and (edProxyLogin.Text<>'') then
      Fix.SetProxy(edHost.Text,StrToInt(edPort.Text),edProxyLogin.Text,edProxyPass.Text)
    else
      Fix.SetProxy(edHost.Text,StrToInt(edPort.Text));
  pcMain.ActivePage := tsMain;
  pcReuploader.ActivePage := tsSetting;
end;

procedure TfmMain.btPauseClick(Sender: TObject);
begin
  Pause := not Pause;
  if Pause then
  begin
    btPause.Caption := 'РАБОТАЙ';
    Fix.Suspend;
  end
  else
  begin
    btPause.Caption := 'ПАУЗА';
    Fix.Resume;
  end;
end;

procedure TfmMain.EndCallback;
begin
  if Pos('ошибка',moProgress.Lines[moProgress.Lines.Count-1])=0 then
    moProgress.Lines[moProgress.Lines.Count-1] := Fix.LastPost + ' - готово';

  inc(PostIndex);
  pbCurrent.Progress := 0;
  pbTotal.Progress := pbTotal.Progress + 1;
  if PostIndex >= moPosts.Lines.Count then
  begin
    btPause.Enabled := False;
    btStop.Caption := 'НАЗАД';
    pbTotal.Progress := 0;
    pcMain.Enabled := True;
    ShowMessage('Я всё!');
    Exit;
  end;
end;

procedure TfmMain.btStartClick(Sender: TObject);
begin
  btPause.Enabled := True;
  btStop.Caption := 'ЗАГЛОХНИ';
  Fix := Fix.MakeCopy;
  moProgress.Clear;
  pcReuploader.ActivePage := tsProgress;
  pbTotal.MaxValue := fmMain.moPosts.Lines.Count;
  pbTotal.Progress := 0;
  PostIndex := 0;
  Fix.PBCallback := PBCB;
  Fix.EndCallback := EndCallback;
  Fix.ErrCallback := ErrCB;
  Fix.URLS.Assign(moPosts.Lines);
  Fix.Domains.Assign(moDomains.Lines);
  Fix.ImgDir := ImgDir;
  Fix.Start;
//  pcMain.Enabled := False;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  Pause := False;
  btPause.Caption := 'ПАУЗА';
  Fix.Terminate;
  pcReuploader.ActivePage := tsSetting;
end;

procedure TfmMain.btUnderstandClick(Sender: TObject);
begin
  Fix.FlickrRedirect;
  pcMain.ActivePage := tsFlickrAuth;
end;

procedure TfmMain.edHostClick(Sender: TObject);
begin
  if edHost.Text <> 'Хост' then Exit;
  edHost.Text := '';
end;

procedure TfmMain.edLoginClick(Sender: TObject);
begin
  if edLogin.Text <> 'Логин' then Exit;
  edLogin.Text := '';
end;

procedure TfmMain.edPassClick(Sender: TObject);
begin
  if edPass.Text <> 'Пароль' then Exit;
  edPass.Text := '';
  edPass.PasswordChar := '*';
end;

procedure TfmMain.edPathToFolderExit(Sender: TObject);
begin
  if not DirectoryExists(edPathToFolder.Text) then
  begin
    ShowMessage('Заданный путь недействителен!');
    edPathToFolder.Text := '';
  end;
end;

procedure TfmMain.edPortClick(Sender: TObject);
begin
  if edPort.Text <> 'Порт' then Exit;
  edPort.Text := '';
end;

procedure TfmMain.edProxyLoginClick(Sender: TObject);
begin
  if edProxyLogin.Text <> 'Логин' then Exit;
  edProxyLogin.Text := '';
end;

procedure TfmMain.edProxyPassClick(Sender: TObject);
begin
  if edProxyPass.Text <> 'Пароль' then Exit;
  edProxyPass.Text := '';
  edProxyPass.PasswordChar := '*';
end;

function OwnDir: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function SlashDir(path: String): String;
begin
  Result:=path;
  if Length(path)=0 then Exit;
  if path[Length(path)]='\' then Exit;
  Result:=path+'\';
end;

procedure TfmMain.FormShow(Sender: TObject);
var
  Config: TextFile;
begin
  if not FileExists('config.ini') then
  begin
    AssignFile(Config,'config.ini');
    Rewrite(Config);
    Writeln(Config,'[FIX]');
    Writeln(Config,'minimal_picture_size=4096');
    CloseFile(Config);
  end;
  System.SysUtils.CreateDir(SlashDir(OwnDir)+'backup');
  System.SysUtils.CreateDir(SlashDir(OwnDir)+'temp');
  pcMain.ActivePage := tsLJLogin;
  ErrPosts := TStringList.Create;
  Pause := False;
end;

procedure TfmMain.moDomainsMouseEnter(Sender: TObject);
begin
  lbHint.Font.Size := 8;
  lbHint.Caption := 'А сюда - фильтры доменов. Чтобы добавить правило - просто впишите его с новой строки. Чтобы исключить домен, укажите перед ним "!". Чтобы указать все домены, кроме исключённых, добавьте "*"';
end;

procedure TfmMain.moDomainsMouseLeave(Sender: TObject);
begin
  lbHint.Font.Size := 12;
  lbHint.Caption := '';
end;

procedure TfmMain.moPostsMouseEnter(Sender: TObject);
begin
  lbHint.Caption := 'Сюда пихайте ссылки на посты';
end;

procedure TfmMain.moPostsMouseLeave(Sender: TObject);
begin
  lbHint.Caption := '';
end;

end.

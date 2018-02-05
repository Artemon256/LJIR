unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.NetEncoding, Vcl.StdCtrls, idHash,
  ShellAPI, Vcl.ComCtrls, Fix, ClipBrd, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
  Vcl.Samples.Gauges;

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
    procedure moDomainsClick(Sender: TObject);
    procedure moPostsClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure PBCB(value: Integer; Total: Boolean);
    procedure EndCallback;
  end;

var
  fmMain: TfmMain;
  Fix: TFix;
  PostIndex: Integer;

implementation

{$R *.dfm}

procedure TfmMain.PBCB(value: Integer; Total: Boolean);
begin
  if Total then
  begin
    pbCurrent.MaxValue := value;
    pbCurrent.MinValue := 0;
  end
  else
    pbCurrent.Progress := value;
  pbCurrent.Update;
end;

procedure TfmMain.btFlickrAuthClick(Sender: TObject);
begin
  Fix.FlickrAuth(Clipboard.AsText);
  pcMain.ActivePage := tsProxy;
end;

procedure TfmMain.btLJLoginClick(Sender: TObject);
begin
  Fix := TFix.Create(edLogin.Text,edPass.Text);
  pcMain.ActivePage := tsFlickr;
end;

procedure TfmMain.btOKClick(Sender: TObject);
begin
  if (edHost.Text<>'Хост') and (edHost.Text<>'') then
    if (edProxyLogin.Text<>'Логин') and (edProxyLogin.Text<>'') then
      Fix.SetProxy(edHost.Text,StrToInt(edPort.Text),edProxyLogin.Text,edProxyPass.Text)
    else
      Fix.SetProxy(edHost.Text,StrToInt(edPort.Text));
  pcMain.ActivePage := tsMain;
end;

procedure TfmMain.EndCallback;
begin
  pbCurrent.Progress := 0;
  pbTotal.Progress := pbTotal.Progress + 1;
  if PostIndex >= moPosts.Lines.Count then
  begin
    Fix := Fix.MakeCopy;
    pbTotal.Progress := 0;
    pcMain.Enabled := True;
    ShowMessage('Я всё!');
    Exit;
  end;
  inc(PostIndex);
end;

procedure TfmMain.btStartClick(Sender: TObject);
begin
  pbTotal.MaxValue := fmMain.moPosts.Lines.Count;
  pbTotal.Progress := 0;
  PostIndex := 0;
  Fix.PBCallback := PBCB;
  Fix.EndCallback := EndCallback;
  Fix.URLS.Assign(moPosts.Lines);
  Fix.Domains.Assign(moDomains.Lines);
  Fix.Start;
  pcMain.Enabled := False;
  EndCallback;
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

procedure TfmMain.FormShow(Sender: TObject);
begin
  pcMain.ActivePage := tsLJLogin;
end;

procedure TfmMain.moDomainsClick(Sender: TObject);
begin
  if moDomains.Text = 'А сюда - домены, которые хотите обработать'+#13#10 then
    moDomains.Clear;
end;

procedure TfmMain.moPostsClick(Sender: TObject);
begin
  if moPosts.Text = 'Сюда пихайте ссылки на посты'+#13#10 then
    moPosts.Clear;
end;

end.

unit Fix;

interface

uses
  System.Net.HTTPClientComponent, LJAPI, FlickrAPI, System.Classes, ShellAPI,
  WinAPI.Windows, System.SysUtils, System.Net.URLClient;

type
  TProgressBarCallback = procedure(value: Integer; Total: Boolean) of object;
  TEndCallback = procedure of object;
  TErrCallback = procedure of object;
  TFix = class(TThread)
    private
      HTTPclient: TNetHTTPClient;
      LiveJournal: TLiveJournal;
      Flickr: TFlickr;
      function UploadPhoto(Path: string): String;
      function DownloadPhoto(PhotoURL: string): String;
      procedure FixEvent(EventURL: string);
      const
        APIKEY = '0de65c9c27e2573c08899e307e94d786';
        SECRET = 'f1a32811594e2057';
    public
      Domains, URLS: TStringList;
      PBCallback: TProgressBarCallback;
      EndCallback: TEndCallback;
      ErrCallback: TErrCallback;
      constructor Create(Login, Pass: String); overload;
      procedure FlickrRedirect;
      procedure FlickrAuth(Verifier: String);
      constructor Create(NewFlickr: TFlickr; NewLJ: TLiveJournal; NewHTTP: TNetHTTPClient); overload;
      destructor Free;
      procedure SetProxy(ProxyHost: string; ProxyPort: Integer; ProxyLogin: string = '';
        ProxyPass: string = '');
      function MakeCopy: TFix;
      procedure Execute; override;
      function Reupload(URL: String): String;
  end;

implementation

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

constructor TFix.Create(Login, Pass: String);
begin
  inherited Create(True);
  LiveJournal := TLiveJournal.Create(Login, Pass);
  Flickr := TFlickr.Create(APIKEY,SECRET);
  HTTPclient := TNetHTTPClient.Create(nil);
  HTTPclient.ResponseTimeout := 5000;
  Domains := TStringList.Create;
  URLS := TStringList.Create;
  Flickr.GetRequestToken;
end;

constructor TFix.Create(NewFlickr: TFlickr; NewLJ: TLiveJournal; NewHTTP: TNetHTTPClient);
begin
  inherited Create(True);
  LiveJournal := NewLJ;
  Flickr := NewFlickr;
  HTTPclient := NewHTTP;
  HTTPclient.ResponseTimeout := 5000;
  Domains := TStringList.Create;
  URLS := TStringList.Create;
end;

procedure TFix.FlickrAuth(Verifier: string);
begin
  Flickr.Auth(Verifier);
end;

procedure TFix.FlickrRedirect;
begin
  ShellExecute(0,'open',PWideChar(Flickr.VerifyURL),'','',SW_HIDE);
end;

destructor TFix.Free;
begin
  Domains.Free;
  URLS.Free;
  LiveJournal.Free;
  Flickr.Free;
  HTTPclient.Free;
end;

procedure TFix.FixEvent(EventURL: string);
var
  Post: TLJPost;
  Data, CurURL: String;
  BackupStream: TStringStream;
  i, j, ps, q: Integer;
  IsTag, IsImgTag, IsSrc, IsEq, IsURL, IsURLClosed, DomainFound: Boolean;
begin
  BackupStream := TStringStream.Create;
  try
    Post := LiveJournal.GetEvent(EventURL);
    Synchronize(procedure begin
      if Assigned(PBCallback) then
        PBCallback(Length(Post.Text),True);
    end);
    BackupStream.WriteString(Post.Text);
    BackupStream.SaveToFile(SlashDir(OwnDir)+'backup\'+
      ExtractFileName(StringReplace(EventURL,'/','\',[rfReplaceAll])));
    BackupStream.Free;
    IsTag := False; IsImgTag := False; IsSrc := False; IsEq := False; IsURL := False;
    IsURLClosed := False;
    for i := 1 to Length(Post.Text) do
    begin
      Synchronize(procedure begin
        if Assigned(PBCallback) then
          PBCallback(i,False);
      end);
      if IsURLClosed then
      begin
        IsURLClosed := False;
        if Domains.Count=0 then
          Data := Data + '"' + Reupload(CurURL) + '"'
        else
        begin
          DomainFound := False;
          for j := 0 to Domains.Count-1 do
            if Pos(Domains[j],CurURL)>0 then
            begin
              DomainFound := True;
              Break;
            end;
          if DomainFound then
            Data := Data + '"' + Reupload(CurURL) + '"'
          else
            Data := Data + '"' + CurURL + '"';
        end;
        CurURL := '';
      end;

      if (Post.Text[i]='<') and not IsTag then
        IsTag := True;
      if (Post.Text[i]+Post.Text[i+1]+Post.Text[i+2]='img') and IsTag and not IsImgTag then
        IsImgTag := True;
      if (Post.Text[i]+Post.Text[i+1]+Post.Text[i+2]='src') and IsImgTag and not IsSrc then
        IsSrc := True;
      if (Post.Text[i]='=') and IsSrc and not IsEq then
        IsEq := True;
      if ((Post.Text[i]='"') or (Post.Text[i]='''')) and IsEq and not IsURL then
      begin
        IsURL := True;
        Continue;
      end;

      if ((Post.Text[i]='"') or (Post.Text[i]='''')) and IsURL then
      begin
        IsURL := False;
        IsEq := False;
        IsSrc := False;
        IsURLClosed := True;
        Continue;
      end;
      if (Post.Text[i]='>') and IsTag then
      begin
        IsTag := False;
        IsImgTag := False;
      end;

      if IsURL then
        CurURL := CurURL + Post.Text[i]
      else
        Data := Data + Post.Text[i];
    end;
  except
    Synchronize(procedure begin
      if Assigned(ErrCallback) then
        ErrCallback;
    end);
    Synchronize(procedure begin
      if Assigned(EndCallback) then
        EndCallback;
    end);
    raise;
  end;

  ps := Pos('share.php?type=livejournal&amp;id=',Data);
  if ps>0 then
  begin
    for i := ps downto 1 do
    begin
      q:=i;
      if Data[i]='<' then
        Break;
    end;
    Data := Copy(Data,1,q-1);
    Data := Data + '<lj-like buttons="repost,facebook,twitter,google,vkontakte" /></lj-cut>';
  end;

  Post.Text := Data;
  LiveJournal.EditEvent(Post);
  Synchronize(procedure begin
    if Assigned(EndCallback) then
      EndCallback;
  end);
end;

function GetFileSize(Path: String): Int64;
var
  F: File of Byte;
begin
  AssignFile(F,Path);
  Reset(F);
  Result := FileSize(F);
  CloseFile(F);
end;

function TFix.Reupload(URL: string): String;
var
  Path: String;
begin
  Result := URL;
  Path := DownloadPhoto(URL);
  if GetFileSize(Path)<4096 then
  begin
    System.SysUtils.DeleteFile(Path);
    Exit;
  end;
  Result := UploadPhoto(Path);
end;

function TFix.UploadPhoto(Path: string): String;
begin
  try
    try
      Result := Flickr.UploadPhoto(Path);
    except
      Sleep(1000);
      try
        Result := Flickr.UploadPhoto(Path);
      except
        Sleep(1000);
        Result := Flickr.UploadPhoto(Path);
      end;
    end;
  finally
    System.SysUtils.DeleteFile(Path);
  end;
end;

function TFix.DownloadPhoto(PhotoURL: string): String;
var
  ImgStream: TFileStream;
  FN: String;
begin
  FN := SlashDir(OwnDir)+'temp\'+Flickr.GetTimeStamp;
  ImgStream := TFileStream.Create(FN,fmCreate);
  HTTPclient.Get(PhotoURL,ImgStream);
  ImgStream.Free;
  Result := FN;
end;

procedure TFix.SetProxy(ProxyHost: string; ProxyPort: Integer; ProxyLogin: string = ''; ProxyPass: string = '');
var
  EmptyProxySettings: TProxySettings;
  OldIP, NewIP: String;
const
  IP_URL = 'https://api.ipify.org/?format=raw';
begin
  OldIP := HTTPclient.Get(IP_URL).ContentAsString;
  HTTPclient.ProxySettings := TProxySettings.Create(ProxyHost,ProxyPort,ProxyLogin,ProxyPass,'http');
  try
    NewIP := HTTPclient.Get(IP_URL).ContentAsString;
  except
    HTTPclient.ProxySettings := EmptyProxySettings;
    raise Exception.Create('Ошибка при установке прокси (Fix/HTTP)');
  end;
  if OldIP=NewIP then
    raise Exception.Create('Ошибка при установке прокси (Fix/Proxy)');
end;

function TFix.MakeCopy: TFix;
begin
  Result := TFix.Create(Flickr,LiveJournal,HTTPclient);
  Domains.Free;
  URLS.Free;
end;

procedure TFix.Execute;
var
  i: Integer;
begin
  for i := 0 to URLS.Count-1 do
    try
      FixEvent(URLS[i]);
    except
      Continue;
    end;
end;

end.

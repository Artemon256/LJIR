unit Fix;

interface

uses
  FlickrAPI, LJAPI, System.Net.HTTPClientComponent, System.Net.URLClient,
  System.SysUtils, System.Classes, ShellAPI, WinAPI.Windows;

type
  TProgressBarCallback = procedure(value: Integer; Total: Boolean) of object;
  TEndCallback = procedure of object;
  TFix = class(TThread)
    private
      LiveJournal: TLiveJournal;
      Flickr: TFLickr;
      HTTPclient: TNetHTTPClient;
      procedure FixEvent(EventURL: string; EventDomains: TStringList);
      function UploadPhoto(Path: string): String;
      function DownloadPhoto(PhotoURL: string): String;
      const
        FLICKR_APIKEY = '0de65c9c27e2573c08899e307e94d786';
        FLICKR_SECRET = 'f1a32811594e2057';
    public
      Domains, URLS: TStringList;
      PBCallback: TProgressBarCallback;
      EndCallback: TEndCallback;
      constructor Create(Login, Pass: String); overload;
      constructor Create(FL: TFlickr; LJ: TLiveJournal; HTTP: TNetHTTPClient); overload;
      destructor Free;
      procedure Execute; override;
      procedure SetProxy(ProxyHost: string; ProxyPort: Integer; ProxyLogin: string = '';
        ProxyPass: string = '');
      procedure FlickrRedirect;
      procedure FlickrAuth(Verifier: String);
      destructor FreeWithoutAPI;
      function MakeCopy: TFix;
  end;

implementation

constructor TFix.Create(Login, Pass: String);
begin
  inherited Create(True);
  LiveJournal := TLiveJournal.Create(Login,Pass);
  Flickr := TFlickr.Create(FLICKR_APIKEY,FLICKR_SECRET);
  HTTPclient := TNetHTTPClient.Create(nil);
  Domains := TStringList.Create;
  URLS := TStringList.Create;
end;

constructor TFix.Create(FL: TFlickr; LJ: TLiveJournal; HTTP: TNetHTTPClient);
begin
  inherited Create(True);
  LiveJournal := LJ;
  Flickr := FL;
  HTTPclient := TNetHTTPClient.Create(nil);
  Domains := TStringList.Create;
  URLS := TStringList.Create;
end;

destructor TFix.Free;
begin
  URLS.Free;
  Domains.Free;
  LiveJournal.Free;
  Flickr.Free;
  HTTPclient.Free;
  inherited Destroy;
end;

destructor TFix.FreeWithoutAPI;
begin
  URLS.Free;
  Domains.Free;
end;

procedure TFix.FlickrAuth(Verifier: string);
begin
  Flickr.Auth(Verifier);
end;

procedure TFix.FlickrRedirect;
begin
  ShellExecute(0,'open',PWideChar(Flickr.VerifyURL),'','',SW_HIDE);
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

procedure TFix.FixEvent(EventURL: string; EventDomains: TStringList);
var
  i, j: Integer;
  Searching: Byte;
  Data, Result, PhotoURL: String;
  Post: TLJPost;
begin
  Searching := 0;
  Result := '';
  Post := LiveJournal.GetEvent(EventURL);
  Data := Post.Text;
  Synchronize(procedure begin
    if Assigned(PBCallback) then
      PBCallback(Length(Data),True);
  end);

  try
    for i := 1 to Length(Data) do
    begin
      Synchronize(procedure begin
        if Assigned(PBCallback) then
          PBCallback(i,False);
      end);
      if Data[i]+Data[i+1]='</' then begin
        Searching:=0;
        Result := Result + Data[i];
        Continue;
      end;

      if (Searching=0) and (Data[i]='<') then Searching:=1;
      if (Searching=1) and (Data[i]+Data[i+1]+Data[i+2]='img') then Searching:=2;
      if (Searching=2) and (Data[i]+Data[i+1]+Data[i+2]+Data[i+3]='src=') then Searching:=3;
      if (Searching=3) and ((Data[i]='"') or (Data[i]='''')) then
      begin
        Searching:=4;
        Result := Result + Data[i];
        Continue;
      end;
      if (Searching=4) and ((Data[i]='"') or (Data[i]='''')) then
      begin
        Searching:=5;
        Result := Result + '%s';
        Result := Result + Data[i];
        Continue;
      end;
      if (Searching=4) and (Data[i]<>' ') then
      begin
        PhotoURL := PhotoURL+Data[i];
        Continue;
      end;
      if (Searching=5) and (Data[i]='>') then
      begin
        if EventDomains.Count>0 then
        begin
          for j := 0 to EventDomains.Count-1 do
            if Pos(EventDomains[j],PhotoURL)<>0 then
            begin
              Result:=Format(Result,[UploadPhoto(DownloadPhoto(PhotoURL))]);
              Break;
            end;
        end
        else
          Result:=Format(Result,[UploadPhoto(DownloadPhoto(PhotoURL))]);
        Result:=Format(Result,[PhotoURL]);
        PhotoURL:='';
        Searching:=0;
      end;
      Result := Result + Data[i];
    end;
  finally
    Post.Text := Result;
    LiveJournal.EditEvent(Post);
    Synchronize(procedure begin
      if Assigned(EndCallback) then
        EndCallback;
    end)
  end;
end;

function TFix.UploadPhoto(Path: string): String;
begin
  Result := Flickr.UploadPhoto(Path);
  DeleteFile(PWideChar(Path));
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

function TFix.DownloadPhoto(PhotoURL: string): String;
var
  ImgStream: TFileStream;
  FN: String;
begin
  FN := SlashDir(OwnDir)+Flickr.GetTimeStamp;
  ImgStream := TFileStream.Create(FN,fmCreate);
  HTTPclient.Get(PhotoURL,ImgStream);
  ImgStream.Free;
  Result := FN;
end;

procedure TFix.Execute;
var
  i: Integer;
begin
  for i := 0 to URLS.Count-1 do
    FixEvent(URLS[i],Domains);
end;

function TFix.MakeCopy: TFix;
begin
  Result := TFix.Create(Flickr,LiveJournal,HTTPclient);
  FreeWithoutAPI;
end;

end.

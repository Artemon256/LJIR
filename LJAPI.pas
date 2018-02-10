unit LJAPI;

interface

uses
  System.Net.HTTPclientComponent, System.Classes, System.SysUtils, IdHashMessageDigest,
  idURI;

type
  TLJPost = record
    Header,Text,Year,Mon,Day,Hour,Minute,ItemID: String;
  end;
  TLiveJournal = class
    private
      HTTPclient: TNetHTTPClient;
      Username, Password: String;
      function GetChallenge: String;
      function TryLogIn: Boolean;
      const
        LJURL='http://www.livejournal.com/interface/flat';
    public
      function GetEvent(URL: String): TLJPost;
      procedure EditEvent(Post: TLJPost);
      constructor Create(Login,Pass: String);
      destructor Free;
  end;

implementation

function MD5(s: String): String;
begin
  Result := '';
  with TIdHashMessageDigest5.Create do
  try
    Result := AnsiLowerCase(HashStringAsHex(s));
  finally
    Free;
  end;
end;

function EncodeURL(s: string): String;
var
    I : Integer;
    Ch : Char;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        Ch := S[I];
        if ((Ch >= '0') and (Ch <= '9')) or
           ((Ch >= 'a') and (Ch <= 'z')) or
           ((Ch >= 'A') and (Ch <= 'Z')) or
           (Ch = '.') or (Ch = '-') or (Ch = '_') or (Ch = '~')then
            Result := Result + Ch
        else
            Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
end;


procedure TLiveJournal.EditEvent(Post: TLJPost);
var
  i: Integer;
  Request, Response: TStringList;
  Challenge: String;
  OK: Boolean;
begin
  Request := TStringList.Create;
  Response := TStringList.Create;
  Request.Add('mode=editevent');
  Request.Add('user='+Username);
  Request.Add('auth_method=challenge');
  Challenge := GetChallenge;
  Request.Add('auth_challenge='+Challenge);
  Request.Add('auth_response='+MD5(Challenge+MD5(Password)));
  Request.Add('ver=1');
  Request.Add('selecttype=one');
  Request.Add('itemid='+Post.ItemID);
  Request.Add('event='+Post.Text);
  Request.Add('subject='+Post.Header);
  Request.Add('year='+Post.Year);
  Request.Add('mon='+Post.Mon);
  Request.Add('day='+Post.Day);
  Request.Add('hour='+Post.Hour);
  Request.Add('min='+Post.Minute);
  try
    Response.LoadFromStream(HTTPclient.Post(LJURL,Request).ContentStream);
  except
    Raise Exception.Create('Ошибка при редактировании поста (LiveJournal)');
  end;
  OK := False;
  for i := 0 to Response.Count-1 do
    if (Response[i]='success') and (Response[i+1]='OK') then
    begin
      OK := True;
      Break;
    end;
  if not OK then
    Raise Exception.Create('Ошибка при редактировании поста (LiveJournal)');
  //Request.SaveToFile('request.txt');
  //Response.SaveToFile('response.txt');
end;

function TLiveJournal.GetEvent(URL: string): TLJPost;
var
  i, ItemID, state: Integer;
  Request, Response: TStringList;
  Challenge, DItemID, PostTime, Debug: String;
  OK: Boolean;
begin
  URL:=ExtractFileName(StringReplace(URL,'/','\',[rfReplaceAll]));
  for i := 1 to Length(URL) do
    if URL[i]='.' then
      Break
    else
      DItemID := DItemID + URL[i];
  ItemID := StrToInt(DItemID);
  ItemID := (ItemID - (ItemID - (ItemID div 256) * 256)) div 256;
  Result.ItemID:=IntToStr(ItemID);
  Request := TStringList.Create;
  Response := TStringList.Create;
  Request.Add('mode=getevents');
  Request.Add('user='+Username);
  Request.Add('auth_method=challenge');
  Challenge := GetChallenge;
  Request.Add('auth_challenge='+Challenge);
  Request.Add('auth_response='+MD5(Challenge+MD5(Password)));
  Request.Add('ver=1');
  Request.Add('selecttype=one');
  Request.Add('itemid='+IntToStr(ItemID));
  try
    Debug := AnsiToUtf8(HTTPclient.Post(LJURL,Request).ContentAsString);
    Response.Text := Debug;
  except
    Raise Exception.Create('Ошибка при загрузке поста (LiveJournal)');
  end;

  OK := False;
  for i := 0 to Response.Count-1 do
    if (Response[i]='success') and (Response[i+1]='OK') then
    begin
      OK := True;
      Break;
    end;
  if not OK then
    Raise Exception.Create('Ошибка при загрузке поста (LiveJournal)');

  for i := 0 to Response.Count-1 do
  begin
    if Response[i]='events_1_subject' then
      Result.Header := Response[i+1];

    if Response[i]='events_1_event' then
      Result.Text := TIdURI.URLDecode(Response[i+1]);

    if Response[i]='events_1_eventtime' then
      PostTime := TIdURI.URLDecode(Response[i+1]);
  end;

  state:=0;
  for i := 1 to Length(PostTime) do
  begin
    if (PostTime[i] = '-') or (PostTime[i] = ':') or (PostTime[i] = ' ') then
    begin
      inc(state);
      Continue;
    end;

    case state of
      0:
        Result.Year := Result.Year + PostTime[i];
      1:
        Result.Mon := Result.Mon + PostTime[i];
      2:
        Result.Day := Result.Day + PostTime[i];
      3:
        Result.Hour := Result.Hour + PostTime[i];
      4:
        Result.Minute := Result.Minute + PostTime[i];
      5:
        Break;
    end;
  end;
end;

function TLiveJournal.TryLogIn: Boolean;
var
  Request, Response: TStringList;
  Challenge: String;
  i: Integer;
begin
  Request := TStringList.Create;
  Response := TStringList.Create;
  Challenge := GetChallenge;
  Request.Add('mode=login');
  Request.Add('user='+Username);
  Request.Add('auth_method=challenge');
  Request.Add('auth_challenge='+Challenge);
  Request.Add('auth_response='+MD5(challenge+MD5(Password)));
  Request.Add('ver=1');
  try
    try
      Response.LoadFromStream(HTTPclient.Post(LJURL,Request).ContentStream);
    except
      Result := False;
      Exit;
    end;
    Result := False;
    for i := 0 to Response.Count-1 do
      if (Response[i]='success') and (Response[i+1]='OK') then
      begin
        Result := True;
        Break;
      end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

constructor TLiveJournal.Create(Login: string; Pass: string);
begin
  inherited Create;
  Username := Login;
  Password := Pass;
  HTTPclient := TNetHTTPClient.Create(nil);
  HTTPclient.ResponseTimeout := 10000;
  if not TryLogIn then
    Raise Exception.Create('Ошибка входа (LiveJournal)');
end;

destructor TLiveJournal.Free;
begin
  HTTPclient.Free;
  inherited Destroy;
end;

function TLiveJournal.GetChallenge: String;
var
  Request, Response: TStringList;
  i: Integer;
  OK: Boolean;
begin
  Request := TStringList.Create;
  Response := TStringList.Create;
  Request.Add('mode=getchallenge');
  try
    try
      Response.LoadFromStream(HTTPclient.Post(LJURL,Request).ContentStream);
    except
      Raise Exception.Create('Ошибка получения челленджа (LiveJournal)');
    end;
    OK := False;
    for i := 0 to Response.Count-1 do
      if (Response[i]='success') and (Response[i+1]='OK') then
      begin
        OK := True;
        Break;
      end;
    if not OK then
      Raise Exception.Create('Ошибка получения челленджа (LiveJournal)');

    for i := 0 to Response.Count do
      if Response[i]='challenge' then
      begin
        Result := Response[i+1];
        Break;
      end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

end.

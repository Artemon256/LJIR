unit FlickrAPI;

interface

uses
  System.Net.HTTPClientComponent, System.Net.URLClient, System.SysUtils,
  System.Classes, Math, DateUtils, idHMACSHA1, IdGlobal, XMLObject, System.NetEncoding,
  idCoderMIME, System.Generics.Collections, System.Net.Mime;

type
  TFLickr = class
    private
      Consumer, Secret, Token, TokenSecret: String;
      HTTPclient: TNetHTTPClient;
      procedure GetRequestToken;
      function GetNonce: String;
      function GetURLByID(id: string): String;
    public
      VerifyURL: String;
      function GetTimestamp: String;
      constructor Create(ApiKey, SecretKey: String);
      destructor Free;
      procedure Auth(Verifier: String);
      function UploadPhoto(Filename: String; Attempt: Integer=0): String;
  end;

implementation

function Base64Encode(const Input: TIdBytes): string;
begin
  Result := TIdEncoderMIME.EncodeBytes(Input);
end;

function ParseResp(Resp: String): TDictionary<String,String>;
var
  i: Integer;
  Key,Value: String;
  IsKey: Boolean;
begin
  Result := TDictionary<String,String>.Create;
  IsKey := True;
  Key := ''; Value := '';
  for i := 1 to Length(Resp) do
  begin
    if IsKey then
      if Resp[i] <> '=' then
        Key := Key + Resp[i]
      else
      begin
        IsKey := not IsKey;
        Continue;
      end
    else
      if Resp[i] <> '&' then
        Value := Value + Resp[i]
      else
      begin
        Result.Add(Key,Value);
        Key := ''; Value := '';
        IsKey := not IsKey;
        Continue;
      end
  end;
  Result.Add(Key,Value);
end;

function EncryptHMACSha1(Input, AKey: string): TIdBytes;
begin
  with TIdHMACSHA1.Create do
  try
    Key := ToBytes(AKey);
    Result := HashValue(ToBytes(Input));
  finally
    Free;
  end;
end;

constructor TFlickr.Create(ApiKey: string; SecretKey: string);
begin
  inherited Create;
  Consumer := ApiKey;
  Secret := SecretKey;
  HTTPclient := TNetHTTPClient.Create(nil);
  GetRequestToken;
end;

destructor TFlickr.Free;
begin
  HTTPclient.Free;
  inherited Destroy;
end;

procedure TFlickr.Auth(Verifier: string);
const
  EXCHANGE_URL = 'https://www.flickr.com/services/oauth/access_token';
var
  BaseString, Response, Request: String;
  ParsedResp: TDictionary<string,string>;
begin
  try
    Request:='oauth_callback=oob'+'&oauth_consumer_key='+Consumer+'&oauth_nonce='
      +GetNonce+'&oauth_signature_method=HMAC-SHA1'+'&oauth_timestamp='+GetTimestamp
      +'&oauth_token='+Token+'&oauth_verifier='+Verifier;
    BaseString:='GET&'+TNetEncoding.URL.Encode(EXCHANGE_URL)+'&'+TNetEncoding.URL.Encode(Request);
    Request := EXCHANGE_URL+'?'+Request+'&oauth_signature='+TNetEncoding.URL.Encode(Base64Encode(EncryptHMACSHA1(BaseString,Secret+'&'+TokenSecret)));
    try
      Response := HTTPclient.Get(Request).ContentAsString;
      ParsedResp := ParseResp(Response);
      Token := ParsedResp.Items['oauth_token'];
      TokenSecret := ParsedResp.Items['oauth_token_secret'];
    except
      raise Exception.Create('Ошибка при получении ключа запроса (Flickr/HTTP)');
    end;
  finally
    if Assigned(ParsedResp) then
      ParsedResp.Free;
  end;
end;

function TFlickr.GetNonce: String;
var
  i: Byte;
begin
  for i := 0 to 8 do
    Result := IntToStr(RandomRange(0,9));
end;

procedure TFlickr.GetRequestToken;
const
  REQUEST_URL = 'https://www.flickr.com/services/oauth/request_token';
  AUTH_URL = 'https://www.flickr.com/services/oauth/authorize?perms=write&oauth_token=';
var
  BaseString, Response, Request: String;
  ParsedResp: TDictionary<string,string>;
begin
  try
    Request:='oauth_callback=oob'+'&oauth_consumer_key='+Consumer+'&oauth_nonce='+GetNonce
      +'&oauth_signature_method=HMAC-SHA1'+'&oauth_timestamp='+GetTimestamp;
    BaseString:='GET&'+TNetEncoding.URL.Encode(REQUEST_URL)+'&'+TNetEncoding.URL.Encode(Request);
    Request := REQUEST_URL+'?'+Request+'&oauth_signature='+TNetEncoding.URL.Encode(Base64Encode(EncryptHMACSHA1(BaseString,Secret+'&')));
    try
      Response := HTTPclient.Get(Request).ContentAsString;
      ParsedResp := ParseResp(Response);
      Token := ParsedResp.Items['oauth_token'];
      TokenSecret := ParsedResp.Items['oauth_token_secret'];
    except
      raise Exception.Create('Ошибка при получении ключа запроса (Flickr/HTTP)');
    end;
    VerifyURL := AUTH_URL+Token;
  finally
    if Assigned(ParsedResp) then
      ParsedResp.Free;
  end;
end;

function TFlickr.GetTimestamp: String;
begin
  Result := IntToStr(DateTimeToUnix(Now));
end;

function TFlickr.GetURLByID(id: string): String;
const
  FLICKR_URL='https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&oauth_consumer_key=%s&photo_id=%s';
  STATIC_URL='https://farm%s.staticflickr.com/%s/%s_%s_o.%s';
var
  XML: TXMLObject;
  URL, Farm, Server, PhotoSecret, Ext: String;
  Debug: TStringStream;
begin
  XML := TXMLObject.Create;
  URL := Format(FLICKR_URL,[Consumer,id]);
  Debug := TStringStream.Create;
  try
    Debug.LoadFromStream(HTTPclient.Get(URL).ContentStream);
    Debug.SaveToFile('debug.txt');
    XML.LoadFromStream(HTTPclient.Get(URL).ContentStream);
    Farm:=XML.NodeByPath('rsp/photo').AttributeByName('farm').AsString;
    Server:=XML.NodeByPath('rsp/photo').AttributeByName('server').AsString;
    PhotoSecret:=XML.NodeByPath('rsp/photo').AttributeByName('originalsecret').AsString;
    Ext:=XML.NodeByPath('rsp/photo').AttributeByName('originalformat').AsString;
  except
    XML.Free;
    Debug.Free;
    Raise Exception.Create('Ошибка получения URL фотографии (Flickr)');
  end;

  Result := Format(STATIC_URL,[Farm,Server,id,PhotoSecret,Ext]);

  Debug.Free;
  XML.Free;
end;

function TFlickr.UploadPhoto(Filename: string; Attempt: Integer=0): String;
const
  FLICKR_UPLOAD_URL = 'https://up.flickr.com/services/upload/';
var
  Request: TMultipartFormData;
  BaseString, Nonce, Timestamp, PhotoID, XS: String;
  XML: TXMLObject;
//  DebugStream: TStringStream;
  P, i: Integer;
begin
  XML := TXMLObject.Create;
  Request := TMultipartFormData.Create;
//  DebugStream := TStringStream.Create;
  Result := '';
  try
    Nonce:=GetNonce;
    Timestamp:=GetTimeStamp;

    Request.AddField('oauth_consumer_key',Consumer);
    Request.AddField('oauth_nonce',Nonce);
    Request.AddField('oauth_signature_method','HMAC-SHA1');
    Request.AddField('oauth_timestamp',Timestamp);
    Request.AddField('oauth_token',Token);

    BaseString := 'POST&' + TNetEncoding.URL.Encode(FLICKR_UPLOAD_URL) + '&' +
      TNetEncoding.URL.Encode('oauth_consumer_key='+Consumer+'&oauth_nonce='+Nonce+'&oauth_signature_method=HMAC-SHA1'
      +'&oauth_timestamp='+Timestamp+'&oauth_token='+Token);
    BaseString := (Base64Encode(EncryptHMACSha1(BaseString, Secret+'&'+TokenSecret)));

    Request.AddField('oauth_signature',BaseString);
    Request.AddFile('photo',Filename);
//    DebugStream.LoadFromStream(Request.Stream);
//    DebugStream.SaveToFile('request');
    try
      PhotoID:='';
      XS := HTTPclient.Post(FLICKR_UPLOAD_URL,Request).ContentAsString;
      P := Pos('<photoid>',XS);
      P := P+Length('<photoid>');
      for i := P to Length(XS) do
        if XS[i]='<' then Break
        else PhotoID := PhotoID + XS[i];
    except
      if Attempt<3 then
      begin
        Sleep(1000);
        UploadPhoto(Filename, Attempt+1);
      end
      else
        raise Exception.Create('Ошибка при загрузке фото (Flick/HTTP)');
    end;
    try
      if P=0 then
        raise Exception.Create('');
    except
      raise Exception.Create('Ошибка при загрузке фото (Flickr/Photo)');
    end;
    Result := GetURLByID(PhotoID);
  finally
//    DebugStream.Free;
    XML.Free;
    Request.Free;
  end;
end;

end.

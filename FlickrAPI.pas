unit FlickrAPI;

interface

uses
  System.Net.HTTPClientComponent, System.SysUtils, System.DateUtils, System.Math,
  System.Generics.Collections, System.NetEncoding, idGlobal, idCoderMIME,
  idHMACSHA1, System.Net.Mime, XML.XMLdoc, System.Classes, System.Variants, Forms;

type
  TFlickr = class
    private
      HTTPclient: TNetHTTPClient;
      Consumer, Secret, Token, TokenSecret: String;
      function GetNonce: String;
      function GetSignature(const s: String): String;
      function GetURLByID(const id: String): String;
    public
      VerifyURL: String;
      function GetTimeStamp: String;
      procedure Auth(const Verifier: string);
      procedure GetRequestToken;
      function UploadPhoto(const Filename: String): String;
      constructor Create(const ApiKey, SecretKey: String);
      destructor Free;
  end;

implementation

function TFlickr.GetSignature(const s: String): String;

  function Base64Encode(const Input: TIdBytes): string;
  begin
    Result := TIdEncoderMIME.EncodeBytes(Input);
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

begin
  Result := Base64Encode(EncryptHMACSha1(s,Secret+'&'+TokenSecret));
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

constructor TFlickr.Create(const ApiKey, SecretKey: String);
begin
  inherited Create;
  Consumer := ApiKey;
  Secret := SecretKey;
  Token := '';
  TokenSecret := '';
  HTTPclient := TNetHTTPClient.Create(nil);
  HTTPclient.ResponseTimeout := 5000;
end;

destructor TFlickr.Free;
begin
  HTTPclient.Free;
  inherited Destroy;
end;

function TFlickr.GetTimestamp: String;
begin
  Result := IntToStr(DateTimeToUnix(Now));
end;

function TFlickr.GetNonce: String;
var
  i: Byte;
begin
  for i := 0 to 8 do
    Result := IntToStr(RandomRange(0,9));
end;

procedure TFlickr.Auth(const Verifier: string);
const
  EXCHANGE_URL = 'https://www.flickr.com/services/oauth/access_token';
var
  BaseString, Response, Request: String;
  ParsedResp: TDictionary<string,string>;
begin
  try
    Request:='oauth_callback=oob'+'&oauth_consumer_key='+Consumer+'&oauth_nonce='+GetNonce
      +'&oauth_signature_method=HMAC-SHA1'+'&oauth_timestamp='+GetTimestamp
      +'&oauth_token='+Token+'&oauth_verifier='+Verifier;
    BaseString:='GET&'+TNetEncoding.URL.Encode(EXCHANGE_URL)+'&'+
      TNetEncoding.URL.Encode(Request);
    Request := EXCHANGE_URL+'?'+Request+'&oauth_signature='+
      TNetEncoding.URL.Encode(GetSignature(BaseString));
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
    BaseString:='GET&'+TNetEncoding.URL.Encode(REQUEST_URL)+'&'+
      TNetEncoding.URL.Encode(Request);
    Request := REQUEST_URL+'?'+Request+'&oauth_signature='+
      TNetEncoding.URL.Encode(GetSignature(BaseString));
    try
      Response := HTTPclient.Get(Request).ContentAsString;
      ParsedResp := ParseResp(Response);
      Token := ParsedResp.Items['oauth_token'];
      TokenSecret := ParsedResp.Items['oauth_token_secret'];
      VerifyURL := AUTH_URL+Token;
    except
      raise Exception.Create('Ошибка при получении ключа запроса (Flickr/HTTP)');
    end;
  finally
    if Assigned(ParsedResp) then
      ParsedResp.Free;
  end;
end;

function TFlickr.UploadPhoto(const Filename: string): String;
const
  FLICKR_UPLOAD_URL = 'https://up.flickr.com/services/upload/';
var
  Request: TMultipartFormData;
  Nonce, Timestamp, BaseString, ID: String;
  XML: TXMLDocument;
begin
  XML := TXMLDocument.Create(Application);
  Request := TMultipartFormData.Create;
  try
    Nonce := GetNonce;
    Timestamp := GetTimeStamp;

    BaseString := 'POST&' + TNetEncoding.URL.Encode(FLICKR_UPLOAD_URL) + '&' +
      TNetEncoding.URL.Encode('oauth_consumer_key='+Consumer+'&oauth_nonce='+Nonce+'&oauth_signature_method=HMAC-SHA1'
      +'&oauth_timestamp='+Timestamp+'&oauth_token='+Token);

    Request.AddField('oauth_consumer_key',Consumer);
    Request.AddField('oauth_nonce',Nonce);
    Request.AddField('oauth_signature_method','HMAC-SHA1');
    Request.AddField('oauth_timestamp',Timestamp);
    Request.AddField('oauth_token',Token);
    Request.AddField('oauth_signature',GetSignature(BaseString));
    Request.AddFile('photo',Filename);
    try
      XML.LoadFromStream(HTTPclient.Post(FLICKR_UPLOAD_URL,Request).ContentStream);
    except
      raise Exception.Create('Ошибка при загрузке фото (Flickr/HTTP)');
    end;
    try
      if XML.ChildNodes[1].ChildNodes[0].NodeName <> 'photoid' then
        raise Exception.Create('');
      ID := String(XML.ChildNodes[1].ChildNodes[0].NodeValue);
    except
      raise Exception.Create('Ошибка при загрузке фото (Flickr/Photo)');
    end;

    Result:=GetURLByID(ID);
  finally
    Request.Free;
    XML.Free;
  end;
end;

function TFlickr.GetURLByID(const id: string): String;
const
  FLICKR_URL='https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&oauth_consumer_key=%s&photo_id=%s';
  STATIC_URL='https://farm%s.staticflickr.com/%s/%s_%s_o.%s';
var
  XML: TXMLDocument;
  Farm, Server, PhotoSecret, Ext: String;
begin
  XML := TXMLDocument.Create(Application);
  try
    try
      XML.LoadFromStream(HTTPclient.Get(Format(FLICKR_URL,[Consumer,id])).ContentStream);
    except
      raise Exception.Create('Ошибка при получении URL фото (Flickr/HTTP)');
    end;
    try
      if XML.ChildNodes[1].ChildNodes[0].NodeName <> 'photo' then
        raise Exception.Create('');
      Farm := String(XML.ChildNodes[1].ChildNodes[0].Attributes['farm']);
      Server := String(XML.ChildNodes[1].ChildNodes[0].Attributes['server']);
      PhotoSecret := String(XML.ChildNodes[1].ChildNodes[0].Attributes['originalsecret']);
      Ext := String(XML.ChildNodes[1].ChildNodes[0].Attributes['originalformat']);
    except
      raise Exception.Create('Ошибка при получении URL фото (Flickr/Photo)');
    end;
    Result := Format(STATIC_URL,[Farm,Server,id,PhotoSecret,Ext]);
  finally
    XML.Free;
  end;
end;

end.

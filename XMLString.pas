unit XMLString;

interface

uses Math, SysUtils, Windows, Classes, Graphics;

type TTagKind = (tkOpen, tkClose, tkOpenClose, tkSystem);

type TTagParam = record
     Name : AnsiString;//String[100];
     Value : AnsiString;//String[100];
end;

type TTag = record
     Name : String;
     Pos : Integer;
     Params : array of TTagParam;
     Text : AnsiString;//String[255];
     Kind : TTagKind;
end;

type PTag = ^TTag;

function ReadTag(InStream : TStream; UpCase : Boolean = True) : TTag;
function ReadValue(InStream : TStream; TrimValue: Boolean = True) : String;

type TVarType = (vtString, vtInteger, vtFloat, vtDate);

type TDelimiter = set of #32..#255;
     CharSet = Set of Char;

const NumericSet = ['0'..'9'];
      HexadecimalSet = ['0'..'9', 'a'..'f', 'A'..'F', '#', '$'];
      AlphaSet = ['A'..'Z', 'a'..'z', '_'];
      PathSet = ['.'];
      ExpressionSet = ['+', '-', '*', '/', '=', '>', '<', ':', ',', '|'];
      BracketSet =  ['(', ')', '[', ']', '{', '}'];
(*
function Between (Field, vTop, vEnd : String; vType : TVarType) : String;
function Equal (Field, Value : String; vType : TVarType) : String;
function OrString (vTop, vOr, vEnd : Variant) : String;
function AndString (vTop, vAnd, vEnd : Variant) : String;
*)
function GetBetween (Line, sTop, sEnd : String) : String;
function SetBetween (Line : String; sTop, sEnd, Value : String) : String;
(*
{function GetBetweenStr(Sour : String; xTop, xEnd : String) : String;}
*)

function OrStr (const sTop, sOr, sEnd : String) : String;
function AndStr (const sTop, sAnd, sEnd : String) : String;

function iff (ExprVal : Boolean; ThenVal, ElseVal : Variant) : Variant;
function ifp (ExprVal : Boolean; ThenVal, ElseVal : Pointer) : Pointer;
function ifs (ExprVal : Boolean; ThenVal, ElseVal : String) : String;

function AddRemark (Line, sTop, sEnd : String) : String;
function DelRemark (Line, sTop, sEnd : String) : String;
function SubString(Sour : String; xTop, xEnd : Array of String) : String;
function DelString(Sour : String; xPos, xCnt : Integer) : String;

function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
function ExtractDelimiter(N : Integer; const S : string; const WordDelims : TSysCharSet): string;

function Plural(Number: Integer; Words: Array of String; Join: Boolean = True) : String;
function UpperFirstChar (Arg : String) : String;
procedure DeleteChars(var Str : String; Ch : Char);
//function IntToName64 (Arg : LongInt) : String;
function PosFrom (Sub, S: String; i: Integer = 1): Integer;
function StrCount(Sub, S: String): Integer;
function IsNumber(s : String) : Boolean;
function IsBoolean(s : String) : Boolean;
function IsDate(s : String) : Boolean;
function IsValidIdent(const Ident: string): Boolean;
function ExtractNumber(s : String) : Extended;
function IsValidForSet(const Ident: string; const xSet : CharSet): Boolean;
function MutuallyInclude (const Str1, Str2 : String) : Boolean;
function PosStrInArray(const S : String; const A : Array of string) : Integer;
function PosObjInArray(const O : TObject; const A : Array of TObject) : Integer;
function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

function Quote(const xStr : String; Symbol: Char = '"'): String;
function Unquote(const xStr : String): String;
function MakeStr(aLength: Integer; aChar: Char = #32): String;

function ConvertTo_ML(const xStr : String) : String;
function ConvertFrom_ML(const xStr : String) : String;
function ConvertToXML(const xStr : String) : String;
function ConvertToHTML(const xStr : String; aConvertBBCodes: Boolean = False) : String;
function ConvertFromXML(const xStr : String) : String;
function ConvertFromHTML(const xStr : String) : String;

function ConvertBBCodesToHTML(const xStr : String) : String;
function DropBBCodes(const xStr : String) : String;

type THexColor = String[13];
function IntToHTMLColor(aColor: TColor): THexColor;

function  EncodeBase64(const S, Alphabet: String; const Pad: Boolean = False;
          const PadMultiple: Integer = 4; const PadChar: Char = '='): String;
function  DecodeBase64(const S, Alphabet: String; const PadSet: CharSet = []): String;

function LoadStringFromResource(ResName: String): String;
function Hyphenation(DelimitedText: String; const Delimeters: TDelimiter; StrLen: Word; CRLF: String = #13#10; Indent: Word = 0): String;


const
  b64_MIMEBase64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64_UUEncode   = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  b64_XXEncode   = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

  set_Win1251  : Array [1..64] of String =
                 ('й','ц','у','к','е','н','г','ш','щ','з','х','ъ',
                  'ф','ы','в','а','п','р','о','л','д','ж','э','я',
                  'ч','с','м','и','т','ь','б','ю',
                  'Й','Ц','У','К','Е','Н','Г','Ш','Щ','З','Х','Ъ',
                  'Ф','Ы','В','А','П','Р','О','Л','Д','Ж','Э','Я',
                  'Ч','С','М','И','Т','Ь','Б','Ю');

  set_Translit : Array [1..64] of String =
                 ('j','ts','u','k','e','n','g','sh','sch','z','h',#39,
                  'f','y','v','a','p','r','o','l','d','zh','e','ya',
                  'ch','s','m','i','t',#39,'b','u',
                  'J','TS','U','K','E','N','G','SH','SCH','Z','H',#39,
                  'F','Y','V','A','P','R','O','L','D','ZH','E','YA',
                  'CH','S','M','I','T',#39,'B','U');

//  set_Koi8     = 'КГХЛЕОЗЫЭЪИЯЖЩЧБРТПМДЦЬСЮУНЙФШВАкгхлеозыэъияжщчбртпмдцьсюунйфшва';
//  set_Dos866   = '';

function  MIMEBase64Decode(const S: String): String;
function  MIMEBase64Encode(const S: String): String;
function  UUDecode(const S: String): String;
function  XXEncode(const S: String): String;
function  XXDecode(const S: String): String;

function Win1251toTranslit(const S: String) : String;

implementation

uses {JclStrings,} Variants;
//, Other, DateMatic, Clipbrd

{----------------------------------------------------------------}
(*
{ Создает выражение фильтра для записей у которых Field находится
 между vTop и vEnd. vEnd не обязательно. }

function Between (Field, vTop, vEnd : String; vType : TVarType) : String;
var App : String;
begin
     Result:=''; Field := Trim (Field); App:='';
     vTop:=Trim(vTop); vEnd:=Trim(vEnd);

     if vEnd = '' then begin
                       if vTop = '' then Exit;
                       vEnd := Copy (vTop, 1, Length(vTop)-1) +
                               Chr (Ord (vTop [Length(vTop)]) + 1);
                       end;

     if vType = vtString then App:='''';

     Result := '(' + Field + ' >= ' + App + vTop + App +
               ') and (' + Field + ' < ' + App + vEnd + App + ')';
end;
*)
{ Создает фильтр, для Field равного строчному Value }

{ String concatenation }

function OrStr (const sTop, sOr, sEnd : String) : String;
begin
  if (sTop<>'') and (sOr<>'') and (sEnd<>'') then Result:=sTop+sOr+sEnd
    else if sTop<>'' then Result:=sTop else if sEnd<>''
      then Result:=sEnd else Result:='';
end;

function AndStr (const sTop, sAnd, sEnd : String) : String;
begin
  if (sTop<>'') and (sAnd<>'') and (sEnd<>'')
    then Result:=sTop+sAnd+sEnd
    else Result:='';
end;

function Equal (Field, Value : String; vType : TVarType) : String;
var App : String;
begin
     Result:=''; App:='';
     Field := Trim (Field); Value := Trim (Value);
     if Value = '' then Exit;

     if vType = vtString then App:='''';

     Result := '(' + Field + ' = ' + App + Value + App + ')';
end;

function iff (ExprVal : Boolean; ThenVal, ElseVal : Variant) : Variant;
begin if ExprVal then Result:=ThenVal else Result:=ElseVal end;

function ifp (ExprVal : Boolean; ThenVal, ElseVal : Pointer) : Pointer;
begin if ExprVal then Result:=ThenVal else Result:=ElseVal end;

function ifs (ExprVal : Boolean; ThenVal, ElseVal : String) : String;
begin if ExprVal then Result:=ThenVal else Result:=ElseVal end;



function ToString (Arg : Variant) : String;
begin
     case VarType(Arg) of
      varString  : Result:=Arg;
     $0002,$0003 : if Arg=0 then Result:='' else Result:=Trim(IntToStr(Arg));
    $0004..$0006 : if Arg=0 then Result:='' else Result:=Trim(FloatToStr(Arg));
     end;
end;
(*
function OrString (vTop, vOr, vEnd : Variant) : String;
var sTop, sOr, sEnd : String;
begin
     sTop:=ToString(vTop); sOr:=ToString(vOr); sEnd:=ToString(vEnd);
     if (sTop<>'') and (sOr<>'') and (sEnd<>'') then Result:=sTop+sOr+sEnd
        else if sTop<>'' then Result:=sTop else if sEnd<>''
             then Result:=sEnd else Result:='';
end;

function AndString (vTop, vAnd, vEnd : Variant) : String;
var sTop, sAnd, sEnd : String;
begin
     sTop:=ToString(vTop); sAnd:=ToString(vAnd); sEnd:=ToString(vEnd);
     if (sTop<>'') and (sAnd<>'') and (sEnd<>'')
        then Result:=sTop+sAnd+sEnd
        else Result:='';
end;
*)
{----------------------------------------------------------------}

procedure TopAndEnd (Line, sTop, sEnd : String; var pTop, pEnd : Word);
begin
     if (sTop='') or (Pos(sTop,Line)=0)
        then pTop:=1 else pTop:=Pos(sTop,Line);
     if (sEnd='') or (Pos(sEnd,Line)=0)
        then pEnd:=Length(Line) else pEnd:=Pos(sEnd,Line)+Length(sEnd);
end;

procedure TopAndLen (Line, sTop, sEnd : String; var pTop, pLen : Word);
var pEnd : Word;
begin
     if (sTop='') or (Pos(sTop,Line)=0)
        then pTop:=1 else pTop:=Pos(sTop,Line)+Length(sTop);
     if (sEnd='') or (Pos(sEnd,Line)=0)
        then pEnd:=Length(Line)+1 else pEnd:=Pos(sEnd,Line);
     pLen:=pEnd-pTop;
end;

function GetBetween (Line, sTop, sEnd : String) : String;
var Top, Len : Word;
begin
     Result:=''; if Line='' then Exit;
     TopAndLen(Line, sTop, sEnd, Top, Len);
     Result:=Copy(Line,Top,Len);
end;

function SetBetween (Line : String; sTop, sEnd, Value : String) : String;
var Top, Len : Word;
begin
     TopAndLen(Line, sTop, sEnd, Top, Len);
     Delete(Line,Top,Len);
     Insert(Value,Line,Top);
     Result:=Line;
end;

{function GetBetweenStr(Sour : String; xTop, xEnd : String) : String;
var inx, iny : Word;
    Med : String;
begin
     Result:=Sour;
     for inx:=1 to WordCount(xTop,[',']) do
         for iny:=1 to WordCount(xEnd,[',']) do
             begin
             Med:=GetBetween(Sour, #32 + ExtractWord(inx,xTop,[',']) + #32,
                                   #32 + ExtractWord(iny,xEnd,[',']) + #32);
             if Result='' then Result:=Med;
             if (Med<>'') and (Length(Med)<Length(Result)) then Result:=Med;
             end;
end;}

function AddRemark (Line, sTop, sEnd : String) : String;
var xTop, xEnd : Word;
begin
     TopAndEnd(Line, sTop, sEnd, xTop, xEnd);
     if xEnd=Length(Line) then xEnd:=xEnd+1;
     Insert('*/',Line,xEnd);
     Insert('/*',Line,xTop);
     Result:=Line;
end;

function DelRemark (Line, sTop, sEnd : String) : String;
var xTop, xEnd : Word;
begin
     TopAndEnd(Line, sTop, sEnd, xTop, xEnd);
     if xEnd=Length(Line) then xEnd:=xEnd-1;
     // else xEnd:=xEnd-2;
     Delete(Line,xEnd,2);
     Delete(Line,xTop-2,2);
     Result:=Line;
end;

function SubString(Sour : String; xTop, xEnd : Array of String) : String;
var inx, iny : Word;
    Med : String;
begin
     Result:=Sour;
     for inx:=0 to High(xTop) do
         for iny:=0 to High(xEnd) do
             begin
             Med:=GetBetween(Sour,#32+xTop[inx]+#32,#32+xEnd[iny]+#32);
             if Result='' then Result:=Med;
             if (Med<>'') and (Length(Med)<Length(Result)) then Result:=Med;
             end;
end;

function DelString(Sour : String; xPos, xCnt : Integer) : String;
begin
     Delete(Sour,xPos,xCnt);
     Result:=Sour;
end;

function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do begin
    while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    { skip over delimiters }
    while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not(S[I] in WordDelims) do begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

function ExtractDelimiter(N : Integer; const S : string; const WordDelims : TSysCharSet): string;
var inx, iny : Integer;
    Wrd : String;
begin
     iny:=WordPosition(N+1, S, WordDelims);
     if iny=0 then iny := Length(S)+1;
     inx:=WordPosition(N, S, WordDelims);
     Wrd:=ExtractWord(N, S, WordDelims);
     inx:=inx + Length(Wrd);
     Result:=Copy(S, inx, iny-inx);
end;

{----------------------------------------------------------------}

function Plural(Number: Integer; Words: Array of String; Join: Boolean = True) : String;
var
  LastNum: Integer;
begin
  LastNum := Trunc(Frac(Number/10)*10);
  case LastNum of
       1 : LastNum:=0;
    2..4 : LastNum:=1;
  5..9,0 : LastNum:=2;
  end;
  Result := Words[LastNum];
  if Join then Result := IntToStr(Number) + ' ' + Result;
end;

function UpperFirstChar (Arg : String) : String;
var Len : Integer;
begin
     Len:=Length(Arg); Result:=Arg;
     if Len=0 then Exit;
     if Len=1 then Result:=AnsiUpperCase(Arg);
     if Len>1 then Result:=AnsiUpperCase(Arg)[1] + Copy(Arg,2,Len-1);
end;

procedure DeleteChars(var Str : String; Ch : Char);
var i : Integer;
begin
     i:=1;
     while i<=Length(Str) do
           if Str[i]=Ch then Delete(Str,i,1) else inc(i);
end;
{
function IntToName64 (Arg : Integer) : String;
const ValidChars = 'abcdefghijklmnopqrstuvwxyz'+
                   'абвгдежзиклмнопрстуфхцчшщэюя'+
                   '0123456789';
var NoVal : String;
    Del : Boolean;
    inx : Byte;
begin
     NoVal:=Dec2Numb(Arg,8,64); Result:=''; Del:=True;
     for inx:=1 to Length(NoVal) do
         begin
         if (NoVal[inx]=#48) and Del then Continue;
         Result:=Result+ValidChars[Ord(NoVal[inx])-47];
         Del:=False;
         end;
end;
}
function PosFrom (Sub, S: String; i: Integer = 1): Integer;
var inx, Ln : Integer;
begin
     Ln:=Length(Sub); Result:=0;
     for inx:=i to Length(S) do
         if CompareText(Copy(S,inx,Ln), Sub) = 0 then
            begin Result:=inx; Break; end;
end;

function StrCount(Sub, S: String): Integer;
var inx, Ln : integer;
begin
  Ln:=Length(Sub); Result:=0;
  for inx:=1 to Length(S) do
    if CompareText(Copy(S, inx, Ln), Sub) = 0
      then inc(Result);
end;

function ExtractNumber(s : String) : Extended;
var inx : Integer;
    Res : String;
    NSet : TSysCharSet;
begin
     Res:=''; NSet:=NumericSet + [FormatSettings.DecimalSeparator];
     for inx:=1 to Length(s) do
         if s[inx] in NSet
            then Res:=Res + s[inx];
     Result:=StrToFloatDef(Res, 0);
end;

function IsNumber(s : String) : Boolean;
begin
     Result:=False;
     if Trim(s)='' then Exit;
     if s[1] in ['+','-'] then Delete(s, 1, 1);
     Result:=IsValidForSet(s, NumericSet);
end;

function IsBoolean(s : String) : Boolean;
begin
  Result:=False;
  if Trim(s)='' then Exit;
  try
    StrToBool(s);
    Result := True;
  except
    Result := False;
  end;
end;

function IsDate(s : String): Boolean;
begin
  Result:=False;
  if Trim(s)='' then Exit;
  Result := (StrToDateDef(s, 0) <> 0);
end;

function IsValidIdent(const Ident: string): Boolean;
var I: Integer;
begin
     Result := False;
     if (Length(Ident) = 0) then Exit;
     for I := 1 to Length(Ident) do
         if not (Ident[I] in AlphaSet + NumericSet) then Exit;
     Result := True;
end;


function IsValidForSet(const Ident: string; const xSet : CharSet): Boolean;
var inx : Integer;
begin
     Result:=False;
     if (Length(Ident) = 0) then Exit;
     for inx:=1 to Length(Ident) do
         if not (Ident[inx] in xSet) then Exit;
     Result:=True;
end;

function MutuallyInclude (const Str1, Str2 : String) : Boolean;
begin
     Result:=( Pos(Str1, Str2)>0 ) or ( Pos(Str2, Str1)>0 );
end;

function PosStrInArray(const S : String; const A : Array of string) : Integer;
var inx : Integer;
begin
  Result:=Low(A)-1;
  if S='' then Exit;
  for inx:=Low(A) to High(A) do
    if AnsiCompareText(A[inx],S)=0 then begin Result:=inx+1; Exit; end;
end;

function PosObjInArray(const O : TObject; const A : Array of TObject) : Integer;
var inx : Integer;
begin
     Result:=Low(A)-1;
     for inx:=Low(A) to High(A) do
         if A[inx]=O then begin Result:=inx+1; Exit; end;
end;

function FindPart(const HelpWilds, InputStr: string): Integer;
var
  I, J: Integer;
  Diff: Integer;
begin
  I := Pos('?', HelpWilds);
  if I = 0 then begin
    { if no '?' in HelpWilds }
    Result := Pos(HelpWilds, InputStr);
    Exit;
  end;
  { '?' in HelpWilds }
  Diff := Length(InputStr) - Length(HelpWilds);
  if Diff < 0 then begin
    Result := 0;
    Exit;
  end;
  { now move HelpWilds over InputStr }
  for I := 0 to Diff do begin
    for J := 1 to Length(HelpWilds) do begin
      if (InputStr[I + J] = HelpWilds[J]) or
        (HelpWilds[J] = '?') then
      begin
        if J = Length(HelpWilds) then begin
          Result := I + 1;
          Exit;
        end;
      end
      else Break;
    end;
  end;
  Result := 0;
end;

function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

 function SearchNext(var Wilds: string): Integer;
 { looking for next *, returns position and string until position }
 begin
   Result := Pos('*', Wilds);
   if Result > 0 then Wilds := Copy(Wilds, 1, Result - 1);
 end;

var
  CWild, CInputWord: Integer; { counter for positions }
  I, LenHelpWilds: Integer;
  MaxInputWord, MaxWilds: Integer; { Length of InputStr and Wilds }
  HelpWilds: string;
begin
  if Wilds = InputStr then begin
    Result := True;
    Exit;
  end;
  repeat { delete '**', because '**' = '*' }
    I := Pos('**', Wilds);
    if I > 0 then
      Wilds := Copy(Wilds, 1, I - 1) + '*' + Copy(Wilds, I + 2, MaxInt);
  until I = 0;
  if Wilds = '*' then begin { for fast end, if Wilds only '*' }
    Result := True;
    Exit;
  end;
  MaxInputWord := Length(InputStr);
  MaxWilds := Length(Wilds);
  if IgnoreCase then begin { upcase all letters }
    InputStr := AnsiUpperCase(InputStr);
    Wilds := AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxInputWord = 0) then begin
    Result := False;
    Exit;
  end;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  repeat
    if InputStr[CInputWord] = Wilds[CWild] then begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then begin { handling of '*' }
      HelpWilds := Copy(Wilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      if I = 0 then begin
        { no '*' in the rest, compare the ends }
        if HelpWilds = '' then Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        for I := 0 to LenHelpWilds - 1 do begin
          if (HelpWilds[LenHelpWilds - I] <> InputStr[MaxInputWord - I]) and
            (HelpWilds[LenHelpWilds - I]<> '?') then
          begin
            Result := False;
            Exit;
          end;
        end;
        Exit;
      end;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(InputStr, CInputWord, MaxInt));
      if I= 0 then begin
        Result := False;
        Exit;
      end;
      CInputWord := I + LenHelpWilds;
      Continue;
    end;
    Result := False;
    Exit;
  until (CInputWord > MaxInputWord) or (CWild > MaxWilds);
  { no completed evaluation }
  if CInputWord <= MaxInputWord then Result := False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then Result := False;
end;


const Quotes = ['''','"'];

function Quote(const xStr : String; Symbol: Char = '"') : String;
begin
  Result := xStr;
  if Result = '' then
  begin
    Result := Symbol + Symbol;
    Exit;
  end;
  if not (Result[1] in Quotes)
    then Result := Symbol + Result;
  if not (Result[Length(Result)] in Quotes)
    then Result := Result + Symbol;
end;

function Unquote(const xStr : String) : String;
begin
     Result:=xStr;
     if Result='' then Exit;
     if (Result[1] in Quotes) then Delete(Result,1,1);
     if (Result[Length(Result)] in Quotes) then Delete(Result,Length(Result),1);
end;

function MakeStr(aLength: Integer; aChar: Char = #32): String;
begin
  Result := '';
  while aLength > 0 do
  begin
    Result := Result + aChar;
    dec(aLength);
  end;
end;

function ConvertTo_ML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, '&' , '&amp;' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<' , '&lt;'  , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '>' , '&gt;'  , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '''', '&#39;' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '"' , '&quot;', [rfReplaceAll, rfIgnoreCase]);
end;

function ConvertFrom_ML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, '&amp;' , '&' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '&lt;'  , '<' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '&gt;'  , '>' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '&#39;' , '''', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '&quot;', '"' , [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '&nbsp;', ' ' , [rfReplaceAll, rfIgnoreCase]);
end;

function ConvertToXML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, #13#10, '<br/>', [rfReplaceAll, rfIgnoreCase]);
  Result:=ConvertTo_ML(Result);
end;

function ConvertFromXML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=ConvertFrom_ML(Result);
  Result:=StringReplace(Result, '<br/>',  #13#10, [rfReplaceAll, rfIgnoreCase]);
end;

function ConvertBBCodesToHTML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, '[b]',  '<b>',  [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/b]', '</b>', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[i]',  '<i>',  [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/i]', '</i>', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[u]',  '<u>',  [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/u]', '</u>', [rfReplaceAll, rfIgnoreCase]);
end;

function DropBBCodes(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, '[b]',  '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/b]', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[i]',  '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/i]', '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[u]',  '', [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '[/u]', '', [rfReplaceAll, rfIgnoreCase]);
end;

function ConvertToHTML(const xStr : String; aConvertBBCodes: Boolean = False) : String;
begin
  Result:=xStr;
  Result:=ConvertTo_ML(Result);
  Result:=StringReplace(Result, #13#10,  '<br>', [rfReplaceAll, rfIgnoreCase]);
  if aConvertBBCodes
    then Result:=ConvertBBCodesToHTML(Result);
end;

function ConvertFromHTML(const xStr : String) : String;
begin
  Result:=xStr;
  Result:=StringReplace(Result, '<br>',  #13#10, [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '<br/>',  #13#10, [rfReplaceAll, rfIgnoreCase]);
  Result:=ConvertFrom_ML(Result);
end;

function IntToHTMLColor(aColor: TColor): THexColor;
begin
  aColor := ColorToRGB(aColor);
  Result := '"' + IntToStr(GetRValue(aColor)) + ',' + IntToStr(GetGValue(aColor)) + ',' + IntToStr(GetBValue(aColor)) + '"';
end;

{----------------------------------------------------------------}


function EncodeBase64(const S, Alphabet: String; const Pad: Boolean; const PadMultiple: Integer; const PadChar: Char): String;
var R, C : Byte;
    F, L, M, N, U : Integer;
    P : PChar;
    T : Boolean;
begin
  Assert(Length(Alphabet) = 64, 'Alphabet must contain 64 characters.');
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := L mod 3;
  N := (L div 3) * 4 + M;
  if M > 0 then
    Inc(N);
  T := Pad and (PadMultiple > 1);
  if T then
    begin
      U := N mod PadMultiple;
      if U > 0 then
        begin
          U := PadMultiple - U;
          Inc(N, U);
        end;
    end else
    U := 0;
  SetLength(Result, N);
  P := Pointer(Result);
  R := 0;
  For F := 0 to L - 1 do
    begin
      C := Byte(S [F + 1]);
      Case F mod 3 of
        0 : begin
              P^ := Alphabet[C shr 2 + 1];
              Inc(P);
              R := (C and 3) shl 4;
            end;
        1 : begin
              P^ := Alphabet[C shr 4 + R + 1];
              Inc(P);
              R := (C and $0F) shl 2;
            end;
        2 : begin
              P^ := Alphabet[C shr 6 + R + 1];
              Inc(P);
              P^ := Alphabet[C and $3F + 1];
              Inc(P);
            end;
      end;
    end;
  if M > 0 then
    begin
      P^ := Alphabet[R + 1];
      Inc(P);
    end;
  For F := 1 to U do
    begin
      P^ := PadChar;
      Inc(P);
    end;
end;

function DecodeBase64(const S, Alphabet: String; const PadSet: CharSet): String;
var F, L, M, P : Integer;
    B, OutPos  : Byte;
    OutB       : Array[1..3] of Byte;
    Lookup     : Array[Char] of Byte;
    R          : PChar;
begin
  Assert(Length(Alphabet) = 64, 'Alphabet must contain 64 characters.');
  L := Length(S);
  P := 0;
  if PadSet <> [] then
    While (L - P > 0) and (S[L - P] in PadSet) do
      Inc(P);
  M := L - P;
  if M = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, (M * 3) div 4);
  FillChar(Lookup, Sizeof(Lookup), #0);
  For F := 0 to 63 do
    Lookup[Alphabet[F + 1]] := F;
  R := Pointer(Result);
  OutPos := 0;
  For F := 1 to L - P do
    begin
      B := Lookup[S[F]];
      Case OutPos of
          0 : OutB[1] := B shl 2;
          1 : begin
                OutB[1] := OutB[1] or (B shr 4);
                R^ := Char(OutB[1]);
                Inc(R);
                OutB[2] := (B shl 4) and $FF;
              end;
          2 : begin
                OutB[2] := OutB[2] or (B shr 2);
                R^ := Char(OutB[2]);
                Inc(R);
                OutB[3] := (B shl 6) and $FF;
              end;
          3 : begin
                OutB[3] := OutB[3] or B;
                R^ := Char(OutB[3]);
                Inc(R);
              end;
        end;
      OutPos := (OutPos + 1) mod 4;
    end;
  if (OutPos > 0) and (P = 0) then // incomplete encoding, add the partial byte if not 0
    if OutB[OutPos] <> 0 then
      Result := Result + Char(OutB[OutPos]);
end;

function MIMEBase64Encode(const S: String): String;
begin
  Result := EncodeBase64(S, b64_MIMEBase64, True, 4, '=');
end;

function UUDecode(const S: String): String;
begin
  // Line without size indicator (first byte = length + 32)
  Result := DecodeBase64(S, b64_UUEncode, ['`']);
end;

function MIMEBase64Decode(const S: String): String;
begin
  Result := DecodeBase64(S, b64_MIMEBase64, ['=']);
end;

function XXEncode(const S: String): String;
begin
  Result := EncodeBase64(S, b64_XXEncode);
end;

function XXDecode(const S: String): String;
begin
  Result := DecodeBase64(S, b64_XXEncode, []);
end;

function Win1251toTranslit(const S: String) : String;
var inx, ps : Integer;
begin
     Result:='';
     for inx:=1 to Length(S) do
         begin
         ps:=PosStrInArray(S[inx], set_Win1251);
         if ps>0 then Result:=Result + set_Translit[ps]
                 else Result:=Result + S[inx];
         end;
end;

{ ---------------------------- TagParser ---------------------------- }

type TParserState = (inTagName, inParamName, inParamValue, inStringValue, inComment);

function ReadTag(InStream : TStream; UpCase : Boolean = True) : TTag;
var Ch, OpenStr : Char;
    State, OldState : TParserState;
    Token : AnsiString;//String[100];
begin
     State:=inTagName; Token:=''; OpenStr:=#32;
     SetLength(Result.Params,0);
     Result.Name:=''; Result.Text:='';

     repeat
     if InStream.Read(Ch, 1)<>1 then Break;
     OldState:=State;

     // Обработка терминаторов, независимо от состояния парсера
     if Ch in [#13,#10] then Ch:=' ';
     if Ch <> '>' then Result.Text:=Result.Text+Ch;
     //if Ch = '<' then State:=inComment; // Это комментарий

     // Обработка терминаторов, в зависимости от состояния парсера
     case State of
        inTagName : if Ch in ['>',' '] then
                       begin
                       Result.Name:=Trim(Token);
                       if UpCase then Result.Name:=AnsiUpperCase(Result.Name);
                       case Result.Name[1] of
                               '/' : Result.Kind:=tkClose;
                          '?', '!' : Result.Kind:=tkSystem;
                          else if Result.Name[Length(Result.Name)]='/'
                                  then Result.Kind:=tkOpenClose
                                  else Result.Kind:=tkOpen;
                       end;
                       Result.Pos:=InStream.Position-Length(Token)-2;
                       if Ch=' ' then State:=inParamName;
                       end;
      inParamName : if Ch = '=' then
                       begin
                       SetLength(Result.Params, Length(Result.Params)+1);
                       Result.Params[Length(Result.Params)-1].Name:=Trim(Token);
                       if UpCase then Result.Params[Length(Result.Params)-1].Name:=AnsiUpperCase(Result.Params[Length(Result.Params)-1].Name);
                       State:=inParamValue;
                       end;
     inParamValue, inStringValue
                  : begin
                    if ((Ch='>') or ((Ch=' ') and (Trim(Token)>''))) and
                       (State=inParamValue) then
                       begin
                       if Copy(Trim(Token),Length(Trim(Token)),1)='/' then
                          begin
                          Result.Kind:=tkOpenClose;
                          Token := Copy(Trim(Token),1,Length(Trim(Token))-1);
                          end;
                       Result.Params[Length(Result.Params)-1].Value:=Trim(Token);
                       if Ch=' ' then State:=inParamName;
                       end;
                    if Ch in ['"',''''] then
                       begin
                       if State=inParamValue
                          then begin
                               State:=inStringValue;
                               OpenStr := Ch;
                               end
                          else if OpenStr = Ch
                                  then State:=inParamValue;
                       end;
                    end;
//        inComment : ;
     end;

     // Обработка терминатора "конец тега"
     if (Ch = '>') and (State<>inStringValue) then Break;

     // Наполнение токена (в зависимости от состояния парсера)
     if (OldState=State) or ( (OldState in [inParamValue, inStringValue])
        and (State in [inParamValue, inStringValue]) )
        then Token:=Token+Ch else Token:='';

     until False;
end;

function ReadValue(InStream : TStream; TrimValue: Boolean = True) : String;
var Ch : Char;
    Tag : TTag;
begin
     Result:='';
     repeat
     if InStream.Read(Ch, 1)<>1 then Break;
     if Ch='<'
        then begin
             Tag:=ReadTag(InStream);
             if Tag.Kind = tkOpenClose
                then Result:=Result + '<' + Tag.Text + '>'
                else Break;
             end
        else Result:=Result+Ch;
     until False;

     if TrimValue
       then Result:=Trim(Result);

     InStream.Seek(Tag.Pos, soFromBeginning);
end;

function LoadStringFromResource(ResName: String): String;
var
  ResHandle: Cardinal;
  ResSize: Cardinal;
  Stream: TStringStream;
  //tmpStr: String;
begin
  Result := '';
  if ResName = '' then Exit;
  ResHandle := FindResource(HInstance, PChar(AnsiUpperCase(ResName)), 'TEXT');
  if ResHandle = 0 then Exit;
  ResSize := SizeofResource(HInstance, ResHandle);
  ResHandle := LoadResource(HInstance, ResHandle);
  if ResHandle = 0 then Exit;

  Stream:=TStringStream.Create('');
  try
    Stream.Write(Pointer(ResHandle)^, ResSize);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function Hyphenation(DelimitedText: String; const Delimeters: TDelimiter; StrLen: Word; CRLF: String = #13#10; Indent: Word = 0): String;
var
  i, Prev, Cur: word;
  aIndent: String;
begin
  Prev := 1; Cur := 1; Result := '';
  DelimitedText := Trim(DelimitedText);

  if Length(DelimitedText) > StrLen-Indent
  then
    for i:=1 to Length(DelimitedText) do
    begin
      if (DelimitedText[i] = #10) then
      begin
        Result := Result + copy(DelimitedText, Prev, i-Prev+1);
        Prev := i+1;
      end;
      if (DelimitedText[i] in Delimeters) and (i-Prev>=StrLen-Indent) then
      begin
        Result := Result + copy(DelimitedText, Prev, i-Prev+1) + CRLF;
        Prev := i+1;
      end;
      Cur := i;
    end
  else
    Cur := Length(DelimitedText);

  if Cur-Prev >= 0
    then Result := Result + copy(DelimitedText, Prev, Cur-Prev+1);

  // отступ
  if Indent > 0 then
  begin
    aIndent := MakeStr(Indent, #32);
    Result := StringReplace(Result, CRLF, CRLF+aIndent, [rfReplaceAll, rfIgnoreCase]);
    Result := aIndent + Trim(Result);
  end;
end;

end.


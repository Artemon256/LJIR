unit XMLObject;

{$DEFINE XMLDB}

interface

uses Classes, SysUtils, XMLString
{$IFDEF XMLDB} , DB {$ENDIF} ;

const
  COMP_VERSION      = 0.12;

  XML_SPECIFICATION = '<?xml version="%s" encoding="%s"?>';

type

  TXMLNode = class;

  TXMLTokenKind = (xmlTagVersion, xmlTagOpen, xmlTagClose, xmlElement, xmlAttribute);

  TXMLEnumNodeEvent = procedure(Sender: TObject; ANode: TXMLNode) of object;
  TXMLAssignNodeEvent = procedure (Sour, Dest: TXMLNode);

  TXMLNodeAttributeList = class;

  // *** TXMLNodeElement ***

  TXMLNodeAttribute = class
  protected
    FParent : TXMLNodeAttributeList;
    FName: string;
    FValue: string;
    FIsModified: Boolean;
    FIsInherited: Boolean;
    FOldValue: string;
    FUserData: Integer;
    procedure SetValue(const AValue: string);
    procedure SetName(AValue: string);
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsDateTime(const Value: TDateTime);
  public
    constructor Create; virtual;
    property Name: string read FName write SetName;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property Parent : TXMLNodeAttributeList read FParent;
    property IsModified: Boolean read FIsModified write FIsModified;
    property IsInherited: Boolean read FIsInherited write FIsInherited;
    property OldValue: string read FOldValue write FOldValue;
    property UserData: Integer read FUserData write FUserData;
  end;

  // *** TXMLNodeAttributeList ***

  TXMLNodeAttributeList = class
  protected
    FParent : TXMLNode;
    FList: TList;
    function GetCount: integer;
    function GetAttribute(index: integer): TXMLNodeAttribute;
    procedure SetAttribute(index: integer; const Value: TXMLNodeAttribute);
  public
    constructor Create(AParent: TXMLNode);
    destructor Destroy; override;
    function AddAttribute(Name, Value : String) : TXMLNodeAttribute; overload;
    procedure AddAttribute(AAttr: TXMLNodeAttribute); overload;
    procedure DelAttribute(AAttr: TXMLNodeAttribute);
    procedure Clear;
    function AttributeByName(AName: string): TXMLNodeAttribute; overload;
    function AttributeByName(AName: string; Default : String): TXMLNodeAttribute; overload;
    property Count: integer read GetCount;
    property Attribute[index: integer]: TXMLNodeAttribute read GetAttribute write SetAttribute; default;
    property Parent : TXMLNode read FParent;
  end;

//------------------------------------------------------------------------------

  TXMLNodeList = class;
  TXMLObject = class;

  TAssignMode = (amAppend, amReplace, amNodeName, amKeyAttr, amAutoNodeAttr);
  TAssignModeSet = set of TAssignMode;

  // *** TXMLNode ***

  TXMLNode = class(TPersistent)
  private
    function GetPath: string;
  protected
    FNodes: TXMLNodeList;
    FAList: TXMLNodeAttributeList;
    FOwner: TXMLObject;
    FParent: TXMLNode;
    FName: string;
    FKind : TTagKind;
    FValue: string;
    FIsModified: Boolean;
    FIsInherited: Boolean;
    procedure SetIsInherited(const Value: Boolean);
    procedure SetIsModified(const Value: Boolean);
    procedure SetValue(const AValue: string);
    function GetIsLeafNode: Boolean;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInteger: integer;
    function GetAsString: string;
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInteger(const Value: integer);
    procedure SetAsString(const Value: string);
    procedure SetAsDateTime(const Value: TDateTime);
    function GetIndex: integer;
  public
    constructor Create(AParentNode: TXMLNode); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent; aCallback: TXMLAssignNodeEvent = nil); reintroduce;
    procedure AssignWithChild(Source: TXMLNode; ExcludeNodes: array of TXMLNode;
      aMode: TAssignModeSet = [amAppend, amReplace, amNodeName];
      aCallback: TXMLAssignNodeEvent = nil); virtual;
    procedure InheritFrom(Source: TXMLNode; ExcludeNodes: array of TXMLNode;
      aMode: TAssignModeSet = [amAppend, amReplace, amNodeName]); virtual;
    //procedure InheritTo(Dest: TXMLNode; ExcludeNodes: array of TXMLNode);
    function CloseTag: string;
    function Level: integer;
    function OpenTag: string;
    procedure EnumerateNodes(ACallback: TXMLEnumNodeEvent; WithChild: Boolean = TRUE);
    function NodeByName(AName: string): TXMLNode; overload;
    function NodeByName(AName: string; Default : String): TXMLNode; overload;
    function NodeByPath(xPath : String; aCreate : Boolean = False): TXMLNode;
    function AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
    function AttributeByName(AName: string): TXMLNodeAttribute; overload;
    function AttributeByName(AName: string; Default : String): TXMLNodeAttribute; overload;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property Attributes : TXMLNodeAttributeList read FAList;
    property Nodes : TXMLNodeList read FNodes;
    property IsLeafNode: Boolean read GetIsLeafNode;
    property Name: string read FName write FName;
    property Index: integer read GetIndex;
    property Parent: TXMLNode read FParent;
    property Owner: TXMLObject read FOwner;
    property Kind : TTagKind read FKind;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property IsInherited: Boolean read FIsInherited write SetIsInherited;
    property Path: string read GetPath;
  end;

//------------------------------------------------------------------------------

  // *** TXMLNodeList ***

  TXMLNodeList = class
  private
    FKeyAttribute: String;
    FIsNodesOwner: Boolean;
  protected
    FCurrentNode: TXMLNode;
    FList: TList;
    FParent: TXMLNode;
    function GetCount: integer;
    function GetNode(index: integer): TXMLNode;
    procedure SetNode(index: integer; const Value: TXMLNode);
    function GetRoot: TXMLNode;
    procedure AddNode(ANode: TXMLNode);
  public
    constructor Create(AParent: TXMLNode);
    destructor Destroy; override;
    function AddLeaf(AName: string): TXMLNode;
    function AddOpenTag(AName: string): TXMLNode;
    function NodeByName(AName: string): TXMLNode; overload;
    function NodeByName(AName: string; Default : String): TXMLNode; overload;
    function NodePos(aNode: TXMLNode): Integer;
    function NodeByPath (xPath : String; aCreate : Boolean = False) : TXMLNode;
    function AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
    function NodeByValue (NodeName, Mask : String;
                          IgnoreCase : Boolean = True;
                          MaxLevel : Integer = 65535;
                          ResultList : TXMLNodeList = nil) : TXMLNodeList;
    function NodeByAttrValue (NodeName, NodeMask: String;
                          AttrName, AttrMask: String;
                          IgnoreCase : Boolean = True;
                          MaxLevel : Integer = 65535;
                          ResultList : TXMLNodeList = nil) : TXMLNodeList;
    procedure AddCloseTag;
    procedure SwapNodes(Node1, Node2 : Integer);
    procedure MoveNode(aFromPos, aToPos: Integer);
    procedure NextNode;
    procedure Clear;
    procedure DeleteNode(aName: string); overload;
    procedure DeleteNode(aNumber: integer); overload;
    property Count: integer read GetCount;
    property Parent : TXMLNode read FParent;
    property CurrentNode: TXMLNode read FCurrentNode write FCurrentNode;
    property Node[index: integer]: TXMLNode read GetNode write SetNode; default;
    property Root: TXMLNode read GetRoot;
    property KeyAttribute: String read FKeyAttribute write FKeyAttribute;
    property IsNodesOwner: Boolean read FIsNodesOwner write FIsNodesOwner;
  end;

//------------------------------------------------------------------------------

  // *** TXMLObject ***

  TXMLObject = class
  private
    FSaveInherited: Boolean;
    FOnBeforeSave: TNotifyEvent;
    FOnAfterLoad: TNotifyEvent;
    procedure SetNodesNotInherited(Sender: TObject; ANode: TXMLNode);
    procedure SetNodesNotModified(Sender: TObject; ANode: TXMLNode);
  protected
    FVersion: string;
    FIsInherited: Boolean;
    FIsModified: Boolean;
    Stream : TStream;
    FNodes: TXMLNodeList;
    FEncoding: string;
    procedure SetIsInherited(const Value: Boolean);
    procedure SetIsModified(const Value: Boolean);
    function GetSpecification: string;
    function GetCR: string;
    function GetIndent(ALevel: integer): string;
    function ReadTag: TTag;
    function ReadValue: String;
    procedure NodeToString(ANode: TXMLNode);
    procedure XMLToObject;
    procedure ObjectToXML;
  public
    FileName: TFileName;
    AutoIndent: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    property Nodes: TXMLNodeList read FNodes;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property IsInherited: Boolean read FIsInherited write SetIsInherited;
    property SaveInherited: Boolean read FSaveInherited write FSaveInherited;
{$IFDEF XMLDB}
    procedure LoadFromBlob(Field: TBlobField);
{$ENDIF}
    procedure LoadFromFile(XMLFile: TFileName = '');
    procedure LoadFromStream(Stm: TStream; aSize: Integer = -1);
    procedure LoadFromString(var StrRef: String);
    procedure LoadFromResource(ResName: String);
{$IFDEF XMLDB}
    procedure SaveToBlob(Field: TField);
{$ENDIF}
    procedure SaveToFile(XMLFile: TFileName = '');
    procedure SaveToStream(Stm: TStream; var aSize: Integer); overload;
    procedure SaveToStream(Stm: TStream); overload;
    procedure SaveToString(var StrRef: String);
    procedure SaveToPChar(var StrRef: PAnsiChar);

    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeSave: TNotifyEvent read FOnBeforeSave write FOnBeforeSave;

    function AsString : AnsiString;
    procedure InitEmpty(RootName : String = 'XML');
    function Root : TXMLNode;
    property Specification: string read GetSpecification;
    property Encoding: string read FEncoding write FEncoding;
    property Version: string read FVersion write FVersion;
    function NodeByPath (xPath : String; aCreate : Boolean = False) : TXMLNode;
    function AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
    function NodeByValue (NodeName, Mask : String;
                          IgnoreCase : Boolean = True;
                          MaxLevel : Integer = 65535) : TXMLNodeList;
    function NodeByAttrValue (NodeName, NodeMask: String;
                          AttrName, AttrMask: String;
                          IgnoreCase : Boolean = True;
                          MaxLevel : Integer = 65535;
                          ResultList: TXMLNodeList = nil) : TXMLNodeList;
  end;

  // *** TXMLObjectLink ***

  TXMLObjectLink = class(TPersistent)
  private
    FSkipEmpty: Boolean;
    FResourceName: String;
    procedure SetAutoIndent(const Value: Boolean);
    procedure SetFileName(const Value: TFileName);
    function GetAutoIndent: Boolean;
    function GetFileName: TFileName;
  protected
  public
    XMLObject: TXMLObject;
    constructor Create; virtual;
  published
    property AutoIndent: Boolean read GetAutoIndent write SetAutoIndent default True;
    //property SkipEmptyNode: Boolean read FSkipEmpty write FSkipEmpty default True;
    property FileName: TFileName read GetFileName write SetFileName;
    property ResourceName: String read FResourceName write FResourceName;
  end;

const
  DefAssignModeSet: TAssignModeSet = [amAppend, amReplace, amNodeName];

var
  XMLFormatSettings: TFormatSettings;

implementation

uses Windows;
//{$IFDEF XMLDB} , DBTables {$ENDIF} ;

const
  Null_Date_Str = '01.01.1800';

// *** Unit Functions ***

function StringToFloat(aValue: String): Extended;
begin
  if Length(Trim(aValue)) = 0
    then Result := 0.0
    else Result := StrToFloat(aValue, XMLFormatSettings);
end;

function StringToDateTime(aValue: String): TDateTime;
begin
  Result := StrToDateTimeDef(aValue,
    StrToDate(Null_Date_Str, XMLFormatSettings), XMLFormatSettings);
end;

function DateTimeToString(aValue: TDateTime): String;
begin
  Result := FormatDateTime('', aValue, XMLFormatSettings);
end;

function GetNextToken(AXml: string; var CursorPos: integer): string;
begin
     Result := '';
     if AXml[CursorPos] = '<'
        then // token is an XML tag...
            repeat
              Result := Result + AXml[CursorPos];
              Inc(CursorPos);
            until AXml[CursorPos-1] = '>'
        else // token is an XML value
            repeat
              Result := Result + AXml[CursorPos];
              Inc(CursorPos);
            until AXml[CursorPos] = '<';
end;

function GetTokenKind(AToken: string): TXMLTokenKind;
begin
  Result := xmlElement;
  if Length(AToken) < 2 then Exit;
  if AToken[1] = '<' then
  begin
    if AToken[2] = '/' then
      Result := xmlTagClose
    else
    if AToken[2] = '?' then
      Result := xmlTagVersion
    else
      Result := xmlTagOpen;
  end;
end;

//------------------------------------------------------------------------------
var Buf : array[0..65535] of AnsiChar;

procedure WriteStrToStream(Stream : TStream; Txt: String);
begin
     StrPCopy(Buf, Txt);
     Stream.Write(Buf, Length(Txt));
end;

//------------------------------------------------------------------------------

// *** TXMLNodeAttribute ***

procedure TXMLNodeAttribute.SetName(AValue: string);
begin
  FName := Trim(AValue);
end;

procedure TXMLNodeAttribute.SetValue(const AValue: string);
begin
  if FValue = AValue then Exit;
  FOldValue := FValue;
  FValue := AValue;
  FIsModified := True;
  FIsInherited := False;
  if (Parent <> nil) and (Parent.Parent <> nil) then
  begin
    Parent.Parent.IsModified := True;
    Parent.Parent.IsInherited := False;
  end;
end;

constructor TXMLNodeAttribute.Create;
begin
  FIsModified := False;
  FIsInherited := False;
end;

function TXMLNodeAttribute.GetAsBoolean: Boolean;
begin
  if FValue = ''
    then Result := False
    else if IsNumber(FValue)
      then Result := Boolean(StrToInt(FValue))
      else Result := Boolean(StrToBool(FValue));
end;

function TXMLNodeAttribute.GetAsDateTime: TDateTime;
begin
  Result := StringToDateTime(FValue);
end;

function TXMLNodeAttribute.GetAsFloat: Extended;
begin
  Result := StringToFloat(FValue);
end;

function TXMLNodeAttribute.GetAsInteger: integer;
begin
     if Length(Trim(FValue)) = 0
        then Result := 0
        else Result := StrToInt(FValue);
end;

function TXMLNodeAttribute.GetAsString: string;
begin
     Result := FValue;
end;

function TXMLNodeAttribute.GetAsObject: TObject;
begin
     Result := TObject(GetAsInteger);
end;

procedure TXMLNodeAttribute.SetAsBoolean(const Value: Boolean);
begin
  SetValue(IntToStr(Ord(Value)));
end;

procedure TXMLNodeAttribute.SetAsDateTime(const Value: TDateTime);
begin
  SetValue(DateTimeToString(Value));
end;

procedure TXMLNodeAttribute.SetAsFloat(const Value: Extended);
begin
  SetValue(FloatToStr(Value, XMLFormatSettings));
end;

procedure TXMLNodeAttribute.SetAsInteger(const Value: integer);
begin
  SetValue(IntToStr(Value));
end;

procedure TXMLNodeAttribute.SetAsString(const Value: string);
begin
  SetValue(Value);
end;

procedure TXMLNodeAttribute.SetAsObject(const Value: TObject);
begin
  SetAsInteger(Integer(Value));
end;

//------------------------------------------------------------------------------

{ TXMLNodeAttributeList }

constructor TXMLNodeAttributeList.Create(AParent: TXMLNode);
begin
     FParent := AParent;
     FList:=TList.Create;
end;

destructor TXMLNodeAttributeList.Destroy;
begin
     Clear;
     FList.Free;
     inherited;
end;

procedure TXMLNodeAttributeList.Clear;
begin
     while Count > 0 do
           begin
           Attribute[0].Free;
           FList.Delete(0);
           end;
end;

function TXMLNodeAttributeList.GetCount: integer;
begin
     Result:=FList.Count;
end;

function TXMLNodeAttributeList.AddAttribute(Name, Value : String) : TXMLNodeAttribute;
begin
     Result:=TXMLNodeAttribute.Create;
     Result.Name:=Name;
     Result.AsString:=Value;
     AddAttribute(Result);
end;

procedure TXMLNodeAttributeList.AddAttribute(AAttr: TXMLNodeAttribute);
begin
     AAttr.FParent:=Self;
     FList.Add(AAttr);
end;

procedure TXMLNodeAttributeList.DelAttribute(AAttr: TXMLNodeAttribute);
var ICount : integer;
begin
     for ICount := 0 to Count-1 do
         if Attribute[ICount] = AAttr then
            begin
            FList.Delete(ICount);
            Exit;
            end;
end;

function TXMLNodeAttributeList.GetAttribute(index: integer): TXMLNodeAttribute;
begin
     Result:=TXMLNodeAttribute(Flist[index]);
end;

function TXMLNodeAttributeList.AttributeByName(AName: string): TXMLNodeAttribute;
var ICount : integer;
begin
     Result:=nil; AName:=AnsiUpperCase(AName);
     for ICount := 0 to Count-1 do
         if AnsiUpperCase(Attribute[ICount].Name) = AName then
            begin
            Result := Attribute[ICount];
            Exit;
            end;
end;

function TXMLNodeAttributeList.AttributeByName(AName: string; Default : String): TXMLNodeAttribute;
begin
     Result:=AttributeByName(AName);
     if Result=nil then Result:=AddAttribute(AName, Default);
end;

procedure TXMLNodeAttributeList.SetAttribute(index: integer;
  const Value: TXMLNodeAttribute);
begin
     Flist[index]:=Value;
end;

//------------------------------------------------------------------------------

// *** TXMLNode ***

constructor TXMLNode.Create(AParentNode: TXMLNode);
begin
  inherited Create;
  FNodes := TXMLNodeList.Create(Self);
  FAList := TXMLNodeAttributeList.Create(Self);
  FParent := AParentNode;
  FOwner := nil;
  if FParent <> nil
    then FOwner := FParent.Owner;
  FIsModified := False;
  FIsInherited := False;
end;

destructor TXMLNode.Destroy;
begin
  FAList.Free;
  FNodes.Free;
  inherited Destroy;
end;

procedure TXMLNode.Assign(Source: TPersistent; aCallback: TXMLAssignNodeEvent = nil);
var
  i: Integer;
  tmpSourNode: TXMLNode;
begin
  if Source.ClassName <> 'TXMLNode' //not (Source is TXMLNode)
    then inherited Assign(Source)
    else
      begin
        tmpSourNode := TXMLNode(Source);

        if Assigned(aCallback)
          then aCallback(tmpSourNode, Self)
          else
            begin
              Name := tmpSourNode.Name;
              AsString := tmpSourNode.AsString;
              // copy attributes
              for i:=0 to tmpSourNode.Attributes.Count-1 do
                AttributeByName(tmpSourNode.Attributes[i].Name, '').AsString := tmpSourNode.Attributes[i].AsString;
            end;
      end;
end;

procedure TXMLNode.AssignWithChild(Source: TXMLNode; ExcludeNodes: array of TXMLNode;
  aMode: TAssignModeSet = [amAppend, amReplace, amNodeName];
  aCallback: TXMLAssignNodeEvent = nil);
var
  i: Integer;
  tmpSourNode, tmpDestNode: TXMLNode;
  tmpNodeList: TXMLNodeList;

  function NodeInArray(Node: TXMLNode; Nodes: array of TXMLNode): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i:=Low(Nodes) to High(Nodes) do
      if Node = Nodes[i] then
      begin
        Result := True;
        Exit;
      end;
  end;

begin
  Assign(Source, aCallback);
  tmpSourNode := TXMLNode(Source);

  // copy child nodes
  for i:=0 to tmpSourNode.Nodes.Count-1 do
  begin
    if NodeInArray(tmpSourNode.Nodes[i], ExcludeNodes)
      then Continue;

    tmpDestNode := nil;
    if amReplace in aMode
      then
        if amNodeName in aMode
          then
            if amAppend in aMode
              then tmpDestNode := Nodes.NodeByName(tmpSourNode.Nodes[i].Name, '')
              else tmpDestNode := Nodes.NodeByName(tmpSourNode.Nodes[i].Name)
          else
            begin
              tmpNodeList := Nodes.NodeByAttrValue(tmpSourNode.Nodes[i].Name, '', Nodes.KeyAttribute,
                tmpSourNode.Nodes[i].AttributeByName(Nodes.KeyAttribute, '').AsString, True, 1);
              if tmpNodeList.Count > 0
                then tmpDestNode := tmpNodeList[0]
                else if amAppend in aMode
                  then tmpDestNode := Nodes.AddOpenTag(tmpSourNode.Nodes[i].Name);
            end
      else
        if amAppend in aMode
          then tmpDestNode := Nodes.AddOpenTag(tmpSourNode.Nodes[i].Name);
    if tmpDestNode = nil
      then Continue;
    tmpDestNode.AsString := tmpSourNode.Nodes[i].AsString;
    tmpDestNode.AssignWithChild(tmpSourNode.Nodes[i], ExcludeNodes, aMode, aCallback);
    if (amAppend in aMode) and (not (amReplace in aMode))
      then Nodes.AddCloseTag;
  end;
end;

procedure InheritFromCallback(Sour, Dest: TXMLNode);
var
  i: Integer;
  SourNA, DestNA: TXMLNodeAttribute;
begin
  Dest.Name := Sour.Name;
  Dest.AsString := Sour.AsString;
  // copy attributes
  for i:=0 to Sour.Attributes.Count-1 do
    begin
      SourNA := Sour.Attributes[i];
      DestNA := Dest.AttributeByName(SourNA.Name);
      if DestNA = nil then
        begin
          DestNA := Dest.Attributes.AddAttribute(SourNA.Name, '');
          DestNA.AsString := SourNA.AsString;
          DestNA.IsInherited := True;
          DestNA.IsModified := False;
        end;
    end;
end;

procedure TXMLNode.InheritFrom(Source: TXMLNode; ExcludeNodes: array of TXMLNode;
  aMode: TAssignModeSet = [amAppend, amReplace, amNodeName]);
begin
  AssignWithChild(Source, ExcludeNodes, aMode, InheritFromCallback);
  //IsModified := False;
end;
{
procedure InheritToCallback(Sour, Dest: TXMLNode);
var
  i: Integer;
  SourNA, DestNA: TXMLNodeAttribute;
begin
  Dest.Name := Sour.Name;
  Dest.AsString := Sour.AsString;
  // copy attributes
  for i:=0 to Sour.Attributes.Count-1 do
    begin
      SourNA := Sour.Attributes[i];
      DestNA := Dest.AttributeByName(SourNA.Name);
      if DestNA = nil then
        begin
          DestNA := Dest.Attributes.AddAttribute(SourNA.Name, '');
          DestNA.AsString := SourNA.AsString;
          DestNA.IsInherited := True;
          DestNA.IsModified := False;
        end;
    end;
end;

procedure TXMLNode.InheritTo(Dest: TXMLNode; ExcludeNodes: array of TXMLNode);
begin
  Dest.AssignWithChild(Self, ExcludeNodes, True, InheritToCallback);
  //Dest.IsModified := False;
end;
}
function TXMLNode.NodeByName(AName: string): TXMLNode;
begin
     Result:=Nodes.NodeByName(AName);
end;

function TXMLNode.NodeByName(AName: string; Default : String): TXMLNode;
begin
     Result:=Nodes.NodeByName(AName, Default);
end;

function TXMLNode.NodeByPath(xPath : String; aCreate : Boolean = False): TXMLNode;
begin
     Result:=Nodes.NodeByPath(xPath, aCreate);
end;

function TXMLNode.AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
begin
     Result:=Nodes.AttributeByPath(xPath, aCreate);
end;

function TXMLNode.AttributeByName(AName: string): TXMLNodeAttribute;
begin
     Result:=Attributes.AttributeByName(AName);
end;

function TXMLNode.AttributeByName(AName: string; Default : String): TXMLNodeAttribute;
begin
     Result:=Attributes.AttributeByName(AName, Default);
end;

function TXMLNode.CloseTag: string;
begin
     Result := Format('</%s>',[Name]);
end;

function TXMLNode.Level: integer;
var
  AParent: TXMLNode;
begin
  AParent := Parent;
  Result := 0;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end;
end;

function TXMLNode.OpenTag: string;
var
  inx : integer;
  tmpName, tmpValue: String;
begin
  Result := '<' + Name;
  for inx:=0 to FAList.Count-1 do
  if FAList.Attribute[inx].IsModified or
    (not FAList.Attribute[inx].IsInherited) or
    (FAList.Attribute[inx].IsInherited and (Owner<>nil) and Owner.SaveInherited) then // Inherited attrs is not saved
  begin
    {if inx > FAList.Count-1
      then raise EAssertionFailed.Create('Index error in OpenTag');}
    tmpName := FAList.Attribute[inx].Name;
    tmpValue := FAList.Attribute[inx].AsString;
    Result := Result + Format(' %s=%s', [tmpName, Quote(ConvertToXML(tmpValue))]);
  end;
  Result := Result + '>';
end;

procedure TXMLNode.EnumerateNodes(ACallback: TXMLEnumNodeEvent; WithChild: Boolean = TRUE);
var
  ICount: integer;
begin
  for ICount := 0 to FNodes.Count-1 do
  begin
    if Assigned(ACallback) then ACallback(Self, FNodes[ICount]);
    if WithChild
      then FNodes[ICount].EnumerateNodes(ACallback, WithChild);
  end;
end;

function TXMLNode.GetAsBoolean: Boolean;
begin
     if IsNumber(FValue)
        then Result := Boolean(StrToInt(FValue))
        else Result := Boolean(StrToBool(FValue));
end;

function TXMLNode.GetAsDateTime: TDateTime;
begin
  Result := StringToDateTime(FValue);
end;

function TXMLNode.GetAsFloat: Extended;
begin
  Result := StringToFloat(FValue);
end;

function TXMLNode.GetAsInteger: integer;
begin
  if Trim(FValue) = ''
    then Result := 0
    else Result := StrToInt(FValue);
end;

function TXMLNode.GetAsString: string;
begin
  Result := FValue;
end;

procedure TXMLNode.SetValue(const AValue: string);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  FIsModified := True;
  FIsInherited := False;
end;

procedure TXMLNode.SetAsBoolean(const Value: Boolean);
begin
  SetValue(IntToStr(Ord(Value)));
end;

procedure TXMLNode.SetAsDateTime(const Value: TDateTime);
begin
  SetValue(DateTimeToString(Value));
end;

procedure TXMLNode.SetAsFloat(const Value: Extended);
begin
  SetValue(FloatToStr(Value, XMLFormatSettings));
end;

procedure TXMLNode.SetAsInteger(const Value: integer);
begin
  SetValue(IntToStr(Value));
end;

procedure TXMLNode.SetAsString(const Value: string);
begin
  SetValue(Value);
end;

procedure TXMLNode.SetIsInherited(const Value: Boolean);
{var
  i: Integer;}
begin
  if FIsInherited = Value then Exit;
  FIsInherited := Value;
  {if not FIsInherited then
  begin
    // reset attrs FIsInherited
    for i:=0 to Attributes.Count-1 do
      Attributes[i].IsInherited := False;
  end;}
end;

procedure TXMLNode.SetIsModified(const Value: Boolean);
var
  i: Integer;
begin
  if FIsModified = Value then Exit;
  FIsModified := Value;
  if FIsModified and (Owner <> nil)
    then Owner.IsModified := True;
  if not FIsModified then
  begin
    // reset attrs IsModified
    for i:=0 to Attributes.Count-1 do
      Attributes[i].IsModified := False;
  end;
end;

function TXMLNode.GetIndex: integer;
begin
  if Parent = nil
    then Result := -1
    else Result := Parent.FNodes.FList.IndexOf(Self);
end;

function TXMLNode.GetIsLeafNode: Boolean;
begin
  Result := FNodes.Count = 0;
end;


function TXMLNode.GetPath: string;
var
  aNode: TXMLNode;
begin
  aNode := Self; Result := '';
  repeat
    Result := OrStr(aNode.Name, '/', Result);
    aNode := aNode.Parent;
  until (aNode=nil); // or (aNode.Parent=nil);
end;

//------------------------------------------------------------------------------

// *** TXMLNodeList ***

constructor TXMLNodeList.Create(AParent: TXMLNode);
begin
  inherited Create;
  FList := TList.Create;
  FParent := AParent;
  FCurrentNode := AParent;
  FKeyAttribute := '';
  FIsNodesOwner := True;
end;

destructor TXMLNodeList.Destroy;
var
  ICount: integer;
begin
  if IsNodesOwner then
    for ICount := Count-1 downto 0 do
      Node[ICount].Free;
  FList.Free;
  inherited Destroy;
end;

function TXMLNodeList.AddLeaf(AName: string): TXMLNode;
begin
  Result := AddOpenTag(AName);
  AddCloseTag;
end;

function TXMLNodeList.AddOpenTag(AName: string): TXMLNode;
begin
  if FCurrentNode=nil
    then Result := TXMLNode.Create(nil)
    else Result := TXMLNode.Create(FCurrentNode);
  Result.Name := AName;
  if Parent <> nil
    then Result.FOwner := Parent.Owner;

  if Result.Name[Length(Result.Name)]='/'
     then Result.FKind:=tkOpenClose
     else Result.FKind:=tkOpen;

  if FCurrentNode = nil then
    AddNode(Result)
  else
    FCurrentNode.Nodes.AddNode(Result);

  FCurrentNode := Result;
end;

procedure TXMLNodeList.AddCloseTag;
begin
  FCurrentNode := FCurrentNode.Parent;
end;

procedure TXMLNodeList.NextNode;
var
  AIndex: integer;
begin
  AIndex := FList.IndexOf(FCurrentNode);
  if AIndex < FList.Count then
  FCurrentNode := TXMLNode(FList[AIndex]);
end;

procedure TXMLNodeList.Clear;
//var
  //ICount: integer;
begin
     while Count > 0 do
           begin
           Node[0].Nodes.Clear;
           DeleteNode(0);
           end;
{
     for ICount := FList.Count-1 downto 0 do
         begin
         Node[ICount].Nodes.Clear;
         Node[ICount].Free;
         Node[ICount] := nil;
         end;
     FList.Clear;
//     FCurrentNode := nil;
}
     FCurrentNode := Self.Parent;
end;

procedure TXMLNodeList.DeleteNode(aName: string);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if AnsiCompareText(aName, Node[i].Name) =0 then
    begin
      DeleteNode(i);
      Exit;
    end;
end;

procedure TXMLNodeList.DeleteNode(aNumber: integer);
begin
  if IsNodesOwner
    then Node[aNumber].Free;
  FList.Delete(aNumber);
end;

function TXMLNodeList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TXMLNodeList.GetNode(index: integer): TXMLNode;
begin
  Result := TXMLNode(FList[index]);
end;

function TXMLNodeList.NodeByName(AName: string): TXMLNode;
var ICount: integer;
begin
     Result := nil; AName := AnsiUpperCase(AName);
     for ICount := 0 to Count-1 do
         begin
         if AnsiUpperCase(Node[ICount].Name) = AName then
            begin
            Result := Node[ICount];
            Exit;
            end;
         end;
end;

function TXMLNodeList.NodeByName(AName: string; Default : String): TXMLNode;
begin
     Result:=NodeByName(AName);
     if (Result=nil) then
        begin
        Result:=Self.AddLeaf(AName);
        Result.AsString:=Default;
        end;
end;

function TXMLNodeList.GetRoot: TXMLNode;
begin
     if Count > 0
        then Result := Node[0]
        else Result := nil;
end;

procedure TXMLNodeList.AddNode(ANode: TXMLNode);
begin
     FList.Add(ANode);
end;

procedure TXMLNodeList.SetNode(index: integer; const Value: TXMLNode);
begin
  FList[index] := Value;
end;

function TXMLNodeList.NodeByPath (xPath : String; aCreate : Boolean = False) : TXMLNode;
var
  inx, num : Integer;
  Tag, Del : String;
  Ch, Ch2 : TXMLNode;
  DelSet: TSysCharSet;
begin
  Result:=nil; DelSet := ['/',':','[',']'];
  // first/second/third
  if Parent=nil
    then Ch:=Root
    else Ch:=Parent;
  for inx:=1 to WordCount(xPath, DelSet) do
  begin
    Tag := ExtractWord(inx, xPath, DelSet);
    Del := ExtractDelimiter(inx, xPath, DelSet);

//ShowMessage(xPath + #13#10 + 'Tag: '+ Tag + '; Del: ' + Del);

    if Tag > '' then
    begin
      if (Copy(Del,1,1) <> ']') then // Standart node
      begin
        if (inx=1) and (AnsiCompareText(Tag, Ch.Name) = 0)
          then Ch2 := Ch
          else Ch2 := Ch.Nodes.NodeByName(Tag);
        if Ch2 = nil then
        begin
          if aCreate
            then Ch2 := Ch.Nodes.AddLeaf(Tag)
            else Exit;
        end;
        Ch := Ch2;
      end;

      if Copy(Del,1,1) = ']' then // Numbered list item
      begin
        if AnsiCompareText(Tag, 'First') = 0
          then Tag := '0';
        if AnsiCompareText(Tag, 'Last') = 0
          then Tag := IntToStr(Ch.Nodes.Count-1);
        num := StrToIntDef(Tag, -1);
        if (num >= 0) and (num < Ch.Nodes.Count)
          then Ch := Ch.Nodes[num]
          else Exit; // What I must create ?
      end;
    end;

    if (Del = ':') or (Del = ']:') // Attribute next
      then Break;
  end;
  Result:=Ch;
end;

function TXMLNodeList.AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
var
  Attr: String;
  Ch: TXMLNode;
  At: TXMLNodeAttribute;
begin
  Result:=nil; Attr := '';
  Ch := NodeByPath(xPath, aCreate);
  Attr := ExtractWord(2, xPath, [':']);
  if (Ch=nil) or (Attr='')
    then Exit;
  At := Ch.AttributeByName(Attr);
  if (At=nil) and aCreate
    then At := Ch.AttributeByName(Attr, '');
  Result := At;
end;

function TXMLNodeList.NodeByValue (NodeName, Mask : String;
                                   IgnoreCase : Boolean = True;
                                   MaxLevel : Integer = 65535;
                                   ResultList : TXMLNodeList = nil) : TXMLNodeList;
var inx : Integer;
begin
  Result:=nil;
  if MaxLevel = -1
    then Exit;
  NodeName:=UpperCase(NodeName);
  if not Assigned(ResultList) then
  begin
    ResultList:=TXMLNodeList.Create(nil);
    ResultList.IsNodesOwner := False;
  end;
  for inx:=0 to Count-1 do
  begin
    if ((NodeName='') or (UpperCase(Node[inx].Name) = NodeName)) and
      IsWild(Node[inx].AsString, Mask, IgnoreCase)
      then ResultList.AddNode(Node[inx]);
    Node[inx].Nodes.NodeByValue(NodeName, Mask, IgnoreCase, MaxLevel-1, ResultList);
  end;
  Result:=ResultList;
end;

function TXMLNodeList.NodePos(aNode: TXMLNode): Integer;
var
  ICount: Integer;
begin
  Result := -1;
  for ICount := 0 to Count-1 do
    if Node[ICount] = aNode then
    begin
      Result := ICount;
      Exit;
    end;
end;

function TXMLNodeList.NodeByAttrValue(NodeName, NodeMask, AttrName,
  AttrMask: String; IgnoreCase: Boolean; MaxLevel: Integer;
  ResultList: TXMLNodeList): TXMLNodeList;
var
  inx, iny: integer;
  aNodeList: TXMLNodeList;
begin
  aNodeList := TXMLNodeList.Create(nil);
  aNodeList.IsNodesOwner := False;
  NodeByValue (NodeName, NodeMask, IgnoreCase, MaxLevel, aNodeList);
  AttrName := UpperCase(AttrName);
  for inx:=0 to aNodeList.Count-1 do
    for iny := 0 to aNodeList[inx].Attributes.Count-1 do
      if ((AttrName='') or (UpperCase(aNodeList[inx].Attributes[iny].Name) = AttrName)) and
        IsWild(aNodeList[inx].Attributes[iny].AsString, AttrMask, IgnoreCase)
        then ResultList.AddNode(aNodeList[inx]);
  aNodeList.Free;
  Result := ResultList;
end;

procedure TXMLNodeList.SwapNodes(Node1, Node2 : Integer);
var TmpNode : TXMLNode;
begin
     TmpNode:=Node[Node1];
     Node[Node1]:=Node[Node2];
     Node[Node2]:=TmpNode;
end;

procedure TXMLNodeList.MoveNode(aFromPos, aToPos: Integer);
begin
  FList.Move(aFromPos, aToPos);
end;

//------------------------------------------------------------------------------

// *** TXMLObject ***

constructor TXMLObject.Create;
begin
  inherited Create;
  FNodes := TXMLNodeList.Create(nil);
  AutoIndent := True;
  FEncoding := 'windows-1251';
  FVersion := '1.0';
  FIsModified := False;
  FIsInherited := False;
  FSaveInherited := False;
end;

destructor TXMLObject.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

function TXMLObject.Root : TXMLNode;
begin
  Result:=Nodes.Root;
end;

function TXMLObject.NodeByPath (xPath : String; aCreate : Boolean = False) : TXMLNode;
begin
  Result:=Root.Nodes.NodeByPath(xPath, aCreate);
end;

function TXMLObject.AttributeByPath (xPath : String; aCreate : Boolean = False) : TXMLNodeAttribute;
begin
  Result:=Root.Nodes.AttributeByPath(xPath, aCreate);
end;

function TXMLObject.NodeByValue (NodeName, Mask : String;
                                    IgnoreCase : Boolean = True;
                                    MaxLevel : Integer = 65535) : TXMLNodeList;
begin
  Result:=Root.Nodes.NodeByValue (NodeName, Mask, IgnoreCase, MaxLevel)
end;

function TXMLObject.NodeByAttrValue (NodeName, NodeMask: String;
                          AttrName, AttrMask: String;
                          IgnoreCase : Boolean = True;
                          MaxLevel : Integer = 65535;
                          ResultList: TXMLNodeList = nil) : TXMLNodeList;
begin
  Result:=Root.Nodes.NodeByAttrValue (NodeName, NodeMask, AttrName, AttrMask, IgnoreCase, MaxLevel, ResultList);
end;


function TXMLObject.GetSpecification: string;
begin
  Result := Format(XML_SPECIFICATION, [Version, Encoding]);
end;

function TXMLObject.GetCR: string;
begin
  if AutoIndent then
    Result := #13#10
  else
    Result := '';
end;

function TXMLObject.GetIndent(ALevel: integer): string;
var
  ICount: integer;
begin
  Result := '';
  if not AutoIndent then Exit;
  for ICount := 1 to ALevel do
    Result := Result + #9;
end;

procedure TXMLObject.LoadFromStream(Stm : TStream; aSize: Integer = -1);
begin
  Stream:=TMemoryStream.Create;
  TMemoryStream(Stream).LoadFromStream(Stm);
  if aSize >=0
    then Stream.Size := aSize;
  if Stream.Size = 0
    then InitEmpty
    else XMLToObject;
  Stream.Free;
end;

procedure TXMLObject.LoadFromFile(XMLFile : TFileName = '');
begin
     if XMLFile='' then XMLFile:=FileName;
     if FileExists(XMLFile)
      then Stream:=TFileStream.Create(XMLFile, fmOpenRead)
      else Stream:=TFileStream.Create(XMLFile, fmCreate);
     if Stream.Size = 0
      then InitEmpty
      else XMLToObject;
     Stream.Free;
end;

procedure TXMLObject.LoadFromResource(ResName: String);
var
  ResHandle: Cardinal;
  ResSize: Cardinal;
begin
  if ResName = '' then Exit;
  ResHandle := FindResource(HInstance, PChar(AnsiUpperCase(ResName)), 'TEXT');
  if ResHandle = 0 then Exit;
  ResSize := SizeofResource(HInstance, ResHandle);
  ResHandle := LoadResource(HInstance, ResHandle);
  if ResHandle = 0 then Exit;

  Stream:=TMemoryStream.Create;
  try
    Stream.Write(Pointer(ResHandle)^, ResSize);
    if Stream.Size = 0
      then InitEmpty
      else XMLToObject;
  finally
    Stream.Free;
  end;
end;

procedure TXMLObject.LoadFromString(var StrRef: String);
var
  Stm: TStringStream;
begin
  Stream := TMemoryStream.Create;
  Stm := TStringStream.Create(StrRef);
  LoadFromStream(Stm);
  Stm.Free;
{  try
    TMemoryStream(Stream).LoadFromStream(Stm);
    if Stream.Size = 0
      then InitEmpty
      else XMLToObject;
  finally
    Stm.Free;
    Stream.Free;
  end; }
end;

{$IFDEF XMLDB}
procedure TXMLObject.LoadFromBlob(Field : TBlobField);
//var Stm : TBlobStream;
begin
   Stream:=TMemoryStream.Create;
   Field.SaveToStream(Stream);
   if Stream.Size = 0
    then InitEmpty
    else XMLToObject;
   Stream.Free;
   //Stm:=TBlobStream.Create(Field, bmRead);
   //LoadFromStream(Stm);
   //Stm.Free;
end;
{$ENDIF}

procedure TXMLObject.SaveToStream(Stm: TStream; var aSize: Integer);
begin
     Stream:=TMemoryStream.Create;
     ObjectToXML;
     aSize := Stream.Size;
     TMemoryStream(Stream).SaveToStream(Stm);
     Stream.Free;
end;

procedure TXMLObject.SaveToStream(Stm: TStream);
begin
     Stream:=TMemoryStream.Create;
     ObjectToXML;
     //TMemoryStream(Stream).Position := 0;
     TMemoryStream(Stream).SaveToStream(Stm);
     Stream.Free;
end;

procedure TXMLObject.SaveToString(var StrRef: String);
var
  Stm: TStringStream;
begin
  Stm:=TStringStream.Create(StrRef);
  SaveToStream(Stm);
  StrRef:=Stm.DataString;
  Stm.Free;
end;

procedure TXMLObject.SaveToPChar(var StrRef: PAnsiChar);
var
  Stm: TStringStream;
begin
  Stm:=TStringStream.Create(StrRef);
  SaveToStream(Stm);
  GetMem(StrRef, Stm.Size+1);
  StrPCopy(StrRef, Stm.DataString);
  Stm.Free;
end;

procedure TXMLObject.SetNodesNotInherited(Sender: TObject; ANode: TXMLNode);
begin
  aNode.IsInherited := False;
end;

procedure TXMLObject.SetIsInherited(const Value: Boolean);
begin
  FIsInherited := Value;
  if not FIsInherited then // reset nodes FIsInherited
    Root.EnumerateNodes(SetNodesNotInherited);
end;

procedure TXMLObject.SetNodesNotModified(Sender: TObject; ANode: TXMLNode);
begin
  aNode.IsModified := False;
end;

procedure TXMLObject.SetIsModified(const Value: Boolean);
begin
  FIsModified := Value;
  if not FIsModified then // reset nodes IsModified
    Root.EnumerateNodes(SetNodesNotModified);
end;

{$IFDEF XMLDB}
procedure TXMLObject.SaveToBlob(Field: TField);
var Stm : TStream;
  AutoPost: Boolean;
begin
     AutoPost := not (Field.DataSet.State in [dsEdit, dsInsert]);
     if AutoPost
      then Field.DataSet.Edit;
     Stm:=Field.DataSet.CreateBlobStream(Field, bmWrite);
     try
     SaveToStream(Stm);
     if AutoPost
      then Field.DataSet.Post;
     finally
     Stm.Free;
     end;
end;
{$ENDIF}

procedure TXMLObject.SaveToFile(XMLFile: TFileName = '');
begin
     if XMLFile='' then XMLFile:=FileName;
     Stream:=TFileStream.Create(XMLFile, fmCreate);
     ObjectToXML;
     Stream.Free;
end;

function TXMLObject.ReadValue : String;
begin
     Result:=XMLString.ReadValue(Stream, False);
end;

function TXMLObject.ReadTag : TTag;
begin
     Result:=XMLString.ReadTag(Stream, False);
end;


procedure TXMLObject.XMLToObject;
var Tag : TTag;
    Ch : Char;
    inx : Integer;
begin
  Stream.Position:=0;
  Nodes.Clear;
  while Stream.Read(Ch, 1)=1 do
  begin
    case Ch of
      '<':
        begin
          Tag:=ReadTag;
          case Tag.Kind of
            tkSystem: Continue;
            tkClose: Nodes.AddCloseTag;
          else
            begin
              with Nodes.AddOpenTag(Tag.Name) do
                for inx:=0 to Length(Tag.Params)-1 do
                  Attributes.AddAttribute(Tag.Params[inx].Name, Unquote(ConvertFromXML(Tag.Params[inx].Value)));
              if Tag.Kind = tkOpenClose
                then Nodes.AddCloseTag;
            end; // else
          end; // case
        end; // item
      #0..#19,
      #32:
        Continue;
    else
      Nodes.CurrentNode.AsString:=ConvertFromXML(Ch + ReadValue);
    end;
  if Nodes.Count=1
    then Root.FOwner := Self;
  end;
  if Nodes.Count=0
    then Nodes.AddLeaf('XML').FOwner := Self;
  if Assigned(FOnAfterLoad)
    then FOnAfterLoad(Self);
end;

procedure TXMLObject.NodeToString(ANode: TXMLNode);
var ICount: integer;
begin
     if ANode.IsLeafNode
        then begin
             if ANode.Kind = tkOpen
                then WriteStrToStream(Stream, GetIndent(ANode.Level) + ANode.OpenTag +
                                      ConvertToXML(ANode.AsString) + ANode.CloseTag + GetCR)
                else WriteStrToStream(Stream, GetIndent(ANode.Level) + ANode.OpenTag + GetCR);
             end
        else begin
             WriteStrToStream(Stream, GetIndent(ANode.Level) + ANode.OpenTag + ConvertToXML(ANode.AsString) + GetCR);
             for ICount := 0 to ANode.FNodes.Count-1 do
                 NodeToString(ANode.Nodes.Node[ICount]);
             WriteStrToStream(Stream, GetIndent(ANode.Level) + ANode.CloseTag + GetCR);
             end;
end;

procedure TXMLObject.ObjectToXML;
var ICount: integer;
begin
  if Assigned(FOnBeforeSave)
    then FOnBeforeSave(Self);
  if FNodes.Count = 0 then Exit;
  WriteStrToStream(Stream, Specification + GetCR);
  for ICount := 0 to FNodes.Count-1 do
    NodeToString(FNodes.Node[ICount]);
end;

function TXMLObject.AsString : AnsiString;
var Res : TStringStream;
begin
     Res:=TStringStream.Create(Result);
     SaveToStream(Res);
     Result:=Res.DataString;
     Res.Free;
end;

procedure TXMLObject.InitEmpty(RootName : String = 'XML');
begin
     Nodes.Clear;
     Nodes.AddOpenTag(RootName);
     Nodes.AddCloseTag;
end;

{ TXMLObjectLink }

constructor TXMLObjectLink.Create;
begin
     inherited Create;
     FSkipEmpty:=True;
end;

function TXMLObjectLink.GetAutoIndent: Boolean;
begin
     Result:=True;
     if XMLObject<>nil then Result:=XMLObject.AutoIndent;
end;

function TXMLObjectLink.GetFileName: TFileName;
begin
     Result:='';
     if XMLObject<>nil then Result:=XMLObject.FileName;
end;

procedure TXMLObjectLink.SetAutoIndent(const Value: Boolean);
begin
     if XMLObject<>nil then XMLObject.AutoIndent:=Value;
end;

procedure TXMLObjectLink.SetFileName(const Value: TFileName);
begin
     if XMLObject<>nil then XMLObject.FileName:=Value;
end;

initialization

  GetLocaleFormatSettings(LCID_INSTALLED, XMLFormatSettings);
  XMLFormatSettings.DecimalSeparator := '.';
  XMLFormatSettings.DateSeparator := '.';
  XMLFormatSettings.TimeSeparator := ':';
  XMLFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  XMLFormatSettings.ShortTimeFormat := 'hh:nn';
  XMLFormatSettings.LongTimeFormat := 'hh:nn:ss';

end.

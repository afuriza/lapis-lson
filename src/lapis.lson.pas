unit Lapis.Lson;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, FGL, Lapis.TypeUtils,Dialogs, Graphics;

type
  EParserException = class(Exception);
  TSymbolClass = (scWhiteSpace, scHex, scPascalHex, scAssign, scNumber, scLetters,
    scQuote, scOpeningBracket, scClosingBracket, scOther);

  TLSONNode = class;

  TAttributeTypeVal = (atvString, atvInteger, atvHex, atvIdentifier, atvObject);

  TAttribute = class
  public
    Value: TLSONVariant;
    TypeVal: TAttributeTypeVal;
    function AsString: TLSONString;
    function AsInteger: TLSONInteger;
    function AsHex: TLSONHex;
  end;

  TLSONNodeMap = specialize TFPGMapObject<string, TLSONNode>;

  TLSONNode = class(TPersistent)
  private
    procedure setAttributeType(AValue: TAttributeTypeVal);
    function getAttributeType: TAttributeTypeVal;
  protected
    FParent:TLSONNode;
    FNodeName: string;
    FAttribute: TAttribute;
    FObject: TObject;
    FChilds:TLSONNodeMap;
    function FindNodeInternal(const ANodePath: string; AIndex: integer = 0): TLSONNode;
    function ReadLSONStr(AName:String; ANode:TLSONNode; Deepness: integer = 0): string;
  public
    constructor Create(AParent:TLSONNode; AName: string);
    destructor Destroy;override;
    function FindNode(const ANodePath: string): TLSONNode;
    function NewChildNode(const ANodeName: string): TLSONNode;
    class function ParseLSON(const ALSONDocument: string): TLSONNode;
    property NodeName: string read FNodeName;
    property Parent:TLSONNode read FParent;
    property HandledObject: TObject read FObject write FObject;
    property Childs: TLSONNodeMap read FChilds;
    property AttrValue: TAttribute read fAttribute write FAttribute;
    property AttrType: TAttributeTypeVal read getAttributeType write setAttributeType;
    function getLSONString: TLSONString;
  end;

  TState = (stWaitForID, stID, stWaitForOpeningBracket, stPrepareAssign, stAttributeName,
    stWaitForAttributeValue, stAttributeValueString, stAttributeValueNumber,
    stAttributeValueHex, stAttributeValueIdentifier);

  TLSON = class(TPersistent)
    protected
      FState:TState;
      FName, FValue:String;
      FChrIndex: integer;
      FDocument: string;
      FCurrentNode:TLSONNode;
      FNodes:TLSONNode;
      function GetSymbolClass(ASymbol:String):TSymbolClass;
      procedure ProcessSymbol(ASymbol:String);
      procedure ValidateNext;
    public
      constructor Create;
      destructor Destroy;override;
      procedure Parse(AString:String);
      property RootNode:TLSONNode read FNodes;
  end;

const
  SymbolClasses:array[TSymbolClass] of String = (
    ' '#10#13,
    '#',
    '$',
    ':',
    '01234567890',
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_/',
    '"',
    '{',
    '}',
    ''
  );


implementation

{ LSON Attribute }

function TAttribute.AsHex: TLSONHex;
begin
  Result := Value.asHex;
end;

function TAttribute.AsString: TLSONString;
begin
  Result := Value.asString;
end;

function TAttribute.AsInteger: TLSONInteger;
begin
  Result := Value.asInteger;
end;

{ LSON Node }

constructor TLSONNode.Create(AParent:TLSONNode; AName: string);
begin
  FParent := AParent;
  FNodeName := AName;
  FAttribute := TAttribute.Create;
  FChilds := TLSONNodeMap.Create;
end;

destructor TLSONNode.Destroy;
begin
  Childs.Free;
  FAttribute.Free;
end;

class function TLSONNode.ParseLSON(const ALSONDocument: string): TLSONNode;
var
  LSON: TLSON;
begin
  LSON := TLSON.Create;
  try
    // there's a bug here
    LSON.Parse(ALSONDocument+' ');
  finally
    Result := LSON.RootNode;
    Result.AttrType := atvObject;
    LSON.FCurrentNode := nil;
    LSON.FNodes := nil;
    FreeAndNil(LSON);
  end;
end;

function TLSONNode.ReadLSONStr(AName:String; ANode:TLSONNode; Deepness: integer = 0): string;
var
  i: integer;
  function DeepnessToSpaces(AValue: integer): string;
  var
    i: integer;
  begin
    Result := '';
    for i := 1 to AValue do
    begin
      result += '  ';
    end;
  end;

begin
  case ANode.AttrType of
    atvString:
      result := AName + ': "' + ANode.AttrValue.AsString + '"';
    atvInteger:
      result := AName + ': ' + ANode.AttrValue.AsInteger.ToString;
    atvHex:
      result := AName + ': $' + IntToHex(ANode.AttrValue.AsHex);
    atvIdentifier:
      result := AName + ': ' + ANode.AttrValue.AsString;
    atvObject:
    begin
      //if (ANode.NodeName <> 'Root') and (ANode.Parent <> nil) then
      //begin
        if (ANode.NodeName <> 'Root') and (ANode.Parent <> nil) then
          Deepness -= 1;
        if (ANode.NodeName <> 'Root') and (ANode.Parent <> nil) then
          result := AName;
        for i := 0 to ANode.Childs.Count - 1 do
        begin
          if i = 0 then
          begin
            if (ANode.NodeName <> 'Root') and (ANode.Parent <> nil) then
              result += ' {';
            result += #10#13;
            result += DeepnessToSpaces(Deepness+1);
            result += ReadLSONStr(ANode.Childs.Keys[I], ANode.Childs.Data[I], Deepness+1);
          end
          else if i = ANode.Childs.Count - 1 then
          begin
            // there's a bug here
            result += #10#13;
            result += DeepnessToSpaces(Deepness+1);
            result += ReadLSONStr(ANode.Childs.Keys[I], ANode.Childs.Data[I], Deepness+1);
            if (ANode.NodeName <> 'Root') and (ANode.Parent <> nil) then
            begin
              result += #10#13;
              result += DeepnessToSpaces(Deepness);
              result += '}';
            end;
          end
          else
          begin
            result += #10#13;
            result += DeepnessToSpaces(Deepness+1);
            result += ReadLSONStr(ANode.Childs.Keys[I], ANode.Childs.Data[I], Deepness+1);
          end;
        end;
      //end;
    end;
  end;
end;

function TLSONNode.getLSONString: TLSONString;
begin
  Result := '';
  result := ReadLSONStr(Self.NodeName, Self);
end;

function TLSONNode.NewChildNode(const ANodeName: string): TLSONNode;
begin
  FChilds[ANodeName.ToLower] := TLSONNode.Create(Self, ANodeName.ToLower);
  Result := FChilds[ANodeName.ToLower];
end;

function TLSONNode.FindNodeInternal(const ANodePath: string; AIndex: integer = 0): TLSONNode;
var
  NodeNames: TStringList;
begin
  Result := nil;
  NodeNames := TStringList.Create;
  NodeNames.Delimiter := '.';
  NodeNames.DelimitedText := ANodePath;
  if Childs.IndexOf(NodeNames[AIndex]) <> -1 then
  begin
    if AIndex < NodeNames.Count -1 then
      Result := TLSONNode(Childs[NodeNames[AIndex]]).FindNodeInternal(
        ANodePath, AIndex + 1
      )
    else
      Result := TLSONNode(Childs[NodeNames[AIndex]]);
  end
  else
    Result := Self;
  NodeNames.Free;
end;

function TLSONNode.FindNode(const ANodePath: string): TLSONNode;
begin
  Result := FindNodeInternal(ANodePath.ToLower);
end;


procedure TLSONNode.setAttributeType(AValue: TAttributeTypeVal);
begin
  if FAttribute.TypeVal = AValue then
    Exit;
  fAttribute.TypeVal := AValue;
end;

function TLSONNode.getAttributeType: TAttributeTypeVal;
begin
  Result := fAttribute.TypeVal;
end;

{ LSON Parser }

constructor TLSON.Create;
begin
  FNodes := TLSONNode.Create(nil, 'Root');

  FCurrentNode := FNodes;
end;

destructor TLSON.Destroy;
begin
  FNodes.Free;
  if FCurrentNode <> FNodes then FCurrentNode.Free;
end;

procedure TLSON.Parse(AString:String);
var
  i:Integer;
  s:String;
begin
  FDocument := AString;
  for i := 1 to Length(AString) do begin
    s := AString[i];
    FChrIndex := i;
    ProcessSymbol(s);
  end;
end;

function TLSON.GetSymbolClass(ASymbol:String):TSymbolClass;
var
  i: integer;
  function DefineSymbol(ACurrentSymbol: String): TSymbolClass;
  begin
    for Result := Low(Result) to High(Result) do begin
      if Pos(ACurrentSymbol, SymbolClasses[Result], 1) > 0 then Break;
    end;
  end;
begin
  Result := DefineSymbol(ASymbol);
  if Result = scNumber then
  begin
    for i := FChrIndex downto 1 do
    begin
      if (DefineSymbol(FDocument[i]) = scNumber) or
        (DefineSymbol(FDocument[i]) = scLetters) then
      begin
        if DefineSymbol(FDocument[i]) = scLetters then
        begin
          Result := scLetters;
        end;
      end
      else if (DefineSymbol(FDocument[i]) = scHex) then
      begin
        Result := scHex;
      end
      else if (DefineSymbol(FDocument[i]) = scPascalHex) then
      begin
        Result := scPascalHex;
      end
      else
        Break;
    end;
  end;
  if Result = scLetters then
  begin
    for i := FChrIndex downto 1 do
    begin
      if (DefineSymbol(FDocument[i]) = scHex) then
      begin
        Result := scHex;
      end
      else if (DefineSymbol(FDocument[i]) = scPascalHex) then
      begin
        Result := scPascalHex;
      end
      else
      begin
        if (DefineSymbol(FDocument[i]) <> scNumber) and
          (DefineSymbol(FDocument[i]) <> scLetters) then
        begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TLSON.ValidateNext;
begin
  if FChrIndex >= Length(FDocument) then
    raise EParserException.Create('Unclosed token symbol: ' + FDocument[FChrIndex]);
end;

procedure TLSON.ProcessSymbol(ASymbol:String);
var
  Temp:TLSONNode;
  scError: string;
begin
  case FState of
    stWaitForID:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:{Skip};
        scLetters:begin
          FName := FName + ASymbol;
          FState := stID;
        end;
        scQuote:FState := stAttributeName;
        scOpeningBracket:begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode := Temp;
          FName := '';
        end;
        scClosingBracket:begin
          if Assigned(FCurrentNode.Parent) then begin
            FCurrentNode := FCurrentNode.Parent;
          end
          else begin
            WriteStr(scError, GetSymbolClass(ASymbol));
            raise EParserException.Create('Unexpected symbol (' +
              FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
          end;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stID:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:FState := stWaitForOpeningBracket;
        scLetters:begin
          FName := FName + ASymbol;
        end;
        scOpeningBracket:begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);

          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvObject;
          FCurrentNode := Temp;
          FName := '';
          FState := stWaitForID;
        end;
        scAssign:begin
          FState := stPrepareAssign;
        end
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stWaitForOpeningBracket:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:{Skip};
        scAssign:begin
          FState := stPrepareAssign;
        end;
        scOpeningBracket:begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvObject;
          FCurrentNode := Temp;
          FName := '';
          FState := stWaitForID;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stPrepareAssign:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:{Skip};
        scQuote:FState := stAttributeValueString;
        scLetters:
        begin
          FState := stAttributeValueIdentifier;
          FValue := FValue + ASymbol;
        end;
        scNumber:
        begin
          FState := stAttributeValueNumber;
          FValue := FValue + ASymbol;
        end;
        scHex:
        begin
          FState := stAttributeValueHex;
          FValue := FValue + ASymbol;
        end;
        scPascalHex:
        begin
          FState := stAttributeValueHex;
          FValue := FValue + ASymbol;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stAttributeValueString:begin
      ValidateNext;
      case GetSymbolClass(ASymbol) of
        scQuote:
        begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvString;
          FCurrentNode.Childs[FName.ToLower].AttrValue.Value := FValue;
          FName := '';
          FValue := '';
          FState := stWaitForID;
        end;
        else begin
          FValue := FValue + ASymbol;
        end;
      end;
    end;
    stAttributeValueNumber:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:
        begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvInteger;
          FCurrentNode.Childs[FName.ToLower].AttrValue.Value := FValue.ToInteger;
          FName := '';
          FValue := '';
          FState := stWaitForID;
        end;
        scNumber: begin
          FValue := FValue + ASymbol;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stAttributeValueIdentifier:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:
        begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvIdentifier;
          FCurrentNode.Childs[FName.ToLower].AttrValue.Value := FValue;
          FName := '';
          FValue := '';
          FState := stWaitForID;
        end;
        scLetters: begin
          FValue := FValue + ASymbol;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
        end;
      end;
    end;
    stAttributeValueHex:begin
      case GetSymbolClass(ASymbol) of
        scWhiteSpace:
        begin
          Temp := TLSONNode.Create(FCurrentNode, FName.ToLower);
          FCurrentNode.Childs[FName.ToLower] := Temp;
          FCurrentNode.Childs[FName.ToLower].AttrType := atvHex;
          if FValue.Contains('#') then
            FCurrentNode.Childs[FName.ToLower].AttrValue.Value := HTMLColorToPascalHex(FValue)
          else
            FCurrentNode.Childs[FName.ToLower].AttrValue.Value := LongWord(StringToColor(FValue));
          if  FCurrentNode.Childs[FName.ToLower].AttrType = atvHex then
          ShowMessage(Fname+';'+ColorToString(FCurrentNode.Childs[FName.ToLower].AttrValue.AsHex));
          FName := '';
          FValue := '';
          FState := stWaitForID;
        end;
        scHex: begin
          FValue := FValue + ASymbol;
        end;
        scPascalHex: begin
          FValue := FValue + ASymbol;
        end;
        else begin
          WriteStr(scError, GetSymbolClass(ASymbol));
          raise EParserException.Create('Unexpected symbol (' +
            FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError);
        end;
      end;
    end;
    else begin
      WriteStr(scError, GetSymbolClass(ASymbol));
      raise EParserException.Create('Unexpected symbol (' +
        FChrIndex.ToString + '): ' + ASymbol + ' as ' + scError)
    end;
  end;
end;
end.


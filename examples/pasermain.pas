unit PaserMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Lapis.Lson,
  Rtti, TypInfo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo2: TMemo;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public
    Tabs:Integer;
    Parser:TLSON;
    procedure PrintNode(AName:String;ANode:TLSONNode);
    procedure Print(AString:String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  MyNode: TLSONNode;
  SL: TStrings;
  i: integer;
  PropInfo: PPropInfo;
  DummyStrings: TStrings;
  kindstr: string;
begin
  //ShowMessage(IntToHex(clWhite));
  //DummyStrings := TStringList.Create;
  //DummyStrings.Text := 'waaa';
  //PropInfo := GetPropInfo(Memo1.ClassInfo, 'Font');
  //WriteStr(kindStr, PropInfo^.PropType^.Kind);
  //ShowMessage(kindStr);
  //TypInfo.SetObjectProp(Memo1, 'Lines', DummyStrings);
  //TStrings(TypInfo.GetObjectProp(Memo1, 'Lines')).Text := 'waaa';

  //DummyStrings.Free;

  //SL := TStringList.Create;
  //GetComponentPropertyNames(Memo1.Lines, SL);
  //ShowMessage(SL.Text);

  MyNode := TLSONNode.ParseLSON(Memo1.Text);
  Memo2.Clear;
  PrintNode('Specific Node Path', MyNode.FindNode('TLapisButton.Style.Font'));
  PrintNode(MyNode.NodeName, MyNode);
  ShowMessage(MyNode.getLSONString);

  Memo1.Color := MyNode.FindNode('TLapisButton.Style.Color').AttrValue.asHex;
  MyNode.Free;
end;

procedure TForm1.PrintNode(AName:String;ANode:TLSONNode);
var
  i: integer;
begin
  case ANode.AttrType of
    atvString:
      Print('[String] Node: ' + AName + '; Value: ' + ANode.AttrValue.AsString);
    atvInteger:
      Print('[Integer] Node: ' + AName + '; Value: ' + ANode.AttrValue.AsInteger.ToString);
    atvHex:
      Print('[Hex] Node: ' + AName + '; Value: ' + IntToHex(ANode.AttrValue.AsHex));
    atvIdentifier:
      Print('[Identifier] Node: ' + AName + '; Value: ' + ANode.AttrValue.AsString);
    atvObject:
    begin
      if Anode.Parent <> nil then
        Print('[Object] Node: ' + AName + '; Parent: ' + Anode.Parent.NodeName)
      else
        Print('[Object] Node: ' + AName);
    end;
  end;
  Inc(Tabs);
  for I := 0 to ANode.Childs.Count - 1 do begin
    PrintNode(ANode.Childs.Keys[I], ANode.Childs.Data[I]);
  end;
  Dec(Tabs);
end;

procedure TForm1.Print(AString:String);
  var S:String;I:Integer;
begin
  for I := 0 to Tabs - 1 do begin
    S := S + #09;
  end;
  Memo2.Lines.Add(S + AString);
end;

end.


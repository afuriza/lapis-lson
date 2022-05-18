unit LapisLazuli.ComponentUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, FGL;

procedure GetComponentPropertyNames(Component: TComponent; OutList: TStrings);

type
  TComponentMap = specialize TFPGMapObject<string, TComponent>;

implementation

//procedure MapComponentProperties(AComponent: TComponent; OutComponentMap: TComponentMap);
//var
//  I: Integer;
//  Count, Size: Integer;
//  PropList: PPropList;
//  PropInfo: PPropInfo;
//begin
//  OutList.BeginUpdate;
//  try
//    OutList.Clear;
//
//    Count := GetPropList(AComponent.ClassInfo, tkAny, nil);
//    Size  := Count * SizeOf(Pointer);
//    GetMem(PropList, Size);
//    try
//      Count := GetPropList(Component.ClassInfo, tkAny, PropList);
//      for I := 0 to Count -1 do
//      begin
//        PropInfo := PropList^[I];
//        if not (PropInfo^.PropType^.Kind = tkMethod) then
//        begin
//          OutList.Add(PropInfo^.Name);
//        end;
//      end;
//    finally
//      FreeMem(PropList);
//    end;
//  finally
//    OutList.EndUpdate;
//  end;
//end;

procedure GetComponentPropertyNames(Component: TPersistent; OutList: TStrings);
var
  I: Integer;
  Count, Size: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  OutList.BeginUpdate;
  try
    OutList.Clear;

    Count := GetPropList(Component.ClassInfo, tkAny, nil);
    Size  := Count * SizeOf(Pointer);
    GetMem(PropList, Size);
    try
      Count := GetPropList(Component.ClassInfo, tkAny, PropList);
      for I := 0 to Count -1 do
      begin
        PropInfo := PropList^[I];
        if not (PropInfo^.PropType^.Kind = tkMethod) then
        begin
          OutList.Add(PropInfo^.Name);
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  finally
    OutList.EndUpdate;
  end;
end;

end.


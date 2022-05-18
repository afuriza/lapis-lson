unit Lapis.TypeUtils;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Variants, StrUtils;

function HTMLColorToPascalHex(sColor : string): longword;

type
  TLSONVariant = variant;
  TLSONString = string;
  TLSONInteger = integer;
  TLSONHex = longword;

  TLSONVariantHelper = type helper for TLSONVariant
    function AsString: TLSONString;
    function AsInteger: TLSONInteger;
    function AsHex: TLSONHex;
  end;

implementation

function TLSONVariantHelper.AsHex: TLSONHex;
begin
  if Vartype(Self) = varLongWord then
    Result := Self;
end;

function TLSONVariantHelper.AsString: TLSONString;
begin
  if Vartype(Self) = varString then
    Result := Self;
end;

function TLSONVariantHelper.AsInteger: TLSONInteger;
begin
  if Vartype(Self) = varInteger then
    Result := Self;
end;

function HTMLColorToPascalHex(sColor : string): longword;
  function RGB(R, G, B : Byte) : longword;
  begin
    Result := R or (G shl 8) or (B Shl 16);
  end;
begin
  if sColor.IndexOf('#') <> -1 then
    sColor := sColor.Replace('#', '');
  Result :=
    RGB(
      { get red value }
      Hex2Dec( Copy( sColor, 1, 2 ) ),
      { get green value }
      Hex2Dec( Copy( sColor, 3, 2 ) ),
      { get blue value }
      Hex2Dec( Copy( sColor, 5, 2 ) )
    );
end;

end.


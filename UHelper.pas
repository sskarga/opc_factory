unit UHelper;

//------------------------------------------------------------------------------
                            interface
//-----------------------------------------------------------------------------
uses
  Windows, Messages, SysUtils, Classes, Controls, Variants;

function QualityToStr(Quality:integer):string;  
function VariantToString(Value: Variant): String;

function IntToBin(Value: Longint; Digits: Integer): string;
function IntToBool(Value: Longint; GetBit: Integer): Boolean;

function StrToHexStr(strIn: string): string;
function ByteToHexStr(byteIn: array of byte): string; Overload;
function ByteToHexStr(byteIn: array of byte; len: integer): string; Overload;
//------------------------------------------------------------------------------
                          implementation
//------------------------------------------------------------------------------

// HELPER
function QualityToStr(Quality:integer):string;
const
  QUALITY_BAD            = $00;
  QUALITY_UNCERTAIN      = $40;
  QUALITY_GOOD           = $C0;
begin
  case Quality of
    QUALITY_BAD: result:='bad';
    QUALITY_GOOD: result:='good';
  else
    result:='uncertain';
  end;
end;


function VariantToString(Value: Variant): String;
begin
  case TVarData(Value).VType of
    varSmallInt,
    varInteger   : Result := IntToStr(Value);
    varSingle,
    varDouble,
    varCurrency  : Result := FloatToStr(Value);
    varDate      : Result := FormatDateTime('dd/mm/yyyy', Value);
    varBoolean   : if Value then Result := 'True' else Result := 'False';
    varString    : Result := Value;
  else
    Result := '';
  end;
end;

function IntToBin(Value: Longint; Digits: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := Digits downto 0 do
    if Value and (1 shl i) <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

function IntToBool(Value: Longint; GetBit: Integer): Boolean;
begin
  Result := False;

  if Value and (1 shl GetBit) <> 0 then
    Result := True;
end;

function ByteToHexStr(byteIn: array of byte): string;
var
  strHex: string;
  y: Integer;
begin
  strHex:= '';

  for y:= Low(byteIn) to High(byteIn) do
    strHex := strHex + IntToHex(byteIn[y],2)+' ';

  Result:= chr(VK_TAB)+strHex;
end;

function ByteToHexStr(byteIn: array of byte; len: integer): string;
var
  strHex: string;
  y: Integer;
begin
  strHex:= '';

  for y:= Low(byteIn) to len-1 do
    strHex := strHex + IntToHex(byteIn[y],2)+' ';

  Result:= chr(VK_TAB)+strHex;
end;

function StrToHexStr(strIn: string): string;
var
  strHex: string;
  y: Integer;
begin
  strHex:= '';

  for y:= 1 to Length(strIn) do
    strHex := strHex + IntToHex(ord(strIn[y]),2)+' ';

  Result:= chr(VK_TAB)+strHex ;
end;



end.


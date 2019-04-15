unit uArrayAsPHP;

interface

//Ein paar Array-Funktionen wie sie in PHP ublich sind.
//Some Array-functions like in PHP.

// взял из недр интернета.
// перво-источник не известен.

type
  TString = widestring;
  ArrOfStr = array of TString;

function explode(sPart, sInput: TString; EmptyIfNone: boolean = false): ArrOfStr;
function implode(sPart: TString; arrInp: ArrOfStr): TString; overload;
function implode(sPart: TString; arrInp: array of Variant): TString; overload;

implementation

uses StrUtils;

function explode(sPart, sInput: TString; EmptyIfNone: boolean = false): ArrOfStr;
var
  P, Offs: integer;
begin
  Offs := 1;
  P := PosEx(sPart, sInput, Offs);
  if (P = 0) then
    if (EmptyIfNone = true) then
    begin
      SetLength(Result, 0);
      exit;
    end else
    begin
      SetLength(Result, 1);
      Result[0] := sInput;
      exit;
    end;

  SetLength(Result, 0);
  while P <> 0 do
   begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Copy(sInput, Offs, P - Offs);
    Offs := P + Length(sPart);
    P := PosEx(sPart, sInput, Offs);
  end;
  SetLength(Result, Length(Result) + 1);
  Result[Length(Result) - 1] := Copy(sInput, Offs, Length(sInput) - Offs + 1);
end;

function implode(sPart: TString; arrInp: ArrOfStr): TString;
var
   i: Integer;
begin
  if Length(arrInp) <= 1 then Result := arrInp[0]
  else
   begin
    for i := 0 to Length(arrInp) - 2 do Result := Result + arrInp[i] + sPart;
    Result := Result + arrInp[Length(arrInp) - 1];
  end;
end;

function implode(sPart: TString; arrInp: array of Variant): TString;
var
   i: Integer;
begin
  if Length(arrInp) <= 1 then Result := arrInp[0]
  else
   begin
    for i := 0 to Length(arrInp) - 2 do Result := Result + arrInp[i] + sPart;
    Result := Result + arrInp[Length(arrInp) - 1];
  end;
end;

{
procedure sort(arrInp: ArrOfStr);
var
   slTmp: TStringList;
   i: Integer;
begin
  slTmp := TStringList.Create;
  for i := 0 to Length(arrInp) - 1 do slTmp.Add(arrInp[i]);
  slTmp.Sort;
  for i := 0 to slTmp.Count - 1 do arrInp[i] := slTmp[i];
  slTmp.Free;
end;

procedure rsort(arrInp: ArrOfStr);
var
   slTmp: TStringList;
   i: Integer;
begin
  slTmp := TStringList.Create;
  for i := 0 to Length(arrInp) - 1 do slTmp.Add(arrInp[i]);
  slTmp.Sort;
  for i := 0 to slTmp.Count - 1 do arrInp[slTmp.Count - 1 - i] := slTmp[i];
  slTmp.Free;
end;
}

end.

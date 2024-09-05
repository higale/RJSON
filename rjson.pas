{
  TRJ - JSON simple read and write
  - v0.9.1
  - 2024-09-05 by gale
  - https://github.com/higale/RJSON
}
unit rjson;

interface

uses
  System.IOUtils, System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.Generics.Collections;

type
  TJObject = TJSONObject;
  TJArray = TJSONArray;
  TJValue = TJSONValue;
  TJString = TJSONString;
  TJNumber = TJSONNumber;
  TJBool = TJSONBool;
  TJNull = TJSONNull;

  TJVType = type of TJSONValue;

  { JSON root data interface }
  IRJRoot = interface
    function GetData: TJSONValue;
    procedure SetData(const AValue: TJSONValue);
    function ForceJV(AType: TJVType): TJSONValue;
    property Data: TJSONValue read GetData write SetData;
  end;

  { JSON root data interface class }
  TRJRoot = class(TInterfacedObject, IRJRoot)
  private
    FData: TJSONValue;
    function GetData: TJSONValue;
    procedure SetData(const AValue: TJSONValue);
    function ForceJV(AType: TJVType): TJSONValue;
  public
    constructor Create; overload;
    constructor Create(const AValue: TJSONValue); overload;
    destructor Destroy; override;
  end;

  TRJEnumerator = class;

  { TRJ is equivalent to TRecordJSON }
  TRJ = record
  private
    FRoot: IRJRoot;
    FPath: string;
    function LinkPath(const ALeft, ARight: string): string;
    function GetJSONValue: TJSONValue; inline;
    function GetItems(const APath: string): TRJ; overload;
    function GetItems(AIndex: Integer): TRJ; overload; inline;
    function GetPairs(AIndex: Integer): TRJ;
    procedure SetValue(const [ref] AValue: TRJ);
    procedure SetItems(const APath: string; const [ref] AValue: TRJ); overload;
    procedure SetItems(AIndex: Integer; const [ref] AValue: TRJ); overload; inline;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetKey: string;
  public
    function GetEnumerator(): TRJEnumerator;
    class operator Initialize(out Dest: TRJ);
    class operator Finalize(var Dest: TRJ);
    class operator Assign(var Dest: TRJ; const [ref] Src: TRJ);
    class operator Implicit(const Value: string): TRJ;
    class operator Implicit(const [ref] Value: TRJ): string;
    class operator Implicit(Value: Integer): TRJ;
    class operator Implicit(const [ref] Value: TRJ): Integer;
    class operator Implicit(Value: Int64): TRJ;
    class operator Implicit(const [ref] Value: TRJ): Int64;
    class operator Implicit(Value: Extended): TRJ;
    class operator Implicit(const [ref] Value: TRJ): Extended;
    class operator Implicit(Value: boolean): TRJ;
    class operator Implicit(const [ref] Value: TRJ): boolean;
    class operator Implicit(const Value: TJSONValue): TRJ;

    function ToStr(const ADefault: string = ''): string;
    function ToInt(ADefault: Integer = 0): Integer;
    function ToInt64(ADefault: Int64 = 0): Int64;
    function ToFloat(ADefault: Extended = 0.0): Extended;
    function ToBool(ADefault: boolean = False): boolean;
    property Items[const APath: string]: TRJ read GetItems write SetItems; default;
    property Items[AIndex: Integer]: TRJ read GetItems write SetItems; default;
    property Pairs[AIndex: Integer]: TRJ read GetPairs;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property Key: string read GetKey;
    property Root: IRJRoot read FRoot;
    property Path: string read FPath;
    function RootIs<T: TJSONValue>: boolean;
    function ValueIs<T: TJSONValue>: boolean;
    property JSONValue: TJSONValue read GetJSONValue;
    function CloneJSONValue: TJSONValue;
    procedure Reset;
    function Format(Indentation: Integer = 4): string;
    procedure ParseJSONValue(const AData: string; AUseBool: boolean = False; ARaiseExc: boolean = False);
    procedure LoadFromFile(const AFileName: string; AUseBool: boolean = False; ARaiseExc: boolean = False);
    procedure SaveToFile(const AFileName: string; AIndentation: Integer = 4; AWriteBOM: boolean = False; ATrailingLineBreak: boolean = False);
  end;

  { Iterators }
  TRJEnumerator = class
  private
    FPData: ^TRJ;
    FIndex: Integer;
    function GetCurrent: TRJ;
  public
    constructor Create(const [ref] AData: TRJ);
    function MoveNext: boolean;
    property Current: TRJ read GetCurrent;
  end;

implementation

{ ============================================================================ }
{ TRJRoot }

constructor TRJRoot.Create;
begin
  inherited;
  FData := nil;
end;

constructor TRJRoot.Create(const AValue: TJSONValue);
begin
  inherited Create;
  FData := AValue;
end;

destructor TRJRoot.Destroy;
begin
  FData.Free;
  inherited;
end;

function TRJRoot.GetData: TJSONValue;
begin
  Result := FData;
end;

procedure TRJRoot.SetData(const AValue: TJSONValue);
begin
  FData := AValue;
end;

function TRJRoot.ForceJV(AType: TJVType): TJSONValue;
begin
  if not(FData is AType) then
  begin
    FData.Free;
    FData := AType.Create;
  end;
  Result := FData;
end;

{ TRJRoot }
{ ============================================================================ }
{ TJSONValueHelper }

type
  TJSONValueHelper = class helper for TJSONValue
  private
    procedure ObjSetItem(const AName: string; const AValue: TJSONValue);
    procedure ArrFill<T: TJSONValue>(ACount: Integer);
    procedure ArrInsert(const AIndex: Integer; const AValue: TJSONValue);
    procedure ArrSetItem(AIndex: Integer; const AValue: TJSONValue);
    function ToType<T>(ADefault: T): T;
    function GetOrCreate<T: TJSONValue>(AName: string): T;
    procedure SetValue(const APath: string; const AValue: TJSONValue);
  end;

procedure TJSONValueHelper.ObjSetItem(const AName: string; const AValue: TJSONValue);
var
  pairTmp: TJSONPair;
begin
  pairTmp := TJSONObject(self).Get(AName);
  if pairTmp = nil then
    TJSONObject(self).AddPair(AName, AValue)
  else
    pairTmp.JSONValue := AValue;
end;

procedure TJSONValueHelper.ArrFill<T>(ACount: Integer);
begin
  for var j := TJSONArray(self).Count to ACount do
    TJSONArray(self).AddElement(T.Create);
end;

procedure TJSONValueHelper.ArrInsert(const AIndex: Integer; const AValue: TJSONValue);
begin
  TJSONArray(self).AddElement(AValue);
  for var i := AIndex to TJSONArray(self).Count - 2 do
    TJSONArray(self).AddElement(TJSONArray(self).Remove(AIndex));
end;

procedure TJSONValueHelper.ArrSetItem(AIndex: Integer; const AValue: TJSONValue);
begin
  ArrFill<TJSONNull>(AIndex - 1);
  if AIndex <= TJSONArray(self).Count - 1 then
    TJSONArray(self).Remove(AIndex).Free;
  ArrInsert(AIndex, AValue);
end;

procedure TJSONValueHelper.SetValue(const APath: string; const AValue: TJSONValue);
var
  LParser: TJSONPathParser;
  preName: string;
  jv: TJSONValue;
begin
  if APath.IsEmpty then
    raise Exception.Create('TJSONValueHelper.SetValue: path cannot be empty');

  jv := self;
  LParser := TJSONPathParser.Create(APath);
  LParser.NextToken;
  while true do
  begin
    preName := LParser.TokenName;
    LParser.NextToken;
    case LParser.Token of
      TJSONPathParser.TToken.Name:
        jv := jv.GetOrCreate<TJSONObject>(preName);
      TJSONPathParser.TToken.ArrayIndex:
        jv := jv.GetOrCreate<TJSONArray>(preName);
      TJSONPathParser.TToken.Eof:
        begin
          if jv is TJSONObject then
            jv.ObjSetItem(preName, AValue)
          else
            jv.ArrSetItem(preName.ToInteger, AValue);
          break;
        end;
    else
      raise Exception.Create('TJSONValueHelper.SetValue, LParser.Token Error!');
    end;
  end;
end;

function TJSONValueHelper.ToType<T>(ADefault: T): T;
begin
  if self = nil then
    Exit(ADefault);
  try
    Result := AsType<T>;
  except
    Result := ADefault;
  end;
end;

function TJSONValueHelper.GetOrCreate<T>(AName: string): T;
begin
  if self is TJSONObject then
  begin
    Result := T(TJSONObject(self).GetValue(AName));
    if not(Result is T) then
    begin
      Result := T.Create;
      ObjSetItem(AName, Result);
    end;
  end
  else if self is TJSONArray then
  begin
    ArrFill<TJSONNull>(AName.ToInteger);
    Result := T(TJSONArray(self).Items[AName.ToInteger]);
    if not(Result is T) then
    begin
      Result := T.Create;
      ArrSetItem(AName.ToInteger, Result);
    end;
  end
  else
  begin
    raise Exception.Create('GetOrCreate<T> Error, self must be TJO or TJA');
  end;
end;

{ TJSONValueHelper }
{ ============================================================================ }
{ TRJEnumerator }

constructor TRJEnumerator.Create(const [ref] AData: TRJ);
begin
  inherited Create;
  FPData := @AData;
  FIndex := -1;
end;

function TRJEnumerator.GetCurrent: TRJ;
var
  jvTmp: TJSONValue;
begin
  Result.Reset;
  Result.FRoot := FPData^.FRoot;
  jvTmp := FPData^.GetJSONValue;
  if jvTmp is TJSONObject then
  begin
    if FPData^.FPath = '' then
      Result.FPath := TJSONObject(jvTmp).Pairs[FIndex].JsonString.Value
    else
      Result.FPath := FPData^.FPath + '.' + TJSONObject(jvTmp).Pairs[FIndex].JsonString.Value;
  end
  else if jvTmp is TJSONArray then
  begin
    Result.FPath := FPData^.FPath + '[' + FIndex.ToString + ']';
  end;
end;

function TRJEnumerator.MoveNext: boolean;
begin
  Inc(FIndex);
  Exit(FIndex < FPData^.Count)
end;

{ TRJEnumerator }
{ ============================================================================ }
{ TRJ }

function TRJ.GetEnumerator(): TRJEnumerator;
begin
  Result := TRJEnumerator.Create(self);
end;

class operator TRJ.Initialize(out Dest: TRJ);
begin
  Dest.FRoot := TRJRoot.Create;
  Dest.FPath := '';
end;

class operator TRJ.Finalize(var Dest: TRJ);
begin
  Dest.FRoot := nil;
end;

function TRJ.LinkPath(const ALeft, ARight: string): string;
begin
  if ALeft.IsEmpty then
    Result := ARight
  else if ARight.IsEmpty then
    Result := ALeft
  else if ARight.StartsWith('[') then
    Result := ALeft + ARight
  else
    Result := ALeft + '.' + ARight;
end;

function TRJ.GetJSONValue: TJSONValue;
begin
  Result := FRoot.Data.FindValue(FPath);
end;

function TRJ.CloneJSONValue: TJSONValue;
begin
  Result := GetJSONValue;
  if Result <> nil then
    Result := Result.Clone as TJSONValue
  else
    Result := TJSONNull.Create;
end;

class operator TRJ.Assign(var Dest: TRJ; const [ref] Src: TRJ);
begin
  if Dest.FPath.IsEmpty then
  begin
    Dest.FRoot := Src.FRoot;
    Dest.FPath := Src.FPath;
  end
  else
  begin
    Dest.SetValue(Src);
  end;
end;

class operator TRJ.Implicit(const Value: string): TRJ;
begin
  Result.FRoot.Data := TJSONString.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): string;
begin
  Result := Value.ToStr('');
end;

class operator TRJ.Implicit(Value: Integer): TRJ;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Integer;
begin
  Result := Value.ToInt(0);
end;

class operator TRJ.Implicit(Value: Int64): TRJ;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Int64;
begin
  Result := Value.ToInt64(0);
end;

class operator TRJ.Implicit(Value: Extended): TRJ;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Extended;
begin
  Result := Value.ToFloat(0.0);
end;

class operator TRJ.Implicit(Value: boolean): TRJ;
begin
  Result.FRoot.Data := TJSONBool.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): boolean;
begin
  Result := Value.ToBool(False);
end;

class operator TRJ.Implicit(const Value: TJSONValue): TRJ;
begin
  Result.FRoot.Data := Value;
end;

function TRJ.ToStr(const ADefault: string): string;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<string>(ADefault);
end;

function TRJ.ToInt(ADefault: Integer = 0): Integer;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Integer>(ADefault);
end;

function TRJ.ToInt64(ADefault: Int64 = 0): Int64;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Int64>(ADefault);
end;

function TRJ.ToFloat(ADefault: Extended = 0.0): Extended;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Extended>(ADefault);
end;

function TRJ.ToBool(ADefault: boolean = False): boolean;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<boolean>(ADefault);
end;

function TRJ.GetItems(const APath: string): TRJ;
begin
  Result.FRoot := FRoot;
  Result.FPath := LinkPath(FPath, APath);
end;

function TRJ.GetItems(AIndex: Integer): TRJ;
begin
  Result := GetItems('[' + AIndex.ToString + ']');
end;

function TRJ.GetPairs(AIndex: Integer): TRJ;
var
  jvTmp: TJSONValue;
begin
  jvTmp := GetJSONValue;
  if (jvTmp is TJSONObject) then
    Result := GetItems(TJSONObject(jvTmp).Pairs[AIndex].JsonString.Value);
end;

procedure TRJ.SetValue(const [ref] AValue: TRJ);
var
  LValue: TJSONValue;
begin
  if FPath.IsEmpty then
    raise Exception.Create(' TRJ.SetValue: Path is empty');

  LValue := AValue.CloneJSONValue;
  try
    if FPath.StartsWith('[') then
      FRoot.ForceJV(TJSONArray).SetValue(FPath, LValue)
    else
      FRoot.ForceJV(TJSONObject).SetValue(FPath, LValue);
  except
    on E: Exception do
    begin
      LValue.Free;
      raise Exception.Create(E.message);
    end;
  end;

end;

procedure TRJ.SetItems(const APath: string; const [ref] AValue: TRJ);
var
  tmp: TRJ;
begin
  tmp.FRoot := FRoot;
  tmp.FPath := LinkPath(FPath, APath);
  tmp.SetValue(AValue)
end;

procedure TRJ.SetItems(AIndex: Integer; const [ref] AValue: TRJ);
begin
  SetItems('[' + AIndex.ToString + ']', AValue);
end;

function TRJ.GetCount: Integer;
var
  jvTemp: TJSONValue;
begin
  jvTemp := GetJSONValue;
  if jvTemp is TJSONArray then
    Result := TJSONArray(jvTemp).Count
  else if jvTemp is TJSONObject then
    Result := TJSONObject(jvTemp).Count
  else
    Result := 0;
end;

function TRJ.GetIndex: Integer;
var
  strTmp: string;
begin
  Result := -1;
  strTmp := FPath.Substring(FPath.LastIndexOf('[') + 1);
  if strTmp.EndsWith(']') then
    Result := StrToIntDef(strTmp.TrimRight([']']), -1);
end;

function TRJ.GetKey: string;
begin
  Result := FPath.Substring(FPath.LastIndexOf('.') + 1);
  if Result.EndsWith(']') then
    Result := '';
end;

function TRJ.RootIs<T>: boolean;
begin
  Result := FRoot.Data is T;
end;

function TRJ.ValueIs<T>: boolean;
begin
  Result := GetJSONValue is T;
end;

procedure TRJ.Reset;
begin
  FRoot := TRJRoot.Create;
  FPath := '';
end;

function TRJ.Format(Indentation: Integer): string;
var
  LValue: TJSONValue;
begin
  Result := '';
  LValue := GetJSONValue;
  if LValue <> nil then
  begin
    if Indentation > 0 then
      Result := LValue.Format(Indentation)
    else
      Result := LValue.ToString;
  end;
end;

procedure TRJ.ParseJSONValue(const AData: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  Reset;
  FRoot.Data := TJSONValue.ParseJSONValue(AData, AUseBool, ARaiseExc);
end;

procedure TRJ.LoadFromFile(const AFileName: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  ParseJSONValue(TFile.ReadAllText(AFileName, TEncoding.UTF8), AUseBool, ARaiseExc);
end;

procedure TRJ.SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: boolean; ATrailingLineBreak: boolean);
var
  strs: TStrings;
begin
  strs := TStringList.Create;
  try
    strs.WriteBOM := AWriteBOM;
    strs.TrailingLineBreak := ATrailingLineBreak;
    strs.Text := Format(AIndentation);
    strs.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    strs.Free;
  end;
end;

{ TRJ }
{ ============================================================================ }

end.

{
  TRJSON - JSON Simple Read and Write
  - v0.9.7
  - 2024-09-14 by gale
  - https://github.com/higale/RJSON
}
unit rjson;

interface

uses
  System.IOUtils, System.Classes, System.SysUtils, System.JSON, System.Generics.Collections;

type
  TJObject = TJSONObject;
  TJArray = TJSONArray;
  TJValue = TJSONValue;
  TJString = TJSONString;
  TJNumber = TJSONNumber;
  TJBool = TJSONBool;
  TJTrue = TJSONTrue;
  TJFalse = TJSONFalse;
  TJNull = TJSONNull;
  TJVType = type of TJValue;

  IRJRoot = interface
    ['{486F1FA6-2CDD-4124-98C5-CE7C398B7143}']
    function GetData: TJValue;
    procedure SetData(const AValue: TJValue);
    function ForceData(AType: TJVType): TJValue;
    property Data: TJValue read GetData write SetData;
  end;

  TRJSONRoot = class(TInterfacedObject, IRJRoot)
  private
    FData: TJValue;
    function GetData: TJValue;
    procedure SetData(const AValue: TJValue);
    function ForceData(AType: TJVType): TJValue;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRPath = record
  private
    FData: string;
  public
    class operator Implicit(const Value: string): TRPath;
    class operator Implicit(Value: Integer): TRPath;
    class operator Implicit(const [ref] Value: TRPath): string;
  end;

  TRJSONEnumerator = class;

  TRJSON = record
  private
    FIRoot: IRJRoot;
    FPath: string;
    function GetRootRefCount: Integer;
    function ForceRootJValue(const APath: string): TJValue;
    function LinkPath(const ALeft, ARight: string): string;
    function GetJValue: TJValue; inline;
    function GetItems(const APath: TRPath): TRJSON;
    function GetPairs(AIndex: Integer): TRJSON;
    procedure SetValue(const [ref] AValue: TRJSON);
    procedure SetItems(const APath: TRPath; const [ref] AValue: TRJSON);
    function GetS(const APath: TRPath): string; overload;
    procedure SetS(const APath: TRPath; AValue: string); overload;
    function GetI(const APath: TRPath): Integer; overload;
    procedure SetI(const APath: TRPath; AValue: Integer); overload;
    function GetI64(const APath: TRPath): Int64; overload;
    procedure SetI64(const APath: TRPath; AValue: Int64); overload;
    function GetF(const APath: TRPath): Extended; overload;
    procedure SetF(const APath: TRPath; AValue: Extended); overload;
    function GetB(const APath: TRPath): Boolean; overload;
    procedure SetB(const APath: TRPath; AValue: Boolean); overload;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetKey: string;
    function GetRoot: TRJSON;
  public
    function GetEnumerator(): TRJSONEnumerator;
    class operator Initialize(out Dest: TRJSON);
    class operator Finalize(var Dest: TRJSON);
    class operator Assign(var Dest: TRJSON; const [ref] Src: TRJSON);
    class operator Implicit(const Value: string): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): string;
    class operator Implicit(Value: Integer): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): Integer;
    class operator Implicit(Value: Int64): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): Int64;
    class operator Implicit(Value: Extended): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): Extended;
    class operator Implicit(Value: Boolean): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): Boolean;
    class operator Implicit(const Value: TJValue): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): TJValue;
    function ToStr(const ADefault: string = ''): string;
    function ToInt(ADefault: Integer = 0): Integer;
    function ToInt64(ADefault: Int64 = 0): Int64;
    function ToFloat(ADefault: Extended = 0.0): Extended;
    function ToBool(ADefault: Boolean = False): Boolean;

    property Items[const APath: TRPath]: TRJSON read GetItems write SetItems; default;
    property S[const APath: TRPath]: string read GetS write SetS;
    property I[const APath: TRPath]: Integer read GetI write SetI;
    property I64[const APath: TRPath]: Int64 read GetI64 write SetI64;
    property F[const APath: TRPath]: Extended read GetF write SetF;
    property B[const APath: TRPath]: Boolean read GetB write SetB;
    property Pairs[AIndex: Integer]: TRJSON read GetPairs;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property Key: string read GetKey;
    property RootRefCount: Integer read GetRootRefCount;
    property Root: TRJSON read GetRoot;
    property Path: string read FPath;
    property JValue: TJValue read GetJValue;

    function CloneJValue: TJValue;
    function IsRoot: Boolean; inline;
    function RootIsJObject: Boolean; inline;
    function RootIsJArray: Boolean; inline;
    function IsJObject: Boolean;
    function IsJArray: Boolean;
    function IsJString: Boolean;
    function IsJNumber: Boolean;
    function IsJBool: Boolean;
    function IsJNull: Boolean;
    function IsNil: Boolean;
    procedure Reset;
    function ToString: string;
    function ToJSON(AEncodeBelow32: Boolean = true; AEncodeAbove127: Boolean = true): string;
    function Format(AIndentation: Integer = 4): string;
    function ParseJValue(const AData: string; AUseBool: Boolean = False; ARaiseExc: Boolean = False): Boolean;
    function LoadFromFile(const AFileName: string; AUseBool: Boolean = False; ARaiseExc: Boolean = False): Boolean;
    procedure SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: Boolean = False); overload;
    procedure SaveToFile(const AFileName: string; AEncodeBelow32: Boolean = true; AEncodeAbove127: Boolean = true; AWriteBOM: Boolean = False); overload;
  end;

  { Iterators }
  TRJSONEnumerator = class
  private
    FPData: ^TRJSON;
    FIndex: Integer;
    function GetCurrent: TRJSON;
  public
    constructor Create(const [ref] AData: TRJSON);
    function MoveNext: Boolean;
    property Current: TRJSON read GetCurrent;
  end;

implementation

{ ============================================================================ }
{ TRJSONRoot }

constructor TRJSONRoot.Create;
begin
  inherited;
  FData := nil;
end;

destructor TRJSONRoot.Destroy;
begin
  FData.Free;
  inherited;
end;

function TRJSONRoot.GetData: TJValue;
begin
  Result := FData;
end;

procedure TRJSONRoot.SetData(const AValue: TJValue);
begin
  FData := AValue;
end;

function TRJSONRoot.ForceData(AType: TJVType): TJValue;
begin
  if not(FData is AType) then
  begin
    FData.Free;
    FData := AType.Create;
  end;
  Result := FData;
end;

{ TRJSONRoot }
{ ============================================================================ }
{ TJValueHelper }

type
  TJValueHelper = class helper for TJValue
  private
    procedure ObjSetItem(const AName: string; const AValue: TJValue);
    procedure ArrFill<T: TJValue>(ACount: Integer);
    procedure ArrInsert(const AIndex: Integer; const AValue: TJValue);
    procedure ArrSetItem(AIndex: Integer; const AValue: TJValue);
    function ToType<T>(ADefault: T): T;
    function GetOrCreate<T: TJValue>(AName: string): T;
    procedure SetValue(const APath: string; const AValue: TJValue);
    procedure TrySetValue(const APath: string; const AValue: TJValue);
  end;

procedure TJValueHelper.ObjSetItem(const AName: string; const AValue: TJValue);
var
  pairTmp: TJSONPair;
begin
  pairTmp := TJObject(self).Get(AName);
  if pairTmp = nil then
    TJObject(self).AddPair(AName, AValue)
  else
    pairTmp.JSONValue := AValue;
end;

procedure TJValueHelper.ArrFill<T>(ACount: Integer);
begin
  for var j := TJArray(self).Count to ACount do
    TJArray(self).AddElement(T.Create);
end;

procedure TJValueHelper.ArrInsert(const AIndex: Integer; const AValue: TJValue);
begin
  TJArray(self).AddElement(AValue);
  for var I := AIndex to TJArray(self).Count - 2 do
    TJArray(self).AddElement(TJArray(self).Remove(AIndex));
end;

procedure TJValueHelper.ArrSetItem(AIndex: Integer; const AValue: TJValue);
begin
  ArrFill<TJNull>(AIndex - 1);
  if AIndex <= TJArray(self).Count - 1 then
    TJArray(self).Remove(AIndex).Free;
  ArrInsert(AIndex, AValue);
end;

procedure TJValueHelper.SetValue(const APath: string; const AValue: TJValue);
var
  LParser: TJSONPathParser;
  preName: string;
  jv: TJValue;
begin
  if APath.IsEmpty then
    raise Exception.Create('TJValueHelper.SetValue: path cannot be empty');

  jv := self;
  LParser := TJSONPathParser.Create(APath);
  LParser.NextToken;
  while true do
  begin
    preName := LParser.TokenName;
    LParser.NextToken;
    case LParser.Token of
      TJSONPathParser.TToken.Name:
        jv := jv.GetOrCreate<TJObject>(preName);
      TJSONPathParser.TToken.ArrayIndex:
        jv := jv.GetOrCreate<TJArray>(preName);
      TJSONPathParser.TToken.Eof:
        begin
          if jv is TJObject then
            jv.ObjSetItem(preName, AValue)
          else
            jv.ArrSetItem(preName.ToInteger, AValue);
          break;
        end;
    else
      raise Exception.Create('TJValueHelper.SetValue, LParser.Token Error!');
    end;
  end;
end;

procedure TJValueHelper.TrySetValue(const APath: string; const AValue: TJValue);
begin
  try
    SetValue(APath, AValue);
  except
    on E: Exception do
    begin
      AValue.Free;
      raise Exception.Create(E.Message);
    end;
  end;

end;

function TJValueHelper.ToType<T>(ADefault: T): T;
begin
  if self = nil then
    Exit(ADefault);
  try
    Result := AsType<T>;
  except
    Result := ADefault;
  end;
end;

function TJValueHelper.GetOrCreate<T>(AName: string): T;
begin
  if self is TJObject then
  begin
    Result := T(TJObject(self).GetValue(AName));
    if not(Result is T) then
    begin
      Result := T.Create;
      ObjSetItem(AName, Result);
    end;
  end
  else if self is TJArray then
  begin
    ArrFill<TJNull>(AName.ToInteger);
    Result := T(TJArray(self).Items[AName.ToInteger]);
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

{ TJValueHelper }
{ ============================================================================ }
{ TRPath }

class operator TRPath.Implicit(const Value: string): TRPath;
begin
  Result.FData := Value;
end;

class operator TRPath.Implicit(Value: Integer): TRPath;
begin
  Result.FData := '[' + Value.ToString + ']';
end;

class operator TRPath.Implicit(const [ref] Value: TRPath): string;
begin
  Result := Value.FData;
end;

{ TRPath }
{ ============================================================================ }
{ TRJSONEnumerator }

constructor TRJSONEnumerator.Create(const [ref] AData: TRJSON);
begin
  inherited Create;
  FPData := @AData;
  FIndex := -1;
end;

function TRJSONEnumerator.GetCurrent: TRJSON;
var
  jvTmp: TJValue;
begin
  Result.Reset;
  Result.FIRoot := FPData^.FIRoot;
  jvTmp := FPData^.GetJValue;
  if jvTmp is TJObject then
  begin
    if FPData^.FPath = '' then
      Result.FPath := TJObject(jvTmp).Pairs[FIndex].JsonString.Value
    else
      Result.FPath := FPData^.FPath + '.' + TJObject(jvTmp).Pairs[FIndex].JsonString.Value;
  end
  else if jvTmp is TJArray then
  begin
    Result.FPath := FPData^.FPath + '[' + FIndex.ToString + ']';
  end;
end;

function TRJSONEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Exit(FIndex < FPData^.Count)
end;

{ TRJSONEnumerator }
{ ============================================================================ }
{ TRJSON }

function TRJSON.GetEnumerator(): TRJSONEnumerator;
begin
  Result := TRJSONEnumerator.Create(self);
end;

class operator TRJSON.Initialize(out Dest: TRJSON);
begin
  Dest.FIRoot := TRJSONRoot.Create;
  Dest.FPath := '';
end;

class operator TRJSON.Finalize(var Dest: TRJSON);
begin
  Dest.FIRoot := nil;
end;

function TRJSON.GetRootRefCount: Integer;
begin
  Result := (FIRoot as TRJSONRoot).RefCount;
end;

function TRJSON.ForceRootJValue(const APath: string): TJValue;
begin
  if APath.StartsWith('[') then
    Result := FIRoot.ForceData(TJArray)
  else
    Result := FIRoot.ForceData(TJObject);
end;

function TRJSON.LinkPath(const ALeft, ARight: string): string;
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

function TRJSON.GetJValue: TJValue;
begin
  Result := FIRoot.Data.FindValue(FPath);
end;

function TRJSON.CloneJValue: TJValue;
begin
  Result := GetJValue;
  if Result <> nil then
    Result := Result.Clone as TJValue
  else
    Result := TJNull.Create;
end;

class operator TRJSON.Assign(var Dest: TRJSON; const [ref] Src: TRJSON);
begin
  if Dest.FPath.IsEmpty then
  begin
    Dest.FIRoot := Src.FIRoot;
    Dest.FPath := Src.FPath;
  end
  else
  begin
    Dest.SetValue(Src);
  end;
end;

class operator TRJSON.Implicit(const Value: string): TRJSON;
begin
  Result.FIRoot.Data := TJString.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): string;
begin
  Result := Value.ToStr('');
end;

class operator TRJSON.Implicit(Value: Integer): TRJSON;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Integer;
begin
  Result := Value.ToInt(0);
end;

class operator TRJSON.Implicit(Value: Int64): TRJSON;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Int64;
begin
  Result := Value.ToInt64(0);
end;

class operator TRJSON.Implicit(Value: Extended): TRJSON;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Extended;
begin
  Result := Value.ToFloat(0.0);
end;

class operator TRJSON.Implicit(Value: Boolean): TRJSON;
begin
  Result.FIRoot.Data := TJBool.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Boolean;
begin
  Result := Value.ToBool(False);
end;

class operator TRJSON.Implicit(const Value: TJValue): TRJSON;
begin
  Result.FIRoot.Data := Value;
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): TJValue;
begin
  Result := Value.GetJValue;
end;

function TRJSON.ToStr(const ADefault: string): string;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<string>(ADefault);
end;

function TRJSON.ToInt(ADefault: Integer = 0): Integer;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Integer>(ADefault);
end;

function TRJSON.ToInt64(ADefault: Int64 = 0): Int64;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Int64>(ADefault);
end;

function TRJSON.ToFloat(ADefault: Extended = 0.0): Extended;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Extended>(ADefault);
end;

function TRJSON.ToBool(ADefault: Boolean = False): Boolean;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Boolean>(ADefault);
end;

function TRJSON.GetItems(const APath: TRPath): TRJSON;
begin
  Result.FIRoot := FIRoot;
  Result.FPath := LinkPath(FPath, APath);
end;

function TRJSON.GetPairs(AIndex: Integer): TRJSON;
var
  jvTmp: TJValue;
begin
  jvTmp := GetJValue;
  if (jvTmp is TJObject) then
    Result := GetItems(TJObject(jvTmp).Pairs[AIndex].JsonString.Value);
end;

procedure TRJSON.SetValue(const [ref] AValue: TRJSON);
var
  LValue: TJValue;
begin
{$IFDEF DEBUG}
  if FPath.IsEmpty then
    raise Exception.Create(' TRJSON.SetValue: Path is empty');
{$ENDIF}
  LValue := AValue.CloneJValue;
  try
    ForceRootJValue(FPath).SetValue(FPath, LValue);
  except
    on E: Exception do
    begin
      LValue.Free;
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TRJSON.SetItems(const APath: TRPath; const [ref] AValue: TRJSON);
var
  tmp: TRJSON;
begin
  tmp.FIRoot := FIRoot;
  tmp.FPath := LinkPath(FPath, APath);
  tmp.SetValue(AValue)
end;

function TRJSON.GetS(const APath: TRPath): string;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<string>('');
end;

procedure TRJSON.SetS(const APath: TRPath; AValue: string);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJString.Create(AValue));
end;

function TRJSON.GetI(const APath: TRPath): Integer;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Integer>(0);
end;

procedure TRJSON.SetI(const APath: TRPath; AValue: Integer);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJSON.GetI64(const APath: TRPath): Int64;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Int64>(0);
end;

procedure TRJSON.SetI64(const APath: TRPath; AValue: Int64);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJSON.GetF(const APath: TRPath): Extended;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Extended>(0.0);
end;

procedure TRJSON.SetF(const APath: TRPath; AValue: Extended);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJSON.GetB(const APath: TRPath): Boolean;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Boolean>(False);
end;

procedure TRJSON.SetB(const APath: TRPath; AValue: Boolean);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJBool.Create(AValue));
end;

function TRJSON.GetCount: Integer;
var
  jvTemp: TJValue;
begin
  jvTemp := GetJValue;
  if jvTemp is TJArray then
    Result := TJArray(jvTemp).Count
  else if jvTemp is TJObject then
    Result := TJObject(jvTemp).Count
  else
    Result := 0;
end;

function TRJSON.GetIndex: Integer;
var
  strTmp: string;
begin
  Result := -1;
  strTmp := FPath.Substring(FPath.LastIndexOf('[') + 1);
  if strTmp.EndsWith(']') then
    Result := StrToIntDef(strTmp.TrimRight([']']), -1);
end;

function TRJSON.GetKey: string;
begin
  Result := FPath.Substring(FPath.LastIndexOf('.') + 1);
  if Result.EndsWith(']') then
    Result := '';
end;

function TRJSON.GetRoot: TRJSON;
begin
  Result.FIRoot := FIRoot;
  // Result.FPath := '';
end;

function TRJSON.IsRoot: Boolean;
begin
  Result := FPath.IsEmpty;
end;

function TRJSON.RootIsJObject: Boolean;
begin
  Result := FIRoot.Data is TJObject;
end;

function TRJSON.RootIsJArray: Boolean;
begin
  Result := FIRoot.Data is TJArray;
end;

function TRJSON.IsJObject: Boolean;
begin
  Result := GetJValue is TJObject;
end;

function TRJSON.IsJArray: Boolean;
begin
  Result := GetJValue is TJArray;
end;

function TRJSON.IsJString: Boolean;
begin
  Result := GetJValue is TJString;
end;

function TRJSON.IsJNumber: Boolean;
begin
  Result := GetJValue is TJNumber;
end;

function TRJSON.IsJBool: Boolean;
begin
  Result := GetJValue is TJBool;
end;

function TRJSON.IsJNull: Boolean;
begin
  Result := GetJValue is TJNull;
end;

function TRJSON.IsNil: Boolean;
begin
  Result := GetJValue = nil;
end;

procedure TRJSON.Reset;
begin
  FIRoot := TRJSONRoot.Create;
  FPath := '';
end;

function TRJSON.ToJSON(AEncodeBelow32: Boolean = true; AEncodeAbove127: Boolean = true): string;
var
  LValue: TJValue;
  Options: TJSONAncestor.TJSONOutputOptions;
begin
  Result := '';
  LValue := GetJValue;
  if LValue <> nil then
  begin
    Options := [];
    if AEncodeBelow32 then
      Include(Options, TJSONAncestor.TJSONOutputOption.EncodeBelow32);
    if AEncodeAbove127 then
      Include(Options, TJSONAncestor.TJSONOutputOption.EncodeAbove127);
    Result := LValue.ToJSON(Options);
  end;
end;

function TRJSON.ToString: string;
begin
  Result := ToJSON(False, False);
end;

function TRJSON.Format(AIndentation: Integer): string;
var
  LValue: TJValue;
begin
  Result := '';
  LValue := GetJValue;
  if LValue <> nil then
    Result := LValue.Format(AIndentation)
end;

function TRJSON.ParseJValue(const AData: string; AUseBool: Boolean; ARaiseExc: Boolean): Boolean;
begin
  Reset;
  FIRoot.Data := TJValue.ParseJSONValue(AData, AUseBool, ARaiseExc);
  Result := FIRoot.Data <> nil;
end;

function TRJSON.LoadFromFile(const AFileName: string; AUseBool: Boolean; ARaiseExc: Boolean): Boolean;
begin
  Result := False;
  Reset;
  try
    FIRoot.Data := TJValue.ParseJSONValue(TFile.ReadAllText(AFileName, TEncoding.UTF8), AUseBool, ARaiseExc);
    Result := FIRoot.Data <> nil;
  except
    on E: Exception do
    begin
      if ARaiseExc then
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TRJSON.SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: Boolean);
var
  strs: TStrings;
begin
  strs := TStringList.Create;
  try
    strs.WriteBOM := AWriteBOM;
    strs.Text := Format(AIndentation);
    strs.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    strs.Free;
  end;
end;

procedure TRJSON.SaveToFile(const AFileName: string; AEncodeBelow32: Boolean = true; AEncodeAbove127: Boolean = true; AWriteBOM: Boolean = False);
var
  strs: TStrings;
begin
  strs := TStringList.Create;
  try
    strs.WriteBOM := AWriteBOM;
    strs.Text := ToJSON(AEncodeBelow32, AEncodeAbove127);
    strs.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    strs.Free;
  end;
end;

{ TRJSON }
{ ============================================================================ }

end.

{
  TRJ - JSON Simple Read and Write
  - v0.9.4
  - 2024-09-09 by gale
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
  TJNull = TJSONNull;
  TJVType = type of TJValue;

  IRJRoot = interface
    ['{486F1FA6-2CDD-4124-98C5-CE7C398B7143}']
    function GetData: TJValue;
    procedure SetData(const AValue: TJValue);
    function ForceData(AType: TJVType): TJValue;
    property Data: TJValue read GetData write SetData;
  end;

  TRJRoot = class(TInterfacedObject, IRJRoot)
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

  TRJEnumerator = class;

  TRJ = record
  private
    FIRoot: IRJRoot;
    FPath: string;
    function GetRootRefCount: Integer;
    function ForceRootJValue(const APath: string): TJValue;
    function LinkPath(const ALeft, ARight: string): string;
    function GeTJValue: TJValue; inline;
    function GetItems(const APath: TRPath): TRJ;
    function GetPairs(AIndex: Integer): TRJ;
    procedure SetValue(const [ref] AValue: TRJ);
    procedure SetItems(const APath: TRPath; const [ref] AValue: TRJ);
    function GetS(const APath: TRPath): string; overload;
    procedure SetS(const APath: TRPath; AValue: string); overload;
    function GetI(const APath: TRPath): Integer; overload;
    procedure SetI(const APath: TRPath; AValue: Integer); overload;
    function GetI64(const APath: TRPath): Int64; overload;
    procedure SetI64(const APath: TRPath; AValue: Int64); overload;
    function GetF(const APath: TRPath): Extended; overload;
    procedure SetF(const APath: TRPath; AValue: Extended); overload;
    function GetB(const APath: TRPath): boolean; overload;
    procedure SetB(const APath: TRPath; AValue: boolean); overload;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetKey: string;
    function GetRoot: TRJ;
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
    class operator Implicit(const Value: TJValue): TRJ;
    class operator Implicit(const [ref] Value: TRJ): TJValue;
    function ToStr(const ADefault: string = ''): string;
    function ToInt(ADefault: Integer = 0): Integer;
    function ToInt64(ADefault: Int64 = 0): Int64;
    function ToFloat(ADefault: Extended = 0.0): Extended;
    function ToBool(ADefault: boolean = False): boolean;

    property Items[const APath: TRPath]: TRJ read GetItems write SetItems; default;
    property S[const APath: TRPath]: string read GetS write SetS;
    property I[const APath: TRPath]: Integer read GetI write SetI;
    property I64[const APath: TRPath]: Int64 read GetI64 write SetI64;
    property F[const APath: TRPath]: Extended read GetF write SetF;
    property B[const APath: TRPath]: boolean read GetB write SetB;
    property Pairs[AIndex: Integer]: TRJ read GetPairs;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property Key: string read GetKey;
    property RootRefCount: Integer read GetRootRefCount;
    property Root: TRJ read GetRoot;
    property Path: string read FPath;
    property JValue: TJValue read GeTJValue;

    function CloneJValue: TJValue;
    function IsRoot: boolean; inline;
    function RootIsJObject: boolean; inline;
    function RootIsJArray: boolean; inline;
    function IsJObject: boolean;
    function IsJArray: boolean;
    function IsJString: boolean;
    function IsJNumber: boolean;
    function IsJBool: boolean;
    function IsJNull: boolean;
    function IsNil: boolean;
    procedure Reset;
    function ToString: string;
    function ToJSON(AEncodeBelow32: boolean = true; AEncodeAbove127: boolean = true): string;
    function Format(AIndentation: Integer = 4): string;
    procedure ParseJValue(const AData: string; AUseBool: boolean = False; ARaiseExc: boolean = False);
    procedure LoadFromFile(const AFileName: string; AUseBool: boolean = False; ARaiseExc: boolean = False);
    procedure SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: boolean = False); overload;
    procedure SaveToFile(const AFileName: string; AEncodeBelow32: boolean = true; AEncodeAbove127: boolean = true; AWriteBOM: boolean = False); overload;
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

destructor TRJRoot.Destroy;
begin
  FData.Free;
  inherited;
end;

function TRJRoot.GetData: TJValue;
begin
  Result := FData;
end;

procedure TRJRoot.SetData(const AValue: TJValue);
begin
  FData := AValue;
end;

function TRJRoot.ForceData(AType: TJVType): TJValue;
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
{ TRJEnumerator }

constructor TRJEnumerator.Create(const [ref] AData: TRJ);
begin
  inherited Create;
  FPData := @AData;
  FIndex := -1;
end;

function TRJEnumerator.GetCurrent: TRJ;
var
  jvTmp: TJValue;
begin
  Result.Reset;
  Result.FIRoot := FPData^.FIRoot;
  jvTmp := FPData^.GeTJValue;
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
  Dest.FIRoot := TRJRoot.Create;
  Dest.FPath := '';
end;

class operator TRJ.Finalize(var Dest: TRJ);
begin
  Dest.FIRoot := nil;
end;

function TRJ.GetRootRefCount: Integer;
begin
  Result := (FIRoot as TRJRoot).RefCount;
end;

function TRJ.ForceRootJValue(const APath: string): TJValue;
begin
  if APath.StartsWith('[') then
    Result := FIRoot.ForceData(TJArray)
  else
    Result := FIRoot.ForceData(TJObject);
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

function TRJ.GeTJValue: TJValue;
begin
  Result := FIRoot.Data.FindValue(FPath);
end;

function TRJ.CloneJValue: TJValue;
begin
  Result := GeTJValue;
  if Result <> nil then
    Result := Result.Clone as TJValue
  else
    Result := TJNull.Create;
end;

class operator TRJ.Assign(var Dest: TRJ; const [ref] Src: TRJ);
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

class operator TRJ.Implicit(const Value: string): TRJ;
begin
  Result.FIRoot.Data := TJString.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): string;
begin
  Result := Value.ToStr('');
end;

class operator TRJ.Implicit(Value: Integer): TRJ;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Integer;
begin
  Result := Value.ToInt(0);
end;

class operator TRJ.Implicit(Value: Int64): TRJ;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Int64;
begin
  Result := Value.ToInt64(0);
end;

class operator TRJ.Implicit(Value: Extended): TRJ;
begin
  Result.FIRoot.Data := TJNumber.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): Extended;
begin
  Result := Value.ToFloat(0.0);
end;

class operator TRJ.Implicit(Value: boolean): TRJ;
begin
  Result.FIRoot.Data := TJBool.Create(Value);
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): boolean;
begin
  Result := Value.ToBool(False);
end;

class operator TRJ.Implicit(const Value: TJValue): TRJ;
begin
  Result.FIRoot.Data := Value;
end;

class operator TRJ.Implicit(const [ref] Value: TRJ): TJValue;
begin
  Result := Value.GeTJValue;
end;

function TRJ.ToStr(const ADefault: string): string;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<string>(ADefault);
end;

function TRJ.ToInt(ADefault: Integer = 0): Integer;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Integer>(ADefault);
end;

function TRJ.ToInt64(ADefault: Int64 = 0): Int64;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Int64>(ADefault);
end;

function TRJ.ToFloat(ADefault: Extended = 0.0): Extended;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<Extended>(ADefault);
end;

function TRJ.ToBool(ADefault: boolean = False): boolean;
begin
  Result := FIRoot.Data.FindValue(FPath).ToType<boolean>(ADefault);
end;

function TRJ.GetItems(const APath: TRPath): TRJ;
begin
  Result.FIRoot := FIRoot;
  Result.FPath := LinkPath(FPath, APath);
end;

function TRJ.GetPairs(AIndex: Integer): TRJ;
var
  jvTmp: TJValue;
begin
  jvTmp := GeTJValue;
  if (jvTmp is TJObject) then
    Result := GetItems(TJObject(jvTmp).Pairs[AIndex].JsonString.Value);
end;

procedure TRJ.SetValue(const [ref] AValue: TRJ);
var
  LValue: TJValue;
begin
{$IFDEF DEBUG}
  if FPath.IsEmpty then
    raise Exception.Create(' TRJ.SetValue: Path is empty');
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

procedure TRJ.SetItems(const APath: TRPath; const [ref] AValue: TRJ);
var
  tmp: TRJ;
begin
  tmp.FIRoot := FIRoot;
  tmp.FPath := LinkPath(FPath, APath);
  tmp.SetValue(AValue)
end;

function TRJ.GetS(const APath: TRPath): string;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<string>('');
end;

procedure TRJ.SetS(const APath: TRPath; AValue: string);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJString.Create(AValue));
end;

function TRJ.GetI(const APath: TRPath): Integer;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Integer>(0);
end;

procedure TRJ.SetI(const APath: TRPath; AValue: Integer);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJ.GetI64(const APath: TRPath): Int64;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Int64>(0);
end;

procedure TRJ.SetI64(const APath: TRPath; AValue: Int64);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJ.GetF(const APath: TRPath): Extended;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<Extended>(0.0);
end;

procedure TRJ.SetF(const APath: TRPath; AValue: Extended);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJNumber.Create(AValue));
end;

function TRJ.GetB(const APath: TRPath): boolean;
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  Result := ForceRootJValue(LPath).FindValue(LPath).ToType<boolean>(False);
end;

procedure TRJ.SetB(const APath: TRPath; AValue: boolean);
var
  LPath: string;
begin
  LPath := LinkPath(FPath, APath);
  ForceRootJValue(LPath).TrySetValue(LPath, TJBool.Create(AValue));
end;

function TRJ.GetCount: Integer;
var
  jvTemp: TJValue;
begin
  jvTemp := GeTJValue;
  if jvTemp is TJArray then
    Result := TJArray(jvTemp).Count
  else if jvTemp is TJObject then
    Result := TJObject(jvTemp).Count
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

function TRJ.GetRoot: TRJ;
begin
  Result.FIRoot := FIRoot;
  // Result.FPath := '';
end;

function TRJ.IsRoot: boolean;
begin
  Result := FPath.IsEmpty;
end;

function TRJ.RootIsJObject: boolean;
begin
  Result := FIRoot.Data is TJObject;
end;

function TRJ.RootIsJArray: boolean;
begin
  Result := FIRoot.Data is TJArray;
end;

function TRJ.IsJObject: boolean;
begin
  Result := GeTJValue is TJObject;
end;

function TRJ.IsJArray: boolean;
begin
  Result := GeTJValue is TJArray;
end;

function TRJ.IsJString: boolean;
begin
  Result := GeTJValue is TJString;
end;

function TRJ.IsJNumber: boolean;
begin
  Result := GeTJValue is TJNumber;
end;

function TRJ.IsJBool: boolean;
begin
  Result := GeTJValue is TJBool;
end;

function TRJ.IsJNull: boolean;
begin
  Result := GeTJValue is TJNull;
end;

function TRJ.IsNil: boolean;
begin
  Result := GeTJValue = nil;
end;

procedure TRJ.Reset;
begin
  FIRoot := TRJRoot.Create;
  FPath := '';
end;

function TRJ.ToJSON(AEncodeBelow32: boolean = true; AEncodeAbove127: boolean = true): string;
var
  LValue: TJValue;
  Options: TJSONAncestor.TJSONOutputOptions;
begin
  Result := '';
  LValue := GeTJValue;
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

function TRJ.ToString: string;
begin
  Result := ToJSON(False, False);
end;

function TRJ.Format(AIndentation: Integer): string;
var
  LValue: TJValue;
begin
  Result := '';
  LValue := GeTJValue;
  if LValue <> nil then
    Result := LValue.Format(AIndentation)
end;

procedure TRJ.ParseJValue(const AData: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  Reset;
  FIRoot.Data := TJValue.ParseJSONValue(AData, AUseBool, ARaiseExc);
end;

procedure TRJ.LoadFromFile(const AFileName: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  ParseJValue(TFile.ReadAllText(AFileName, TEncoding.UTF8), AUseBool, ARaiseExc);
end;

procedure TRJ.SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: boolean);
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

procedure TRJ.SaveToFile(const AFileName: string; AEncodeBelow32: boolean = true; AEncodeAbove127: boolean = true; AWriteBOM: boolean = False);
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

{ TRJ }
{ ============================================================================ }

end.

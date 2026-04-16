{
  TRJSON - JSON Simple Read and Write
  - v0.9.17
  - 2026-04-15 by gale
  - https://github.com/higale/RJSON
}
unit rjson;

interface

uses
  System.Classes, System.SysUtils, System.Json, System.IOUtils, System.Generics.Collections;

type
  TJObject = TJSONObject;
  TJArray = TJSONArray;
  TJPair = TJSONPair;
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
    function GetJValue: TJValue;
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
    function GetLastPath: string;
    function GetIndex: Integer;
    function GetActualIndex: Integer;
    function GetKey: string;
    function GetRoot: TRJSON;
    function GetParent: TRJSON;
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
    property LastPath: string read GetLastPath;
    property Index: Integer read GetIndex;
    property Key: string read GetKey;
    property RootRefCount: Integer read GetRootRefCount;
    property Root: TRJSON read GetRoot;
    property Parent: TRJSON read GetParent;
    property Path: string read FPath;
    property JValue: TJValue read GetJValue;

    function CloneJValue: TJValue;
    function IsRoot: Boolean; inline;
    function RootIsObject: Boolean; inline;
    function RootIsArray: Boolean; inline;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsString: Boolean;
    function IsNumber: Boolean;
    function IsInt: Boolean;
    function IsInteger: Boolean;
    function IsFloat: Boolean;
    function IsBool: Boolean;
    function IsNull: Boolean;
    function IsNil: Boolean;
    function IsEmpty: Boolean;
    function ItemByValue(const [ref] AValue: TRJSON; AIgnoreCase: Boolean = True): TRJSON;
    function FirstItem: TRJSON;
    function LastItem: TRJSON;
    procedure MoveTo(AIndex: Integer);
    procedure Rename(AName: string);
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveToFirst;
    procedure MoveToLast;

    procedure Add(const AValue: TRJSON);
    procedure Delete(AIndex: Integer); overload;
    procedure Delete; overload;

    procedure Reset;
    function ToJSON(AEscapeUnicode: Boolean = False): string;
    function Format(AIndentation: Integer = 4; AEscapeUnicode: Boolean = False): string;
    procedure ParseJValue(const AData: string; AUseBool: Boolean = False; ARaiseExc: Boolean = False);
      deprecated '函数 ParseJValue 已不建议使用，请使用新的 LoadFromString 函数';
    function LoadFromString(const AStr: string; AUseBool: Boolean = False; ARaiseExc: Boolean = False): Boolean;
    function LoadFromFile(const AFileName: string; AUseBool: Boolean = False; ARaiseExc: Boolean = False): Boolean;
    function SaveToFile(const AFileName: string; AIndentation: Integer = 4; AEscapeUnicode: Boolean = False; AWriteBOM: Boolean = False; ARaiseExc: Boolean = False): Boolean;
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
{ TJValueHelper TJObjectHelper TJArrayHelper }

type
  TJValueHelper = class helper for TJValue
  private
    function ToType<T>(ADefault: T): T;
    function GetOrCreate<T: TJValue>(AName: string): T;
    procedure SetValue(const APath: string; const AValue: TJValue);
    procedure TrySetValue(const APath: string; const AValue: TJValue);
  end;

  TJObjectHelper = class helper for TJObject
  private
    procedure _SetItem(const AName: string; const AValue: TJValue); overload;
    procedure _Insert(const AIndex: Integer; const AValue: TJPair); overload;
  end;

  TJArrayHelper = class helper for TJArray
  private
    procedure _Fill<T: TJValue>(ACount: Integer);
    procedure _Insert(const AIndex: Integer; const AValue: TJValue);
    procedure _SetItem(AIndex: Integer; const AValue: TJValue); overload;
  end;

  TJSONPairHelper = class helper for TJSONPair
  private
    procedure Rename(ANewName: string);
  end;

procedure TJSONPairHelper.Rename(ANewName: string);
begin
  SetJsonString(TJSONString.Create(ANewName));
end;

procedure TJArrayHelper._Fill<T>(ACount: Integer);
begin
  for var j := Count to ACount - 1 do
    AddElement(T.Create);
end;

procedure TJArrayHelper._Insert(const AIndex: Integer; const AValue: TJValue);
begin
  AddElement(AValue);
  for var I := AIndex to Count - 2 do
    AddElement(Remove(AIndex));
end;

procedure TJArrayHelper._SetItem(AIndex: Integer; const AValue: TJValue);
begin
  _Fill<TJNull>(AIndex);
  if AIndex <= Count - 1 then
    Remove(AIndex).Free;
  _Insert(AIndex, AValue);
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
  while True do
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
            TJObject(jv)._SetItem(preName, AValue)
          else
            TJArray(jv)._SetItem(preName.ToInteger, AValue);
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
      TJObject(self)._SetItem(AName, Result);
    end;
  end
  else if self is TJArray then
  begin
    TJArray(self)._Fill<TJNull>(AName.ToInteger + 1);
    Result := T(TJArray(self).Items[AName.ToInteger]);
    if not(Result is T) then
    begin
      Result := T.Create;
      TJArray(self)._SetItem(AName.ToInteger, Result);
    end;
  end
  else
  begin
    raise Exception.Create('GetOrCreate<T> Error, self must be TJO or TJA');
  end;
end;

procedure TJObjectHelper._SetItem(const AName: string; const AValue: TJValue);
var
  pairTmp: TJSONPair;
begin
  pairTmp := Get(AName);
  if pairTmp = nil then
    AddPair(AName, AValue)
  else
    pairTmp.JSONValue := AValue;
end;

procedure TJObjectHelper._Insert(const AIndex: Integer; const AValue: TJPair);
begin
  with self do
  begin
    FMembers.Insert(AIndex, AValue);
  end;
end;

{ TJValueHelper TJObjectHelper TJArrayHelper }
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
  if FIRoot.Data = nil then
    Exit(nil);
  Result := FIRoot.Data.FindValue(FPath);
end;

function TRJSON.CloneJValue: TJValue;
var
  LValue: TJValue;
begin
  LValue := GetJValue;
  if LValue <> nil then
    Exit(TJValue(LValue.Clone));
  Result := nil;
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
  Result := GetJValue.ToType<string>(ADefault);
end;

function TRJSON.ToInt(ADefault: Integer = 0): Integer;
begin
  Result := GetJValue.ToType<Integer>(ADefault);
end;

function TRJSON.ToInt64(ADefault: Int64 = 0): Int64;
begin
  Result := GetJValue.ToType<Int64>(ADefault);
end;

function TRJSON.ToFloat(ADefault: Extended = 0.0): Extended;
begin
  Result := GetJValue.ToType<Extended>(ADefault);
end;

function TRJSON.ToBool(ADefault: Boolean = False): Boolean;
begin
  Result := GetJValue.ToType<Boolean>(ADefault);
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
  LValue := AValue.CloneJValue;
  if LValue = nil then
    LValue := TJNull.Create;
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
  LRoot: TJValue;
begin
  LRoot := FIRoot.Data;
  if LRoot = nil then
    Exit('');
  LPath := LinkPath(FPath, APath);
  Result := LRoot.FindValue(LPath).ToType<string>('');
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
  LRoot: TJValue;
begin
  LRoot := FIRoot.Data;
  if LRoot = nil then
    Exit(0);
  LPath := LinkPath(FPath, APath);
  Result := LRoot.FindValue(LPath).ToType<Integer>(0);
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
  LRoot: TJValue;
begin
  LRoot := FIRoot.Data;
  if LRoot = nil then
    Exit(0);
  LPath := LinkPath(FPath, APath);
  Result := LRoot.FindValue(LPath).ToType<Int64>(0);
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
  LRoot: TJValue;
begin
  LRoot := FIRoot.Data;
  if LRoot = nil then
    Exit(0.0);
  LPath := LinkPath(FPath, APath);
  Result := LRoot.FindValue(LPath).ToType<Extended>(0.0);
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
  LRoot: TJValue;
begin
  LRoot := FIRoot.Data;
  if LRoot = nil then
    Exit(False);
  LPath := LinkPath(FPath, APath);
  Result := LRoot.FindValue(LPath).ToType<Boolean>(False);
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

function TRJSON.GetLastPath: string;
begin
  if FPath.IsEmpty then
    Exit('');
  Result := Key;
  if Result.IsEmpty then
  begin
    Result := '[' + Index.ToString + ']';
    if Result = '[-1]' then
      Result := '';
  end;
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
end;

function TRJSON.GetParent: TRJSON;
var
  iPos: Integer;
begin
  if FPath.IsEmpty then
    Exit;
  iPos := FPath.LastIndexOfAny(['[', '.']);
  if iPos < 0 then
    Exit(Root);
  Result.FIRoot := FIRoot;
  Result.FPath := FPath.Substring(0, iPos);
end;

function TRJSON.IsRoot: Boolean;
begin
  Result := FPath.IsEmpty;
end;

function TRJSON.RootIsObject: Boolean;
begin
  Result := FIRoot.Data is TJObject;
end;

function TRJSON.RootIsArray: Boolean;
begin
  Result := FIRoot.Data is TJArray;
end;

function TRJSON.IsObject: Boolean;
begin
  Result := GetJValue is TJObject;
end;

function TRJSON.IsArray: Boolean;
begin
  Result := GetJValue is TJArray;
end;

function TRJSON.IsString: Boolean;
begin
  if JValue <> nil then
    Exit(JValue.ClassName = 'TJSONString');
  Result := False;
end;

function TRJSON.IsNumber: Boolean;
begin
  Result := GetJValue is TJNumber;
end;

function TRJSON.IsInt: Boolean;
begin
  if IsNumber then
    Exit(ToStr.IndexOf('.') < 0);
  Result := False;
end;

function TRJSON.IsInteger: Boolean;
var
  LI64: Int64;
begin
  if IsInt then
  begin
    LI64 := ToInt64;
    Exit((LI64 >= Integer.MinValue) and (LI64 <= Integer.MaxValue));
  end;
  Result := False;
end;

function TRJSON.IsFloat: Boolean;
begin
  if IsNumber then
    Exit(ToStr.IndexOf('.') >= 0);
  Result := False;
end;

function TRJSON.IsBool: Boolean;
begin
  Result := GetJValue is TJBool;
end;

function TRJSON.IsNull: Boolean;
begin
  Result := GetJValue is TJNull;
end;

function TRJSON.IsNil: Boolean;
begin
  Result := GetJValue = nil;
end;

function TRJSON.IsEmpty: Boolean;
begin
  Result := FPath.IsEmpty and (FIRoot.Data = nil);
end;

function TRJSON.ItemByValue(const [ref] AValue: TRJSON; AIgnoreCase: Boolean): TRJSON;
begin
  for var item in self do
  begin
    if (AValue.JValue <> nil) and (item.JValue <> nil) and
       (AValue.JValue.ClassType = item.JValue.ClassType) then
      if string.Compare(AValue.ToStr, item.ToStr, AIgnoreCase) = 0 then
        Exit(item);
  end;
end;

function TRJSON.FirstItem: TRJSON;
begin
  if Count = 0 then
    Exit;
  if IsArray then
    Result := Items[0]
  else if IsObject then
    Result := Pairs[0];
end;

function TRJSON.LastItem: TRJSON;
begin
  if Count = 0 then
    Exit;
  if IsArray then
    Result := Items[Count - 1]
  else if IsObject then
    Result := Pairs[Count - 1];
end;

function TRJSON.GetActualIndex: Integer;
var
  LParent: TJValue;
  LKey: string;
begin
  Result := Index;
  if Result >= 0 then
    Exit;
  LParent := Parent.JValue;
  LKey := Key;
  if (LParent is TJObject) and (LKey <> '') then
    for var j := 0 to TJObject(LParent).Count - 1 do
      if TJObject(LParent).Pairs[j].JsonString.Value = LKey then
        Exit(j);
end;

procedure TRJSON.MoveTo(AIndex: Integer);
var
  LParent: TJValue;
  LActualIndex: Integer;
begin
  LParent := Parent.JValue;
  LActualIndex := GetActualIndex;
  if (AIndex >= Parent.Count) or (AIndex < 0) then
    raise Exception.Create('Index out of bounds');
  if LParent is TJArray then
  begin
    TJArray(LParent)._Insert(AIndex, TJArray(LParent).Remove(LActualIndex));
    var LParentPath := Parent.FPath;
    if LParentPath.IsEmpty then
      FPath := '[' + AIndex.ToString + ']'
    else
      FPath := LParentPath + '[' + AIndex.ToString + ']';
  end
  else if LParent is TJObject then
  begin
    TJObject(LParent)._Insert(AIndex, TJObject(LParent).RemovePair(Key));
  end;
end;

procedure TRJSON.Rename(AName: string);
begin
  if not Parent.IsObject then
    Exit;
  TJObject(Parent.JValue).Get(Key).Rename(AName);
  if Parent.IsRoot then
    FPath := AName
  else
    FPath := FPath.Substring(0, FPath.LastIndexOf('.') + 1) + AName;
end;

procedure TRJSON.MoveUp;
var
  LIndex: Integer;
begin
  LIndex := GetActualIndex;
  if LIndex > 0 then
    MoveTo(LIndex - 1);
end;

procedure TRJSON.MoveDown;
var
  LIndex: Integer;
begin
  LIndex := GetActualIndex;
  if (LIndex >= 0) and (LIndex < Parent.Count - 1) then
    MoveTo(LIndex + 1);
end;

procedure TRJSON.MoveToFirst;
begin
  MoveTo(0);
end;

procedure TRJSON.MoveToLast;
begin
  MoveTo(Parent.Count - 1);
end;

procedure TRJSON.Add(const AValue: TRJSON);
begin
  if IsArray then
  begin
    Items[Count] := AValue.CloneJValue;
  end
  else
  begin
    Items[0] := AValue.CloneJValue;
  end;
end;

procedure TRJSON.Delete(AIndex: Integer);
var
  jvTmp: TJValue;
begin
  jvTmp := GetJValue;
  if jvTmp is TJArray then
    TJArray(jvTmp).Remove(AIndex).Free
  else if jvTmp is TJObject then
    TJObject(jvTmp).RemovePair(TJObject(jvTmp).Pairs[AIndex].JsonString.Value).Free;
end;

procedure TRJSON.Delete;
var
  LParentValue: TJValue;
begin
  if IsRoot then
  begin
    Reset;
    Exit;
  end;
  LParentValue := Parent.JValue;
  if LParentValue is TJObject then
  begin
    TJObject(LParentValue).RemovePair(Key).Free;
  end
  else if LParentValue is TJArray then
  begin
    TJArray(LParentValue).Remove(Index).Free;
  end;
end;

procedure TRJSON.Reset;
begin
  FIRoot := TRJSONRoot.Create;
  FPath := '';
end;

function TRJSON.ToJSON(AEscapeUnicode: Boolean): string;
var
  LValue: TJValue;
  Options: TJSONAncestor.TJSONOutputOptions;
begin
  Result := '';
  LValue := GetJValue;
  if LValue <> nil then
  begin
    Options := [TJSONAncestor.TJSONOutputOption.EncodeBelow32];
    if AEscapeUnicode then
      Include(Options, TJSONAncestor.TJSONOutputOption.EncodeAbove127);
    Result := LValue.ToJSON(Options);
  end;
end;

function JSONEscapeUnicode(const AStr: string): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
var
  ch: char;
  I, Len: Integer;
  U: Integer;
  sb: TStringBuilder;
begin
  Len := AStr.Length;
  if Len = 0 then
    Exit('');
  sb := TStringBuilder.Create(Len);
  try
    for I := 1 to Len do
    begin
      ch := AStr[I];
      U := Ord(ch);
      if U > 127 then
        sb.Append('\u').Append(HexChars[(U shr 12) and $F])
          .Append(HexChars[(U shr 8) and $F])
          .Append(HexChars[(U shr 4) and $F])
          .Append(HexChars[U and $F])
      else
        sb.Append(ch);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TRJSON.Format(AIndentation: Integer; AEscapeUnicode: Boolean): string;
var
  LValue: TJValue;
begin
  if AIndentation >= 0 then
  begin
    Result := '';
    LValue := GetJValue;
    if LValue <> nil then
    begin
      Result := LValue.Format(AIndentation);
      if AEscapeUnicode then
        Result := JSONEscapeUnicode(Result);
    end;
  end
  else
  begin
    Result := ToJSON(AEscapeUnicode);
  end;
end;

procedure TRJSON.ParseJValue(const AData: string; AUseBool: Boolean; ARaiseExc: Boolean);
begin
  self := TJValue.ParseJSONValue(AData, AUseBool, ARaiseExc);
end;

function TRJSON.LoadFromString(const AStr: string; AUseBool: Boolean; ARaiseExc: Boolean): Boolean;
begin
  self := TJSONValue.ParseJSONValue(AStr, AUseBool, ARaiseExc);
  Result := not IsNil;
end;

function TRJSON.LoadFromFile(const AFileName: string; AUseBool: Boolean; ARaiseExc: Boolean): Boolean;
begin
  Result := False;
  try
    Result := LoadFromString(TFile.ReadAllText(AFileName, TEncoding.UTF8), AUseBool, ARaiseExc);
  except
    on E: Exception do
    begin
      if ARaiseExc then
        raise Exception.Create(E.Message);
    end;
  end;
end;

function TRJSON.SaveToFile(const AFileName: string; AIndentation: Integer; AEscapeUnicode: Boolean; AWriteBOM: Boolean; ARaiseExc: Boolean): Boolean;
var
  strs: TStrings;
begin
 Result := False;
  try
    strs := TStringList.Create;
    try
      strs.WriteBOM := AWriteBOM;
      strs.TrailingLineBreak := False;
      strs.Text := Format(AIndentation, AEscapeUnicode);
      strs.SaveToFile(AFileName, TEncoding.UTF8);
      Result := True;
    finally
      strs.Free;
    end;
  except
    on E: Exception do
    begin
      if ARaiseExc then
        raise Exception.Create(E.Message);
    end;
  end;
end;

{ TRJSON }
{ ============================================================================ }

end.

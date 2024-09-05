(*
# Delphi JSON common operation encapsulation JSON数据读写封装
- v0.9.1
- 2024-09-05 by gale
- https://github.com/higale/rjson

## Properties:
- Items[Path] 路径读写，对Object和Array都有效

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- Items[Index] 数组读写

        a[3][1] := 'hello';

- Pairs[Index] 获取JSONObject下的键值对

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- Count  Object或Array包含条目数，其它类型返回0
- Index  条目在Array中的索引值，不是数组数条目据返回-1
- Key    如果是键值对数据，返回键值，否则返回空
- Root   根数据接口
- Path   值的路径
- JSONValue 包含的TJSONValue值

## Methods
- ToStr 转换为字符串，缺省为空

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('没有标题');

- ToInt 转换为整数，缺省为0

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);


- ToInt64 转换为64位整数，缺省为0

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- ToFloat 转换为浮点数(使用 Extended)，缺省为0.0

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- ToBool 转换为 Boolean，缺省为 False

        var b:Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- RootIs<T: TJSONValue> 根是否是某种类型(TJSONObject、TJSONArray等)
- ValueIs<T: TJSONValue> 当前值是否是某种类型(TJSONObject、TJSONArray等)
- CloneJSONValue 克隆一份当前值，如果当前值不存在，则生成 TJSONNull
- Reset 复位到出厂状态
- Format 输出格式化好的JSON字符串

        str1 := RJ.Format(2); // 缩进2个空格(缺省4个)
        str2 := RJ.Format(0); // 压缩格式，无缩进无换行

- ParseJSONValue 从字符串加载数据

        RJ.ParseJSONValue('{"a":1}');

- LoadFromFile 从文件加载数据

        procedure LoadFromFile(
            const AFileName: string;   // JSON文件名
            AUseBool: boolean = False; // 遇到JSON数据中的 true 或 false 时，是否创建 TJSONBool 类型的值
            ARaiseExc: boolean = False // 遇到无效的 JSON 数据时是否抛出异常
        );

- SaveToFile 保存到文件

        procedure aveToFile(
            const AFileName: string;            // 文件名
            AIndentation: Integer = 4;          // 缩进空格数，0:不缩进不换行
            AWriteBOM: boolean = False;         // 文件是否添加 BOM 标记
            ATrailingLineBreak: boolean = False // 是否在结尾添加一个空行
        );

## Example:
    procedure TFormMain.btnTestClick(Sender: TObject);
    var
      RJ, RJ1: TRJSON;
      fTemp: Extended;
    begin
      RJ['title'] := 'hello world! 你好，世界！';
      RJ['a.num'] := 1;
      RJ['a.hah'] := false;
      RJ['b[2]'] := 505;
      RJ['b[0]'] := 'first';
      RJ['good'] := True;
      RJ1 := RJ['c'];
      RJ1['c1'] := 1.1;
      RJ1['c2[2]'] := 2.33;
      with RJ['x'] do
      begin
        items[1] := 100;
        items[2] := '202';
      end;
      with RJ['y'] do
      begin
        items['ya'] := 'y1';
        items['yb'] := -2;;
      end;
      Memo1.Text := RJ.Format;
      Memo1.Lines.Add('-----------------------------------------------------------');
      fTemp := RJ['c.c2[3]'];
      Memo1.Lines.Add('fTemp:' + fTemp.ToString);
      fTemp := RJ['c.c3[3]'].ToFloat(-100);
      Memo1.Lines.Add('fTemp:' + fTemp.ToString);
      Memo1.Lines.Add(RJ['a.num'].ToStr('a.num not exist'));
      Memo1.Lines.Add(RJ['none'].ToFloat(-1).ToString);
      Memo1.Lines.Add(RJ['none.a3'].ToStr('none.a3 not exist'));
      RJ.SaveToFile('test.json', 0);
    end;

    procedure TFormMain.btnOpenClick(Sender: TObject);
    var
      RJ: TRJSON;
      strTmp: string;
    begin
      RJ.LoadFromFile('test.json');
      Memo1.Text := RJ.Format(8);
      Memo1.Lines.Add('-----------------------ROOT--------------------------');
      for var item in RJ do
      begin
        strTmp := Format('Index: %d  Key: %s  Value: %s',
          [item.Index, item.Key, item.Format(0)]);
        Memo1.Lines.Add(strTmp);
      end;
      Memo1.Lines.Add('-----------------------RJ[''a'']--------------------------');
      for var item in RJ['a'] do
      begin
        strTmp := Format('Index: %d  Key: %s  Value: %s',
          [item.Index, item.Key, item.Format(0)]);
        Memo1.Lines.Add(strTmp);
      end;
      Memo1.Lines.Add('-----------------------RJ[''b'']--------------------------');
      for var item in RJ['b'] do
      begin
        strTmp := Format('Index: %d  Key: %s  Value: %s',
          [item.Index, item.Key, item.Format(0)]);
        Memo1.Lines.Add(strTmp);
      end;
      Memo1.Lines.Add('-----------------------RJ[''c'']--------------------------');
      for var i := 0 to RJ['c'].Count - 1 do
      begin
        strTmp := Format('Index: %d  Key: %s  Value: %s',
          [RJ['c'].Pairs[i].Index, RJ['c'].Pairs[i].Key, RJ['c'].Pairs[i].Format(0)]);
        Memo1.Lines.Add(strTmp);
      end;
    end;
*)
unit rjson;

interface

uses
  System.IOUtils, System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.Generics.Collections;

type
  TJO = TJSONObject;
  TJA = TJSONArray;
  TJV = TJSONValue;
  TJSONValueType = type of TJSONValue;
  TRJSONEnumerator = class;

  { JSON root data interface }
  IRJSONRoot = interface
    function GetData: TJSONValue;
    procedure SetData(const AValue: TJSONValue);
    function ForceJV(AType: TJSONValueType): TJSONValue;
    property Data: TJSONValue read GetData write SetData;
  end;

  { JSON root data interface class }
  TRJSONRoot = class(TInterfacedObject, IRJSONRoot)
  private
    FData: TJSONValue;
    function GetData: TJSONValue;
    procedure SetData(const AValue: TJSONValue);
    function ForceJV(AType: TJSONValueType): TJSONValue;
  public
    constructor Create; overload;
    constructor Create(const AValue: TJSONValue); overload;
    destructor Destroy; override;
  end;

  { JSON data operation structure }
  TRJSON = record
  private
    FRoot: IRJSONRoot;
    FPath: string;
    function LinkPath(const ALeft, ARight: string): string;
    function GetJSONValue: TJSONValue; inline;
    function GetItems(const APath: string): TRJSON; overload;
    function GetItems(AIndex: Integer): TRJSON; overload; inline;
    function GetPairs(AIndex: Integer): TRJSON;
    procedure SetValue(const [ref] AValue: TRJSON);
    procedure SetItems(const APath: string; const [ref] AValue: TRJSON); overload;
    procedure SetItems(AIndex: Integer; const [ref] AValue: TRJSON); overload; inline;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetKey: string;
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
    class operator Implicit(Value: boolean): TRJSON;
    class operator Implicit(const [ref] Value: TRJSON): boolean;
    class operator Implicit(const Value: TJSONValue): TRJSON;

    function ToStr(const ADefault: string = ''): string;
    function ToInt(ADefault: Integer = 0): Integer;
    function ToInt64(ADefault: Int64 = 0): Int64;
    function ToFloat(ADefault: Extended = 0.0): Extended;
    function ToBool(ADefault: boolean = False): boolean;
    property Items[const APath: string]: TRJSON read GetItems write SetItems; default;
    property Items[AIndex: Integer]: TRJSON read GetItems write SetItems; default;
    property Pairs[AIndex: Integer]: TRJSON read GetPairs;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property Key: string read GetKey;
    property Root: IRJSONRoot read FRoot;
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
  TRJSONEnumerator = class
  private
    FPData: ^TRJSON;
    FIndex: Integer;
    function GetCurrent: TRJSON;
  public
    constructor Create(const [ref] AData: TRJSON);
    function MoveNext: boolean;
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

constructor TRJSONRoot.Create(const AValue: TJSONValue);
begin
  inherited Create;
  FData := AValue;
end;

destructor TRJSONRoot.Destroy;
begin
  FData.Free;
  inherited;
end;

function TRJSONRoot.GetData: TJSONValue;
begin
  Result := FData;
end;

procedure TRJSONRoot.SetData(const AValue: TJSONValue);
begin
  FData := AValue;
end;

function TRJSONRoot.ForceJV(AType: TJSONValueType): TJSONValue;
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
{ TRJSONEnumerator }

constructor TRJSONEnumerator.Create(const [ref] AData: TRJSON);
begin
  inherited Create;
  FPData := @AData;
  FIndex := -1;
end;

function TRJSONEnumerator.GetCurrent: TRJSON;
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

function TRJSONEnumerator.MoveNext: boolean;
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
  Dest.FRoot := TRJSONRoot.Create;
  Dest.FPath := '';
end;

class operator TRJSON.Finalize(var Dest: TRJSON);
begin
  Dest.FRoot := nil;
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

function TRJSON.GetJSONValue: TJSONValue;
begin
  Result := FRoot.Data.FindValue(FPath);
end;

function TRJSON.CloneJSONValue: TJSONValue;
begin
  Result := GetJSONValue;
  if Result <> nil then
    Result := Result.Clone as TJSONValue
  else
    Result := TJSONNull.Create;
end;

class operator TRJSON.Assign(var Dest: TRJSON; const [ref] Src: TRJSON);
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

class operator TRJSON.Implicit(const Value: string): TRJSON;
begin
  Result.FRoot.Data := TJSONString.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): string;
begin
  Result := Value.ToStr('');
end;

class operator TRJSON.Implicit(Value: Integer): TRJSON;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Integer;
begin
  Result := Value.ToInt(0);
end;

class operator TRJSON.Implicit(Value: Int64): TRJSON;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Int64;
begin
  Result := Value.ToInt64(0);
end;

class operator TRJSON.Implicit(Value: Extended): TRJSON;
begin
  Result.FRoot.Data := TJSONNumber.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): Extended;
begin
  Result := Value.ToFloat(0.0);
end;

class operator TRJSON.Implicit(Value: boolean): TRJSON;
begin
  Result.FRoot.Data := TJSONBool.Create(Value);
end;

class operator TRJSON.Implicit(const [ref] Value: TRJSON): boolean;
begin
  Result := Value.ToBool(False);
end;

class operator TRJSON.Implicit(const Value: TJSONValue): TRJSON;
begin
  Result.FRoot.Data := Value;
end;

function TRJSON.ToStr(const ADefault: string): string;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<string>(ADefault);
end;

function TRJSON.ToInt(ADefault: Integer = 0): Integer;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Integer>(ADefault);
end;

function TRJSON.ToInt64(ADefault: Int64 = 0): Int64;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Int64>(ADefault);
end;

function TRJSON.ToFloat(ADefault: Extended = 0.0): Extended;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<Extended>(ADefault);
end;

function TRJSON.ToBool(ADefault: boolean = False): boolean;
begin
  Result := FRoot.Data.FindValue(FPath).ToType<boolean>(ADefault);
end;

function TRJSON.GetItems(const APath: string): TRJSON;
begin
  Result.FRoot := FRoot;
  Result.FPath := LinkPath(FPath, APath);
end;

function TRJSON.GetItems(AIndex: Integer): TRJSON;
begin
  Result := GetItems('[' + AIndex.ToString + ']');
end;

function TRJSON.GetPairs(AIndex: Integer): TRJSON;
var
  jvTmp: TJSONValue;
begin
  jvTmp := GetJSONValue;
  if (jvTmp is TJSONObject) then
    Result := GetItems(TJSONObject(jvTmp).Pairs[AIndex].JsonString.Value);
end;

procedure TRJSON.SetValue(const [ref] AValue: TRJSON);
var
  LValue: TJSONValue;
begin
  if FPath.IsEmpty then
    raise Exception.Create(' TRJSON.SetValue: Path is empty');

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

procedure TRJSON.SetItems(const APath: string; const [ref] AValue: TRJSON);
var
  tmp: TRJSON;
begin
  tmp.FRoot := FRoot;
  tmp.FPath := LinkPath(FPath, APath);
  tmp.SetValue(AValue)
end;

procedure TRJSON.SetItems(AIndex: Integer; const [ref] AValue: TRJSON);
begin
  SetItems('[' + AIndex.ToString + ']', AValue);
end;

function TRJSON.GetCount: Integer;
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

function TRJSON.RootIs<T>: boolean;
begin
  Result := FRoot.Data is T;
end;

function TRJSON.ValueIs<T>: boolean;
begin
  Result := GetJSONValue is T;
end;

procedure TRJSON.Reset;
begin
  FRoot := TRJSONRoot.Create;
  FPath := '';
end;

function TRJSON.Format(Indentation: Integer): string;
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

procedure TRJSON.ParseJSONValue(const AData: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  Reset;
  FRoot.Data := TJSONValue.ParseJSONValue(AData, AUseBool, ARaiseExc);
end;

procedure TRJSON.LoadFromFile(const AFileName: string; AUseBool: boolean; ARaiseExc: boolean);
begin
  ParseJSONValue(TFile.ReadAllText(AFileName, TEncoding.UTF8), AUseBool, ARaiseExc);
end;

procedure TRJSON.SaveToFile(const AFileName: string; AIndentation: Integer; AWriteBOM: boolean; ATrailingLineBreak: boolean);
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

{ TRJSON }
{ ============================================================================ }

end.

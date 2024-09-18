# TRJSON - JSON Simple Read and Write
- by gale
- https://github.com/higale/RJSON

**Languages: [English](README.md) | [中文](README_zh.md)**

## 类型别名
- TJObject = TJSONObject;
- TJArray = TJSONArray;
- TJValue = TJSONValue;
- TJString = TJSONString;
- TJNumber = TJSONNumber;
- TJBool = TJSONBool;
- TJNull = TJSONNull;

## 属性：
- `Items[Path]` 缺省属性，基于路径读写，适用于对象和数组，Path 可以是字符串或整数索引。
- `S[Path]` 字符串数值读写
- `I[Path]` 整数数值读写
- `I64[Path]` 64位整数数值读写
- `F[Path]` 浮点数数值读写，使用 Extended 类型
- `B[Path]` 布尔数值读写
- `Pairs[Index]` 获取 JSONObject 下的键值对。
- `Count` 对象或数组中的条目数量，其他类型返回 0。
- `Index` 数组中的条目索引，如果不是数组数据则返回 -1。
- `Key` 如果是键值对数据，则返回键，否则返回空字符串。
- `RootRefCount` Root接口引用次数，主要用于调试
- `Root` 根数据。
- `Path` 值的路径。
- `JValue` 包含的JSON对象。

## 方法
- `ToStr` 转换为字符串，默认为空字符串。
- `ToInt` 转换为整数，默认为 0。
- `ToInt64` 转换为 64 位整数，默认为 0。
- `ToFloat` 转换为浮点数（使用 Extended），默认为 0.0。
- `ToBool` 转换为布尔值，默认为 False。

- `CloneJValue` 克隆当前值，如果当前值不存在，则生成 TJNull。
- `IsRoot` 是否是根数据（Path为空）
- `RootIsJObject` 根是否是JObject
- `RootIsJArray` 根是否是JArray
- `IsJObject` 值是否是JObject
- `IsJArray` 值是否是JObject
- `IsJString` 值是否是JString
- `IsJNumber` 值是否是JNumber
- `IsJBool` 值是否是JBool
- `IsJNull` 值是否是JJNull
- `IsNil` 值是否为空
- `Reset` 重置为出厂设置

- `ToString` 输出 JSON 字符串，紧凑格式，不做编码。
- `ToJSON` 输出 JSON 字符串，紧凑格式，根据参数对小于32或大于127的字符编码。
- `Format` 输出格式化的 JSON 字符串，不做编码。
- `ParseJSONValue` 从字符串加载数据。
- `LoadFromFile` 从文件加载数据。
- `SaveToFile` 将JSON数据保存到文件。

## 示例：
```pascal
uses rjson;

procedure TFormMain.btnTestClick(Sender: TObject);
var
  RJ, RJ1: TRJSON;
  fTemp: Extended;
begin
  Memo1.Lines.Clear;
  RJ['title1'] := 'hello world!';
  RJ.S['title2'] := '世界，你好！';
  RJ.Items['title3'] := 'Good';
  RJ['a.num'] := 1;
  RJ['a.haha'] := false;
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
    items['yb'] := -2;
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
  Memo1.Lines.Add('-----------------------RJ[a]--------------------------');
  for var item in RJ['a'] do
  begin
    strTmp := Format('Index: %d  Key: %s  Value: %s',
      [item.Index, item.Key, item.Format(0)]);
    Memo1.Lines.Add(strTmp);
  end;
  Memo1.Lines.Add('-----------------------RJ[b]--------------------------');
  for var item in RJ['b'] do
  begin
    strTmp := Format('Index: %d  Key: %s  Value: %s',
      [item.Index, item.Key, item.Format(0)]);
    Memo1.Lines.Add(strTmp);
  end;
  Memo1.Lines.Add('-----------------------RJ[c]--------------------------');
  for var i := 0 to RJ['c'].Count - 1 do
  begin
    strTmp := Format('Index: %d  Key: %s  Value: %s',
      [RJ['c'].Pairs[i].Index, RJ['c'].Pairs[i].Key, RJ['c'].Pairs[i].Format(0)]);
    Memo1.Lines.Add(strTmp);
  end;
end;
```

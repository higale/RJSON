# TRJSON - JSON Simple Read and Write
- by gale
- https://github.com/higale/RJSON

**Languages: [English](README.md) | [中文](README_zh.md)**

## Type Aliases
- TJObject = TJSONObject;
- TJArray = TJSONArray;
- TJValue = TJSONValue;
- TJString = TJSONString;
- TJNumber = TJSONNumber;
- TJBool = TJSONBool;
- TJNull = TJSONNull;

## Properties:
- `Items[Path]` Default property, Reads and writes based on path, applicable to objects and arrays. Path can be a string or integer index.
- `S[Path]` Reads and writes string values.
- `I[Path]` Reads and writes integer values.
- `I64[Path]` Reads and writes 64-bit integer values.
- `F[Path]` Reads and writes floating-point values using the Extended type.
- `B[Path]` Reads and writes boolean values.
- `Pairs[Index]` Retrieves key-value pairs under a JSONObject.
- `Count` The number of entries in an object or array; returns 0 for other types.
- `Index` The index of an entry in an array; returns -1 if not array data.
- `Key` Returns the key if it is key-value pair data; otherwise, returns an empty string.
- `RootRefCount` Reference count of the root interface, mainly used for debugging.
- `Root` Root data.
- `Path` Path of the value.
- `JValue` Contains the JSON object.

## Methods
- `ToStr` Converts to a string; defaults to an empty string.
- `ToInt` Converts to an integer; defaults to 0.
- `ToInt64` Converts to a 64-bit integer; defaults to 0.
- `ToFloat` Converts to a floating-point number (using Extended); defaults to 0.0.
- `ToBool` Converts to a boolean; defaults to False.

- `CloneJValue` Clones the current value; generates TJNull if the current value does not exist.
- `IsRoot` Checks if it is root data (Path is empty).
- `RootIsJObject` Checks if the root is a JObject.
- `RootIsJArray` Checks if the root is a JArray.
- `IsJObject` Checks if the value is a JObject.
- `IsJArray` Checks if the value is a JArray.
- `IsJString` Checks if the value is a JString.
- `IsJNumber` Checks if the value is a JNumber.
- `IsJBool` Checks if the value is a JBool.
- `IsJNull` Checks if the value is a JNull.
- `IsNil` Checks if the value is nil.
- `Reset` Resets to factory settings.

- `ToString` Outputs a JSON string in compact format without encoding.
- `ToJSON` Outputs a JSON string in compact format, encoding characters less than 32 or greater than 127 based on parameters.
- `Format` Outputs a formatted JSON string.
- `ParseJSONValue` Loads data from a string.
- `LoadFromFile` Loads data from a file.
- `SaveToFile` Save JSON data to a file.

## Example:
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
    Items[1] := 100;
    Items[2] := '202';
  end;

  with RJ['y'] do
  begin
    Items['ya'] := 'y1';
    Items['yb'] := -2;
  end;

  Memo1.Lines.Add('ToString:');
  Memo1.Lines.Add(RJ.ToString);
  Memo1.Lines.Add('ToJSON:');
  Memo1.Lines.Add(RJ.ToJSON);
  Memo1.Lines.Add('Format:');
  Memo1.Lines.Add(RJ.Format);

  Memo1.Lines.Add('-----------------------------------------------------------');

  fTemp := RJ['c.c2[3]'];
  Memo1.Lines.Add('fTemp = ' + fTemp.ToString);

  fTemp := RJ['c.none[3]'].ToFloat(-100);
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

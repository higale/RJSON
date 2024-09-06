# TRJ - JSON Simple Read and Write
- by gale
- https://github.com/higale/RJSON

**Languages: [English](README.md) | [简体中文](README_zh_CN.md) | [繁體中文](README_zh_TW.md) | [日本語](README_ja.md) | [한국어](README_ko.md)**

## Properties:
- `Items[Path]` Path-based read/write, works for both Objects and Arrays.

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- `Items[Index]` Array read/write.

        a[3][1] := 'hello';

- `Pairs[Index]` Retrieving key-value pairs under a JSONObject.

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- `Count` Number of entries in an Object or Array, returns 0 for other types.
- `Index` Index of the entry in an Array, returns -1 if not an array data.
- `Key` If it's a key-value pair data, returns the key, otherwise returns an empty string.
- `Root` Interface to root data.
- `Path` Path of the value.
- `JSONValue` Contains the `TJSONValue`.

## Methods
- `ToStr` Converts to a string, defaults to an empty string.

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('No Title');

- `ToInt` Converts to an integer, defaults to 0.

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);

- `ToInt64` Converts to a 64-bit integer, defaults to 0.

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- `ToFloat` Converts to a floating-point number (using Extended), defaults to 0.0.

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- `ToBool` Converts to Boolean, defaults to False.

        var b: Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- `RootIs<T: TJSONValue>` Checks if the root is of a certain type (TJSONObject, TJSONArray, etc.).
- `ValueIs<T: TJSONValue>` Checks if the current value is of a certain type (TJSONObject, TJSONArray, etc.).
- `CloneJSONValue` Clones the current value, generates TJSONNull if the current value does not exist.
- `Reset` Resets to factory settings.
- `Format` Outputs a formatted JSON string.

        str1 := RJ.Format(2); // Indented by 2 spaces (default is 4)
        str2 := RJ.Format(0); // Compressed format, no indentation, no line breaks

- `ParseJSONValue` Loads data from a string.

        RJ.ParseJSONValue('{"a":1}');

- `LoadFromFile` Loads data from a file.

        procedure LoadFromFile(
            const AFileName: string;   // JSON filename
            AUseBool: boolean = False; // Whether to create TJSONBool type values when encountering true or false in JSON data
            ARaiseExc: boolean = False // Whether to throw an exception when encountering invalid JSON data
        );

- `SaveToFile` Saves to a file.

        procedure SaveToFile(
            const AFileName: string;            // Filename
            AIndentation: Integer = 4;          // Number of indentation spaces, 0: no indentation, no line breaks
            AWriteBOM: boolean = False;         // Whether to add a BOM marker to the file
            ATrailingLineBreak: boolean = False // Whether to add an empty line at the end
        );

## Example:
    procedure TFormMain.btnTestClick(Sender: TObject);
    var
      RJ, RJ1: TRJ;
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
      RJ: TRJ;
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
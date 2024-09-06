# TRJ - JSON 簡單讀取與寫入
- v0.9.1
- 2024-09-05 by gale
- https://github.com/higale/RJSON

**語言版本: [English](README.md) | [简体中文](README_zh_CN.md) | [繁體中文](README_zh_TW.md)**

## 屬性：
- `Items[Path]` 基於路徑的讀寫，適用於物件和陣列。

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- `Items[Index]` 陣列讀寫。

        a[3][1] := 'hello';

- `Pairs[Index]` 獲取 JSONObject 下的鍵值對。

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- `Count` 物件或陣列中的條目數量，其他類型返回 0。
- `Index` 陣列中的條目索引，如果不是陣列數據則返回 -1。
- `Key` 如果是鍵值對數據，則返回鍵，否則返回空字符串。
- `Root` 根數據接口。
- `Path` 值的路徑。
- `JSONValue` 包含的 `TJSONValue`。

## 方法
- `ToStr` 轉換為字符串，默認為空字符串。

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('沒有標題');

- `ToInt` 轉換為整數，默認為 0。

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);

- `ToInt64` 轉換為 64 位整數，默認為 0。

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- `ToFloat` 轉換為浮點數（使用 Extended），默認為 0.0。

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- `ToBool` 轉換為布林值，默認為 False。

        var b: Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- `RootIs<T: TJSONValue>` 檢查根是否為特定類型（TJSONObject、TJSONArray 等）。
- `ValueIs<T: TJSONValue>` 檢查當前值是否為特定類型（TJSONObject、TJSONArray 等）。
- `CloneJSONValue` 克隆當前值，如果當前值不存在，則生成 TJSONNull。
- `Reset` 重置為出廠設置。
- `Format` 輸出格式化的 JSON 字符串。

        str1 := RJ.Format(2); // 縮進 2 個空格（默認為 4）
        str2 := RJ.Format(0); // 壓縮格式，無縮進無換行

- `ParseJSONValue` 從字符串加載數據。

        RJ.ParseJSONValue('{"a":1}');

- `LoadFromFile` 從文件加載數據。

        procedure LoadFromFile(
            const AFileName: string;   // JSON 文件名
            AUseBool: boolean = False; // 遇到 JSON 數據中的 true 或 false 時，是否創建 TJSONBool 類型的值
            ARaiseExc: boolean = False // 遇到無效的 JSON 數據時是否拋出異常
        );

- `SaveToFile` 保存到文件。

        procedure SaveToFile(
            const AFileName: string;            // 文件名
            AIndentation: Integer = 4;          // 縮進空格數，0: 不縮進不換行
            AWriteBOM: boolean = False;         // 文件是否添加 BOM 標記
            ATrailingLineBreak: boolean = False // 是否在結尾添加一個空行
        );

## 範例：
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
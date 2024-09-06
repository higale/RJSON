# TRJ - JSON 簡単読み書き
- v0.9.1
- 2024-09-05 by gale
- https://github.com/higale/RJSON

**Languages: [English](README.md) | [简体中文](README_zh_CN.md) | [繁體中文](README_zh_TW.md) | [日本語](README_ja.md) | [한국어](README_ko.md)**

## 属性:
- `Items[Path]` パスベースの読み書き、オブジェクトと配列の両方に対応。

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- `Items[Index]` 配列の読み書き。

        a[3][1] := 'hello';

- `Pairs[Index]` JSONObject 下のキー-値ペアの取得。

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- `Count` オブジェクトまたは配列内のエントリ数、他のタイプでは 0 を返す。
- `Index` 配列内のエントリのインデックス、配列データでない場合は -1 を返す。
- `Key` キー-値ペアデータの場合、キーを返す。それ以外は空文字列を返す。
- `Root` ルートデータへのインターフェース。
- `Path` 値のパス。
- `JSONValue` `TJSONValue` を含む。

## メソッド
- `ToStr` 文字列に変換、デフォルトは空文字列。

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('No Title');

- `ToInt` 整数に変換、デフォルトは 0。

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);

- `ToInt64` 64ビット整数に変換、デフォルトは 0。

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- `ToFloat` 浮動小数点数に変換（Extendedを使用）、デフォルトは 0.0。

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- `ToBool` Booleanに変換、デフォルトは False。

        var b: Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- `RootIs<T: TJSONValue>` ルートが特定のタイプであるかどうかを確認（TJSONObject、TJSONArrayなど）。
- `ValueIs<T: TJSONValue>` 現在の値が特定のタイプであるかどうかを確認（TJSONObject、TJSONArrayなど）。
- `CloneJSONValue` 現在の値をクローン、現在の値がない場合 TJSONNull を生成。
- `Reset` 工場設定に戻す。
- `Format` 形式化された JSON 文字列を出力。

        str1 := RJ.Format(2); // 2スペースのインデント（デフォルトは 4）
        str2 := RJ.Format(0); // 圧縮形式、インデントなし、改行なし

- `ParseJSONValue` 文字列からデータをロード。

        RJ.ParseJSONValue('{"a":1}');

- `LoadFromFile` ファイルからデータをロード。

        procedure LoadFromFile(
            const AFileName: string;   // JSON ファイル名
            AUseBool: boolean = False; // JSON データで true または false を検出したときに TJSONBool 型の値を作成するかどうか
            ARaiseExc: boolean = False // 無効な JSON データを検出したときに例外を発生させるかどうか
        );

- `SaveToFile` ファイルに保存。

        procedure SaveToFile(
            const AFileName: string;            // ファイル名
            AIndentation: Integer = 4;          // インデントのスペース数、0: インデントなし、改行なし
            AWriteBOM: boolean = False;         // ファイルに BOM マーカーを追加するかどうか
            ATrailingLineBreak: boolean = False // 末尾に空行を追加するかどうか
        );

## 例:
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
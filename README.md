# JSON common operation encapsulation
  JSON数据读写封装
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
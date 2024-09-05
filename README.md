# JSON common operation encapsulation
  JSON���ݶ�д��װ
- v0.9.1
- 2024-09-05 by gale
- https://github.com/higale/rjson

## Properties:
- Items[Path] ·����д����Object��Array����Ч

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- Items[Index] �����д

        a[3][1] := 'hello';

- Pairs[Index] ��ȡJSONObject�µļ�ֵ��

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- Count  Object��Array������Ŀ�����������ͷ���0
- Index  ��Ŀ��Array�е�����ֵ��������������Ŀ�ݷ���-1
- Key    ����Ǽ�ֵ�����ݣ����ؼ�ֵ�����򷵻ؿ�
- Root   �����ݽӿ�
- Path   ֵ��·��
- JSONValue ������TJSONValueֵ

## Methods
- ToStr ת��Ϊ�ַ�����ȱʡΪ��

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('û�б���');

- ToInt ת��Ϊ������ȱʡΪ0

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);


- ToInt64 ת��Ϊ64λ������ȱʡΪ0

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- ToFloat ת��Ϊ������(ʹ�� Extended)��ȱʡΪ0.0

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- ToBool ת��Ϊ Boolean��ȱʡΪ False

        var b:Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- RootIs<T: TJSONValue> ���Ƿ���ĳ������(TJSONObject��TJSONArray��)
- ValueIs<T: TJSONValue> ��ǰֵ�Ƿ���ĳ������(TJSONObject��TJSONArray��)
- CloneJSONValue ��¡һ�ݵ�ǰֵ�������ǰֵ�����ڣ������� TJSONNull
- Reset ��λ������״̬
- Format �����ʽ���õ�JSON�ַ���

        str1 := RJ.Format(2); // ����2���ո�(ȱʡ4��)
        str2 := RJ.Format(0); // ѹ����ʽ���������޻���

- ParseJSONValue ���ַ�����������

        RJ.ParseJSONValue('{"a":1}');

- LoadFromFile ���ļ���������

        procedure LoadFromFile(
            const AFileName: string;   // JSON�ļ���
            AUseBool: boolean = False; // ����JSON�����е� true �� false ʱ���Ƿ񴴽� TJSONBool ���͵�ֵ
            ARaiseExc: boolean = False // ������Ч�� JSON ����ʱ�Ƿ��׳��쳣
        );

- SaveToFile ���浽�ļ�

        procedure aveToFile(
            const AFileName: string;            // �ļ���
            AIndentation: Integer = 4;          // �����ո�����0:������������
            AWriteBOM: boolean = False;         // �ļ��Ƿ���� BOM ���
            ATrailingLineBreak: boolean = False // �Ƿ��ڽ�β���һ������
        );

## Example:
    procedure TFormMain.btnTestClick(Sender: TObject);
    var
      RJ, RJ1: TRJSON;
      fTemp: Extended;
    begin
      RJ['title'] := 'hello world! ��ã����磡';
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
# TRJ - JSON Simple Read and Write
- by gale
- https://github.com/higale/RJSON

**Languages: [English](README.md) | [简体中文](README_zh_CN.md) | [繁體中文](README_zh_TW.md) | [日本語](README_ja.md) | [한국어](README_ko.md)**

## 속성:
- `Items[Path]` 경로 기반 읽기/쓰기, 객체와 배열 모두 지원.

        a['x.y[2].z'] := 5;
        b['[3].ok'] := false;

- `Items[Index]` 배열 읽기/쓰기.

        a[3][1] := 'hello';

- `Pairs[Index]` JSONObject 아래의 키-값 쌍 검색.

        for var i := 0 to RJ.Count do
        begin
            Memo1.Lines.Add(RJ.Pairs[i].Key + '=' + RJ.Pairs[i].Format(0));
        end;

- `Count` 객체 또는 배열 내 항목 수, 다른 유형은 0 반환.
- `Index` 배열 내 항목의 인덱스, 배열 데이터가 아닌 경우 -1 반환.
- `Key` 키-값 쌍 데이터일 경우 키 반환, 그렇지 않으면 빈 문자열 반환.
- `Root` 루트 데이터에 대한 인터페이스.
- `Path` 값의 경로.
- `JSONValue` `TJSONValue`를 포함.

## 메서드
- `ToStr` 문자열로 변환, 기본값은 빈 문자열.

        var str: string;

        str := RJ['title'];
        str := RJ['title'].ToStr;
        str := RJ['title'].ToStr('No Title');

- `ToInt` 정수로 변환, 기본값은 0.

        var i: integer;

        i := RJ['num'];
        i := RJ['num'].ToInt;
        i := RJ['num'].ToInt(-1);

- `ToInt64` 64비트 정수로 변환, 기본값은 0.

        var i64: Int64;

        i64 := RJ['num64'];
        i64 := RJ['num64'].ToInt64;
        i64 := RJ['num64'].ToInt64(-1);

- `ToFloat` 부동 소수점 수로 변환 (Extended 사용), 기본값은 0.0.

        var f: Extended;

        f := RJ['num'];
        f := RJ['num'].ToFloat;
        f := RJ['num'].ToFloat(100.0);

- `ToBool` 부울로 변환, 기본값은 False.

        var b: Boolean;

        b := RJ['bool'];
        b := RJ['bool'].ToBool;
        b := RJ['bool'].ToBool(True);

- `RootIs<T: TJSONValue>` 루트가 특정 유형인지 확인 (TJSONObject, TJSONArray 등).
- `ValueIs<T: TJSONValue>` 현재 값이 특정 유형인지 확인 (TJSONObject, TJSONArray 등).
- `CloneJSONValue` 현재 값을 클론, 현재 값이 없는 경우 TJSONNull 생성.
- `Reset` 공장 설정으로 초기화.
- `Format` 형식화된 JSON 문자열 출력.

        str1 := RJ.Format(2); // 2칸 들여쓰기 (기본값은 4칸)
        str2 := RJ.Format(0); // 압축 형식, 들여쓰기 없음, 줄 바꿈 없음

- `ParseJSONValue` 문자열에서 데이터 로드.

        RJ.ParseJSONValue('{"a":1}');

- `LoadFromFile` 파일에서 데이터 로드.

        procedure LoadFromFile(
            const AFileName: string;   // JSON 파일 이름
            AUseBool: boolean = False; // JSON 데이터에서 true 또는 false를 만나면 TJSONBool 타입 값 생성 여부
            ARaiseExc: boolean = False // 유효하지 않은 JSON 데이터를 만났을 때 예외 발생 여부
        );

- `SaveToFile` 파일에 저장.

        procedure SaveToFile(
            const AFileName: string;            // 파일 이름
            AIndentation: Integer = 4;          // 들여쓰기 칸 수, 0: 들여쓰기 없음, 줄 바꿈 없음
            AWriteBOM: boolean = False;         // 파일에 BOM 마커 추가 여부
            ATrailingLineBreak: boolean = False // 마지막에 빈 줄 추가 여부
        );

## 예제:
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
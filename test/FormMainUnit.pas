unit FormMainUnit;

interface

uses
  rjson,
  // System.JSON,
  // System.rtti,
  system.StrUtils,
  system.SysUtils, system.Types, system.UITypes, system.Classes, system.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Ani;

type
  TFormMain = class(TForm)
    Memo1: TMemo;
    btnTest: TButton;
    btnOpen: TButton;
    dlgOpen: TOpenDialog;
    procedure btnTestClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}


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

end.

unit FoMain;

{$M+}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,

  McParam, FrParamGroups,
  McJSON;

type
  TTest = function(Id: Integer; out Msg: string): Boolean of object;

type
  TFormMain = class(TForm)
    PageControl: TPageControl;
    BtClose: TButton;
    BtRun: TButton;
    TabControls: TTabSheet;
    TabReport: TTabSheet;
    MemoReport: TMemo;
    MyGroupBox: TGroupBox;
    MyLabel: TLabel;
    MyEdit: TEdit;
    MyCheckBox: TCheckBox;
    MyRadioButton1: TRadioButton;
    MyMemo: TMemo;
    MyButton: TButton;
    GbxParamGroups: TGroupBox;
    MainMenu: TMainMenu;
    MnFile: TMenuItem;
    MnFileClose: TMenuItem;
    GbxFormSize: TGroupBox;
    LbHeight: TLabel;
    LbWidth: TLabel;
    LbDesc: TLabel;
    TabInternal: TTabSheet;
    LbInternal: TLabel;
    MyRadioButton2: TRadioButton;
    MyListBox: TListBox;
    EdListItem: TEdit;
    BtUp: TButton;
    MemoInternal: TMemo;
    procedure BtRunClick(Sender: TObject);
    procedure BtCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParamGroupsSave(Sender: TObject);
    procedure ParamGroupsLoad(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure MyListBoxClick(Sender: TObject);
    procedure BtUpClick(Sender: TObject);

  private
    FFrameParamGroups: TFrameParamGroups;

  public
    procedure Check(Test: TTest; var Id, Passed, Failed: Integer);
    function TestCreateGroup      (Id: Integer; out Msg: string): Boolean;
    function TestExistsGroup      (Id: Integer; out Msg: string): Boolean;
    function TestSelectGroup      (Id: Integer; out Msg: string): Boolean;
    function TestCopyGroup        (Id: Integer; out Msg: string): Boolean;
    function TestRenameGroup      (Id: Integer; out Msg: string): Boolean;
    function TestDeleteGroup      (Id: Integer; out Msg: string): Boolean;
    function TestInsertItems      (Id: Integer; out Msg: string): Boolean;
    function TestGetItems         (Id: Integer; out Msg: string): Boolean;
    function TestSaveToFile       (Id: Integer; out Msg: string): Boolean;
    function TestMountGroupList   (Id: Integer; out Msg: string): Boolean;
    function TestGettersAndSetters(Id: Integer; out Msg: string): Boolean;
    function TestSetFrom          (Id: Integer; out Msg: string): Boolean;
    function TestGetTo            (Id: Integer; out Msg: string): Boolean;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const C_PARAMS_GROUPS_FILE = 'ParamsGroups.json';

var
  sIndent: string;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // create parameters groups manager.
  FFrameParamGroups := TFrameParamGroups.Create(self, C_PARAMS_GROUPS_FILE);
  FFrameParamGroups.Parent := GbxParamGroups;
  FFrameParamGroups.Align  := alTop;
  FFrameParamGroups.OnSave := ParamGroupsSave;
  FFrameParamGroups.OnLoad := ParamGroupsLoad;
  // by default, run all testes.
  BtRunClick(nil);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LbHeight.Caption := 'Form Height: ' + IntToStr(self.Height);
  LbWidth.Caption  := 'Form Width: '  + IntToStr(self.Width );
end;

procedure TFormMain.MyListBoxClick(Sender: TObject);
begin
  if ( MyListBox.ItemIndex >= 0 ) then
    EdListItem.Text := MyListBox.Items[MyListBox.ItemIndex];
end;

procedure TFormMain.BtUpClick(Sender: TObject);
begin
  if ( MyListBox.ItemIndex >= 0 ) then
    MyListBox.Items[MyListBox.ItemIndex] := EdListItem.Text;
end;

procedure TFormMain.BtCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ParamGroupsSave(Sender: TObject);
var
  frame: TFrameParamGroups;
begin
  frame := (Sender as TFrameParamGroups);
  if ( Assigned(frame) ) then
  begin
    frame.McParam.SetFrom(MyEdit        , 'Text'   );
    frame.McParam.SetFrom(MyCheckBox    , 'Checked');
    frame.McParam.SetFrom(MyRadioButton1, 'Checked');
    frame.McParam.SetFrom(MyRadioButton2, 'Checked');
    frame.McParam.SetFrom(self          , 'Height' );
    frame.McParam.SetFrom(self          , 'Width'  );
    // example with the usual set param.
    frame.McParam.Items.S['MyMemo.Lines.Text'] := McJsonEscapeString(MyMemo.Lines.Text );
    frame.McParam.Items.S['MyListBox.Items.0'] := McJsonEscapeString(MyListBox.Items[0]);
    frame.McParam.Items.S['MyListBox.Items.1'] := McJsonEscapeString(MyListBox.Items[1]);
  end;

end;

procedure TFormMain.ParamGroupsLoad(Sender: TObject);
var
  frame: TFrameParamGroups;
begin
  frame := (Sender as TFrameParamGroups);
  if ( Assigned(frame) ) then
  begin
    frame.McParam.GetTo(MyEdit        , 'Text'   , 'Some text here...');
    frame.McParam.GetTo(MyCheckBox    , 'Checked', True               );
    frame.McParam.GetTo(MyRadioButton1, 'Checked', True               );
    frame.McParam.GetTo(MyRadioButton2, 'Checked', True               );
    frame.McParam.GetTo(self          , 'Height' , 462                );
    frame.McParam.GetTo(self          , 'Width'  , 726                );
    // example with the usual set param.
    MyMemo.Lines.Text  := McJsonUnEscapeString(frame.McParam.Items.S['MyMemo.Lines.Text']);
    MyListBox.Items[0] := McJsonUnEscapeString(frame.McParam.Items.S['MyListBox.Items.0']);
    MyListBox.Items[1] := McJsonUnEscapeString(frame.McParam.Items.S['MyListBox.Items.1']);
  end;
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  if ( Assigned(FFrameParamGroups) ) then
  begin
    MemoInternal.Clear;
    MemoInternal.Lines.Text := FFrameParamGroups.McParam.GetInternalJSON(True);
  end;
end;

procedure TFormMain.BtRunClick(Sender: TObject);
var
  Id, TotalPassed, TotalFailed: Integer;
begin
  ReportMemoryLeaksOnShutdown := true;

  Id := 0;
  TotalPassed := 0;
  TotalFailed := 0;

  MemoReport.Clear;

  // [PASS] [
  sIndent := '       ';

  Check(TestCreateGroup      , Id, TotalPassed, TotalFailed);
  Check(TestExistsGroup      , Id, TotalPassed, TotalFailed);
  Check(TestSelectGroup      , Id, TotalPassed, TotalFailed);
  Check(TestCopyGroup        , Id, TotalPassed, TotalFailed);
  Check(TestRenameGroup      , Id, TotalPassed, TotalFailed);
  Check(TestDeleteGroup      , Id, TotalPassed, TotalFailed);
  Check(TestInsertItems      , Id, TotalPassed, TotalFailed);
  Check(TestGetItems         , Id, TotalPassed, TotalFailed);
  Check(TestSaveToFile       , Id, TotalPassed, TotalFailed);
  Check(TestMountGroupList   , Id, TotalPassed, TotalFailed);
  Check(TestGettersAndSetters, Id, TotalPassed, TotalFailed);
  Check(TestSetFrom          , Id, TotalPassed, TotalFailed);
  Check(TestGetTo            , Id, TotalPassed, TotalFailed);

  if TotalFailed > 0 then
    MemoReport.Lines.Add(IntToStr(TotalFailed) + ' tests FAILED')
  else
    MemoReport.Lines.Add('All tests PASSED');

  PageControl.ActivePage := TabReport;  
end;

procedure TFormMain.Check(Test: TTest; var Id, Passed, Failed: Integer);
var
  S: string;
begin
  if Test(Id, S) then
  begin
    Inc(Passed);
    MemoReport.Lines.Add('[PASS] ' + S);
  end
  else
  begin
    Inc(Failed);
    MemoReport.Lines.Add('[FAIL] ' + S);
  end;
  Inc(Id);
end;

function TFormMain.TestCreateGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': create group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      // test.
      Result := ( prm.ExistsGroup('group-1') and
                  prm.ExistsGroup('group-2') );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestExistsGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': group exists';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      // test.
      Result := ( (prm.ExistsGroup('group-1') = True ) and
                  (prm.ExistsGroup('group-2') = True ) and
                  (prm.ExistsGroup('group-3') = False) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;


function TFormMain.TestSelectGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': select group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // tests
      Result := true;
      // test 1
      prm.CreateGroup('group-1');
      Result := Result and ( (prm.Selected = 'group-1') );
      // test 2
      prm.CreateGroup('group-2');
      Result := Result and ( (prm.Selected = 'group-2') );
      // test 3
      prm.GetGroup('group-1');
      Result := Result and ( (prm.Selected = 'group-1') );
      // test 4
      prm.Selected := 'group-2';
      Result := Result and ( (prm.Selected = 'group-2') );
      // test 5
      prm.Selected := 'group-no-exist';
      Result := Result and ( (prm.Selected = '' ) and
                             (prm.Items    = nil) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestCopyGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': copy group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // tests
      Result := true;
      // test 1
      prm.CreateGroup('group-1');
      prm.Items.S['s1'] := 'v1';
      prm.Items.I['i1'] := 1;
      Result := Result and ( (prm.Selected              = 'group-1'     ) and
                             (prm.Items['s1'].AsString  = 'v1'          ) and
                             (prm.Items['i1'].AsInteger = 1             ) );
      // test 2
      prm.CopyGroup('group-1');
      Result := Result and ( (prm.Selected              = 'group-1-copy') and
                             (prm.Items['s1'].AsString  = 'v1'          ) and
                             (prm.Items['i1'].AsInteger = 1             ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestRenameGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': rename group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // tests
      Result := true;
      // test 1
      prm.CreateGroup('group-1');
      prm.Items.S['s1'] := 'v1';
      prm.Items.I['i1'] := 1;
      Result := Result and ( (prm.Selected              = 'group-1' ) and
                             (prm.Items['s1'].AsString  = 'v1'      ) and
                             (prm.Items['i1'].AsInteger = 1         ) );
      // test 2
      prm.RenameGroup('group-1', 'group-2');
      Result := Result and ( (prm.Selected              = 'group-2' ) and
                             (prm.Items['s1'].AsString  = 'v1'      ) and
                             (prm.Items['i1'].AsInteger = 1         ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestDeleteGroup(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': delete group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // tests
      Result := true;
      // test 1
      prm.CreateGroup('group-1');
      prm.Items.S['s1'] := 'v1';
      prm.Items.I['i1'] := 1;
      Result := Result and ( (prm.Selected              = 'group-1' ) and
                             (prm.Items['s1'].AsString  = 'v1'      ) and
                             (prm.Items['i1'].AsInteger = 1         ) );
      // test 2
      prm.CreateGroup('group-2');
      prm.Items.S['s2'] := 'v2';
      prm.Items.I['i2'] := 2;
      Result := Result and ( (prm.Selected              = 'group-2' ) and
                             (prm.Items['s2'].AsString  = 'v2'      ) and
                             (prm.Items['i2'].AsInteger = 2         ) );
      // test 3
      prm.DeleteGroup(1);
      Result := Result and ( (prm.Selected = '' ) );
      // test 3
      prm.DeleteGroup('group-1');
      Result := Result and ( (prm.Selected = '' ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestInsertItems(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': insert items';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group');
      prm.Items.S['str1' ] := 'value1';
      prm.Items.I['int1' ] := 1;
      prm.Items.B['bool1'] := false;
      // test.
      Result := ( (prm.Items['str1' ].AsString  = 'value1') and
                  (prm.Items['int1' ].AsInteger = 1       ) and
                  (prm.Items['bool1'].AsBoolean = false   ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestGetItems(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': get group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // create and fill some groups
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      // get/select and set items
      prm.GetGroup('group-1');
      prm.Items.S['str1'] := 'value1';
      prm.GetGroup('group-2');
      prm.Items.S['str2'] := 'value2';
      prm.GetGroup('group-3', true);
      prm.Items.S['str3'] := 'value3';
      // tests
      Result := true;
      // test 1.
      prm.GetGroup('group-1');
      Result := Result and ( prm.Selected = 'group-1' );
      Result := Result and ( (prm.Items['str1'].AsString = 'value1') );
      // test 2.
      prm.GetGroup('group-2');
      Result := Result and ( prm.Selected = 'group-2' );
      Result := Result and ( (prm.Items['str2'].AsString = 'value2') );
      // test 4.
      prm.GetGroup('group-3');
      Result := Result and ( prm.Selected = 'group-3' );
      Result := Result and ( (prm.Items['str3'].AsString = 'value3') );
      // test 3.
      prm.GetGroup(0);
      Result := Result and ( prm.Selected = 'group-1' );
      Result := Result and ( (prm.Items['str1'].AsString = 'value1') );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestSaveToFile(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
  fileName: string;
begin
  Msg := 'Test ' + IntToStr(Id) + ': save to file';
  prm := nil;
  fileName := '.\params.json';
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group-1');
      prm.Items.S['str1' ] := 'value1';
      prm.Items.I['int1' ] := 1;
      prm.Items.B['bool1'] := false;
      prm.CreateGroup('group-2');
      prm.Items.S['str2' ] := 'value2';
      prm.Items.I['int2' ] := 2;
      prm.Items.B['bool2'] := true;
      prm.GetGroup('group-2');
      // file operations
      SysUtils.DeleteFile(fileName);
      prm.SaveToFile(fileName);
      // test.
      Result := ( FileExists(fileName) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestMountGroupList(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
  strl: TStringList;
begin
  Msg  := 'Test ' + IntToStr(Id) + ': MountGroupList';
  prm  := nil;
  strl := nil;
  try
    prm  := TMcParam.Create('');
    strl := TStringList.Create();
    try
      // create some groups
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      // fill string list
      prm.MountGroupList(strl);
      // test.
      Result := ( (strl.Count = 2) and
                  (strl.Strings[0] = 'group-1') and
                  (strl.Strings[1] = 'group-2') );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
    strl.Free;
  end;
end;

function TFormMain.TestGettersAndSetters(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test ' + IntToStr(Id) + ': getters and setters';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      // create some groups
      prm.CreateGroup('group-1');
      prm.GetGroup('group-1');
      // add some items
      prm.Items.S['str1' ] := 'value1';
      prm.Items.S['str2' ] := prm.GetS('not-exist', 'value2');
      prm.Items.I['int1' ] := prm.GetI('not-exist', 1       );
      prm.Items.B['bool1'] := prm.GetB('not-exist', true    );
      prm.Items.D['frac1'] := prm.GetD('not-exist', 1.0     );
      prm.Items.N['nul1' ] := prm.GetN('not-exist', 'null'  );
      // tests
      Result := ( (prm.Items.S['str1' ] = 'value1') and
                  (prm.Items.S['str2' ] = 'value2') and
                  (prm.Items.I['int1' ] = 1       ) and
                  (prm.Items.B['bool1'] = true    ) and
                  (prm.Items.D['frac1'] = 1.0     ) and
                  (prm.Items.N['nul1' ] = 'null'  ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestSetFrom(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
  fileName: string;
begin
  Msg := 'Test ' + IntToStr(Id) + ': SetFrom component method';
  prm := nil;
  try
    prm := TMcParam.Create('');
    fileName := '.\params-comp.json';
    try
      // create some groups
      prm.CreateGroup('comps');
      // tests
      Result := true;
      // test 1
      Result := Result and (prm.Items.Count = 0);
      prm.SetFrom(MyButton, 'Caption', 'Overwrite');
      Result := Result and (
        (prm.Items['MyButton.Caption'].AsString = 'Overwrite') );
      Result := Result and (prm.Items.Count = 1);
      // test 2
      // set params from control properties: from Control.Property to Items.
      prm.SetFrom(MnFile        , 'Caption');
      prm.SetFrom(MnFileClose   , 'Caption');
      prm.SetFrom(MyGroupBox    , 'Caption');
      prm.SetFrom(MyGroupBox    , 'Height' );
      prm.SetFrom(MyLabel       , 'Caption');
      prm.SetFrom(MyEdit        , 'Text'   );
      prm.SetFrom(MyCheckBox    , 'Checked');
      prm.SetFrom(MyRadioButton1, 'Checked');
      prm.SetFrom(MyRadioButton2, 'Checked');
      prm.SetFrom(MyButton      , 'Caption');
      // example with the usual set param.
      prm.Items.S['MyMemo.Lines.Text'] := McJsonEscapeString(MyMemo.Lines.Text);
      // file operations
      SysUtils.DeleteFile(fileName);
      prm.SaveToFile(fileName);
      // test.
      Result := Result and (
        (prm.Items['MnFile.Caption'        ].AsString  = MnFile.Caption        ) and
        (prm.Items['MnFileClose.Caption'   ].AsString  = MnFileClose.Caption   ) and
        (prm.Items['MyGroupBox.Caption'    ].AsString  = MyGroupBox.Caption    ) and
        (prm.Items['MyGroupBox.Height'     ].AsInteger = MyGroupBox.Height     ) and
        (prm.Items['MyLabel.Caption'       ].AsString  = MyLabel.Caption       ) and
        (prm.Items['MyEdit.Text'           ].AsString  = MyEdit.Text           ) and
        (prm.Items['MyCheckBox.Checked'    ].AsBoolean = MyCheckBox.Checked    ) and
        (prm.Items['MyRadioButton1.Checked'].AsBoolean = MyRadioButton1.Checked) and
        (prm.Items['MyRadioButton2.Checked'].AsBoolean = MyRadioButton2.Checked) and
        (prm.Items['MyButton.Caption'      ].AsString  = MyButton.Caption      ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

function TFormMain.TestGetTo(Id: Integer; out Msg: string): Boolean;
var
  prm: TMcParam;
  fileName: string;
begin
  Msg := 'Test ' + IntToStr(Id) + ': GetTo component method';
  prm := nil;
  try
    prm := TMcParam.Create('');
    fileName := '.\params-comp.json';
    try
      // file operations
      prm.LoadFromFile(fileName);
      prm.Selected := 'comps';
      // get params to control properties: from Items to Control.Property.
      prm.GetTo(MnFile        , 'Caption', 'FILE'     );
      prm.GetTo(MnFileClose   , 'Caption', 'CLOSE'    );
      prm.GetTo(MyGroupBox    , 'Caption', 'Default-1');
      prm.GetTo(MyGroupBox    , 'Height' , 100        );
      prm.GetTo(MyLabel       , 'Caption', 'Default-2');
      prm.GetTo(MyEdit        , 'Text'   , 'Default-3');
      prm.GetTo(MyCheckBox    , 'Checked', True       );
      prm.GetTo(MyRadioButton1, 'Checked', False      );
      prm.GetTo(MyRadioButton2, 'Checked', True       );
      prm.GetTo(MyButton      , 'Caption', 'Default-4'); // Will use Default-4
      // example with the usual get param.
      MyMemo.Lines.Text := McJsonUnEscapeString( prm.Items.S['MyMemo.Lines.Text'] );
      // test.
      Result := (
        (prm.Items['MnFile.Caption'        ].AsString  = MnFile.Caption        ) and
        (prm.Items['MnFileClose.Caption'   ].AsString  = MnFileClose.Caption   ) and
        (prm.Items['MyGroupBox.Caption'    ].AsString  = MyGroupBox.Caption    ) and
        (prm.Items['MyGroupBox.Height'     ].AsInteger = MyGroupBox.Height     ) and
        (prm.Items['MyLabel.Caption'       ].AsString  = MyLabel.Caption       ) and
        (prm.Items['MyEdit.Text'           ].AsString  = MyEdit.Text           ) and
        (prm.Items['MyCheckBox.Checked'    ].AsBoolean = MyCheckBox.Checked    ) and
        (prm.Items['MyRadioButton1.Checked'].AsBoolean = MyRadioButton1.Checked) and
        (prm.Items['MyRadioButton2.Checked'].AsBoolean = MyRadioButton2.Checked) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Msg := Msg + #13#10 + sIndent + 'JSON = ' + prm.GetInternalJSON();
        Result := False;
      end;
    end;
  finally
    prm.Free;
  end;
end;

end.


//    // array of objects.
//    N.AsJSON := '{"l":[ {"a":{"ka":"va"}}, {"b":{"kb":"vb"}} ], "s":"b"}';
//    // mount a equal json object.
//    M.Add('l', jitArray);
//    I := M['l'].Add('a', jitObject); I := I['a']; I.S['ka'] := 'va';
//    I := M['l'].Add('b', jitObject); I := I['b']; I.S['kb'] := 'vb';
//    M.Add('s').AsString := 'b';
//    // check
//    I := M['l'].Values['b'];// Items[1];
//    Result := Result and ( N.AsJSON = M.AsJSON            );
//    Result := Result and ( I.AsJSON = '{"b":{"kb":"vb"}}' );
//    Result := Result and ( I['b'].HasKey('kb') = True     );

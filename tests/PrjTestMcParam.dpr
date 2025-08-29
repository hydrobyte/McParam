program PrjTestMcParam;

{$APPTYPE CONSOLE}
{$M+}

uses
  Classes,
  SysUtils,
  McParam in '..\src\McParam.pas',
  McJSON in '..\src\McJSON.pas';

type
  TTest = function(out Msg: string): Boolean;

var
  sIndent: string;

procedure Check(Test: TTest; var Passed, Failed: Integer);
var
  S: string;
begin
  if Test(S) then
  begin
    Inc(Passed);
    WriteLn('[PASS] ', S);
  end
  else
  begin
    Inc(Failed);
    WriteLn('[FAIL] ', S);
  end;
end;

function TestCreateGroup(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: create group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group');
      // test.
      Result := ( prm.ExistsGroup('group') );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestSelectGroup(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: select group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      prm.CreateGroup('group');
      prm.Selected := 'group';
      // test.
      Result := ( (prm.Selected = 'group') );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestCountGroups(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: count group';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      Result := true;
      // test.
      Result := Result and ( (prm.Count = 0) );
      // add a new group.
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      prm.Selected := 'group-1';
      // test.
      Result := Result and ( (prm.Count = 2) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestClearAllGroups(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: clear all groups';
  prm := nil;
  try
    prm := TMcParam.Create('');
    try
      Result := True;
      // test.
      Result := Result and ( (prm.Count = 0) );
      // add a new group.
      prm.CreateGroup('group-1');
      prm.CreateGroup('group-2');
      prm.Selected := 'group-1';
      // test.
      Result := Result and ( (prm.Count = 2) );
      // clear all
      prm.Clear;
      // test.
      Result := Result and ( (prm.Count = 0) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestInsertItems(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: insert items';
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
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestGetGroups(out Msg: string): Boolean;
var
  prm: TMcParam;
begin
  Msg := 'Test: get groups';
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
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestSaveToFile(out Msg: string): Boolean;
var
  prm: TMcParam;
  fileName: string;
begin
  Msg := 'Test: save to file';
  prm := nil;
  fileName := '.\params.txt';
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
      prm.Selected := 'group-2';
      // file operations
      SysUtils.DeleteFile(fileName);
      prm.SaveToFile(fileName);
      // test.
      Result := ( FileExists(fileName) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestMountGroupList(out Msg: string): Boolean;
var
  prm: TMcParam;
  strl: TStringList;
begin
  Msg  := 'Test: MountGroupList';
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
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

function TestGettersSetters(out Msg: string): Boolean;
var
  prm: TMcParam;
  strl: TStringList;
begin
  Msg  := 'Test: getters and setters';
  prm  := nil;
  strl := nil;
  try
    prm  := TMcParam.Create('');
    strl := TStringList.Create();
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
      Result := ( (prm.Items.S['str1' ]  = 'value1') and
                  (prm.Items.S['str2' ]  = 'value2') and
                  (prm.Items.I['int1' ]  = 1       ) and
                  (prm.Items.B['bool1'] = true     ) and
                  (prm.Items.D['frac1'] = 1.0      ) and
                  (prm.Items.N['nul1' ] = 'null'   ) );
      // log JSON
      if ( Result = False ) then
        Msg := Msg + #13#10 + 'JSON = ' + prm.GetInternalJSON();
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

procedure RunTests;
var
  TotalPassed, TotalFailed: Integer;
begin
  ReportMemoryLeaksOnShutdown := true;

  TotalPassed := 0;
  TotalFailed := 0;

  // [PASS] [
  sIndent := '       ';

  Check(TestCreateGroup   , TotalPassed, TotalFailed);
  Check(TestSelectGroup   , TotalPassed, TotalFailed);
  Check(TestCountGroups   , TotalPassed, TotalFailed);
  Check(TestClearAllGroups, TotalPassed, TotalFailed);
  Check(TestInsertItems   , TotalPassed, TotalFailed);
  Check(TestGetGroups     , TotalPassed, TotalFailed);
  Check(TestSaveToFile    , TotalPassed, TotalFailed);
  Check(TestMountGroupList, TotalPassed, TotalFailed);
  Check(TestGettersSetters, TotalPassed, TotalFailed);

  WriteLn;
  
  if TotalFailed > 0 then
    WriteLn(TotalFailed, ' tests FAILED')
  else
    WriteLn('All tests PASSED');
end;

begin
  RunTests;
  Readln;
end.
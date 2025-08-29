(*******************************************************************************

  The MIT License (MIT)

  Copyright (c) 2021 - 2025,  HydroByte Software

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*******************************************************************************)

unit McParam;

interface

uses
  Classes, SysUtils,
  Controls, StdCtrls,
  TypInfo, Variants,
  
  McJSON;

const
  C_VAR_NIL = '[MyNil]';

type
  TMcParam = class
  private
    FFileName: string;
    FGroups: TMcJsonItem;
    FItems : TMcJsonItem;

    function  fGetSelectedGroup: string;
    procedure fSetSelectedGroup(aValue: string);
    function  fGetCountGroup: Integer;

  public
    property Items   : TMcJsonItem read FItems            write FItems;
    property Selected: string      read fGetSelectedGroup write fSetSelectedGroup;
    property Count   : Integer     read fGetCountGroup;

    constructor Create(aFileName: string);
    destructor  Destroy; override;

    // save and load
    procedure Save;
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);

    // other
    procedure Clear;

    // getters with defaults
    function GetI(const aKey: string; const aDfl: Integer = 0    ): Integer;
    function GetB(const aKey: string; const aDfl: Boolean = true ): Boolean;
    function GetD(const aKey: string; const aDfl: Double  = 0.0  ): Double ;
    function GetS(const aKey: string; const aDfl: string = ''    ): string ;
    function GetN(const aKey: string; const aDfl: string = 'null'): string ;

    // control helpers
    procedure GetTo(aComp: TComponent; const aProp: string; const aDfl: Variant);
    procedure SetFrom(aComp: TComponent; const aProp: string); overload;
    procedure SetFrom(aComp: TComponent; const aProp: string; const aValue: Variant); overload;

    // group methods
    function  CreateGroup(aName: string): TMcJsonItem;
    function  ExistsGroup(aName: string): Boolean;
    function  GetGroup(aName: string; forceCreate: Boolean = false): TMcJsonItem; overload;
    function  GetGroup(aIndex: Integer): TMcJsonItem; overload;
    procedure CopyGroup(aName: string);
    procedure RenameGroup(aName, aNameNew: string);
    procedure DeleteGroup(aName: string); overload;
    procedure DeleteGroup(aIndex: Integer); overload;

    // other
    procedure MountGroupList(aStrings: TStrings);
    function  GetInternalJSON(aHuman: Boolean = false): string;
  end;

implementation

const
  C_PRM_GROUPS   = 'Groups';
  C_PRM_SELECTED = 'Selected';

{ ---------------------------------------------------------------------------- }
{ TMcParam }
{ ---------------------------------------------------------------------------- }
function TMcParam.fGetSelectedGroup: string;
begin
  Result := FGroups.S[C_PRM_SELECTED]; // { ... {"Selected":"a"} )
end;

procedure TMcParam.fSetSelectedGroup(aValue: string);
var
  idx: Integer;
begin
  idx := FGroups[C_PRM_GROUPS].IndexOf(aValue); // { "g":[ {"a":{}}, ...] } -> 0
  if ( idx >= 0 ) then
  begin
    FItems := FGroups[C_PRM_GROUPS].Items[idx].Values[aValue]; // "a":{}
    FGroups.S[C_PRM_SELECTED] := aValue;                       // "a"
  end
  else
  begin
    FItems := nil;
    FGroups.S[C_PRM_SELECTED] := '';
  end;
end;


function TMcParam.fGetCountGroup: Integer;
begin
  Result := FGroups[C_PRM_GROUPS].Count;
end;

{ ---------------------------------------------------------------------------- }
{ TMcParam - Public methods }
{ ---------------------------------------------------------------------------- }

constructor TMcParam.Create(aFileName: string);
begin
  FFileName := aFileName;
  FGroups := TMcJsonItem.Create;
  FGroups.Add(C_PRM_GROUPS, jitArray);  // { "g":[] }
  LoadFromFile(aFileName);
end;

destructor TMcParam.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

procedure TMcParam.Save;
begin
  SaveToFile(FFileName);
end;

procedure TMcParam.SaveToFile(const aFileName: string);
begin
  // save to file with human reading and no UTF8.
  FGroups.SaveToFile(aFileName, true, false);
end;

procedure TMcParam.LoadFromFile(const aFileName: string);
begin
  if (not FileExists(aFileName)) then Exit;
  FGroups.LoadFromFile(aFileName, false); // asUTF8 = false
  FFileName := aFileName;
end;

procedure TMcParam.Clear;
var
  grs: TMcJsonItem;
  i: integer;
begin
  grs := FGroups[C_PRM_GROUPS];
  grs.Clear;
  fSetSelectedGroup('');
end;

function TMcParam.GetI(const aKey: string; const aDfl: Integer): Integer;
begin
  if ( FItems.HasKey(aKey) )
    then Result := FItems.I[aKey]
    else Result := aDfl;
end;

function TMcParam.GetB(const aKey: string; const aDfl: Boolean): Boolean;
begin
  if ( FItems.HasKey(aKey) )
    then Result := FItems.B[aKey]
    else Result := aDfl;
end;

function TMcParam.GetD(const aKey: string; const aDfl: Double): Double;
begin
  if ( FItems.HasKey(aKey) )
    then Result := FItems.D[aKey]
    else Result := aDfl;
end;

function TMcParam.GetS(const aKey, aDfl: string): string;
begin
  if ( FItems.HasKey(aKey) )
    then Result := FItems.S[aKey]
    else Result := aDfl;
end;

function TMcParam.GetN(const aKey, aDfl: string): string;
begin
  if ( FItems.HasKey(aKey) )
    then Result := FItems.S[aKey]
    else Result := aDfl;
end;

procedure TMcParam.GetTo(aComp: TComponent; const aProp: string; const aDfl: Variant);
var
  propInfo: PPropInfo;
  vVal: Variant;
  sKey: string;
begin
  try
    // get Control.Property value using RTTI
    propInfo := GetPropInfo(aComp.ClassInfo, aProp);
    if ( propInfo <> nil ) then
    begin
      // if not has key, use default
      sKey := aComp.Name + '.' + aProp;
      if ( not FItems.HasKey(sKey) ) then
        vVal := aDfl
      else
      begin
        // get property actual value, just to identify its type
        vVal := GetPropValue(aComp, aProp);
        // get paramter value
        if      (VarType(vVal) = varInteger) then vVal := FItems.I[sKey]
        else if (VarType(vVal) = varBoolean) then vVal := FItems.B[sKey]
        else if (VarType(vVal) = varString ) then vVal := FItems.S[sKey]
        else if (VarType(vVal) = varUString) then vVal := FItems.S[sKey];
      end;
      // set property value: by parameter or default
      SetPropValue(aComp, aProp, vVal);
      // TEMP TO-DO: this will create Control.Property param. Not for now.
      //GetP(aComp, aProp);
    end;
  except
    ;
  end;
end;

procedure TMcParam.SetFrom(aComp: TComponent; const aProp: string);
var
  propInfo: PPropInfo;
  vVal: Variant;
  sKey: string;
begin
  try
    // get Control.Property value using RTTI
    propInfo := GetPropInfo(aComp.ClassInfo, aProp);
    if ( propInfo <> nil ) then
    begin
      vVal := GetPropValue(aComp, aProp); // value of Control.Property
      sKey := aComp.Name + '.' + aProp;   // key "Control.Property"
      // set/create key=vVal
      if      (VarType(vVal) = varInteger) then FItems.I[sKey] := vVal
      else if (VarType(vVal) = varBoolean) then FItems.B[sKey] := vVal
      else if (VarType(vVal) = varString ) then FItems.S[sKey] := vVal
      else if (VarType(vVal) = varUString) then FItems.S[sKey] := vVal;
      //ShowMessage('JSON = ' + FItems.ToString(false));
    end;
  except
    ;
  end;
end;

procedure TMcParam.SetFrom(aComp: TComponent; const aProp: string; const aValue: Variant);
var
  propInfo: PPropInfo;
  vVal: Variant;
  sKey: string;
begin
  try
    // get Control.Property value using RTTI
    propInfo := GetPropInfo(aComp, aProp);
    if ( propInfo <> nil ) then
    begin
      if ( not VarIsEmpty(aValue) )
        then vVal := aValue
        else vVal := GetPropValue(aComp, aProp); // value of Control.Property
      sKey := aComp.Name + '.' + aProp;          // key "Control.Property"
      // set/create "key":"vVal"
      if      (VarType(aValue) = varInteger) then FItems.I[sKey] := vVal
      else if (VarType(aValue) = varBoolean) then FItems.B[sKey] := vVal
      else if (VarType(aValue) = varString ) then FItems.S[sKey] := vVal
      else if (VarType(aValue) = varUString) then FItems.S[sKey] := vVal;
    end;
  except
    ;
  end;
end;

function TMcParam.CreateGroup(aName: string): TMcJsonItem;
begin
  FItems := FGroups[C_PRM_GROUPS].Add(aName, jitObject); // { "g":[{"a":{}}] }
  fSetSelectedGroup(aName);                              // select "a"
  Result := FItems;
end;

function TMcParam.ExistsGroup(aName: string): Boolean;
var
  idx: Integer;
begin
  idx := FGroups[C_PRM_GROUPS].IndexOf(aName);
  Result := ( idx >= 0 );
end;

function TMcParam.GetGroup(aName: string; forceCreate: Boolean): TMcJsonItem;
begin
  fSetSelectedGroup(aName);
  if ( (FItems = nil) and forceCreate ) then
  begin
    CreateGroup(aName);
    fSetSelectedGroup(aName);
  end;
  Result := FItems;
end;

function TMcParam.GetGroup(aIndex: Integer): TMcJsonItem;
var
  grs: TMcJsonItem;
begin
  grs := FGroups[C_PRM_GROUPS];
  if ( (aIndex >= 0          ) and
       (aIndex <  grs.Count) ) then
  begin
    // Items[aIndex] = {"a":{}} - Items[aIndex].Items[0] = "a":{}
    fSetSelectedGroup(grs.Items[aIndex].Items[0].Key);
  end;
  Result := FItems;
end;

procedure TMcParam.CopyGroup(aName: string);
var
  iFr, iTo: TMcJsonItem;
  sNameNew: string;
begin
  sNameNew := aName + '-copy';
  CreateGroup(sNameNew);
  iFr := FGroups[C_PRM_GROUPS].Values[aName   ]; // { "aName":{} }
  iTo := FGroups[C_PRM_GROUPS].Values[sNameNew]; // { "sNameNew":{} }
  // clone object
  iTo.AsJSON := iFr.AsJSON;
  // update key
  iTo[aName].Key := sNameNew; // "aName":{...} to "sNameNew":{...}
  // select cloned
  fSetSelectedGroup(sNameNew);
end;

procedure TMcParam.RenameGroup(aName, aNameNew: string);
var
  itm: TMcJsonItem;
begin
  itm := GetGroup(aNameNew);
  if (itm = nil) then
  begin
    itm := GetGroup(aName);
    itm.Key := aNameNew;
    // select cloned
    fSetSelectedGroup(aNameNew);
  end;
end;

procedure TMcParam.DeleteGroup(aName: string);
var
  grs: TMcJsonItem;
begin
  grs := FGroups[C_PRM_GROUPS];
  if ( grs.Delete(aName) ) then
    fSetSelectedGroup('');
end;

procedure TMcParam.DeleteGroup(aIndex: Integer);
var
  grs: TMcJsonItem;
begin
  grs := FGroups[C_PRM_GROUPS];
  if ( (aIndex >= 0          ) and
       (aIndex <  grs.Count) ) then
  begin
    if ( grs.Delete(aIndex) ) then
      fSetSelectedGroup('');
  end;
end;

procedure TMcParam.MountGroupList(aStrings: TStrings);
var
  grs: TMcJsonItem;
  i: integer;
begin
  if ( not Assigned(aStrings) ) then Exit;
  aStrings.Clear;
  try
  begin
    LoadFromFile(FFileName);
    grs := FGroups[C_PRM_GROUPS];
    for i := 0 to (grs.Count - 1) do
      aStrings.Add(grs.Items[i].Items[0].Key);
  end;
  except
  end;
end;

function TMcParam.GetInternalJSON(aHuman: Boolean): string;
begin
  Result := FGroups.ToString(aHuman);
end;

end.

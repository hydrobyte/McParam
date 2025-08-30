unit FrParamGroupsLz;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, UITypes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  McParam;

type
  TFrameParamGroups = class(TFrame)
    LbName: TLabel;
    CbxGroup: TComboBox;
    BtSave: TSpeedButton;
    BtDel: TSpeedButton;
    procedure BtSaveClick(Sender: TObject);
    procedure BtDelClick(Sender: TObject);
    procedure CbxGroupChange(Sender: TObject);

  private
    fEventsFreezed: Boolean;
    fMcParam: TMcParam;
    fOnSave: TNotifyEvent;
    fOnLoad: TNotifyEvent;
    FGroupSelectedName: string;

    procedure saveGroupItem(const aName: string);
    procedure deleteGroupItem(const aName: string);
    procedure loadGroupItem(aIndex: Integer);
    procedure loadGroupList();
    procedure selectLastGroup();

  public
    property McParam: TMcParam read fMcParam;
    property OnSave: TNotifyEvent read fOnSave write fOnSave;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;

    constructor Create(AOwner: TComponent; const aFilePath: string); overload; virtual;
    destructor  Destroy; override;
  end;

  // Auxiliary
  procedure Log(const aMsg: string);

implementation

{$R *.lfm}

procedure Log(const aMsg: string);
begin
  // do nothing
end;

{ ---------------------------------------------------------------------------- }
{ TFrameParamGroups }
{ ---------------------------------------------------------------------------- }

procedure TFrameParamGroups.saveGroupItem(const aName: string);
begin
  if (aName = '') then exit;
  try
    // Get or create (forceCreate = true)
    FMcParam.GetGroup(aName, true);
    // Call OnSave for external code.
    if ( Assigned(fOnSave) ) then fOnSave(self);
    // Persist changes
    FMcParam.Save();
    // Set selected Conn item
    FMcParam.Selected := aName;
  except
    on E: Exception do
      Log( '[Exception] saveGroupItem: ' + E.Message );
  end;
end;

procedure TFrameParamGroups.deleteGroupItem(const aName: string);
begin
  try
    fMcParam.DeleteGroup(AName);
    fMcParam.Save();
  except
    on E: Exception do
      Log( '[Exception] deleteGroupItem: ' + E.Message );
  end;
end;

procedure TFrameParamGroups.loadGroupItem(aIndex: Integer);
begin
  if (aIndex < 0) then exit;
  try
    fMcParam.GetGroup(aIndex);
    // Call OnLoad for external code.
    if ( Assigned(fOnLoad) ) then fOnLoad(self);
  except
    on E: Exception do
      Log( '[Exception] loadGroupItem: ' + E.Message );
  end;
end;

procedure TFrameParamGroups.loadGroupList();
begin
  try
    // Mount Group combobox list.
    fMcParam.MountGroupList(CbxGroup.Items);
  except
    on E: Exception do
      Log( '[Exception] loadGroupList: ' + E.Message );
  end;
end;

procedure TFrameParamGroups.selectLastGroup();
begin
  try
    CbxGroup.ItemIndex := CbxGroup.Items.IndexOf(fMcParam.Selected);
    CbxGroupChange(nil);
  except
    on E: Exception do
      Log( '[Exception] selectLastGroup: ' + E.Message );
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TFrameParamGroups - Public methods }
{ ---------------------------------------------------------------------------- }

constructor TFrameParamGroups.Create(AOwner: TComponent; const aFilePath: string);
begin
  inherited Create(AOwner);
  fMcParam := TMcParam.Create(aFilePath);
  // Load Conn presets from file.
  loadGroupList();
end;

destructor TFrameParamGroups.Destroy;
begin
  try
    if (CbxGroup.ItemIndex >= 0) then
    begin
      // Persist selected group item
      fMcParam.Selected := CbxGroup.Text;
      fMcParam.Save();
    end;
  finally
    if ( Assigned(fMcParam) ) then
    begin
      fMcParam.Free;
      fMcParam := nil;
    end;
  end;
  inherited Destroy;
end;

procedure TFrameParamGroups.BtSaveClick(Sender: TObject);
var
  ACpt, APmp, AName: string;
  IsToSave, IsNew: Boolean;
  Pos: Integer;
begin
  // Input prompt.
  ACpt  := 'Save Parameters Group';
  APmp  := 'Name:';
  AName := InputBox(ACpt, APmp, CbxGroup.Text);
  // Check is valid and if exists.
  IsToSave := (AName <> '');
  IsNew    := True;
  if ( FMcParam.ExistsGroup(AName) ) then
  begin
    // Confirm overwrite.
    IsToSave := ( MessageDlg('Overwrite "' + AName + '" ?',
                             mtConfirmation, mbYesNo, 0, mbYes) = mrYes );
    IsNew := False;
  end;
  if (IsToSave) then
  begin
    saveGroupItem(AName);
    // Update Preset combobox.
    fEventsFreezed := true;
    if (IsNew) then
      CbxGroup.Items.Add(AName);
    // Select name.
    Pos := CbxGroup.Items.IndexOf(AName);
    if (Pos >= 0) then
      CbxGroup.ItemIndex := Pos;
    fEventsFreezed := false;
  end;
end;

procedure TFrameParamGroups.BtDelClick(Sender: TObject);
var
  sName: string;
begin
  if (CbxGroup.ItemIndex >= 0) then
  begin
    sName := CbxGroup.Text;
    if ( MessageDlg('Delete "' + sName + '" ?',
                    mtConfirmation, mbYesNo, 0, mbYes) = mrYes ) then
    begin
      deleteGroupItem(sName);
      CbxGroup.Items.Delete(CbxGroup.ItemIndex);
      // Set no selected
      CbxGroup.ItemIndex := -1;
      FMcParam.Selected  := '';
      FMcParam.Save();
    end;
  end;
end;

procedure TFrameParamGroups.CbxGroupChange(Sender: TObject);
begin
  if (FEventsFreezed) then exit;
  loadGroupItem(CbxGroup.ItemIndex);
  FGroupSelectedName := CbxGroup.Text;
end;

end.

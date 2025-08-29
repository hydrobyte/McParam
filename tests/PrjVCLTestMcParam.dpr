program PrjVCLTestMcParam;

uses
  Forms,
  FoMain in 'FoMain.pas' {FormMain},
  McParam in '..\src\McParam.pas',
  McJSON in '..\src\McJSON.pas',
  FrParamGroups in '..\src\FrParamGroups.pas' {FrameParamGroups: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

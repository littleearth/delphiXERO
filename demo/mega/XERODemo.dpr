// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program XERODemo;

uses
  Vcl.Forms,
  XERO.Log,
  XERO.Log.Basic,
  frmXERODemoU in 'frmXERODemoU.pas' {frmXERODemo};

{$R *.res}

begin
  SetXEROLogClass(TXEROLogBasic);
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmXERODemo, frmXERODemo);
  Application.Run;

end.

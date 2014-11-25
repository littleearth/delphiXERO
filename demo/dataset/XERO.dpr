program XERO;

uses
  Vcl.Forms,
  frmXEROTestU in 'frmXEROTestU.pas' {frmXERO};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DelphiXERO';
  Application.CreateForm(TfrmXERO, frmXERO);
  Application.Run;
end.

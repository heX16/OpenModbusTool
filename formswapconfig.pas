unit FormSwapConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TfrmSwapConfig }

  TfrmSwapConfig = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbSwap2byte: TCheckBox;
    cbSwap2word: TCheckBox;
    cbSwap2dword: TCheckBox;
    procedure BitBtn2Click(Sender: TObject);
  private

  public

  end;

var
  frmSwapConfig: TfrmSwapConfig;

implementation

{$R *.lfm}

{ TfrmSwapConfig }

procedure TfrmSwapConfig.BitBtn2Click(Sender: TObject);
begin
  Self.ModalResult := mrOK;
  self.Hide();
end;

end.


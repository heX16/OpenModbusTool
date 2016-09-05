unit formBitEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, Buttons, ExtCtrls;

type

  { TfrmBitEdit }

  TfrmBitEdit = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckGroup1: TCheckGroup;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmBitEdit: TfrmBitEdit;

implementation

{$R *.lfm}

end.


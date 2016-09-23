unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    lbAuthor: TLabel;
    lbTranslateAuthor: TLabel;
    lbNameAndVer: TLabel;
    lbAboutText: TLabel;
    lbRepURL: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

{$R *.lfm}

{ TfrmAbout }
function GetMyVersion(VerCount: Byte = 4):string;
type
  TVerInfo=packed record
    Nevazhno: array[0..47] of byte; // ненужные нам 48 байт
    Minor,Major,Build,Release: word; // а тут версия
  end;
var
  s:TResourceStream;
  v:TVerInfo;
begin
  result := '';
  try
    s:=TResourceStream.Create(HInstance,'#1',RT_VERSION); // достаём ресурс
    if s.Size>0 then begin
      s.Read(v,SizeOf(v)); // читаем нужные нам байты
      // версия:
      if VerCount >= 1 then
        Result := Result + IntToStr(v.Major);
      if VerCount >= 2 then
        Result := Result + '.' + IntToStr(v.Minor);
      if VerCount >= 3 then
        Result := Result + '.' + IntToStr(v.Release);
      if VerCount >= 4 then
        Result := Result + '.' + IntToStr(v.Build);
    end;
    s.Free;
  except;
  end;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lbNameAndVer.Caption:='OpenModbusTool';
  lbNameAndVer.Caption:=lbNameAndVer.Caption + '   ver.'+GetMyVersion(4);
  lbAuthor.Caption:='Author: heXor';
end;

end.


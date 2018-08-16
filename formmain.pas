unit FormMain;

{$mode objfpc}{$H+}

{
IDE: Lazarus 1.6 (FPC 3.0.0)

Dependency:
  synapse (Release 40 2012-04-23) - for Serial Port
  delphimodbus-1.6.7 - for Modbus TCP -> https://github.com/coassoftwaresystems/delphi-modbus
  indy-10.2.0.3 - for delphimodbus
}

interface

uses
  //DefaultTranslator, // - forced translation
  //LCLTranslator,
  Classes, SysUtils, FileUtil, ListViewFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, IdComponent, IdTCPClient, StdCtrls, ActnList,
  Menus, IniPropStorage, ThreadModBus, IdModBusServer, ModbusTypes,
  IdModBusClient, IdContext, IdCustomTCPServer;

{
0x (bit, RW) - Discrete Output Coils
1x (bit, RO) - Discrete Input Contacts
3x (word, RO) - Analog Input Registers
4x ( word, RW) - Analog Output Holding Registers
}

type
  { TfrmMain }

  TfrmMain = class(TForm)
    actDissconect: TAction;
    actConnect: TAction;
    actAbout: TAction;
    actExit: TAction;
    actCopy: TAction;
    actCopyValue: TAction;
    actEditExt: TAction;
    actTestServerEnable: TAction;
    actSetX: TAction;
    actSet1: TAction;
    actSet0: TAction;
    actPaste: TAction;
    actOptions: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnConnect: TButton;
    btnDissconect: TButton;
    cbIP: TComboBox;
    cbRegisterType: TComboBox;
    cbRegFormat: TComboBox;
    edRegCount: TEdit;
    IdModBusServerTest: TIdModBusServer;
    IniPropStorage1: TIniPropStorage;
    lbRegCount: TLabel;
    lbAddr: TLabel;
    edRegAddr: TLabeledEdit;
    listMain: TListView;
    ListViewFilterEdit1: TListViewFilterEdit;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    mnStartServer: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnEdit1: TMenuItem;
    mnEditExt: TMenuItem;
    mnSetX: TMenuItem;
    menuMainForm: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnCopyValueOnly: TMenuItem;
    mnSet1: TMenuItem;
    mnPaste: TMenuItem;
    mnSet0: TMenuItem;
    mnCopy: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    menuMainList: TPopupMenu;
    rgViewStyle: TRadioGroup;
    shapeState: TShape;
    StatusBar1: TStatusBar;
    TimerInit: TTimer;
    TimerReadMB: TTimer;
    btnPause: TToggleBox;
    procedure actAboutExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actConnectUpdate(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actMassActionUpdate(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actDissconectExecute(Sender: TObject);
    procedure actDissconectUpdate(Sender: TObject);
    procedure actEditExtExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSet0Execute(Sender: TObject);
    procedure actSet1Execute(Sender: TObject);
    procedure actSetXExecute(Sender: TObject);
    procedure actTestServerEnableExecute(Sender: TObject);
    procedure cbIPKeyPress(Sender: TObject; var Key: char);
    procedure cbRegFormatChange(Sender: TObject);
    procedure cbRegisterTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdModBusServerTestConnect(AContext: TIdContext);
    procedure IdModBusServerTestDisconnect(AContext: TIdContext);
    procedure IdModBusServerTestInvalidFunction(const Sender: TIdContext;
      const FunctionCode: TModBusFunction;
      const RequestBuffer: TModBusRequestBuffer);
    procedure IdModBusServerTestReadCoils(const Sender: TIdContext; const RegNr,
      Count: Integer; var Data: TModCoilData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure IdModBusServerTestReadHoldingRegisters(const Sender: TIdContext;
      const RegNr, Count: Integer; var Data: TModRegisterData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure IdModBusServerTestReadInputBits(const Sender: TIdContext;
      const RegNr, Count: Integer; var Data: TModCoilData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure IdModBusServerTestReadInputRegisters(const Sender: TIdContext;
      const RegNr, Count: Integer; var Data: TModRegisterData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure IdModBusServerTestWriteCoils(const Sender: TIdContext; const RegNr,
      Count: Integer; const Data: TModCoilData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure IdModBusServerTestWriteRegisters(const Sender: TIdContext;
      const RegNr, Count: Integer; const Data: TModRegisterData;
      const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
    procedure listMainEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure listMainEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure mnStartServerClick(Sender: TObject);
    procedure rgViewStyleClick(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
    procedure treeMainDblClick(Sender: TObject);
    procedure threadReadTerminating(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    threadRead: TThreadModBus;
    SetToXValue: string;
    function LoadListRegs(Filename: String): boolean;
    procedure listMainDoOnSelected(Proc: TLVDataEvent);
    procedure DoCopy(Sender: TObject; Item: TListItem);
    procedure CopyValue(Sender: TObject; Item: TListItem);
    procedure SetTo0(Sender: TObject; Item: TListItem);
    procedure SetTo1(Sender: TObject; Item: TListItem);
    procedure SetToX(Sender: TObject; Item: TListItem);
  end;

var
  frmMain: TfrmMain;

const
  // index
  idxColumnMainText = 0;
  idxColumnReg = 0;
  idxColumnValue = 1;
  idxColumnType = 2;
  idxColumnName = 3;
  idxStatusBarStatus = 0;
  idxStatusBarErrorCount = 1;
  idxStatusBarMainText = 2;

function GetBit(Value: QWord; Index: byte): boolean;
function NormalizeConfigFileName(name: string): string;

implementation

{$R *.lfm}

uses
  FormOptions,
  modbuslib,//<-TEMP!
  FormBitEdit,
  FormAbout,
  Clipbrd,
  csvreadwrite;

{ TfrmMain }

function NormalizeConfigFileName(name: string): string;
begin
  result := name;
  if not FileExists(name) then
  begin
    // пытаемся загрузить (только если у файла не прописан путь к дириктории)
    if ExtractFileDir(name) = '' then
      if FileExists(ExtractFileDir(Application.Params[0])+'\'+name) then
        result := ExtractFileDir(Application.Params[0])+'\'+name;
  end;
end;

function GetBit(Value: QWord; Index: byte): boolean;
begin
  Result := ((Value shr Index) and 1) = 1;
end;

//////////////////////////////////////////////////////////////////////////////

procedure TfrmMain.treeMainDblClick(Sender: TObject);
begin
{  if treeMain.Selected<>nil then
  begin
    ShowMessage(
    'register='+IntToStr((DWord(treeMain.Selected.Data) div 1024))+
    ' bit='+IntToStr((DWord(treeMain.Selected.Data) mod 1024)));
  end;}
end;

function TfrmMain.LoadListRegs(Filename: String): boolean;
var
  FileStream: TFileStream;
  Parser: TCSVParser;
  row: array [0..2] of string;
  LastRow: integer;

  procedure ParsePrevRow;
  var i:TListItem;
  begin
    i:=frmMain.listMain.Items.Add;
    i.SubItems.Add(row[0]);
    i.SubItems.Add('???');
    i.SubItems.Add(row[1]);
    i.SubItems.Add(row[2]);
    //i.Caption:=i.SubItems.Text;
    i.Caption:=row[0]+'=?';
  end;

begin
  if not(FileExists(FileName)) then exit;

  try
    Parser:=TCSVParser.Create;
    FileStream := TFileStream.Create(Filename, fmOpenRead+fmShareDenyWrite);

    Parser.Delimiter:=';';
    Parser.SetSource(FileStream);

    LastRow := 1;
    while Parser.ParseNextCell do
    begin
      // Skip header row
      if (Parser.CurrentRow=0) then
        continue;

      // new row detected
      if LastRow <> Parser.CurrentRow then
      begin
        LastRow := Parser.CurrentRow;
        if row[0]<>'' then
          ParsePrevRow;
        row[0]:='';row[1]:='';row[2]:='';
      end;

      // save parsed row
      if (Parser.CurrentCol >= 1) and (Parser.CurrentCol <= 3) then
        row[Parser.CurrentCol] := Trim(Parser.CurrentCellText);
    end;
    ParsePrevRow;

    // fix bug (Laz 1.6): after load filter not update data - need reconect control
    frmMain.ListViewFilterEdit1.FilteredListview:=nil;
    frmMain.ListViewFilterEdit1.FilteredListview:=frmMain.listMain;
  finally
    FreeAndNil(Parser);
    FreeAndNil(FileStream);
  end;
end;

procedure TfrmMain.listMainDoOnSelected(Proc: TLVDataEvent);
var i: integer;
begin
  for i:=0 to listMain.Items.Count-1 do
    if listMain.Items[i].Selected then
      Proc(nil, listMain.Items[i]);
end;

procedure TfrmMain.DoCopy(Sender: TObject; Item: TListItem);
begin
  Clipboard.AsText := Clipboard.AsText + #13 + Item.SubItems[idxColumnReg] + '=' + Item.SubItems[idxColumnValue];
end;

procedure TfrmMain.CopyValue(Sender: TObject; Item: TListItem);
begin
  Clipboard.AsText := Clipboard.AsText + #13 + Item.SubItems[idxColumnValue];
end;

procedure TfrmMain.SetTo0(Sender: TObject; Item: TListItem);
begin
  if threadRead <> nil then
    threadRead.Send(Item.SubItems[idxColumnReg], '0');
end;

procedure TfrmMain.SetTo1(Sender: TObject; Item: TListItem);
begin
  if threadRead <> nil then
    threadRead.Send(Item.SubItems[idxColumnReg], '1');
end;

procedure TfrmMain.SetToX(Sender: TObject; Item: TListItem);
begin
  if threadRead <> nil then
    threadRead.Send(Item.SubItems[idxColumnReg], SetToXValue);
end;

procedure TfrmMain.actConnectExecute(Sender: TObject);
var ip: string;
begin
  ip := Trim(cbIP.Text);
  if ip <> '' then
  begin
    threadRead := TThreadModBus.Create(false);
    actDissconect.Enabled:=true;
  end else
    cbIP.SetFocus;

  // work with history
  if (ip <> '') then
  begin
    // если нет в списке, то сохраняем команды в список
    if cbIP.Items.IndexOf(ip) = -1 then
      cbIP.Items.Insert(0, ip) else
      begin
        // если есть в списке то просто поднимаем на вверх
        cbIP.Items.Delete(cbIP.Items.IndexOf(ip));
        cbIP.Items.Insert(0, ip);
      end;
    // удаляем лишние
    while (cbIP.Items.Count > 30) do
      cbIP.Items.Delete(30);
    cbIP.Text:=ip;
  end;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.actConnectUpdate(Sender: TObject);
begin
  actConnect.Enabled := threadRead = nil;
  cbIP.Enabled := actConnect.Enabled;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText:='';
  listMainDoOnSelected(@DoCopy);
end;

procedure TfrmMain.actMassActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := listMain.SelCount > 0;
end;

procedure TfrmMain.actCopyValueExecute(Sender: TObject);
begin
  Clipboard.AsText:='';
  listMainDoOnSelected(@CopyValue);
end;

procedure TfrmMain.actDissconectExecute(Sender: TObject);
begin
  if (threadRead <> nil) then
  begin
    threadRead.ValidDissconect := true;
    shapeState.Color:=clYellow;
    threadRead.Terminate;
    // wake up! - time for death
    threadRead.EventPauseAfterRead.SetEvent;
  end;
  {
  TThreadModBus.SyncRemoveFromMainProg and TfrmMain.threadReadTerminating
  make all other actions.
  }
  actDissconect.Enabled:=false;
end;

procedure TfrmMain.actDissconectUpdate(Sender: TObject);
begin
  if (threadRead = nil) and (actDissconect.Enabled) then
  begin
    if shapeState.Brush.Color = clGreen then
      // internal error?
      shapeState.Brush.Color := clMaroon;
    actDissconect.Enabled:=false;
  end;
end;

procedure TfrmMain.actEditExtExecute(Sender: TObject);
begin
  if listMain.ItemIndex <> -1 then
  begin
    if frmBitEdit.GetResult(false, StrToIntDef(listMain.Items[listMain.ItemIndex].SubItems[idxColumnValue], 0)) then
      threadRead.Send(listMain.Items[listMain.ItemIndex].SubItems[idxColumnReg], IntToStr(frmBitEdit.EditResult));
  end;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  frmOptions.ShowModal();
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
var
  i, c, v: integer;
  strs: TStringList;
begin
  if threadRead <> nil then
    try
      c := 0;
      strs := TStringList.Create;
      strs.Text := Clipboard.AsText;
      if listMain.SelCount = 1 then
      begin
        // one value to one reg
        if strs.Count=1 then
          threadRead.Send(listMain.Selected.SubItems[idxColumnReg], strs[0]);
        //ToDo: many value to many regs linear, start from selected item
        {if strs.Count>1 then
          for i:=0 to listMain.Items.Count-1 do
            if listMain.Items[i].Selected then
            begin
              //todo: WIP!
              threadRead.Send(listMain.Items[i].SubItems[idxColumnReg], strs[c]);
              c := c + 1;
            end;}
      end;

      if listMain.SelCount > 1 then
      begin
        // one value to many regs
        if strs.Count=1 then
          for i:=0 to listMain.Items.Count-1 do
            if listMain.Items[i].Selected then
              threadRead.Send(listMain.Items[i].SubItems[idxColumnReg], strs[0]);
        // many value to many regs
        if strs.Count>1 then
          for i:=0 to listMain.Items.Count-1 do
            if listMain.Items[i].Selected then
            begin
              if c > strs.Count-1 then break;
              threadRead.Send(listMain.Items[i].SubItems[idxColumnReg], strs[c]);
              c := c + 1;
            end;
      end;
    finally
      strs.Free;
    end;
end;

procedure TfrmMain.actSet0Execute(Sender: TObject);
begin
  listMainDoOnSelected(@SetTo0);
end;

procedure TfrmMain.actSet1Execute(Sender: TObject);
begin
  listMainDoOnSelected(@SetTo1);
end;

procedure TfrmMain.actSetXExecute(Sender: TObject);
begin
  SetToXValue := Trim(InputBox(strEnterValue, strEnterValue, ''));
  if SetToXValue <> '' then
    listMainDoOnSelected(@SetToX);
end;

procedure TfrmMain.actTestServerEnableExecute(Sender: TObject);
begin
  actTestServerEnable.Checked:=not actTestServerEnable.Checked;
  IdModBusServerTest.Active:=actTestServerEnable.Checked;
end;

procedure TfrmMain.cbIPKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    actConnect.Execute();
end;

procedure TfrmMain.cbRegFormatChange(Sender: TObject);
begin
  if threadRead <> nil then
    threadRead.EventPauseAfterRead.SetEvent;
  rgViewStyleClick(nil);
end;

procedure TfrmMain.cbRegisterTypeChange(Sender: TObject);
begin
  if listMain.ViewStyle=vsList then
    if threadRead <> nil then
      case threadRead.RegType of
        tRegBoolRO, tRegBoolRW:
          listMain.Column[idxColumnMainText].Width:=50;
        tRegWordRO, tRegWordRW:
          listMain.Column[idxColumnMainText].Width:=150;
      end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if threadRead <> nil then
    actDissconect.Execute;
end;

procedure TfrmMain.listMainEdited(Sender: TObject; Item: TListItem;
  var AValue: string);
var itm: TModbusItem;
begin
  if threadRead<> nil then
  begin
    if StrToIntDef(Item.SubItems[idxColumnReg], 65536) < 65536 then
    begin
      itm.Addr:=StrToInt(Item.SubItems[idxColumnReg]);
      if StrToIntDef(AValue, 65536) < 65536 then
      begin
        itm.Value:=StrToInt(AValue);
        itm.RegType:=threadRead.RegType;
        threadRead.CritWriteQueueWork.Enter;
        threadRead.WriteQueue.PushFront(itm);
        threadRead.CritWriteQueueWork.Leave;
      end;
    end else
      StatusBar1.Panels[idxStatusBarMainText].Text:='Invalid value';
  end;
end;

procedure TfrmMain.listMainEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  if threadRead = nil then
    AllowEdit:=false else
    begin
      //todo: edit in all format! - (frmBitEdit, etc)
      if (threadRead.RegFormat <> rfDec) or (listMain.ViewStyle=vsReport) then
      begin
        AllowEdit:=false;
        if listMain.ItemIndex <> -1 then
        begin
          if frmBitEdit.GetResult(false, StrToIntDef(listMain.Items[listMain.ItemIndex].SubItems[idxColumnValue], 0)) then
            threadRead.Send(listMain.Items[listMain.ItemIndex].SubItems[idxColumnReg], IntToStr(frmBitEdit.EditResult));
        end;
      end;
    end;
end;

procedure TfrmMain.mnStartServerClick(Sender: TObject);
begin
  IdModBusServerTest.Active:=true;
end;

procedure TfrmMain.rgViewStyleClick(Sender: TObject);
begin
  case rgViewStyle.ItemIndex of
  0: begin
    listMain.ViewStyle:=vsReport;
    listMain.Column[idxColumnMainText].Width:=0;
    //todo: bug (wnd): Columns.Width applies only on start application. after start this code not work - i dont known why.
    listMain.Column[idxColumnType+1  ].Width:=0;
    listMain.Column[idxColumnReg+1   ].Width:=100;
    listMain.Column[idxColumnValue+1 ].Width:=150;
    listMain.Column[idxColumnName+1  ].Width:=0;
  end;
  1: begin
    listMain.ViewStyle:=vsList;
    case TRegReadType(cbRegisterType.ItemIndex) of
      tRegBoolRO, tRegBoolRW:
        listMain.Column[idxColumnMainText].Width:=50;
      tRegWordRO, tRegWordRW:
        listMain.Column[idxColumnMainText].Width:=150;
    end;
  end;
  end;
end;

procedure TfrmMain.TimerInitTimer(Sender: TObject);
//var filename: string;
begin
  if frmMain.Visible then
  begin
    TimerInit.Enabled:=false;
    Application.ProcessMessages;
    // update
    rgViewStyleClick(nil);

    //SetDefaultLang('ru', '', true); // manual localization

    {if Application.ParamCount < 1 then
    begin
      //todo: WIP!
      Halt(666);
      MessageDlg('Ussage: OpenModBusTool.exe [--ip=Host]'+#13+
        #13+
        'Example: OpenModBusTool.exe --ip=192.168.1.10'+#13+
        #13+
        'constructor  [heX]  2016  www.hex.name',
        mtInformation, [mbClose], 0);
      Halt(1);
    end;}

    //todo: WIP
    {
    IdModBusClient1.Host:=Application.Params[1];
    if Application.ParamCount <= 1 then
      // load default file
      filename := ExtractFileName(Application.Params[0])+'.csv' else
      filename := NormalizeConfigFileName(Application.Params[2]);
    if Application.HasOption('c', 'autoclose') then
      AutoClose := true;
    if Application.HasOption('t', 'top') then
      frmMain.FormStyle := fsSystemStayOnTop;
    LoadListRegs(filename);
    }

  end;
end;

procedure TfrmMain.threadReadTerminating(Sender: TObject);
begin
  // thread report about death
  if threadRead <> nil then
    threadRead := nil; // just drop pointer. (FreeOnTerminate=true)
  {Note: this operation thread-safe:
    this action placed in main thread - other access to this pointer is imposible.
  }
end;




//////////////// SERVER (TEST!) //////////////////////

procedure TfrmMain.IdModBusServerTestConnect(AContext: TIdContext);
begin
  //
end;

procedure TfrmMain.IdModBusServerTestDisconnect(AContext: TIdContext);
begin
  //
end;

procedure TfrmMain.IdModBusServerTestInvalidFunction(const Sender: TIdContext;
  const FunctionCode: TModBusFunction; const RequestBuffer: TModBusRequestBuffer
  );
begin
  //
end;

procedure TfrmMain.IdModBusServerTestReadCoils(const Sender: TIdContext;
  const RegNr, Count: Integer; var Data: TModCoilData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do
    Data[i]:=false;
end;

procedure TfrmMain.IdModBusServerTestReadInputBits(const Sender: TIdContext;
  const RegNr, Count: Integer; var Data: TModCoilData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do
    Data[i]:=false;
end;

procedure TfrmMain.IdModBusServerTestReadHoldingRegisters(
  const Sender: TIdContext; const RegNr, Count: Integer;
  var Data: TModRegisterData; const RequestBuffer: TModBusRequestBuffer;
  var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do
    Data[i]:=i;
end;

procedure TfrmMain.IdModBusServerTestReadInputRegisters(const Sender: TIdContext;
  const RegNr, Count: Integer; var Data: TModRegisterData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do
    Data[i]:=i;
end;

procedure TfrmMain.IdModBusServerTestWriteCoils(const Sender: TIdContext;
  const RegNr, Count: Integer; const Data: TModCoilData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
begin
  ErrorCode:=1;
end;

procedure TfrmMain.IdModBusServerTestWriteRegisters(const Sender: TIdContext;
  const RegNr, Count: Integer; const Data: TModRegisterData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
begin
  ErrorCode:=1;
end;

end.


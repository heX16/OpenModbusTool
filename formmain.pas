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
  LCLType, VTUtils, variants, Classes, SysUtils, FileUtil, ListViewFilterEdit,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, IdComponent,
  IdTCPClient, StdCtrls, ActnList, Menus, IniPropStorage, Grids, ThreadModBus,
  IdModBusServer, ModbusTypes, IdModBusClient, IdContext, IdCustomTCPServer,
  IdLogDebug, IdHTTP, IdServerInterceptLogEvent, VirtualTrees, SuperViewZone,
  SuperViewZoneConfig, IdIntercept, IdGlobal;

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
    actShowLog: TAction;
    actSwapConfig: TAction;
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
    btnSwapConfig: TButton;
    Button1: TButton;
    cbIP: TComboBox;
    cbRegisterType: TComboBox;
    cbRegFormat: TComboBox;
    DrawGrid1: TDrawGrid;
    edRegCount: TEdit;
    IdHTTP1: TIdHTTP;
    IdLogDebug1: TIdLogDebug;
    IdModBusServerTest: TIdModBusServer;
    IniPropStorage1: TIniPropStorage;
    lbRegCount: TLabel;
    lbAddr: TLabel;
    edRegAddr: TLabeledEdit;
    listLog: TListBox;
    ListViewFilterEdit1: TListViewFilterEdit;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    mnShowLog: TMenuItem;
    mnSrvFullRnd: TMenuItem;
    mnSrvTestRead: TMenuItem;
    mnSrvAddSmallRandom: TMenuItem;
    MenuItem19: TMenuItem;
    mnSrvStart: TMenuItem;
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
    SplitterLog: TSplitter;
    StatusBar1: TStatusBar;
    TimerUpdateData: TTimer;
    TimerInit: TTimer;
    TimerReadMB: TTimer;
    btnPause: TToggleBox;
    VirtualStringTree1: TVirtualStringTree;
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
    procedure actShowLogExecute(Sender: TObject);
    procedure actSwapConfigExecute(Sender: TObject);
    procedure actTestServerEnableExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbIPKeyPress(Sender: TObject; var Key: char);
    procedure cbRegFormatChange(Sender: TObject);
    procedure cbRegisterTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdLogDebug1Receive(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);
    procedure IdLogDebug1Send(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);
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
    procedure mnSrvFullRndClick(Sender: TObject);
    procedure mnSrvAddSmallRandomClick(Sender: TObject);
    procedure mnStartServer1Click(Sender: TObject);
    procedure rgViewStyleClick(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
    procedure treeMainDblClick(Sender: TObject);
    procedure threadReadTerminating(Sender: TObject);
    procedure VirtualStringTree1Editing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VirtualStringTree1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VirtualStringTree1NewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
  public
    Presenter: TSuperViewPresenterModbus;
    { public declarations }
    threadRead: TThreadModBus;
    SetToXValue: string;
    function LoadListRegs(Filename: String): boolean;
    procedure listMainDoOnSelected(Proc: TSuperViewEnumItemsProc);
  end;

var
  frmMain: TfrmMain;

const
  // index column in tree
  ciColumnReg = 0;
  ciColumnValue = 1;
  ciColumnRegNum = 2;
  ciColumnRegType = 4;

  ciStatusBarStatus = 0;
  ciStatusBarErrorCount = 1;
  ciStatusBarMainText = 2;

function GetBit(Value: QWord; Index: byte): boolean;
function NormalizeConfigFileName(name: string): string;

implementation

{$R *.lfm}

uses
  FormOptions,
  modbuslib,//<-TEMP!
  FormBitEdit,
  FormAbout,
  FormSwapConfig,
  Clipbrd,
  csvreadwrite;

{
procedure DoCopy(Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
procedure CopyValue(Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
procedure SetTo0(Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
procedure SetTo1(Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
procedure SetToX(Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
}

procedure DoCopy(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; UserData: pointer);
begin
  Clipboard.AsText := Clipboard.AsText + #13 + Item.Name + '=' + Item.Info;
end;

procedure CopyValue(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; UserData: pointer);
begin
  Clipboard.AsText := Clipboard.AsText + #13 + Item.Info;
end;

procedure SetTo0(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; UserData: pointer);
begin
  frmMain.threadRead.Send(Item.Name, '0');
end;

procedure SetTo1(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; UserData: pointer);
begin
  frmMain.threadRead.Send(Item.Name, '1');
end;

procedure SetToX(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; UserData: pointer);
begin
  frmMain.threadRead.Send(Item.Name, frmMain.SetToXValue);
end;


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
    //todo: LoadListRegs
    (*
    column:
      Caption - main text
      0-reg
      1-val
      2-type
      3-name

    Presenter.ThreadAddEvent( ... );
    i:=frmMain.listMain.Items.Add;
    i.SubItems.Add(row[0]);//main text
    i.SubItems.Add('???'); //reg
    i.SubItems.Add(row[1]);//value
    i.SubItems.Add(row[2]);//type
                           //name
    //i.Caption:=i.SubItems.Text;
    i.Caption:=row[0]+'=?';
    *)
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

    //todo: LoadListRegs
    // fix bug (Laz 1.6): after load filter not update data - need reconect control
    //frmMain.ListViewFilterEdit1.FilteredListview:=nil;
    //frmMain.ListViewFilterEdit1.FilteredListview:=frmMain.listMain;
  finally
    FreeAndNil(Parser);
    FreeAndNil(FileStream);
  end;
end;

procedure TfrmMain.listMainDoOnSelected(Proc: TSuperViewEnumItemsProc);
begin
  Presenter.EnumSelected(Proc, nil);
end;

procedure TfrmMain.actConnectExecute(Sender: TObject);
var ip: string;
begin
  ip := Trim(cbIP.Text);
  if ip <> '' then
  begin
    threadRead := TThreadModBus.Create(false, Presenter);
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
  (Sender as TAction).Enabled := Presenter.Count() > 0;
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
  if Presenter.SelectedPresent() then
  begin
    //todo: add support float (32bit)
    if frmBitEdit.GetResult(false, Presenter.SelectedItem().Info) then
      threadRead.Send(Presenter.SelectedItem().Name, IntToStr(frmBitEdit.EditResult));
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

type TUserDataStruct1 = record
    thread: TThreadModBus;
    strs: TStringList;
    StrIndex: integer;
  end;
  PUserDataStruct1=^TUserDataStruct1;

procedure DoPasteValues(Sender: TSuperViewPresenter; Item: TSuperViewItem;
  out Delete: boolean; out Stop: boolean; userdata: pointer);
var v: integer;
  data: PUserDataStruct1 absolute userdata;
begin
  // one value to many regs
  if data^.strs.Count=1 then
    data^.thread.Send(Item.Name, data^.strs[0]) else
    if (data^.strs.Count>1) and (data^.StrIndex<data^.strs.Count) then begin
      // many value to many regs
      v:=StrToIntDef(data^.strs[data^.StrIndex], 100000);
      if (v>=0) and (v<65536) then
        data^.thread.Send(StrToInt(Item.Name), v);
      inc(data^.StrIndex);
    end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
var
  strs: TStringList;
  data: TUserDataStruct1;
begin
  if (threadRead <> nil) then
    try
      strs := TStringList.Create;
      strs.Text := Clipboard.AsText;

      data.thread:=threadRead;
      data.strs:=strs;
      data.StrIndex:=0;

      if Presenter.SelectedPresent() then
        Presenter.EnumSelected(@DoPasteValues, @data);
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

procedure TfrmMain.actShowLogExecute(Sender: TObject);
begin
  actShowLog.Checked:=not actShowLog.Checked;
  listLog.Visible:=actShowLog.Checked;
  SplitterLog.Visible:=actShowLog.Checked;
end;

procedure TfrmMain.actSwapConfigExecute(Sender: TObject);
begin
  frmSwapConfig.Show();
end;

procedure TfrmMain.actTestServerEnableExecute(Sender: TObject);
begin
  actTestServerEnable.Checked:=not actTestServerEnable.Checked;
  IdModBusServerTest.Active:=actTestServerEnable.Checked;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  IdHTTP1.Get('http://127.0.0.1:8384/');
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
  //todo: OLD?
  if threadRead <> nil then
    case threadRead.RegType of
      tRegBoolRO, tRegBoolRW:
        ;//VirtualStringTree1.Header.Columns[idxColumnMainText].Width:=50;
      tRegWordRO, tRegWordRW:
        ;//VirtualStringTree1.Header.Columns[idxColumnMainText].Width:=150;
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Presenter := TSuperViewPresenterModbus.Create();
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  IdModBusServerTest.Active:=false;
  if threadRead <> nil then
    actDissconect.Execute;
  FreeAndNil(Presenter);
end;

procedure TfrmMain.IdLogDebug1Receive(ASender: TIdConnectionIntercept;
  var ABuffer: TIdBytes);
begin
  ShowMessage(inttostr(ABuffer[0]));
end;

procedure TfrmMain.IdLogDebug1Send(ASender: TIdConnectionIntercept;
  var ABuffer: TIdBytes);
begin
  ShowMessage(inttostr(ABuffer[0]));
end;

procedure TfrmMain.mnStartServer1Click(Sender: TObject);
begin
  IdModBusServerTest.Active:=true;
end;

procedure TfrmMain.rgViewStyleClick(Sender: TObject);
begin
  case rgViewStyle.ItemIndex of
  0: Presenter.SetViewMode(ViewModeCompactGrid);
  1: Presenter.SetViewMode(ViewModeTree);
  end;
end;

procedure TfrmMain.TimerInitTimer(Sender: TObject);
//var filename: string;
begin
  if frmMain.Visible then
  begin
    TimerInit.Enabled:=false;
    Presenter.Setup(VirtualStringTree1, DrawGrid1, TimerUpdateData, listLog);
    Presenter.ThreadEventsMaxCount := 65536;

    Application.ProcessMessages;
    // update
    rgViewStyleClick(nil);

    //SetDefaultLang('ru', '', true); // manual localization

    //todo: add param support
    {
    if Application.ParamCount < 1 then
    begin
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

procedure TfrmMain.VirtualStringTree1NewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  itm: TModbusItem;
  item: TSuperViewItem;
  v: integer;
begin
  if (threadRead<> nil) and GetObjValid(Sender, Node) then
  begin
    item := GetObj(Sender, Node) as TSuperViewItem;

    //todo: add multi format!
    v := StrToIntDef(NewText, 100000);
    if (v > 0) and (v < 65536) then
    begin
      itm.Addr:=StrToInt(Item.Name);
      itm.Value:=v;
      itm.RegType:=threadRead.RegType;
      threadRead.CritWriteQueueWork.Enter;
      threadRead.WriteQueue.PushFront(itm);
      threadRead.CritWriteQueueWork.Leave;
    end else
      StatusBar1.Panels[ciStatusBarMainText].Text:='Invalid value';
  end;
end;

procedure TfrmMain.VirtualStringTree1Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  if threadRead = nil then
    Allowed:=false else
    begin
      Allowed:=true;
    end;
end;

procedure TfrmMain.VirtualStringTree1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then begin
    //todo: F2 edit support
    //if frmBitEdit.GetResult(false, StrToIntDef(listMain.Items[listMain.ItemIndex].SubItems[ciColumnValue], 0)) then
    //  threadRead.Send(listMain.Items[listMain.ItemIndex].SubItems[idxColumnReg], IntToStr(frmBitEdit.EditResult));
  end;
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
    if (RegNr+i mod 2 = 1) or (mnSrvAddSmallRandom.Checked and (Random(10)=0)) then
      Data[i]:=true
    else
      Data[i]:=false;
end;

procedure TfrmMain.IdModBusServerTestReadInputBits(const Sender: TIdContext;
  const RegNr, Count: Integer; var Data: TModCoilData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do
    if (RegNr+i mod 2 = 1) or (mnSrvAddSmallRandom.Checked and (Random(10)=0)) then
      Data[i]:=true
    else
      Data[i]:=false;
end;

procedure TfrmMain.IdModBusServerTestReadHoldingRegisters(
  const Sender: TIdContext; const RegNr, Count: Integer;
  var Data: TModRegisterData; const RequestBuffer: TModBusRequestBuffer;
  var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Data[i]:=RegNr+i;
    //todo: add random seed???
    if mnSrvFullRnd.Checked then
      Data[i]:=Random(65536);
    if mnSrvAddSmallRandom.Checked then
      Data[i]:=Data[i]+random(2);
  end;
end;

procedure TfrmMain.IdModBusServerTestReadInputRegisters(const Sender: TIdContext;
  const RegNr, Count: Integer; var Data: TModRegisterData;
  const RequestBuffer: TModBusRequestBuffer; var ErrorCode: Byte);
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Data[i]:=RegNr+i+random(1);
    if mnSrvFullRnd.Checked then
      Data[i]:=Random(65536);
    if mnSrvAddSmallRandom.Checked then
      Data[i]:=Data[i]+random(2);
  end;
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

procedure TfrmMain.mnSrvFullRndClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
end;

procedure TfrmMain.mnSrvAddSmallRandomClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
end;

end.


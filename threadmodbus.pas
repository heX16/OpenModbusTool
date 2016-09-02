unit ThreadModBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  syncobjs, gdeque,
  IdModBusClient, ModbusTypes;

type
  {
    Dectimal (123)
    Hex (F0A5)
    Bin (0000 0000 0000 0001)
    Boolean (0-gray, 1-green)
    Float (0.123 - 4 byte)
    Double Float (0.123 - 8 byte)
  }
  TRegShowFormat = (rfDec=0, rfHex=1, rfBin, tfBool, tfFloat, tfDouble);

  {
  1x (bit, RO) - Discrete Input
  0x (bit, RW) - Discrete Coils
  3x (word, RO) - Input Registers
  4x (word, RW) - Holding Registers
  }
  TRegReadType = (
    tRegBoolRO=0, // 0x input coil
    tRegBoolRW=1, // 1x coil
    tRegWordRO=2, // 3x input reg
    tRegWordRW=3  // 4x holding reg
  );

  TModbusItem = record
    Addr: word;
    Value: word;
    RegType: TRegReadType;
  end;

  TModbusItemQueue = specialize TDeque<TModbusItem>;

  { TThreadModBus }

  TThreadModBus = class(TThread)
  protected
    // thread code (NO access to forms)
    procedure Execute; override;
    procedure CheckSize;
    procedure ModBusClientErrorEvent(const FunctionCode: Byte;
      const ErrorCode: Byte; const ResponseBuffer: TModBusResponseBuffer);
    procedure TCP_Disconnect(Sender: TObject);

  public
    StatusBarMsg: string;
    StatusBarStatus: string;

    IdModBusClient: TIdModBusClient;
    connected: boolean;
    MBWord: array of word;
    MBBool: array of boolean;
    MBReadErr: array of byte;
    ErrorPresent: boolean;
    ErrorLastCode: Byte;

    // sync vars
    RegType: TRegReadType;
    RegStart: integer;
    SetNewSize: integer;
    RegFormat: TRegShowFormat;
    WriteQueue: TModbusItemQueue;

    EventPauseAfterRead: TEventObject;
    SectionWriteQueueWork: TCriticalSection;

    constructor Create(CreateSuspended : boolean);

    // SYNC ZONE:
    // (thread safe zone - can access to forms)
    procedure Send(Addr, Value: Word);
    procedure Send(Addr, Value: string);
    procedure SyncDrawList;
    procedure SyncWriteStatusBar;
    procedure SyncUpdateVars;
    procedure SyncUpdateVarsOnChange;
    procedure SyncRemoveFromMainProg;
  end;



function RegToString(RegN: integer; Regs: array of word; format: TRegShowFormat): string;

resourcestring
  StatusBarOffline = 'offline';
  StatusBarOnline = 'ONLINE';
  StatusBarError = 'ERROR';
  StatusBarDisconnect = 'disconnect';
  StatusBarTimeout = 'timeout';

implementation

uses FormMain, ComCtrls, IdStack, {IdException,} IdExceptionCore;

function BoolStr(Data: boolean): string;
begin
  if Data = false then
    Result := '0' else
    Result := '1';
end;

function RegToString(RegN: integer; Regs: array of word; Format: TRegShowFormat
  ): string;
var i: integer;
begin
  Result := '';
  if RegN > High(Regs) then
    Result := '!' else
    case Format of
      rfDec: Result := IntToStr(Regs[RegN]);
      rfHex: Result := IntToHex(Regs[RegN], 4);
      rfBin:
        for i:=0 to 15 do begin
          Result := Result + BoolStr(GetBit(Regs[RegN], i));

        end;
      tfBool:
        Result := BoolToStr(Boolean(Regs[RegN]), true);

      //ToDo: float
      //tfFloat: ;
      //tfDouble; ;
    else
      Result := 'not support';
    end;
end;

{ TThreadModBus }

procedure TThreadModBus.SyncDrawList;
var i: integer;
    itm: TListItem;
begin
  for i:=0 to High(MBWord) do
  begin
    if i > frmMain.listMain.Items.Count-1 then
    begin
      itm := frmMain.listMain.Items.Add;
      itm.SubItems.Add(IntToStr(i+RegStart));//reg
      itm.SubItems.Add('');//val
      itm.SubItems.Add('');//type
      itm.SubItems.Add('');//name
      //itm.Caption:=row[0]+'=?';//main text
    end;
    case RegType of
      tRegWordRO, tRegWordRW:
        frmMain.listMain.Items[i].SubItems[idxColumnValue] := RegToString(i, MBWord, RegFormat);
      tRegBoolRO, tRegBoolRW:
        frmMain.listMain.Items[i].SubItems[idxColumnValue] := BoolStr(MBBool[i]);
    end;

    frmMain.listMain.Items[i].Caption := frmMain.listMain.Items[i].SubItems[idxColumnReg] + '=' + frmMain.listMain.Items[i].SubItems[idxColumnValue];
  end;

  if frmMain.listMain.Items.Count-1 > High(MBWord) then
    for i := High(MBWord)+1 to frmMain.listMain.Items.Count-1 do
    begin
      frmMain.listMain.Items[i].Caption := IntToStr(i+RegStart) + '=' + '-';
      frmMain.listMain.Items[i].SubItems[idxColumnValue] := '-';
    end;
end;

procedure TThreadModBus.SyncWriteStatusBar;
begin
  frmMain.StatusBar1.Panels[idxStatusBarStatus].Text := StatusBarStatus;
  frmMain.StatusBar1.Panels[idxStatusBarMainText].Text := StatusBarMsg;
end;

procedure TThreadModBus.SyncUpdateVars;
begin
  SetNewSize:=StrToIntDef(frmMain.edRegCount.Text, SetNewSize);
  RegStart:=StrToIntDef(frmMain.edRegAddr.Text, RegStart);
  if not frmMain.cbRegFormat.DroppedDown then
    RegFormat:=TRegShowFormat(frmMain.cbRegFormat.ItemIndex);
  if not frmMain.cbRegisterType.DroppedDown then
  begin
    if frmMain.cbRegisterType.ItemIndex>=0 then
      RegType:=TRegReadType(frmMain.cbRegisterType.ItemIndex) else
      RegType:=tRegWordRW;
  end;
end;

procedure TThreadModBus.SyncUpdateVarsOnChange;
begin
  if not frmMain.cbRegFormat.DroppedDown then
    RegFormat:=TRegShowFormat(frmMain.cbRegFormat.ItemIndex);
end;

procedure TThreadModBus.SyncRemoveFromMainProg;
begin
  if frmMain.threadRead <> nil then
    frmMain.threadRead := nil; // just drop pointer!   (FreeOnTerminate=true, give error - very strange...)
end;

procedure TThreadModBus.CheckSize;
begin
  if SetNewSize <> Length(MBWord) then
  begin
    //if RegStart + SetNewSize > 65535 then
    if SetNewSize > 65535 then
      SetNewSize := 65535;
    SetLength(MBWord, SetNewSize);
    SetLength(MBBool, SetNewSize);
    SetLength(MBReadErr, SetNewSize);
  end;
end;

procedure TThreadModBus.ModBusClientErrorEvent(const FunctionCode: Byte;
  const ErrorCode: Byte; const ResponseBuffer: TModBusResponseBuffer);
begin
  ErrorPresent := true;
  ErrorLastCode := ErrorCode and $8F;
end;

procedure TThreadModBus.TCP_Disconnect(Sender: TObject);
begin
  if not Terminated then
  begin
    Terminate;
    StatusBarMsg:=IdModBusClient.LastCmdResult.ToString;
    StatusBarStatus := StatusBarDisconnect;
    Synchronize(@SyncWriteStatusBar);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TThreadModBus.Create(CreateSuspended: boolean);
begin
  SectionWriteQueueWork := TCriticalSection.Create();
  WriteQueue := TModbusItemQueue.Create();

  IdModBusClient := TIdModBusClient.Create;
  IdModBusClient.AutoConnect:=false;
  IdModBusClient.Host:=frmMain.cbIP.Text;
  IdModBusClient.UnitID:=1;
  IdModBusClient.OnDisconnected:=@TCP_Disconnect;
  IdModBusClient.OnResponseError:=@ModBusClientErrorEvent;
  IdModBusClient.ConnectTimeout:=2000;

  self.OnTerminate:=@frmMain.threadReadTerminating;

  EventPauseAfterRead := TEventObject.Create(nil, false, false, '');

  SyncUpdateVars;
  CheckSize;

  connected:=false;
  FreeOnTerminate := false;
  inherited Create(CreateSuspended);
end;

procedure TThreadModBus.Send(Addr, Value: Word);
var itm: TModbusItem;
begin
  itm.RegType:=self.RegType;
  itm.Addr:=Addr;
  itm.Value:=Value;
  SectionWriteQueueWork.Enter;
  WriteQueue.PushFront(itm);
  SectionWriteQueueWork.Leave;
end;

procedure TThreadModBus.Send(Addr, Value: string);
var itm: TModbusItem;
begin
  if StrToIntDef(Addr, 65536) < 65536 then
  begin
    itm.Addr:=StrToInt(Addr);
    if StrToIntDef(Value, 65536) < 65536 then
    begin
      itm.Value:=StrToInt(Value);
      itm.RegType:=self.RegType;
      SectionWriteQueueWork.Enter;
      WriteQueue.PushFront(itm);
      SectionWriteQueueWork.Leave;
    end;
  end;
end;

procedure TThreadModBus.Execute;
var
  i, RegCountMax, Count, Max: integer;
  itm: TModbusItem;
label
  pauseAgain;

begin
  try
    try
      IdModBusClient.Connect;
    except
      on e: EIdSocketError do
      begin
        StatusBarMsg := e.Message;
        StatusBarStatus := StatusBarDisconnect;
        Synchronize(@SyncWriteStatusBar);
        exit;
      end;
      on e: EIdConnectTimeout do
      begin
        StatusBarMsg := e.Message;
        StatusBarStatus := StatusBarTimeout;
        Synchronize(@SyncWriteStatusBar);
        exit;
      end;
    end;

    connected:=true;
    StatusBarStatus := StatusBarOnline;
    Synchronize(@SyncWriteStatusBar);
    while (not Terminated) do
    begin
      try
        // MODE - solid list
        Max := 125;
        if RegStart >= 0 then
        begin
          // write all variable from queue
          while not WriteQueue.IsEmpty() do
          begin
            // safe extract
            SectionWriteQueueWork.Enter;
            itm := WriteQueue.Back();
            WriteQueue.PopBack();
            SectionWriteQueueWork.Leave;
            // write
            case itm.RegType of
              tRegBoolRW: IdModBusClient.WriteCoil(itm.Addr, itm.Value <> 0);
              tRegWordRW: IdModBusClient.WriteRegister(itm.Addr, itm.Value);
            end;
          end;

          // set in handler 'ModBusClientErrorEvent'
          ErrorLastCode := 0;
          ErrorPresent := false;

          RegCountMax := High(MBWord)+1;
          Count := RegCountMax div Max;
          for i:=0 to Count do
          begin
            case RegType of
              tRegBoolRO:
                IdModBusClient.ReadInputBits(RegStart+(i*Max), Max, MBBool[i*Max]);
              tRegBoolRW:
                IdModBusClient.ReadCoils(RegStart+(i*Max), Max, MBBool[i*Max]);
              tRegWordRO:
                IdModBusClient.ReadInputRegisters(RegStart+(i*Max), Max, MBWord[i*Max]);
              tRegWordRW:
                IdModBusClient.ReadHoldingRegisters(RegStart+(i*Max), Max, MBWord[i*Max]);
            end;
            FillChar(MBReadErr[i*Max], Max, ErrorLastCode);
          end;

          Count := RegCountMax mod Max;
          if Count <> 0 then
          begin
            i := ((RegCountMax div Max) * Max);
            case RegType of
              tRegBoolRO:
                IdModBusClient.ReadInputBits(RegStart+i, Count, MBBool[RegStart+i]);
              tRegBoolRW:
                IdModBusClient.ReadCoils(RegStart+i, Count, MBBool[RegStart+i]);
              tRegWordRO:
                IdModBusClient.ReadInputRegisters(RegStart+i, Count, MBWord[RegStart+i]);
              tRegWordRW:
                IdModBusClient.ReadHoldingRegisters(RegStart+i, Count, MBWord[RegStart+i]);
            end;
            FillChar(MBReadErr[RegStart+i], Count, ErrorLastCode);
          end;
          Synchronize(@SyncDrawList);

          if (ErrorPresent) and (ErrorLastCode <> 0) then
              StatusBarMsg := 'MB error = ' + IntToStr(ErrorLastCode);
          Synchronize(@SyncWriteStatusBar);
        end;

        //todo: MODE - segmental list
        //for i:=0 to frmMain.listMain.Items.Count-1 do
        //reg:=StrToIntDef(frmMain.listMain.Items[i].SubItems[idxColumnReg], -1);
        //ok := frmMain.IdModBusClient1.ReadHoldingRegister(reg, 2, MBWord);//float
        //Move(MBQWord[0], fl, 4);
        //frmMain.listMain.Items[i].SubItems[idxColumnValue] := FloatToStrF(fl, ffFixed, 0, 3);
        //...

        Synchronize(@SyncUpdateVars);
        CheckSize;
      except
        on e: EIdSocketError do
          TCP_Disconnect(nil);
        on e: EAccessViolation do
        begin
          StatusBarMsg := e.Message;
          StatusBarStatus := 'AV!';
          Synchronize(@SyncWriteStatusBar);
          Terminate;
        end;
        else
          Terminate;
      end;

      pauseAgain:
      if (EventPauseAfterRead.WaitFor(1000)=wrSignaled) and (not Terminated) then
      begin
        if Terminated then
          break;
        Synchronize(@SyncUpdateVarsOnChange);
        CheckSize;
        Synchronize(@SyncDrawList);
        goto pauseAgain;
      end;

    end; // Thread loop
    IdModBusClient.OnResponseError:=nil;
    IdModBusClient.OnDisconnected:=nil;

    try
      if IdModBusClient.Connected then
        IdModBusClient.Disconnect;
    except
      // skip any disconnect error
    end;

    if StatusBarStatus = StatusBarOnline then
    begin
      StatusBarStatus := StatusBarOffline;
      Synchronize(@SyncWriteStatusBar);
    end;

    Synchronize(@SyncRemoveFromMainProg);
    FreeAndNil(IdModBusClient);
    FreeAndNil(EventPauseAfterRead);
    FreeAndNil(WriteQueue);
    connected:=false;
  except
    on e: Exception do
    begin
      StatusBarMsg := e.Message;
      StatusBarStatus := 'EXCEPT';
      Synchronize(@SyncWriteStatusBar);
    end;
  end;

end;


end.


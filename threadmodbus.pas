unit ThreadModBus;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  Classes, SysUtils,
  syncobjs, gdeque,
  SuperViewZone,
  IdModbusClient, IdGlobal, ModbusTypes;

type
  {
    Dectimal (123)
    Dectimal sign (-123)
    Hex (F0A5)
    Bin (0000 0000 0000 0001)
    Boolean (0-gray, 1-green)
    Float (0.123 - 4 byte)
    Double Float (0.123 - 8 byte)
  }
  TRegShowFormat = (rfDec=0, rfHex=1, rfDecSign=2, rfBin, tfBool, tfFloat, tfDouble);

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
    Presenter: TSuperViewPresenter;

    // Main code. thread code (NO access to forms!)
    procedure Execute; override;
    // Read modbus registers. (run from thread)
    procedure ReadMB(RegType: TRegReadType; Addr, ArrOffs: word; Count: word);
    // Check buffer size, if needed then resize
    procedure CheckSize();
    // callback from "IdModBusClient" for error process errors.
    procedure ModBusClientErrorEvent(const FunctionCode: Byte;
      const ErrorCode: Byte; const ResponseBuffer: TModBusResponseBuffer);
    // callback from "IdModBusClient" for process disconnect.
    procedure TCP_Disconnect(Sender: TObject);

    procedure SendDataToView();
  public
    // status bar message. using in SyncWriteStatusBar.
    StatusBarMsg: string;
    // status bar message. using in SyncWriteStatusBar.
    StatusBarStatus: string;

    // RTU-true, TCP-false
    ModbusRTU: boolean;
    // ModbusTCP client
    IdModBusClient: TIdModBusClient;
    // flag - connected?
    Connected: boolean;
    // flag - is dissconect by user command?
    ValidDissconect: boolean;
    // flag - "access violation" exception detected
    AVDetect: boolean;

    // array for "Holding" register
    MBWord: array of word;
    // array for "Coil" register
    MBBool: array of boolean;
    // array for "Error" status. using in EventPauseAfterRead.
    MBReadErr: array of byte;

    // flag - last read have error? write in ModBusClientErrorEvent.
    ErrorPresent: boolean;
    // error code for flag ErrorPresent.
    ErrorLastCode: Byte;
    // global error code.
    ErrorCount: integer;
    // ??? WIP!
    ReadMBTime: DWord;

    // sync vars.
    RegType: TRegReadType;
    // sync vars.
    RegStart: integer;
    // sync vars.
    SetNewSize: integer;
    // sync vars.
    RegFormat: TRegShowFormat;

    // when user write date, is data wait queue here. sync vars!
    WriteQueue: TModbusItemQueue;

    // ??? wait reading. using MBReadErr.
    EventPauseAfterRead: TEventObject;
    // Critical Section for safety write to 'WriteQueue'
    CritWriteQueueWork: TCriticalSection;

    constructor Create(CreateSuspended : boolean; SetPresenter: TSuperViewPresenter);

    // SYNC ZONE:
    // (thread safe zone - can access to forms)
    procedure SyncEventConnect;
    procedure SyncEventDissconect;
    procedure SyncDrawList;
    procedure SyncWriteStatusBar;
    procedure SyncUpdateVars;
    procedure SyncUpdateVarsOnChange;
    procedure SyncRemoveFromMainProg;
    // procedure for sync zone
    procedure Send(Addr, Value: Word);
    procedure Send(Addr, Value: string);
    procedure UpdateRegAddr;
  end;

// convert register(s) to string (for visualisation)
function RegToString(RegN: integer; Regs: array of word; format: TRegShowFormat): string;

resourcestring
  StatusBarErrorCount = 'Errors';
  StatusBarOffline = 'offline';
  StatusBarOnline = 'ONLINE';
  StatusBarError = 'ERROR';
  StatusBarDisconnect = 'disconnect';
  StatusBarTimeout = 'timeout';
  strEnterValue = 'Enter register value';

implementation

uses
  RegExpr,
  StrUtils,
  FormMain,
  FormOptions,
  Graphics,
  ComCtrls, IdStack, {IdException,} IdExceptionCore, Character;

function BoolStr(Data: boolean): string;
begin
  if Data = false then
    Result := '0' else
    Result := '1';
end;

function RegToString(RegN: integer; Regs: array of word; Format: TRegShowFormat
  ): string;
var
  i: integer;
type
  PFloat = ^Single;
  PDouble = ^Double;
begin
  Result := '';
  if RegN > High(Regs) then
    Result := '!' else
    case Format of
      rfDec:
        Result := IntToStr(Regs[RegN]);
      rfHex:
        Result := IntToHex(Regs[RegN], 4);
      rfBin:
        for i:=0 to 15 do begin
          Result := Result + BoolStr(GetBit(Regs[RegN], i));
          if (i <> 15) and (i mod 4 = 3) then Result := Result + '.';
        end;
      tfBool:
        Result := BoolToStr(Boolean(Regs[RegN]), true);
      tfFloat:
        if (RegN mod 2) = 0 then
        begin
          if RegN+1 <= High(Regs) then
            Result := FloatToStr(PFloat(@Regs[RegN])^) else
            Result := '#';
        end;
      tfDouble:
        if (RegN mod 4) = 0 then
        begin
          if RegN+3 <= High(Regs) then
            Result := FloatToStr(PDouble(@Regs[RegN])^) else
            Result := '#';
        end;
    else
      Result := 'not support';
    end;
end;

{ TThreadModBus }

procedure TThreadModBus.SendDataToView();
var
  i: integer;
  MainText: WideString;
  reg: integer;
  val: string;
  name: WideString;
begin
  try
    for i:=Low(MBWord) to High(MBWord) do
    begin
      case RegType of
        tRegWordRO, tRegWordRW:
          val := RegToString(i, MBWord, RegFormat);
        tRegBoolRO, tRegBoolRW:
          val := BoolStr(MBBool[i]);
      else
        val := 'ERROR!!!';
      end;
      Presenter.ThreadAddEvent(IntToStr(i+RegStart), val, eventAdd);
    end;
  finally
  end;
end;

procedure TThreadModBus.SyncDrawList;
var
  i: integer;
  itm: TListItem;
begin
  try
    for i:=Low(MBWord) to High(MBWord) do
    begin
      {column:
        Caption - main text
        0-reg
        1-val
        2-type
        3-name}

      // update reg num
      {frmMain.listMain.Items[i].SubItems[idxColumnReg] := IntToStr(i+RegStart);
      // update value
      if MBReadErr[i] <> 0 then
        frmMain.listMain.Items[i].SubItems[ciColumnValue] := 'Err'+IntToStr(MBReadErr[i]) else
        begin
          case RegType of
            tRegWordRO, tRegWordRW:
              frmMain.listMain.Items[i].SubItems[ciColumnValue] := RegToString(i, MBWord, RegFormat);
            tRegBoolRO, tRegBoolRW:
              frmMain.listMain.Items[i].SubItems[ciColumnValue] := BoolStr(MBBool[i]);
          end;
        end;

      frmMain.listMain.Items[i].Caption := frmMain.listMain.Items[i].SubItems[idxColumnReg] + '=' + frmMain.listMain.Items[i].SubItems[ciColumnValue];
      }
    end;

    // remove items
    {if frmMain.listMain.Items.Count-1 > High(MBWord) then
      for i:=frmMain.listMain.Items.Count-1 downto High(MBWord)+1 do
         frmMain.listMain.Items.Delete(i);}

  finally
    //frmMain.listMain.EndUpdate;
  end;
end;

procedure TThreadModBus.SyncWriteStatusBar;
begin
  frmMain.StatusBar1.Panels[ciStatusBarStatus].Text := StatusBarStatus;
  if ErrorCount=0 then
    frmMain.StatusBar1.Panels[ciStatusBarErrorCount].Text := '' else
    frmMain.StatusBar1.Panels[ciStatusBarErrorCount].Text := StatusBarErrorCount + ' = ' + IntToStr(ErrorCount);
  frmMain.StatusBar1.Panels[ciStatusBarMainText].Text := StatusBarMsg;
end;

procedure TThreadModBus.SyncUpdateVars;
begin
  SetNewSize:=StrToIntDef(frmMain.edRegCount.Text, SetNewSize);
  UpdateRegAddr();
  if not frmMain.cbRegFormat.DroppedDown then
    RegFormat:=TRegShowFormat(frmMain.cbRegFormat.ItemIndex);
  if SetNewSize<>Length(MBWord) then
    Presenter.SetItemRange(RegStart, RegStart+SetNewSize-1);
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

procedure TThreadModBus.CheckSize();
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

constructor TThreadModBus.Create(CreateSuspended: boolean;
  SetPresenter: TSuperViewPresenter);
begin
  inherited Create(CreateSuspended);
  Presenter:=SetPresenter;
end;

procedure TThreadModBus.SyncEventConnect;
begin
  frmMain.shapeState.Brush.Color:=clGreen;
end;

procedure TThreadModBus.SyncEventDissconect;
begin
  if AVDetect then
  begin
    frmMain.shapeState.Brush.Color:=clMaroon;
  end else
  begin
    if (ValidDissconect) then
      frmMain.shapeState.Brush.Color:=clWhite else
      frmMain.shapeState.Brush.Color:=clRed;
  end;
end;

procedure TThreadModBus.Send(Addr, Value: Word);
var itm: TModbusItem;
begin
  itm.RegType:=self.RegType;
  itm.Addr:=Addr;
  itm.Value:=Value;
  CritWriteQueueWork.Enter;
  WriteQueue.PushFront(itm);
  CritWriteQueueWork.Leave;
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
      CritWriteQueueWork.Enter;
      WriteQueue.PushFront(itm);
      CritWriteQueueWork.Leave;
    end;
  end;
end;

procedure TThreadModBus.UpdateRegAddr;
var a: String;
begin
  a := Trim(frmMain.edRegAddr.Text);
  if (Length(a)=6) and (IsNumber(UnicodeString(a), 1)) then
  begin
    // 6 digit format
    RegStart:=StrToIntDef(Copy(a, 2, 5), RegStart);
    if not frmMain.cbRegisterType.DroppedDown then
    begin
      RegType:=TRegReadType(frmMain.cbRegisterType.ItemIndex);
      case a[1] of
        '1': frmMain.cbRegisterType.ItemIndex := 0; // 1x (bit, RO) - Discrete Input
        '0': frmMain.cbRegisterType.ItemIndex := 1; // 0x (bit, RW) - Discrete Coils
        '3': frmMain.cbRegisterType.ItemIndex := 2; // 3x (word, RO) - Input Registers
        '4': frmMain.cbRegisterType.ItemIndex := 3; // 4x (word, RW) - Holding Registers
      end;
    end;
  end else
  begin
    // normal format - just address
    RegStart:=StrToIntDef(a, RegStart);
    if not frmMain.cbRegisterType.DroppedDown then
      RegType:=TRegReadType(frmMain.cbRegisterType.ItemIndex);
  end;
end;


////////////////////////////////////////////////////////////////////////////////

{constructor TThreadModBus.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
end;}

procedure TThreadModBus.ReadMB(RegType: TRegReadType; Addr, ArrOffs: word; Count: word);
var i2: integer;
var
  TempWordArr: array of word;
  TempBoolArr: array of boolean;
  r: boolean;
begin
  SetLength(TempWordArr, Count);
  SetLength(TempBoolArr, Count);
  //Note: we cant transfer to function 'pointer' of part array, because this function use dynamic array
  //IdModBusClient.ReadHoldingRegisters(RegStart+(i*Max), Max, MBWord[i*Max]); - incorrect! memory is corrupted!

  // change in handler 'ModBusClientErrorEvent'
  ErrorLastCode := 0;

  if not ModbusRTU then
  begin
    // read Modbus TCP
    case RegType of
      tRegBoolRO:
        r:=IdModBusClient.ReadInputBits(Addr, Count, TempBoolArr);
      tRegBoolRW:
        r:=IdModBusClient.ReadCoils(Addr, Count, TempBoolArr);
      tRegWordRO:
        r:=IdModBusClient.ReadInputRegisters(Addr, Count, TempWordArr);
      tRegWordRW:
        r:=IdModBusClient.ReadHoldingRegisters(Addr, Count, TempWordArr);
    end;
  end else
  begin
    //todo: read Modbus RTU
  end;

  // move readed data to data array
  case RegType of
    tRegBoolRO, tRegBoolRW: begin
      for i2:=0 to Count-1 do
        MBBool[ArrOffs+i2] := TempBoolArr[i2];
    end;
    tRegWordRO, tRegWordRW: begin
      for i2:=0 to Count-1 do
        MBWord[ArrOffs+i2] := TempWordArr[i2];
    end;
  end;
  if not r then
    Inc(ErrorCount);
  FillChar(MBReadErr[ArrOffs], Count, ErrorLastCode);
end;

procedure TThreadModBus.Execute;
var
  i, RegCountMax, Count, Max: integer;
  itm: TModbusItem;
  ip: string;

label
  pauseAgain;

begin
  ReadMBTime := 50;
  CritWriteQueueWork := TCriticalSection.Create();
  WriteQueue := TModbusItemQueue.Create();

  Self.OnTerminate:=@frmMain.threadReadTerminating;
  Self.FreeOnTerminate := true;

  EventPauseAfterRead := TEventObject.Create(nil, false, false, '');

  SyncUpdateVars();
  CheckSize();

  if not ModbusRTU then
  begin
    // Modbus TCP
    IdModBusClient := TIdModBusClient.Create;
    IdModBusClient.AutoConnect:=false;
    ip := Trim(frmMain.cbIP.Text);
    if (Pos('[', ip) <> 0) or (Pos('::', ip) <> 0) then
      IdModBusClient.IPVersion:=Id_IPv6;
    if ExecRegExpr('.*[^:][:][0-9]{1,5}', ip) then
    begin
      // port definition detected
      IdModBusClient.Port := StrToInt(Copy(ip, RPos(':', ip)+1, 5));
      ip := Copy(ip, 1, RPos(':', ip)-1);
    end;
    IdModBusClient.Host:=ip;
    IdModBusClient.UnitID:=1;
    IdModBusClient.OnDisconnected:=@TCP_Disconnect;
    IdModBusClient.OnResponseError:=@ModBusClientErrorEvent;
    IdModBusClient.ConnectTimeout:=2000;
    IdModBusClient.ReadTimeout:=1000;
    if frmOptions.chBaseRegisterIs1.Checked then
      IdModBusClient.BaseRegister:=1 else
      IdModBusClient.BaseRegister:=0;

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
  end else
  begin
    // Modbus RTU
    //todo: WIP...
  end;

  try
    Connected:=true;
    Synchronize(@SyncEventConnect);
    StatusBarStatus := StatusBarOnline;
    Synchronize(@SyncWriteStatusBar);

    //////////////// main cycle ////////////////
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
            CritWriteQueueWork.Enter;
            itm := WriteQueue.Back();
            WriteQueue.PopBack();
            CritWriteQueueWork.Leave;
            // write
            if not ModbusRTU then
            begin
              case itm.RegType of
                tRegBoolRW: IdModBusClient.WriteCoil(itm.Addr, itm.Value <> 0);
                tRegWordRW: IdModBusClient.WriteRegister(itm.Addr, itm.Value);
              end;
            end else
            begin
              //todo: write RTU

            end;
          end;

          // read blocks by 125 regs
          RegCountMax := High(MBWord)+1;
          Count := RegCountMax div Max;
          for i:=0 to Count-1 do
            ReadMB(RegType, RegStart + i*Max, i*Max, Max);

          // read last block
          Count := RegCountMax mod Max;
          if Count <> 0 then
          begin
            i := ((RegCountMax div Max) * Max);
            ReadMB(RegType, RegStart + i, i, Count);
          end;

          SendDataToView();
          //Synchronize(@SyncDrawList);

          if (ErrorPresent) and (ErrorLastCode <> 0) then
              StatusBarMsg := 'MB error = ' + IntToStr(ErrorLastCode);
          Synchronize(@SyncWriteStatusBar);
        end;

        //todo: MODE - segmental list
        //for i:=0 to frmMain.listMain.Items.Count-1 do
        //reg:=StrToIntDef(frmMain.listMain.Items[i].SubItems[idxColumnReg], -1);
        //ok := frmMain.IdModBusClient1.ReadHoldingRegister(reg, 2, MBWord);//float
        //Move(MBQWord[0], fl, 4);
        //frmMain.listMain.Items[i].SubItems[ciColumnValue] := FloatToStrF(fl, ffFixed, 0, 3);
        //...

        Synchronize(@SyncUpdateVars);
        CheckSize;
      except
        on e: EIdSocketError do
          TCP_Disconnect(nil);
        on e: EAccessViolation do
        begin
          AVDetect := true;
          StatusBarMsg := e.Message;
          StatusBarStatus := 'AV!';
          Synchronize(@SyncWriteStatusBar);
          Terminate;
        end;
        else
          Terminate;
      end;

      pauseAgain:
      if (EventPauseAfterRead.WaitFor(ReadMBTime)=wrSignaled) and (not Terminated) then
      begin
        Synchronize(@SyncUpdateVarsOnChange);
        CheckSize();
        Synchronize(@SyncDrawList);
        goto pauseAgain;
      end;

    end;
    //////////////// end main cycle ////////////////


    try
      if not ModbusRTU then
      begin
        IdModBusClient.OnResponseError:=nil;
        IdModBusClient.OnDisconnected:=nil;
        { if IdModBusClient.Connected then
        "Connected" make infinity loop!
        IdIOHandlerStack.pas:
          function TIdIOHandlerStack.Connected: Boolean;
          begin
            ReadFromSource(False, 0, False); <-!!!
        }
        IdModBusClient.Disconnect;
      end;
    except
      // skip any disconnect error
    end;
    Synchronize(@SyncEventDissconect);

    if StatusBarStatus = StatusBarOnline then
    begin
      StatusBarStatus := StatusBarOffline;
      Synchronize(@SyncWriteStatusBar);
    end;

    Synchronize(@SyncRemoveFromMainProg);
    if not ModbusRTU then FreeAndNil(IdModBusClient);
    FreeAndNil(EventPauseAfterRead);
    FreeAndNil(WriteQueue);
    FreeAndNil(CritWriteQueueWork);
    SetLength(MBWord, 0);
    SetLength(MBBool, 0);
    SetLength(MBReadErr, 0);
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


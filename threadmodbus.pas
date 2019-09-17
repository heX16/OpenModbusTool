unit ThreadModBus;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  Classes, SysUtils,
  syncobjs, gdeque,
  SuperViewZone,
  Variants,
  IdModbusClient, IdGlobal, IdLogDebug, IdIntercept, ModbusTypes;

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
  //todo: rename!!!!!!!!!!!!!!!! rf,tf
  TRegShowFormat = (
    rfDec=0,
    rfDecSign=1,
    rfHex=2,
    rfBin=3,
    tfBool=4,
    tfFloat4=5,
    tfFloat8=6);

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

  TQWordReal = packed record
    case byte of
    0: (Words: array [0..3] of Word);
    1: (W: Word);
    1: (Float4: Single);
    2: (Float8: Double);
    3: (DWords: array [0..1] of DWord);
    4: (varQWord: QWord);
  end;

  TModbusItemQueue = specialize TDeque<TModbusItem>;

  { TSuperViewPresenterModbus }

  TSuperViewPresenterModbus = class(TSuperViewPresenter)
    RegStart: integer;
    RegType: TRegReadType;
    RegCount: integer;
    RegShowFormat: TRegShowFormat;
    Swap2byte: boolean;
    Swap2reg: boolean;
    Swap2Dword: boolean;
    ChangeCountNeedUpdateVisual: boolean;

    function GetText(Node: TSuperViewItem; Col: integer): UTF8String; override;
    function GetTextCompactMode(Node: TSuperViewItem): UTF8String; override;
    function GetTextLog(Node: TSuperViewItem): UTF8String; override;
  end;

  { TThreadModBus }

  TThreadModBus = class(TThread)
  protected
    Presenter: TSuperViewPresenterModbus;

    // Main code. thread code (NO access to forms!)
    procedure Execute; override;
    // Read modbus registers. (run from thread)
    procedure ReadMB(RegType: TRegReadType; Addr, ArrOffs: word; Count: word);
    // Check buffer size, if needed then resize
    procedure UpdateRegCount();
    // callback from "IdModBusClient" for error process errors.
    procedure ModBusClientErrorEvent(const FunctionCode: Byte;
      const ErrorCode: Byte; const ResponseBuffer: TModBusResponseBuffer);
    // logging
    procedure IdTCPLogDebugReceive(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);
    // logging
    procedure IdTCPLogDebugSend(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);
    // callback from "IdModBusClient" for process disconnect.
    procedure TCP_Disconnect(Sender: TObject);
    // call "Presenter.ThreadAddEvent" for new readed regs to event list
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
    // TCP Logging
    IdTCPIPLogDebug: TIdLogDebug;
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

    // when user write date, is data wait queue here. sync vars!
    WriteQueue: TModbusItemQueue;

    // wait reading. using MBReadErr.
    //todo: небезопасен - к нему есть обращение снаружи треда. переделать - чтобы сюда отдавался внешний TEventObject при инициализации.
    EventPauseAfterRead: TEventObject;

    // Critical Section for safety write to 'WriteQueue'
    CritWriteQueueWork: TCriticalSection;

    constructor Create(CreateSuspended: boolean;
      SetPresenter: TSuperViewPresenterModbus);

    // SYNC ZONE:
    // (thread safe zone - can access to forms)
    procedure SyncEventConnect;
    procedure SyncEventDissconect;

    //todo: remove old code!!!
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
function RegToString(Regs: TQWordReal; format: TRegShowFormat; Swap2byte, Swap2reg, Swap2dword: boolean): string;

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
  FormSwapConfig,
  Graphics,
  ComCtrls, IdStack, {IdException,} IdExceptionCore;

function BoolStr(Data: boolean): string;
begin
  if Data = false then
    Result := '0' else
    Result := '1';
end;

function RegToString(Regs: TQWordReal; format: TRegShowFormat; Swap2byte, Swap2reg, Swap2dword: boolean): string;
var
  i: integer;
type
  PFloat = ^Single;
  PDouble = ^Double;
begin
  Result := '';
  case Format of
    rfDec:
      Result := IntToStr(UInt16(Regs.W));
    rfDecSign:
      Result := IntToStr(Int16(Regs.W));
    rfHex:
      Result := IntToHex(Regs.W, 4);
    rfBin:
      for i:=0 to 15 do begin
        Result := Result + BoolStr(GetBit(Regs.W, i));
        if (i <> 15) and (i mod 4 = 3) then Result := Result + '_';
      end;
    tfBool:
      Result := BoolToStr(Boolean(Regs.W), true);
    tfFloat4:
      Result := FloatToStrF(Regs.Float4, ffFixed, 0, 3, DefaultFormatSettings);
    tfFloat8:
      Result := FloatToStrF(Regs.Float8, ffFixed, 0, 3, DefaultFormatSettings);
  else
    Result := 'not support';
  end;
end;

{ TSuperViewPresenterModbus }

function TSuperViewPresenterModbus.GetText(Node: TSuperViewItem; Col: integer
  ): UTF8String;
var
  RegFirst: integer;
  RegN: integer;
  RegMBN: integer;
  Regs: TQWordReal;
  r2: TSuperViewItem;
  r3: TSuperViewItem;
  r4: TSuperViewItem;
begin
  Result := '';
  RegFirst := StrToIntDef(Node.Path, -1);
  Regs.varQWord:=0;
  if Col = 0 then
    Result:=inherited GetText(Node, Col)
  else
    if (RegFirst >= 0) and (VarIsNumeric(Node.Info)) then
    begin
      RegN := RegFirst - self.RegStart;
      Regs.Words[0] := Node.Info;
      if Swap2byte then
        Regs.Words[0] := swap(Regs.Words[0]);

      if (self.RegShowFormat=tfFloat4) then begin
        if (RegN mod 2) = 0 then begin
          RegMBN := StrToInt(Node.Name);
          //todo: BUG!!!! почемуто Node.Name=20000 но при этом Node.Path=2000
          r2 := Find(IntToStr(RegMBN+1));
          if (r2 <> nil) and (VarIsNumeric(r2.Info)) then begin
            Regs.Words[1] := r2.Info;

            // swap bytes
            if Swap2byte then
              Regs.Words[1] := swap(Regs.Words[1]);
            if Swap2reg then
              Regs.DWords[0] := swap(Regs.DWords[0]);
          end else
            Result := '#';
        end else
          Result := '#';
      end;

      if (self.RegShowFormat=tfFloat8) then begin
        if (RegN mod 4) = 0 then begin
          r2 := Find(IntToStr(RegN+1));
          r3 := Find(IntToStr(RegN+2));
          r4 := Find(IntToStr(RegN+3));
          if (r2 <> nil) and (r3 <> nil) and (r4 <> nil) and
             (VarIsNumeric(r2.Info)) and
             (VarIsNumeric(r3.Info)) and
             (VarIsNumeric(r4.Info)) then
          begin
            // load data
            Regs.Words[1] := r2.Info;
            Regs.Words[2] := r3.Info;
            Regs.Words[3] := r4.Info;

            // swap bytes
            if Swap2byte then begin
              Regs.Words[1] := swap(Regs.Words[1]);
              Regs.Words[2] := swap(Regs.Words[2]);
              Regs.Words[3] := swap(Regs.Words[3]);
            end;
            if Swap2reg then begin
              Regs.DWords[0] := swap(Regs.DWords[0]);
              Regs.DWords[1] := swap(Regs.DWords[1]);
            end;
            if Swap2dword then begin
              Regs.varQWord := swap(Regs.varQWord);
            end;
          end else
            Result := '#';
        end else
          Result := '#';
      end;

      if Result = '' then
        Result := RegToString(Regs, self.RegShowFormat, Swap2byte, Swap2reg, Swap2Dword);
    end else
      Result := '#';
end;

function TSuperViewPresenterModbus.GetTextCompactMode(Node: TSuperViewItem
  ): UTF8String;
begin
  Result:=inherited GetTextCompactMode(Node);
end;

function TSuperViewPresenterModbus.GetTextLog(Node: TSuperViewItem): UTF8String;
var i: integer;
begin
  Result := '';
  if not VarIsNull(Node.Info) and VarIsArray(Node.Info) then begin
    for i:=VarArrayLowBound(Node.Info, 1) to VarArrayHighBound(Node.Info, 1) do begin
      Result := Result + ',' + Node.Info[i];
    end;
  end;
  //Result:=inherited GetTextLog(Node);
end;

{ TThreadModBus }

constructor TThreadModBus.Create(CreateSuspended: boolean;
  SetPresenter: TSuperViewPresenterModbus);
begin
  inherited Create(CreateSuspended);
  Presenter:=SetPresenter;
end;

procedure TThreadModBus.SendDataToView();
var
  i: integer;
  val: Word;
begin
  try
    for i:=Low(MBWord) to High(MBWord) do
    begin
      case RegType of
        tRegWordRO, tRegWordRW:
          val := MBWord[i];
        tRegBoolRO, tRegBoolRW:
          val := Word(MBBool[i]);
      else
        val := 0;
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

procedure TThreadModBus.UpdateRegAddr;
begin
  if (Presenter.RegStart <> self.RegStart) or (Presenter.RegType <> self.RegType) then begin
    self.RegType:=Presenter.RegType;
    self.RegStart:=Presenter.RegStart;
    Presenter.SetItemRange(self.RegStart, self.RegStart+Length(MBWord)-1);
  end;
end;

procedure TThreadModBus.SyncUpdateVars;
begin
  if not Terminated then begin
    UpdateRegAddr();
  end;
end;

procedure TThreadModBus.SyncUpdateVarsOnChange;
begin
  if Presenter.ChangeCountNeedUpdateVisual then begin
    // need remove old event, else event add old items to new list
    Presenter.ClearThreadEventBuffer();
    Presenter.SetItemRange(RegStart, RegStart+Presenter.RegCount-1);
    Presenter.ChangeCountNeedUpdateVisual := false;
    Presenter.ClearThreadEventBuffer();
  end;

  //todo: глобальный баг - при закрытии главного окна, окно frmOptions разрушается, но тред еще работает, нужно налаживать синхронизацию...
  if frmOptions.chBaseRegisterIs.ItemIndex = 0 then
    IdModBusClient.BaseRegister:=0 else
    IdModBusClient.BaseRegister:=1;
end;

procedure TThreadModBus.SyncRemoveFromMainProg;
begin
  if frmMain.threadRead <> nil then
    frmMain.threadRead := nil; // just drop pointer!   (FreeOnTerminate=true, give error - very strange...)
end;

procedure TThreadModBus.UpdateRegCount();
begin
  if Presenter.RegCount <> Length(MBWord) then
  begin
    //if RegStart + SetNewSize > 65535 then
    //todo: if read coil - can more
    if Presenter.RegCount > 65535 then
      Presenter.RegCount := 65535;
    SetLength(MBWord, Presenter.RegCount);
    SetLength(MBBool, Presenter.RegCount);
    SetLength(MBReadErr, Presenter.RegCount);
    Presenter.ChangeCountNeedUpdateVisual := true;
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


procedure TThreadModBus.IdTCPLogDebugReceive(ASender: TIdConnectionIntercept;
  var ABuffer: TIdBytes);
begin
  //Presenter. ...;
  //ShowMessage(inttostr(ABuffer[0]));
end;

procedure TThreadModBus.IdTCPLogDebugSend(ASender: TIdConnectionIntercept;
  var ABuffer: TIdBytes);
begin
  //ShowMessage(inttostr(ABuffer[0]));
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
  pauseResult: TWaitResult;

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
  UpdateRegCount();

  if not ModbusRTU then
  begin
    // Modbus TCP
    IdModBusClient := TIdModBusClient.Create();
    IdModBusClient.AutoConnect:=false;

    IdTCPIPLogDebug := TIdLogDebug.Create();
    IdTCPIPLogDebug.OnReceive:=@IdTCPLogDebugReceive;
    IdTCPIPLogDebug.OnSend:=@IdTCPLogDebugSend;
    IdModBusClient.Intercept:=IdTCPIPLogDebug;

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
    if frmOptions.chBaseRegisterIs.ItemIndex = 0 then
      IdModBusClient.BaseRegister:=0 else
      IdModBusClient.BaseRegister:=1;

    try
      IdModBusClient.Connect;
    except
      //todo: MEM LEAK!!!
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

          //todo:??? почему я закомментил???
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
        UpdateRegCount();
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
      // pause between reads
      pauseResult := EventPauseAfterRead.WaitFor(ReadMBTime);

      if not Terminated then begin
        UpdateRegCount();
        Synchronize(@SyncUpdateVarsOnChange);
        Synchronize(@SyncDrawList);
      end;

      // analize pause result:
      if (pauseResult=wrSignaled) and (not Terminated) then
      begin
        // pause again after change
        // такой подход дает двойную паузу чтения
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
    if not ModbusRTU then begin
      IdModBusClient.Intercept:=nil;
      FreeAndNil(IdTCPIPLogDebug);
      FreeAndNil(IdModBusClient);
    end;
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


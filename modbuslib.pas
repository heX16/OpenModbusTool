unit modbuslib;

interface

// Define constants for the ModBus functions
const
  mbfError = $00;
  mbfReadCoils = $01;
  mbfReadInputBits = $02;
  mbfReadRegs = $03;
  mbfReadInputRegs = $04;
  mbfWriteCoil = $05;
  mbfWriteReg = $06;
  mbfWriteCoils = $0F;
  mbfWriteRegs = $10;
  mbfReadFileRecord = $14;
  mbfWriteFileRecord = $15;
  mbfMaskWriteReg = $16;
  mbfReadWriteRegs = $17;
  mbfReadFiFoQueue = $18;

// Define constants for the ModBus exceptions
const
  mbeOk = $00;
  mbeIllegalFunction = $01;
  mbeIllegalRegister = $02;
  mbeIllegalDataValue = $03;
  mbeServerFailure = $04;
  mbeAcknowledge = $05;
  mbeServerBusy = $06;
  mbeGatewayPathNotAvailable = $0A;
  mbeGatewayNoResponseFromTarget = $0B;

const
  mbTCPPort = 502;
  mbMaxRegLen = 125;
  mbMaxCoils = 2000;
  mbMaxDataLen = (mbMaxRegLen * 2) + 2; // (add 2 byte for CRC)

type
  TByteArray = array [0..65535] of byte;
  PByteArray = ^TByteArray;

type
  TModBusFunction = Byte;

type
  TModBusDataReg = array[0..mbMaxRegLen] of Word;
  TModBusData = array[0..mbMaxDataLen] of Byte;

type
  TModBusHeaderTCP = packed record
    TransactionID: Word;
    ProtocolID: Word;
    RecLength: Word;
  end;

  // Requests: ////////////////

  // Func: 01, 02, 03, 04
  TModBusSendFuncRead = packed record
    Count: Word; // The total number of coils/registers requested
    CRC: Word;
  end;

  // Func: 05, 06
  TModBusSendFuncSingleWrite = packed record
    // Write Single Coil (FC=05): The status to write ( FF00 = ON,  0000 = OFF )
    // Write Single Reg. (FC=06): The value to write
    Value: Word;
    CRC: Word;
  end;

  // Func: 15 (0x0F), 16 (0x10)
  TModBusSendFuncMultipleWrite = packed record
    CountReg: Word;
    CountByte: Byte;
    case byte of
    0: (ValuesByte: TModBusData);
    1: (ValuesWord: TModBusDataReg);
    //at last: CRC: Word;
  end;

  TModBusSendBuffer = packed record
    //Header: TModBusHeaderTCP; // TCP "Payload"
    UnitID: Byte; // The Slave Address
    FunctionCode: TModBusFunction; // The Function Code
    RegAddress: Word; // The Data Address of the coil/register
    // Payload:
    case byte of
    0: (Read: TModBusSendFuncRead);
    1: (SingleWrite: TModBusSendFuncSingleWrite);
    2: (MultipleWrite: TModBusSendFuncMultipleWrite);
  end;

  // Responses: ////////////////

  // Func: 01, 02, 03, 04
  TModBusRecvFuncRead = packed record
    CountByte: Byte; // The number of data bytes to follow
    case byte of
    0: (ValuesByte: TModBusData);
    1: (ValuesWord: TModBusDataReg);
    //at last: CRC: Word;
  end;

  // Func: 05, 06
  TModBusRecvFuncSingleWrite = packed record
    RegAddress: Word; // The Data Address of the coil/register
    // Write Single Coil (FC=05): The status to write ( FF00 = ON,  0000 = OFF )
    // Write Single Reg. (FC=06): The value to write
    Value: Word;
    CRC: Word;
  end;

  // Func: 15 (0x0F), 16 (0x10)
  TModBusRecvFuncMultipleWrite = packed record
    RegAddress: Word; // The Data Address of the coil/register
    WriteCount: Word; // The number of coils/registers to written
    CRC: Word;
  end;

  TModBusRecvBuffer = packed record
    //Header: TModBusHeaderTCP; // TCP addon
    UnitID: Byte; // The Slave Address
    FunctionCode: TModBusFunction; // The Function Response Code
    // Payload:
    case byte of
    0: (Read: TModBusRecvFuncRead);
    1: (SingleWrite: TModBusRecvFuncSingleWrite);
    2: (MultipleWrite: TModBusRecvFuncMultipleWrite);
  end;











  // object for build modbus packet
  TModbusBuilder = object
    s: TModBusSendBuffer;
    FBaseRegister: shortint;
    FLastTransactionID: word;
    RawBuffer: array of byte;
    function TCPGetNewTransactionID: word;

    procedure ReadRegistersCommon(UnitID: byte; Func: byte; RegNo: Word; Count: Word);

    procedure ReadCoils(UnitID: byte; RegNo: Word; Count: Word);
    procedure ReadRegisters(UnitID: byte; RegNo: Word; Count: Word);
    procedure ReadInputBits(UnitID: byte; RegNo: Word; Count: Word);
    procedure ReadInputRegisters(UnitID: byte; RegNo: Word; Count: Word);

    procedure WriteCoil(UnitID: byte; RegNo: Word; Value: Boolean);
    procedure WriteHoldingRegister(UnitID: byte; RegNo: Word; Value: Word);

    procedure WriteCoils(UnitID: byte; RegNo: Word; Count: Word; RegisterData: array of Boolean);
    procedure WriteHoldingRegisters(UnitID: byte; RegNo: Word; Count: Word; RegisterData: array of Word);
  end;

  // object for parse modbus packet

  { TModbusParser }

  TModbusParser = object
    r: TModBusRecvBuffer;
    function GetFunc(): Byte;
    function GetErrorCode(): Byte;

    function ReadCoils(var UnitID: byte; var Count: Word; var RegisterData: array of Boolean): boolean;
    function ReadRegisters(var UnitID: byte; var Count: Word; var RegisterData: array of Word): boolean;
  end;

implementation

procedure PutBit(var Value: Byte; Index: Byte; State: Boolean); inline;
begin
  Value := (Value and ((Byte(1) shl Index) xor High(Byte))) or (Byte(State) shl Index);
end;

function GetBit(Value: Byte; Index: Byte): Boolean; inline;
begin
  Result := ((Value shr Index) and 1) = 1;
end;

function Swap16(DataToSwap: Word): Word;
begin
  //todo: optimize!
  Result := (DataToSwap div 256) + ((DataToSwap mod 256)*256);
end;

function CalcCRC16(Buffer: PByteArray; Len: Word): Word;
const
  CRC16Table: array[0..255] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
  );
var
  i: Integer;
  bTemp: Byte;
begin
  Result := 0;
  if Len = 0 then
    exit;
  Len := Len - 1;
  for i := 0 to Len do
  begin
    bTemp := Buffer^[i] xor Result;
    Result := Result shr 8;
    Result := Result xor CRC16Table[bTemp];
  end;
end;

{ TModbusParser }

function TModbusParser.GetFunc: Byte;
begin
  if r.FunctionCode > 127 then
    Result := mbfError else
    Result := r.FunctionCode;
end;

function TModbusParser.GetErrorCode: Byte;
begin
  //todo: WIP!!!!
end;

function TModbusParser.ReadCoils(var UnitID: byte; var Count: Word;
  var RegisterData: array of Boolean): boolean;
var
  i: integer;
  CRC: word;
  CRCpacket: word;
begin
  UnitID:=r.UnitID;
  Count:=r.Read.CountByte * 8; // The number of data bytes to follow (Coils / 8 bits per byte)
  // copy data from packet
  for i:=0 to Count-1 do
    RegisterData[i] := GetBit(r.Read.ValuesByte[i div 8], i mod 8);

  // get crc from packet
  CRCpacket := r.Read.ValuesByte[r.Read.CountByte + 1] shl 8;
  CRCpacket := CRCpacket or r.Read.ValuesByte[r.Read.CountByte + 2];
  // calc crc
  CRC := CalcCRC16(@r, r.Read.CountByte + 3);
  // compare crc
  Result := CRC = CRCpacket;
end;

function TModbusParser.ReadRegisters(var UnitID: byte; var Count: Word;
  var RegisterData: array of Word): boolean;
var
  i: integer;
  CRC: word;
  CRCpacket: word;
begin
  UnitID:=r.UnitID;
  Count:=r.Read.CountByte div 2; // The number of data bytes to follow (registers x 2 bytes each)
  // copy data from packet
  for i:=0 to Count-1 do
    RegisterData[i] := r.Read.ValuesWord[i];

  // get crc from packet
  CRCpacket := r.Read.ValuesByte[r.Read.CountByte + 1] shl 8;
  CRCpacket := CRCpacket or r.Read.ValuesByte[r.Read.CountByte + 2];
  // calc crc
  CRC := CalcCRC16(@r, r.Read.CountByte + 3);
  // compare crc
  Result := CRC = CRCpacket;
end;

function TModbusBuilder.TCPGetNewTransactionID: word;
begin
  //todo: push/pop { $OverflowChecks-}
  Inc(FLastTransactionID);
  Result := FLastTransactionID;
end;

procedure TModbusBuilder.ReadCoils(UnitID: byte; RegNo: Word; Count: Word);
begin
  ReadRegistersCommon(UnitID, mbfReadCoils, RegNo, Count);
end;

procedure TModbusBuilder.ReadRegisters(UnitID: byte; RegNo: Word; Count: Word);
begin
  ReadRegistersCommon(UnitID, mbfReadRegs, RegNo, Count);
end;

procedure TModbusBuilder.ReadInputBits(UnitID: byte; RegNo: Word; Count: Word);
begin
  ReadRegistersCommon(UnitID, mbfReadInputBits, RegNo, Count);
end;

procedure TModbusBuilder.ReadInputRegisters(UnitID: byte; RegNo: Word;
  Count: Word);
begin
  ReadRegistersCommon(UnitID, mbfReadInputRegs, RegNo, Count);
end;

procedure TModbusBuilder.ReadRegistersCommon(UnitID: byte; Func: byte;
  RegNo: Word; Count: Word);
begin
  s.FunctionCode:=Func;
  s.RegAddress:=RegNo;
  s.UnitID:=UnitID;
  s.Read.Count:=Count;
  s.Read.CRC:=CalcCRC16(@s, 6);
end;

procedure TModbusBuilder.WriteCoil(UnitID: byte; RegNo: Word; Value: Boolean);
begin
  s.FunctionCode:=mbfWriteCoil;
  s.RegAddress:=RegNo;
  s.UnitID:=UnitID;
  if Value=true then
    s.SingleWrite.Value:=$FF00 else
    s.SingleWrite.Value:=$0000;
  s.SingleWrite.CRC:=CalcCRC16(@s, 6);
end;

procedure TModbusBuilder.WriteHoldingRegister(UnitID: byte; RegNo: Word;
  Value: Word);
begin
  s.FunctionCode:=mbfWriteReg;
  s.RegAddress:=RegNo;
  s.UnitID:=UnitID;
  s.SingleWrite.Value:=Value;
  s.SingleWrite.CRC:=CalcCRC16(@s, 6);
end;

procedure TModbusBuilder.WriteCoils(UnitID: byte; RegNo: Word; Count: Word;
  RegisterData: array of Boolean);
var
  i: integer;
  CRC: word;
begin
  s.FunctionCode:=mbfWriteCoils;
  s.RegAddress:=RegNo;
  s.UnitID:=UnitID;
  s.MultipleWrite.CountReg:=Count;
  s.MultipleWrite.CountByte:=Count div 8; // The number of data bytes to follow (Coils <count> / 8 bits per byte)
  for i:=0 to Count-1 do
    PutBit(s.MultipleWrite.ValuesByte[i div 8], i mod 8, RegisterData[i]);
  CRC := CalcCRC16(@s, s.MultipleWrite.CountByte + 7);
  s.MultipleWrite.ValuesByte[s.MultipleWrite.CountByte + 1] := Hi(CRC);
  s.MultipleWrite.ValuesByte[s.MultipleWrite.CountByte + 2] := Lo(CRC);
end;

procedure TModbusBuilder.WriteHoldingRegisters(UnitID: byte; RegNo: Word;
  Count: Word; RegisterData: array of Word);
var
  i: integer;
  CRC: word;
begin
  s.FunctionCode:=mbfWriteRegs;
  s.RegAddress:=RegNo;
  s.UnitID:=UnitID;
  s.MultipleWrite.CountReg:=Count;
  s.MultipleWrite.CountByte:=Count * 2; // The number of data bytes to follow (registers <count> x 2 bytes each)
  for i:=0 to Count-1 do
    s.MultipleWrite.ValuesWord[i] := RegisterData[i];
  CRC := CalcCRC16(@s, s.MultipleWrite.CountByte + 7);
  s.MultipleWrite.ValuesByte[s.MultipleWrite.CountByte + 1] := Hi(CRC);
  s.MultipleWrite.ValuesByte[s.MultipleWrite.CountByte + 2] := Lo(CRC);
end;

end.


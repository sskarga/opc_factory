unit Server_Protocol_Mercury;

{
  Модуль опроса счетчика меркурий 230-R с трансфоматорами тока и напряжения
  Пароль: 111111 (строка 299)

  - коэф. трансформации напряжения
  - коэф. трансформации тока

  - напряжение,  В       *0,01
  - ток,         А       *0,001
  - мощность,    Вт      *0,01

  - активна энергия       кВт*ч   *0,001
  - реактивная энергия    кВАр*ч  *0,001
}

//------------------------------------------------------------------------------
                                interface
//------------------------------------------------------------------------------

uses
  Server_Devices, cport, sysutils, Windows, SyncObjs, Classes, variants, Contnrs,
  ComCtrls, Server_UDP, XMLIntf, msxmldom;

type
  TMomentValue = class
    handle: TObject;   // указатель
    cmd: array [0..5] of Byte;  // подготовленный пакет команды
    factor: Integer;
  end;


type
  TMercury = class(TRealDev)
   private
    password: array [0..5] of byte; // "111111" - для первого уровня доступа;

    queryOpen:  array [0..10] of byte;    // Заросы - открыть сессию
    queryEnergy:  array [0..5] of byte;   // активная реактивная энергия
    queryClose:  array [0..3] of byte;    // закрыть сессию

    momentValue: array of TMomentValue; // запросы напряжения и тока

    factorVoltage : Integer;  // коэф. трансформации напряяжения
    factorCurrent : Integer;  // коэф. трансформации тока

    procedure queryBild(); // построитель запросов на открытие и завершении сессии, получения энергии

    function Byte2143ToDword (inp:PByteArray; offset: integer = 0):longword; // Получаем значение перестановкой
    function Byte21ToDword (inp:PByteArray; offset: integer = 0):word;
    procedure SetEnergyValue(inp:PByteArray); // устанавливаем значение ативной и реактивной энергии
    procedure SetEnergyBadValue(); // устанавливаем флаг ошибки чтения энергий
   public
    constructor Create(xmlfile: IXMLNode); overload;
    destructor Destroy; override;

    procedure draw(inlist: tlistview); override;
    procedure Read(port: TComPort); override;
    procedure Write(port: TComPort); override;
  end;

//------------------------------------------------------------------------------
                                    implementation
//------------------------------------------------------------------------------
uses
  UHelper;


const
  srCRCHi: array[0..255] of byte = (
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40);

  srCRCLo: array[0..255] of byte = (
    $00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05, $C5, $C4, $04,
    $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA, $CB, $0B, $C9, $09, $08, $C8,
    $D8, $18, $19, $D9, $1B, $DB, $DA, $1A, $1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC,
    $14, $D4, $D5, $15, $D7, $17, $16, $D6, $D2, $12, $13, $D3, $11, $D1, $D0, $10,
    $F0, $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35, $34, $F4,
    $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B, $FB, $39, $F9, $F8, $38,
    $28, $E8, $E9, $29, $EB, $2B, $2A, $EA, $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C,
    $E4, $24, $25, $E5, $27, $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0,
    $A0, $60, $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64, $A4,
    $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68,
    $78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C,
    $B4, $74, $75, $B5, $77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0,
    $50, $90, $91, $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54,
    $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99, $59, $58, $98,
    $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F, $8D, $4D, $4C, $8C,
    $44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43, $83, $41, $81, $80, $40);

const
  InitCRC: word = $FFFF;

function UpdCRC(C: byte; oldCRC: word): word;
var
  i: byte;
  arrCRC: array[0..1] of byte absolute oldCRC;
begin
  i := arrCRC[1] xor C;
  arrCRC[1] := arrCRC[0] xor srCRCHi[i];
  arrCRC[0] := srCRCLo[i];
  UpdCRC := oldCRC;
end;

procedure CalcCRCMercury (Length:word; inp:PByteArray);
var
i:integer;
crc:word;
begin
   Crc:=UpdCRC(inp[0],InitCrc);
   For i:=1 to Length-3 do Crc:=UpdCRC(inp[i],Crc);
   inp[Length-2]:=crc div 256;
   inp[Length-1]:=crc mod 256;
end;

{ TMercury }

constructor TMercury.Create(xmlfile: IXMLNode);
var
  confNode: IXMLNode;
  tmpitm: Tservitem;
  handle: integer;  // сетевой адрес
  netaddress: word; // сетевой адрес

  i: Integer;
  nameTag: string;     // имя узла
  listObjectPos: Integer;  // позиция добавленого элемента в списке
  momentValueCount: Integer; // количество momentValue
  sendCmd: array [0..5] of byte; // команда для momentValue
begin
  FillChar(password, 6*SizeOf(Byte), 1);  // для первого уровня доступа пароль: 111111

  handle := 0;
  listObjectPos := 0;
  momentValueCount := 0;

  try
    self.adress := xmlfile.Attributes['address'];
    Self.devtype := 'mercury';

    inherited Create( xmlfile.Attributes['name'], strtoint(xmlfile.Attributes['portnum']));

    itemlist := TObjectList.Create;

    factorVoltage := 1;
    factorCurrent := 1;

    // --- node factor
    confNode := xmlfile.ChildNodes.FindNode('factor');
    if confNode <> nil then
    try
      factorVoltage := StrToInt(confNode.Attributes['voltage']);
      factorCurrent := StrToInt(confNode.Attributes['current']);
      log.debug('TMercury.Create. Сonfigured factor success.');
    except
      on E: Exception do
        log.error('TMercury.Create. Сonfigured factor. ' + E.ClassName + ': ' + E.Message);

      else
        log.error('Unknown error. TMercury.Create. Сonfigured factor.' );
    end;

    // --- node energy
    confNode := xmlfile.ChildNodes.FindNode('energy');
    if confNode <> nil then
    try
      // active
      handle := AddItem('active', 4, StrToInt(confNode.Attributes['active_idopc']), false );
      listObjectPos := itemlist.Add( TDevItem.Create(handle, 'active', 0, Null, False ) );
      tmpitm := serv.items[handle];

      // reactive
      handle := AddItem('reactive', 4, StrToInt(confNode.Attributes['reactive_idopc']), false );
      listObjectPos := itemlist.Add( TDevItem.Create(handle, 'reactive', 0, Null, False ) );
      tmpitm := serv.items[handle];

      // создание запросов на открытие, закрытие, получение атинвой и реактивной энергий
      queryBild();

      log.debug('TMercury.Create. Сonfigured energy success.');
    except
      on E: Exception do
        log.error('TMercury.Create. Сonfigured energy. '+ E.ClassName + ': ' + E.Message);

      else
        log.error('Unknown error. TMercury.Create. Сonfigured energy.' );
    end;


    // --- node voltage
    confNode := xmlfile.ChildNodes.FindNode('voltage');
    if confNode <> nil then
    try
      for i := 1 to 3 do
      begin
        nameTag := 'u'+IntToStr(i);
        handle := AddItem(nameTag, 4, StrToInt(confNode.Attributes[nameTag+'_idopc']), False);
        listObjectPos := itemlist.Add( TDevItem.Create(handle, nameTag, 0, Null, False ) );
        tmpitm := serv.items[handle];

        sendCmd[0]:= StrToInt(Self.adress);
        sendCmd[1]:= $8;
        sendCmd[2]:= $11; // многовенных значений

        case i of
          1: sendCmd[3]:= $11; // Напряжение фаза 1
          2: sendCmd[3]:= $12; // Напряжение фаза 2
          3: sendCmd[3]:= $13; // Напряжение фаза 3
        end;

        momentValueCount:= momentValueCount + 1;
        SetLength( momentValue, momentValueCount);
        momentValue[momentValueCount-1] := TMomentValue.Create;
        momentValue[momentValueCount-1].handle := itemlist.Items[listObjectPos] ;
        momentValue[momentValueCount-1].factor := factorVoltage;

        CalcCRCMercury(6, @sendCmd);
        Move(sendCmd[0],momentValue[momentValueCount-1].cmd[0],6 );

      end;

      log.debug('TMercury.Create. Сonfigured voltage success.');
    except
      on E: Exception do
        log.error('TMercury.Create. Сonfigured voltage. '+ E.ClassName + ': ' + E.Message);

      else
        log.error('Unknown error. TMercury.Create. Сonfigured voltage.' );
    end;

    // --- node current
    confNode := xmlfile.ChildNodes.FindNode('current');
    if confNode <> nil then
    try
      for i := 1 to 3 do
      begin
        nameTag := 'i'+IntToStr(i);
        handle := AddItem(nameTag, 4, StrToInt(confNode.Attributes[nameTag+'_idopc']), False );
        listObjectPos := itemlist.Add( TDevItem.Create(handle, nameTag, 0, Null, False ) );
        tmpitm := serv.items[handle];

        sendCmd[0]:= StrToInt(Self.adress);
        sendCmd[1]:= $8;
        sendCmd[2]:= $11; // многовенных значений

        case i of
          1: sendCmd[3]:= $21; // Ток фаза 1
          2: sendCmd[3]:= $22; // Ток фаза 2
          3: sendCmd[3]:= $23; // Ток фаза 3
        end;

        momentValueCount:= momentValueCount + 1;
        SetLength( momentValue, momentValueCount);
        momentValue[momentValueCount-1] := TMomentValue.Create;
        momentValue[momentValueCount-1].handle := itemlist.Items[listObjectPos] ;
        momentValue[momentValueCount-1].factor := factorVoltage;

        CalcCRCMercury(6, @sendCmd);
        Move(sendCmd[0],momentValue[momentValueCount-1].cmd[0],6 );

      end;

      log.debug('TMercury.Create. Сonfigured current success.');
    except
      on E: Exception do
        log.error('TMercury.Create. Сonfigured current. '+ E.ClassName + ': ' + E.Message);

      else
        log.error('Unknown error. TMercury.Create. Сonfigured current.' );
    end;

    // --- node power
    confNode := xmlfile.ChildNodes.FindNode('power');
    if confNode <> nil then
    try
      for i := 1 to 3 do
      begin
        nameTag := 'p'+IntToStr(i);
        handle := AddItem(nameTag, 4, StrToInt(confNode.Attributes[nameTag+'_idopc']), False );
        listObjectPos := itemlist.Add( TDevItem.Create(handle, nameTag, 0, Null, False ) );
        tmpitm := serv.items[handle];

        sendCmd[0]:= StrToInt(Self.adress);
        sendCmd[1]:= $8;
        sendCmd[2]:= $11; // многовенных значений

        case i of
          1: sendCmd[3]:= $1; // Мощность фаза 1
          2: sendCmd[3]:= $2; // Мощность фаза 2
          3: sendCmd[3]:= $3; // Мощность фаза 3
        end;

        momentValueCount:= momentValueCount + 1;
        SetLength( momentValue, momentValueCount);
        momentValue[momentValueCount-1] := TMomentValue.Create;
        momentValue[momentValueCount-1].handle := itemlist.Items[listObjectPos] ;
        momentValue[momentValueCount-1].factor := factorVoltage;

        CalcCRCMercury(6, @sendCmd);
        Move(sendCmd[0],momentValue[momentValueCount-1].cmd[0],6 );

      end;

      log.debug('TMercury.Create. Сonfigured power success.');
    except
      on E: Exception do
        log.error('TMercury.Create. Сonfigured power. '+ E.ClassName + ': ' + E.Message);

      else
        log.error('Unknown error. TMercury.Create. Сonfigured power.' );
    end;

  except
    on E: Exception do
      log.error('TMercury.Create. Сonfigured. '+ E.ClassName + ': ' + E.Message);

    else
      log.error('Unknown error. TMercury.Create. Сonfigured.' );
  end;

end;

destructor TMercury.Destroy;
var
  i: Integer;
begin
  for i:= Low(momentValue) to High(momentValue) do
    momentValue[i].Free;

  SetLength( momentValue, 0);

  inherited Destroy;
end;

procedure TMercury.queryBild();
var
  i: Integer;
  sendCmd: array [0..10] of byte;
begin
    // Запрос на открытие канала связи
    sendCmd[0]:= StrToInt(Self.adress);  // Адрес
    sendCmd[1]:= 1; // Код запроса = 1h
    sendCmd[2]:= 1; // Уровень доступа - пользователь
    for i:=0 to 5 do sendCmd[i+3]:= password[i]; // пароль
    CalcCRCMercury(11, @sendCmd);  // CRC

    Move(sendCmd[0],queryOpen[0],11 );

    // запрос на чтение массивов регистров накопленной энергии
    sendCmd[1]:= 5;
    sendCmd[2]:= 0;
    sendCmd[3]:= 0;
    CalcCRCMercury(6, @sendCmd);

    Move(sendCmd[0],queryEnergy[0],6 );

    // close session
    sendCmd[1]:= 2;
    CalcCRCMercury(4, @sendCmd);

    Move(sendCmd[0],queryClose[0],4 );
end;

procedure TMercury.draw(inlist: tlistview);
var
  i: integer;
  drawindex: tlistitem;
begin
  inlist.clear;

  for i := 0 to itemlist.Count - 1 do
  begin
    drawindex := inlist.Items.Add();
    drawindex.ImageIndex:= 0;
    drawindex.Caption := TDevItem(itemlist.Items[i]).name;

    case TDevItem(itemlist.Items[i]).name[1] of
      'a': drawindex.Caption := 'Активная';
      'r': drawindex.Caption := 'Реактивная';
      'u': drawindex.Caption := 'Наряжение Ф'+TDevItem(itemlist.Items[i]).name[2];
      'i': drawindex.Caption := 'Ток Ф'+TDevItem(itemlist.Items[i]).name[2];
      'p': drawindex.Caption := 'Мощность Ф'+TDevItem(itemlist.Items[i]).name[2];
    else
      drawindex.Caption := TDevItem(itemlist.Items[i]).name;
    end;

    if (TDevItem(itemlist.Items[i]).Value <> null) and (not VarIsEmpty(TDevItem(itemlist.Items[i]).Value)) then
    begin
      case TDevItem(itemlist.Items[i]).name[1] of
      'a': drawindex.SubItems.Add(FloatToStrF(TDevItem(itemlist.Items[i]).Value/1000, ffFixed, 10, 1)+ ' кВт*ч' );
      'r': drawindex.SubItems.Add(FloatToStrF(TDevItem(itemlist.Items[i]).Value/1000, ffFixed, 10, 1)+ ' кВАр*ч' );
      'u': drawindex.SubItems.Add(FloatToStrF(TDevItem(itemlist.Items[i]).Value/100, ffFixed, 4, 2)+ ' В' );
      'i': drawindex.SubItems.Add(FloatToStrF(TDevItem(itemlist.Items[i]).Value/1000, ffFixed, 4, 2)+ ' А' );
      'p': drawindex.SubItems.Add(FloatToStrF(TDevItem(itemlist.Items[i]).Value/100, ffFixed, 10, 1)+ ' Вт' );
      else
        drawindex.SubItems.Add(intToStr(TDevItem(itemlist.Items[i]).Value) );
      end;

      drawindex.SubItems.Add(QualityToStr(serv.items[TDevItem(itemlist.Items[i]).handle].Quality));
      drawindex.SubItems.Add(inttostr(timer));
    end
    else
    begin
      drawindex.SubItems.Add('NULL');
      drawindex.SubItems.Add(QualityToStr(serv.items[TDevItem(itemlist.Items[i]).handle].Quality));
      drawindex.SubItems.Add(inttostr(timer));
    end;


  end;
end;


procedure TMercury.Write(port: TComPort);
begin
  // empty
end;

procedure TMercury.Read(port: TComPort);
var
  responseToCmd: array [0..20] of byte;
  event: TEvent;
  count: integer;
  Events: TComEvents;

  i: Integer;
begin
    PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR);

    Event := TEvent.Create(nil,  // параметры защиты, берутся от родителя
                           True, // режим управления
                           False, // начальное состояние, несигнальное состояние - занято
                            '');  // имя объекта
    Events := [evRxChar];

    // Запрос на открытие канала связи
    port.Write(queryOpen,11);
    port.WaitForEvent(Events, Event.Handle, 350);

    count := port.InputCount;

    // пытаемся повторно открыть канал связи
    if count <> 4 then
    begin
      log.debug('Mercury. Repeated send auth paket for open channel. Send raw:'+ByteToHexStr(queryOpen));
      PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR);
      Event := TEvent.Create(nil, True, False, '');
      Events := [evRxChar];

      port.Write(queryOpen,11);
      port.WaitForEvent(Events, Event.Handle, 350);

      count := port.InputCount;
    end;


    if count = 4 then
    begin
      port.Read(responseToCmd[0], count);
      // проверяем адрес и код ответа, контрольную сумму игнорируем
      if (responseToCmd[0] = queryOpen[0]) and (responseToCmd[1] = 0) then
      begin
        // гарантированный тайм-аут
        sleep(25);
        // запрос на чтение массивов регистров накопленной энергии

        { Опрос
          команда:  1байт - адрес
                    1байт = 05 команда
                    2байта = 00 00 период
                    2байта - контрольная сумма

          ответ:    1байт - адрес
                    4байта - А+
                    4байта - А-, если не подерживается то FF FF FF FF
                    4байта - R+
                    4байта - R-, если не подерживается то FF FF FF FF
                    2байта - контрольная сумма
        }

        PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR);
        Event := TEvent.Create(nil, True, False, '');
        Events := [evRxChar];

        port.Write(queryEnergy,6);
        port.WaitForEvent(Events, Event.Handle, 150);
        port.WaitForEvent(Events, Event.Handle, 150);  // TODO: Ложное срабатывание, длина пакета 14. FFFFFFFF?
        sleep(25); // TODO: Ложное срабатывание, длина пакета 14
        count := port.InputCount;
        port.Read(responseToCmd[0], count);

        if count > 13 then  // TODO: Ложное срабатывание, длина пакета 14
        begin
          if (responseToCmd[0] = queryEnergy[0]) then
          begin
            SetEnergyValue(@responseToCmd);

            // получаем мгновенные значания
            for i:= Low(momentValue) to High(momentValue) do
            begin
              PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR);
              Event := TEvent.Create(nil, True, False, '');
              Events := [evRxChar];

              port.Write(momentValue[i].cmd,6);
              port.WaitForEvent(Events, Event.Handle, 150);
              count := port.InputCount;
              port.Read(responseToCmd[0], count);

              { ответ
               1 байт - адрес
               1 байт = 0
               2 байта = значение
               2 байта - контрольная сумма
              }
              if (count = 6 ) and (momentValue[i].cmd[0] = responseToCmd[0])  then
              begin
                TDevItem(momentValue[i].handle).value := Byte21ToDword(@responseToCmd,2) * momentValue[i].factor;
                serv.items[TDevItem(momentValue[i].handle).handle].Quality := OPC_QUALITY_GOOD;
                log.debug('Mercury. Response moment data. Send: '+ByteToHexStr(momentValue[i].cmd,6) + 'Response raw:' +ByteToHexStr(responseToCmd,count) );
              end
              else
              begin
                TDevItem(momentValue[i].handle).value := Null;
                serv.items[TDevItem(momentValue[i].handle).handle].Quality := OPC_QUALITY_UNCERTAIN;
                log.error('Mercury. Error response moment data. Send: '+ByteToHexStr(momentValue[i].cmd,6) + 'Response raw:' +ByteToHexStr(responseToCmd,count) );
              end;

              serv.Items[TDevItem(momentValue[i].handle).handle].value := TDevItem(momentValue[i].handle).value;
            end;

          end
          else
          begin
            log.error('Mercury. Error response energy data. Response raw:' +ByteToHexStr(responseToCmd,count) );
            SetEnergyBadValue();
          end;

        end
        else
        begin
          // error response energy
          log.error('Mercury. Error response energy data. Send raw:' +ByteToHexStr(queryEnergy)+'. Response raw:'+ByteToHexStr(responseToCmd,count) );
          // close session
          port.Write(queryClose,4);
          SetEnergyBadValue();
        end;

      end
      else
      begin
        // error response
        log.error('Mercury. Error response. Response raw:' +ByteToHexStr(responseToCmd,4));
        // close session
        port.Write(queryClose,4);
        SetEnergyBadValue();
      end;
    end
    else
    begin
      // error timeout
      log.error('Mercury. Error timeout. Send raw:'+ByteToHexStr(queryOpen) );
      SetEnergyBadValue();
    end;

    Event.Free;
    Sleep(50);
end;



procedure TMercury.SetEnergyValue(inp:PByteArray);
var
  i: Integer;
begin
  for i:=0 to itemlist.count-1 do
  begin

    if TDevItem(itemlist.Items[i]).name = 'active' then
    begin
      TDevItem(itemlist.Items[i]).value := Byte2143ToDword(inp,1) * factorVoltage * factorCurrent;
      serv.Items[TDevItem(itemlist.Items[i]).handle].value := TDevItem(itemlist.Items[i]).value;
      serv.items[TDevItem(itemlist.Items[i]).handle].Quality := OPC_QUALITY_GOOD;
      Continue;
    end;

    if TDevItem(itemlist.Items[i]).name = 'reactive' then
    begin
      TDevItem(itemlist.Items[i]).value := Byte2143ToDword(inp,9) * factorVoltage * factorCurrent;
      serv.Items[TDevItem(itemlist.Items[i]).handle].value := TDevItem(itemlist.Items[i]).value;
      serv.items[TDevItem(itemlist.Items[i]).handle].Quality := OPC_QUALITY_GOOD;
      Continue;
    end;

  end;
end;

procedure TMercury.SetEnergyBadValue();
var
  i: Integer;
begin
  for i:=0 to itemlist.count-1 do
  begin
    TDevItem(itemlist.Items[i]).value := null;
    serv.Items[TDevItem(itemlist.Items[i]).handle].value := TDevItem(itemlist.Items[i]).value;
    serv.items[TDevItem(itemlist.Items[i]).handle].Quality := OPC_QUALITY_UNCERTAIN;
  end;
end;

function TMercury.Byte2143ToDword (inp:PByteArray; offset: integer = 0):longword;
var
  value:longword;
begin
   Result :=   (inp[offset+3] shl 8)
            or  inp[offset+2]
            or (inp[offset+1] shl 24)
            or (inp[offset] shl 16) ;
end;

function TMercury.Byte21ToDword (inp:PByteArray; offset: integer = 0):word;
var
  value:longword;
begin
   Result := (inp[offset+1] shl 8) or inp[offset];
end;


end.


unit Server_Protocol_Modbus;

//------------------------------------------------------------------------------
                                  interface
//------------------------------------------------------------------------------

uses
  Server_Devices, cport, sysutils, Windows, SyncObjs, Classes, variants, Contnrs,
  ComCtrls, Server_UDP, XMLIntf, msxmldom, math, UTheadFIFOQueue;

type
  tmoditm = class(TDevItem)
    oletype: string;
    mfunction: byte;
    numbytes: single;

    constructor create(padress: integer;
      poletype: string;
      pname: string;
      phandle: integer;
      mfunction: byte;
      miswrite: boolean;
      numbytes: single);
  end;

type
  TmodbusGroup = class
    begaddress, endaddress, mfunction: integer;
    itemslist: tobjectlist;
    numbites: single;
    oletype: string;
    address: string;
    query: array[1..8] of byte;
    constructor create();
    function AddItem(item: tmoditm): boolean;
    function ProcessQuery(port: TComPort): integer;
  end;

type
  TModbus = class(TRealDev)

    writed: string;
    activeitem: tmoditm;
    ConErrorsItem: tmoditm;
    groups: tobjectlist;
    outitemlist: tobjectlist;
    adresses: array[0..200] of string;
    numerrors: cardinal;
    input: array[0..200] of boolean;
    output: array[0..200] of boolean;

    constructor Create(xmlfile: IXMLNode; idactiveitem: integer); overload;

    procedure draw(inlist: tlistview); override;
    procedure AddActiveItem(iditem: integer);
    procedure AddItemToGroup(item: tmoditm);
    procedure Read(port: TComPort); override;
    procedure Write(port: TComPort); override;
    procedure WriteParams(szname: Integer; const Value: Variant); override;

    destructor Destroy; override;
  end;

  function Modbus_CHSM(stNdx, cntB: word; Buffer: pointer): word;
//------------------------------------------------------------------------------
                              implementation
//------------------------------------------------------------------------------

uses UHelper, TLevelUnit;

function Modbus_CHSM(stNdx, cntB: word; Buffer: pointer): word;
type
  pB = ^Byte;
var
  i: word;
  ctShift: word;
  CRC: word;
  flC: boolean;
  CRC_lo: word;
  teckB: byte;
  B: pB;
begin
  B := pB(Buffer);
  inc(B, stNdx);
  CRC := $FFFF;
  for i := stNdx to stNdx + cntB - 1 do
  begin
    ctShift := 0;
    CRC_lo := CRC and $FF;
    CRC := CRC and $FF00;

    teckB := B^;

    CRC := CRC or (CRC_lo xor teckB);
    repeat
      if CRC and 1 > 0 then
        flC := true
      else
        flC := false;
      CRC := CRC shr 1;
      inc(ctShift);
      if (flC) then
        CRC := CRC xor $A001;
    until ctShift = 8;
    inc(B);
  end;
  result := CRC;
end;


procedure TModbus.draw(inlist: tlistview);
var
  i: integer;
  tmpitm: tmoditm;
  drawindex: tlistitem;
begin

  inlist.clear;

  for i := 0 to self.itemlist.Count - 1 do
  begin
    tmpitm := tmoditm(self.itemlist.items[i]);
    drawindex := inlist.Items.Add();

    if serv.items[tmpitm.handle].id < 2000 then
    begin
    if not tmoditm(tmpitm).iswrite then
    begin
      drawindex.ImageIndex:= 0;
      drawindex.Caption := 'in.'+ serv.items[tmpitm.handle].name; 
    end
    else
    begin
      drawindex.ImageIndex:= 1;
      drawindex.Caption := 'out.'+ serv.items[tmpitm.handle].name;
    end;
    end
     else
      begin
       drawindex.Caption := 'tech.'+ serv.items[tmpitm.handle].name;
       drawindex.ImageIndex:= 2;
      end;

    if (serv.items[tmpitm.handle].Value <> null) and (not VarIsEmpty(serv.items[tmpitm.handle].Value)) then
    begin
      if tmpitm.oletype = 'real' then
        drawindex.SubItems.Add(floattostrf(serv.items[tmpitm.handle].Value, ffFixed, 5, 2))
      else
       if tmpitm.oletype = 'bool' then
        drawindex.SubItems.Add(BoolToStr(serv.items[tmpitm.handle].Value, true))
       else
        drawindex.SubItems.Add(intToStr(serv.items[tmpitm.handle].Value));

      drawindex.SubItems.Add(QualityToStr(serv.items[tmpitm.handle].Quality));
      drawindex.SubItems.Add(inttostr(timer));
      drawindex.SubItems.Add('ID = ' + IntToStr(serv.items[tmpitm.handle].id) + ', type: ' +tmpitm.oletype);
    end
    else
    begin
      drawindex.SubItems.Add('NULL');
      drawindex.SubItems.Add(QualityToStr(serv.items[tmpitm.handle].Quality));
      drawindex.SubItems.Add(inttostr(timer));
      drawindex.SubItems.Add('ID = ' + IntToStr(serv.items[tmpitm.handle].id) + ', type: ' +tmpitm.oletype);
    end;
  end;

end;

// input - netaddress

procedure TModbus.WriteParams(szname: Integer; const Value: Variant);
var
  i: integer;
  tmpmodbus: tmoditm;
  t: word;
  tmpdouble: single;
  tmpword: word;
  tmpbyte: byte;
  tmpbuf: array[0..3] of byte;
  s: string;

  strType: string;

begin
  for i := 0 to itemlist.Count - 1 do
  begin
    tmpmodbus := tmoditm(itemlist.Items[i]);
    if szname = tmpmodbus.handle then
    begin

      strType := tmoditm(itemlist.Items[i]).oletype;

      if (strType = 'real') or (strType = 'long') then
      begin
        s := #0#0#0#0#0#0#0#0#0#0#0#0#0;
        tmpdouble := VarAsType(Value, varSingle);
        CopyMemory(@tmpbuf[0], @tmpdouble, 4);
        s[1] := chr(strtoint(adress));
        s[2] := chr(16);
        s[3] := chr(hi(tmpmodbus.adress));
        s[4] := chr(lo(tmpmodbus.adress));
        s[5] := chr(0);
        s[6] := chr(2);
        s[7] := chr(4);
        s[8] := chr(tmpbuf[1]);
        s[9] := chr(tmpbuf[0]);
        s[10] := chr(tmpbuf[3]);
        s[11] := chr(tmpbuf[2]);
        t := Modbus_CHSM(0, 11, @s[1]);
        s[12] := chr(lo(t));
        s[13] := chr(hi(t));
        cmdQueue.Enqueue(s);
      end;

      if strType = 'int' then
      begin
        s := #0#0#0#0#0#0#0#0#0#0#0;
        tmpword := VarAsType(Value, varWord);
        CopyMemory(@tmpbuf[0], @tmpword, 2);
        s[1] := chr(strtoint(adress));
        s[2] := chr(16);
        s[3] := chr(hi(tmpmodbus.adress));
        s[4] := chr(lo(tmpmodbus.adress));
        s[5] := chr(0);
        s[6] := chr(1);
        s[7] := chr(2);
        s[8] := chr(tmpbuf[0]);
        s[9] := chr(tmpbuf[1]);
        t := Modbus_CHSM(0, 9, @s[1]);
        s[10] := chr(lo(t));
        s[11] := chr(hi(t));
        cmdQueue.Enqueue(s);
      end;

      if strType = 'bool' then
      begin
        s := #0#0#0#0#0#0#0#0;
        tmpbyte := VarAsType(Value, varByte);
        s[1] := chr(strtoint(adress));
        s[2] := #5;
        s[3] := chr(hi(tmpmodbus.adress));
        s[4] := chr(lo(tmpmodbus.adress));
        if tmpbyte = 1 then
          s[5] := #255
        else
          s[5] := #0;
        s[6] := #0; //chr(1);
        t := Modbus_CHSM(0, 6, @s[1]);
        s[7] := chr(lo(t));
        s[8] := chr(hi(t));
        cmdQueue.Enqueue(s);
      end;

      if (log.GetLevel = TLevelUnit.DEBUG) or
         (log.GetLevel = TLevelUnit.TRACE) or
         (log.GetLevel = TLevelUnit.ALL)
      then
        log.Debug('Modbus out: '+ StrToHexStr(s));

      break;

    end;

  end;
end;

procedure TModbus.Write(port: TComPort);
var
  cmdString: string[20];
  event: TEvent;
  Events: TComEvents;
begin
  // Цикл записи
  While not cmdQueue.isEmpty Do
  begin
    PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);

    Event := TEvent.Create(nil, True, False, '');
    Events := [evRxChar];

    cmdString:= cmdQueue.Dequeue;

    if port.WriteStr(cmdString) <= 0 then
        log.error('Write error, port #' + IntToStr(Self.dport)+ '. Data: ' + StrToHexStr(cmdString));

    if log.GetLevel = TLevelUnit.DEBUG then
        log.debug('Write to port #' + IntToStr(Self.dport)+ '. Data: ' + StrToHexStr(cmdString));

    Event.Free;
    sleep(25);
  end;
end;


procedure TModbus.Read(port: TComPort);
var
  i: Integer;
begin
  PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);

// Цикл чтение
  for i := 0 to groups.Count - 1 do
  begin
    if TmodbusGroup(groups.Items[i]).ProcessQuery(port) = 1 then
    begin
      log.error('Read params modbus. Query: '
        + ByteToHexStr( TmodbusGroup(groups.Items[i]).query ) );

      serv.items[activeitem.handle].value := 0;
      numerrors := numerrors + 1;
      serv.items[ConErrorsItem.handle].value := numerrors;
    end
    else
    begin
      serv.items[activeitem.handle].value := 1;
    end;
    sleep(25);     // Не менее 15мс на 19200

  end;
end;

constructor tmoditm.create(padress: integer; poletype: string; pname: string;
  phandle: integer; mfunction: byte; miswrite: boolean;
  numbytes: single);
begin
  inherited Create;
  _address := padress; // Адресс устройства
  oletype := poletype; // Тип
  _name := pname; // Имя тега
  _handle := phandle; // netaddress
  self.mfunction := mfunction; // фуекция чтения
  _iswrite := miswrite; // Чтение-запись
  Self.numbytes := numbytes; // колиество байт
end;

constructor TModbus.Create(xmlfile: IXMLNode; idactiveitem: integer);
var
  nodeTeg: IXMLNode;
  i: integer;
  tmpitm: Tservitem;
  tmpmoditem: tmoditm;
  mname, mtype: string;
  handle: integer;
  mfunction, address: integer;
  miswrite: boolean;
  checkwrite: Boolean; // проверять записаные значения
  numbytes: single;
  idopc: word;
begin
  handle := 0;
  numbytes := 0;
  checkwrite := False;

  itemlist := TObjectList.Create;
  groups := TObjectList.Create;
  groups.Add(TmodbusGroup.create);

  numerrors := 0;
  self.adress := xmlfile.Attributes['address'];
  self.devtype := 'modbus';

   if xmlfile.HasAttribute('checkwrite') then
     if xmlfile.Attributes['checkwrite'] = 'true' then
        checkwrite := True;

  inherited Create( xmlfile.Attributes['name'], strtoint(xmlfile.Attributes['portnum']));

  for i := 0 to xmlfile.ChildNodes.Count - 1 do
  begin

    nodeTeg := xmlfile.ChildNodes[i];
    mname := nodeTeg.Text;
    address := strtoint(nodeTeg.Attributes['address']);
    idopc := strtoint(nodeTeg.Attributes['idopc']);
    mtype := nodeTeg.Attributes['valtype'];

    miswrite := False;

    if nodeTeg.HasAttribute('write') then
     if nodeTeg.Attributes['write'] = 'true' then
        miswrite := True;

    // TODO: Исправить типы тегов
    if mtype = 'int' then
    begin
      handle := AddItem(mname, 3, idopc, miswrite);   // word  = 3 lenbyte 2
      numbytes := 1;
    end;
    if mtype = 'long' then
    begin
      handle := AddItem(mname, 4, idopc, miswrite);  // dword = 4 lenbyte 4
      numbytes := 2;
    end;
    if mtype = 'real' then
    begin
      handle := AddItem(mname, 5, idopc, miswrite);  // float = 5 lenbyte 8
      numbytes := 2;
    end;
    if mtype = 'bool' then
    begin
      handle := AddItem(mname, 2, idopc, miswrite);  // int   = 2 lenbyte 1 , bool  = 1 lenbyte 1
      numbytes := 1;
    end;

    mfunction := strtoint(nodeTeg.Attributes['mbusfunction']);
    tmpmoditem := tmoditm.create(address, mtype, mname, handle, mfunction, miswrite, numbytes);

    itemlist.Add(tmpmoditem);

    if not(miswrite) or checkwrite then
      AddItemToGroup(tmpmoditem);

    tmpitm := serv.items[handle];
    tmpitm.Value := 0;
    tmpitm.updated := now;
    tmpitm.Quality := OPC_QUALITY_GOOD;
  end;
  AddActiveItem(idactiveitem);
end;


destructor TModbus.Destroy;
begin

  groups.Free;
  itemlist.Free;

  inherited Destroy;
end;


{ TmodbusGroup }

// создаем групу для групового запроса на чтение
function TmodbusGroup.AddItem(item: tmoditm): boolean;
var
  qsize: integer;
  t: dword;
begin
  result := false;
  
  //if item.iswrite then Exit;

    SetRoundMode(rmup);  // Режим округления вверх

    if begaddress = -1 then
    begin
      // Первоначальная инициализация
      itemslist.Add(item);
      mfunction := item.mfunction;
      begaddress := item.adress;
      oletype := item.oletype;
      address := TModbus(serv.items[item.handle].parent).adress;
      endaddress := begaddress;
      numbites := item.numbytes;
      result := true;
    end
    else
    begin
      // Проверка, добавление в очередь
      if (item.mfunction = mfunction)
         and (address = TModbus(serv.items[item.handle].parent).adress)
         and (oletype = item.oletype)
      then
      begin
         //в начало очереди
         if (item.adress = begaddress - round(numbites)) then
         begin
           itemslist.Add(item);
           begaddress := item.adress;
           result := true;
         end;

         //в конец очереди
         if (item.adress = endaddress + round(numbites)) then
         begin
           itemslist.Add(item);
           endaddress := item.adress;
           result := true;
         end
      end;
    end;

    // Если добавили в группу обновляем запрос
    if result then
    begin
      qsize := round(itemslist.Count * numbites);
      query[1] := strtoint(address);
      query[2] := mfunction;
      query[3] := hi(begaddress);
      query[4] := lo(begaddress);
      query[5] := hi(qsize);
      query[6] := lo(qsize);
      t := Modbus_CHSM(0, 6, @query[1]);
      query[7] := lo(t);
      query[8] := hi(t);
    end

end;

constructor TmodbusGroup.create;
begin
  itemslist := TObjectList.Create;
  begaddress := -1;
  endaddress := -1;
end;

function TmodbusGroup.ProcessQuery(port: TComPort): integer;
var
  event: TEvent;
  Events: TComEvents;
  inarray: array of byte;
  tmpbyte: byte;
  tmpint: word;
  baseaddress: byte;
  syze: integer;
  tmpdouble: single;
  tmplong: cardinal;
  tmpitm: tmoditm;
  //numread: dword;
  i, j: integer;
begin
  result := 0;

  if begaddress <> -1 then
  begin

    if port.Write(query, 8) <> 8 then
      log.error('Write data: ' + ByteToHexStr(query));

    PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);
    Event := TEvent.Create(nil, True, False, '');
    Events := [evRxChar];
    Port.WaitForEvent(Events, Event.Handle, 50);

    if port.InputCount > 3 then
    begin

      try

        SetLength(inarray, 3);
        port.Read(inarray[0], 3); // numread:=
        if (inarray[0] = StrToInt(address)) and (inarray[1] = mfunction) then
        begin
          i := 0;
          syze := inarray[2];
          SetLength(inarray, syze + 2);
          port.Read(inarray[0], syze + 2);

          case mfunction of
            1, 2:
              begin
                for i := 0 to itemslist.count - 1 do
                begin
                  tmpitm := tmoditm(itemslist.items[i]);
                  baseaddress := tmpitm.adress - begaddress;
                  tmpbyte := inarray[trunc(baseaddress / 8)] and (1 shl (baseaddress mod 8));
                  tmpitm.value := tmpbyte <> 0;
                  serv.Items[tmpitm.handle].value := tmpitm.Value;
                  serv.Items[tmpitm.handle].quality := OPC_QUALITY_GOOD;
                end;
              end;
            3, 4:
              begin
                while i < syze do
                begin

                  for j := 0 to itemslist.Count - 1 do
                  begin
                    if tmoditm(itemslist.items[j]).adress = begaddress + i / 2
                      then
                    begin
                      tmpitm := tmoditm(itemslist.items[j]);
                    end;
                  end;

                  try

                    if tmpitm.oletype = 'int' then
                    begin
                      CopyMemory(@tmpint, @inarray[i + 1], 1);
                      CopyMemory(pointer(integer(@tmpint) + 1), @inarray[i], 1);
                      tmpitm.Value := word(tmpint);
                      serv.Items[tmpitm.handle].value := tmpitm.Value;
                      serv.Items[tmpitm.handle].quality := OPC_QUALITY_GOOD;
                      i := i + 2;
                    end;

                    if tmpitm.oletype = 'long' then
                    begin
                      CopyMemory(@tmplong, @inarray[i], 2);
                      tmpitm.Value := longword(tmplong);
                      serv.Items[tmpitm.handle].value := tmpitm.Value;
                      serv.Items[tmpitm.handle].quality := OPC_QUALITY_GOOD;
                      i := i + 2;
                    end;

                    if tmpitm.oletype = 'real' then
                    begin
                      tmpbyte := inarray[i];
                      inarray[i] := inarray[i + 1];
                      inarray[i + 1] := tmpbyte;
                      tmpbyte := inarray[i + 2];
                      inarray[i + 2] := inarray[i + 3];
                      inarray[i + 3] := tmpbyte;
                      CopyMemory(@tmpdouble, @inarray[i], 4);
                      tmpitm.Value := tmpdouble;
                      serv.Items[tmpitm.handle].value := tmpitm.Value;
                      serv.Items[tmpitm.handle].quality := OPC_QUALITY_GOOD;
                      i := i + 4;
                    end;

                  except
                    serv.Items[tmpitm.handle].quality := OPC_QUALITY_UNCERTAIN;
                    log.error('Get convert value in ProcessQuery function 3,4. Data: ' + ByteToHexStr(query));
                  end;

                end;
              end;
          end;
        end;

      except
        log.error('Read value in ProcessQuery. Data: ' + ByteToHexStr(query));

        for i := 0 to itemslist.count - 1 do
        begin
          tmpitm := tmoditm(itemslist.items[i]);
          tmpitm.value := null;
          serv.Items[tmpitm.handle].quality := OPC_QUALITY_BAD;
        end;
        result := 1;
        
      end;


    end

    else

    begin
      for i := 0 to itemslist.count - 1 do
      begin
        tmpitm := tmoditm(itemslist.items[i]);
        tmpitm.value := null;
        serv.Items[tmpitm.handle].quality := OPC_QUALITY_BAD;
      end;
      result := 1;
    end;

    event.Free;
  end;

end;

procedure TModbus.AddItemToGroup(item: tmoditm);
var
  i: integer;
  added: boolean;
begin
  added := false;

  // Пытаем сгрупировать и добавить в существующую группу
  for i := 0 to groups.Count - 1 do
  begin
    if TmodbusGroup(groups.Items[i]).AddItem(item) then
    begin
      added := true;
      break;
    end;
  end;

  // Если не нашлось подходящей группы создаем новую
  if not added then
  begin
    groups.Add(TmodbusGroup.create);
    TmodbusGroup(groups.Items[groups.Count-1]).AddItem(item);
  end;
end;

procedure TModbus.AddActiveItem(iditem: integer);
var
  tmpmoditem: tmoditm;
  handle: dword;
  tmpitm: TServItem;
begin
  handle := AddItem('Active', 4, 2010+iditem, False);
  tmpmoditem := tmoditm.create(0, 'long', 'Active', handle, 0, false, 0);
  tmpmoditem.value := 0;
  itemlist.Add(tmpmoditem);
  tmpitm := serv.items[handle];
  tmpitm.Value := 0;
  tmpitm.updated := now;
  tmpitm.Quality := OPC_QUALITY_GOOD;
  activeitem := tmpmoditem;

  handle := AddItem('ConnectionErrors', 4, 2040+iditem, False);
  tmpmoditem := tmoditm.create(0, 'long', 'ConnectionErrors', handle, 0, false, 0);
  tmpmoditem.value := 0;
  itemlist.Add(tmpmoditem);
  ConErrorsItem := tmpmoditem;
  tmpitm := serv.items[handle];
  tmpitm.Value := 0;
  tmpitm.updated := now;
  tmpitm.Quality := OPC_QUALITY_GOOD;
end;

end.


unit Server_Protocol_DCOM;

//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------

uses
  Server_Devices,
  cport,
  sysutils,
  Windows,
  SyncObjs,
  Classes,
  ComCtrls,
  Contnrs,
  Server_UDP,
  XMLIntf,
  msxmldom;

type
  t7053item = class(TDevItem)
    dbid: integer;
    constructor create(padress: integer; pname: string; phandle: integer;
      pvalue: variant; miswrite: boolean; mdbid: integer);
  end;

type
  TDcom = class(TRealDev)
    port: TComPort; // !!! Переопределения родителя
    lock: TCriticalSection;

    procedure draw(inlist: tlistview); override;
    constructor Create(xmlfile: IXMLNode); overload;
    procedure Read(port: TComPort); override;
    procedure Write(port: TComPort); override;

    procedure WriteParams(szname: Integer; const Value: Variant); override;

  end;

  //------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

uses TLevelUnit;

procedure TDcom.draw(inlist: tlistview);
var
  i: integer;
  tmpitem: TListItem;
begin
  inlist.clear;

  for i := 0 to itemlist.Count - 1 do
  begin
    tmpitem := inlist.Items.Add();
    if t7053item(t7053item(itemlist.Items[i])).iswrite then
    begin
      tmpitem.ImageIndex := 1;
      tmpitem.Caption := 'out.' + t7053item(itemlist.Items[i]).name;
    end
    else
    begin
      tmpitem.ImageIndex := 0;
      tmpitem.Caption := 'in.' + t7053item(itemlist.Items[i]).name;
    end;

    tmpitem.Caption := t7053item(itemlist.Items[i]).name;
    tmpitem.SubItems.Add(BoolToStr(t7053item(itemlist.Items[i]).Value, true));
    tmpitem.SubItems.Add(inttostr(timer));
  end;
end;

procedure TDcom.WriteParams(szname: Integer; const Value: Variant);
var
  i: Smallint;
  strbit : string;
  strval : string;
  bit : Integer;
begin
  strbit := '00';
  strval := '00';

  for i := 0 to itemlist.Count - 1 do
  begin

     if t7053item(itemlist.Items[i]).handle = szname then
     begin
       bit := t7053item(itemlist.Items[i]).adress;

       if (bit >= 0) and (bit < 8) then
         strbit := 'A' + IntToStr(bit);

       if (bit >= 8) and (bit < 16) then
         strbit := 'B' + IntToStr(bit - 8);

       if Boolean(Value) then
         strval := '01';

       Break;
     end;

  end;

  cmdQueue.Enqueue('#' + adress + strbit + strval);

end;

procedure TDcom.Write(port: TComPort);
var
  s: string;
  event: TEvent;
  count: integer;
  Events: TComEvents;
  cmdString: string[20];
begin
  // Цикл записи
  while not cmdQueue.isEmpty do
  begin
    PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);

    Event := TEvent.Create(nil, True, False, '');
    Events := [evRxChar];

    cmdString := cmdQueue.Dequeue;

    port.WriteStr(cmdString + #13);
    s := #0#0#0#0#0#0#0#0#0;
    Port.WaitForEvent(Events, Event.Handle, 50);
    count := port.InputCount;
    port.Readstr(s, count);

    if (Length(s) > 0) then
      if s[1] <> '>' then
        log.error('Write error, port #' + IntToStr(Self.dport) + '. Data: ' + cmdString);

    if log.GetLevel = TLevelUnit.DEBUG then
      log.debug('Write to port #' + IntToStr(dport) + '. Data: ' + cmdString);

    Event.Free;
    sleep(25);
  end;
end;

procedure TDcom.Read(port: TComPort);
var
  s: string;
  val, val1: byte;
  tmpitm: Tservitem;
  tmpdevitem: t7053item;
  event: TEvent;
  count: integer;
  Events: TComEvents;
  i: Integer;

  cmdString: string[20];
begin

  // Цикл чтение
  PurgeComm(port.Handle, PURGE_RXABORT or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_TXCLEAR);

  Event := TEvent.Create(nil, True, False, '');
  Events := [evRxChar];

  port.WriteStr('@' + adress + #13);
  s := '';
  //Sleep(200);
  Port.WaitForEvent(Events, Event.Handle, 50);
  if port.InputCount = 6 then
    port.ReadStr(s, port.InputCount);
  if s <> '' then
  begin
    HexToBin(@s[4], @val, 1);
    HexToBin(@s[2], @val1, 1);
    for i := 0 to itemlist.Count - 1 do
    begin
      tmpdevitem := t7053item(itemlist.items[i]);
      tmpitm := Tservitem(serv.items[tmpdevitem.handle]);
      if (i >= 0) and (i <= 7) then
      begin
        if tmpdevitem.value <> boolean(val and (1 shl i)) then
        begin
          tmpitm.Value := boolean(val and (1 shl i));
          tmpdevitem.value := boolean(val and (1 shl i));
          tmpitm.updated := now;
          tmpitm.Quality := OPC_QUALITY_GOOD;
        end;
      end;
      if (i >= 8) and (i <= 15) then
      begin
        if tmpdevitem.value <> boolean(val1 and (1 shl (i - 8))) then
        begin
          tmpitm.Value := boolean(val1 and (1 shl (i - 8)));
          tmpdevitem.value := boolean(val1 and (1 shl (i - 8)));
          tmpitm.updated := now;
          tmpitm.Quality := OPC_QUALITY_GOOD;
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to itemlist.Count - 1 do
    begin
      tmpdevitem := t7053item(itemlist.items[i]);
      tmpitm := Tservitem(serv.items[tmpdevitem.handle]);
      if (i >= 0) and (i <= 7) then
      begin
        if tmpdevitem.value <> boolean(val and (1 shl i)) then
        begin
          tmpitm.updated := now;
          tmpitm.Quality := OPC_QUALITY_BAD;
        end;
      end;
      if (i >= 8) and (i <= 15) then
      begin
        if tmpdevitem.value <> boolean(val1 and (1 shl (i - 8))) then
        begin
          tmpitm.updated := now;
          tmpitm.Quality := OPC_QUALITY_BAD;
        end;
      end;
    end;
  end;

  sleep(15);
  Event.Free;
end;

{ t7053item }

constructor t7053item.create(padress: integer; pname: string;
  phandle: integer; pvalue: variant; miswrite: boolean; mdbid: integer);
begin
  inherited Create;
  _address := padress;
  _name := pname;
  _handle := phandle;
  _value := pvalue;
  _iswrite := miswrite;
  Self.dbid := mdbid;
end;

constructor TDcom.Create(xmlfile: IXMLNode);
var
  i: integer;
  tmpitm: TServItem;
  handle: integer;
  itemname: string;
  nodeTeg: IXMLNode;

  tagIsWrite: Boolean;
begin
  self.adress := Trim(xmlfile.Attributes['address']);

  if Length(Self.adress) < 2 then
   Self.adress := '0'+Self.adress;

  inherited Create(xmlfile.Attributes['name'], strtoint(xmlfile.Attributes['portnum']));

  tagIsWrite := False;

  if xmlfile.HasAttribute('write') then
    if xmlfile.Attributes['write'] = 'true' then
      tagIsWrite := True;

  itemlist := TObjectList.Create;

  for i := 0 to xmlfile.ChildNodes.Count - 1 do
  begin
    nodeTeg := xmlfile.ChildNodes[i];
    itemname := nodeTeg.Text;

    handle := AddItem(itemname, 1, strtoint(nodeTeg.Attributes['idopc']), tagIsWrite);
    itemlist.Add(t7053item.create(
                   strtoint(nodeTeg.Attributes['bit']),
                   itemname,
                   handle,
                   false,
                   false,
                   0
                  ));

    tmpitm := TServItem(serv.items[handle]);
    tmpitm.Value := false;
  end;
end;

end.


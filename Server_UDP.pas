unit Server_UDP;

//------------------------------------------------------------------------------
                                interface
//------------------------------------------------------------------------------

uses
  Windows, Messages, Graphics, Controls, Forms, Dialogs, IdWinsock2, 
  SysUtils, Classes, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle,
  contnrs, ExtCtrls, Server_Devices, XMLIntf, msxmldom, XMLDoc, USWMRGuard;

const
  OPC_QUALITY_BAD            = $00;
  OPC_QUALITY_UNCERTAIN      = $40;
  OPC_QUALITY_GOOD           = $C0;

type
  TServItem = class  // ф-ция структура для хранения данных тегов
  private
    wrLock: TSingleWriterMultipleReaderGuard;

  protected
    _value: variant;
    _iswrite: boolean;
    _name: string;
    _parent: TRealDev;

    procedure SetValue(svalue: variant);
    function GetValue():Variant;

  public
    id: integer;    // netaddress
    SIType: byte;   // type
      {
         bool  = 1 lenbyte 1
         int   = 2 lenbyte 1
         word  = 3 lenbyte 2
         dword = 4 lenbyte 4
         float = 5 lenbyte 8
      }
    updated: tdatetime;
    quality: byte;

    property value: variant read GetValue write SetValue;
    property name: string read _name;      // Имя_устроства.название_тега
    property parent: TRealDev read _parent; // экземпляр realdev
    property iswrite: boolean read _iswrite;

    function Send(size: pointer; itms: array of TServItem): string; // формирует часть пакета ответа клиенту
    constructor create(i: integer; itype: byte; name: string; parent: TRealDev; tagIsWrite: Boolean = False); // просто присваивает значения
    destructor Destroy; override;
  end;

type
  TItems = array[0..2100] of TServItem;

type
  ClientUnit = class  // ф-ция подписка клиента. Оформление подписки и формирование ответа клиенту

    ClientHandle: TIdSocketHandle;
    RequestedItems: tobjectlist; // список из ServItem

    procedure AddItem(i: integer; itype: byte; name: string; parent: TRealDev; tagWrite: Boolean = False); // добавление в список подписки RequestedItems <-- ServItem
    procedure ClearItems(); // очистка RequestedItems
    procedure SetItems(itemsstring: string; items: TItems); // разбирает входной поток, подписка клиента
    constructor create(ClientHandle: TIdSocketHandle);
    procedure SendDataToClient(itms: TItems); //собирает пакет для клиента. тип пакета #2
  end;

type
  MyUdpServer = class
   private
     arrRealDev: array of TRealDev;

   public
    Items: TItems;            // теги
    UDPServer: TIdUDPServer; // udp server
    numitems: integer;       // количество тегов
    xmlconf: txmldocument; // xml файл настройки
    clients: tobjectlist;  // список клиентов
    devs: tobjectlist;     // устройства
    flagRead: Boolean;     // флаг выставляется при приеме udp, используется для определения простоя и завершения работы
    countDevices: Integer; // счетчик устройств

    function AddDev(dev: TRealDev): TRealDev;  // Добавляем устройство
    procedure LoadDevs(); // Подключаем модули
    function AddItem(name: string; numitem: integer; itype: byte; parent: TRealDev; tagWrite: Boolean = False): integer; // Добавляем теги
    function AddClient(ClientHandle: TIdSocketHandle): ClientUnit; // Добавление нового клиента
    function FindClient(handle: integer): ClientUnit; // Поиск подписки клиента
    procedure DeleteClient(handle: integer); // удаление клиента
    procedure start();
    procedure Stop();
    procedure MyServerUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle); //событие, чтение входного потока
    procedure SetItemValue(ItemHandle: integer; const Value: Variant); // передаем значения тега в устройство
    constructor Create(dport: integer; xmldoc: TXMLDocument);
    destructor Destroy; override;
  end;

var
  serv: MyUdpServer;

//------------------------------------------------------------------------------
                              implementation
//------------------------------------------------------------------------------

uses
  Server_Protocol_Modbus,
  Server_Protocol_DCOM,
  Server_Protocol_Mercury,
  Variants,
  UHelper;

{ class ServItem --------------------------------------------------------------}

//  i = netaddress
//  itype = тип

constructor TServItem.create(i: integer; itype: byte; name: string; parent: TRealDev; tagIsWrite: Boolean = False);
begin
  Self._name := name;
  Self._parent := parent;
  self.SIType := itype;
  self.id := i;

   {
         bool  = 1 lenbyte 1
         int   = 2 lenbyte 1
         word  = 3 lenbyte 2
         dword = 4 lenbyte 4
         float = 5 lenbyte 8
      }

  case itype of
    1 : _value := VarAsType(0, varBoolean);
    2 : _value := VarAsType(0, varByte);
    3 : _value := VarAsType(0, varWord);
    4 : _value := VarAsType(0, varInteger);
    5 : _value := VarAsType(0, varDouble);
  else
   _value := Null;
  end;

  _value := Null;
  Self._iswrite := tagIsWrite;
  self.updated := now;
  self.Quality := OPC_QUALITY_UNCERTAIN;
  Self.wrLock := TSingleWriterMultipleReaderGuard.Create;
end;

destructor TServItem.Destroy;
begin
  _value := Null;
  Self.wrLock.Free;

  inherited Destroy;
end;

procedure TServItem.SetValue(svalue: variant);
begin
  wrLock.WaitToWrite;
    _value:= svalue;
  wrLock.Done;
end;

function TServItem.GetValue():Variant;
begin
  wrLock.WaitToRead;
    Result:= _value;
  wrLock.Done;
end;

// формирует часть пакета ответа клиенту
// id - 2 байта + значение - количество байт зависит от типа
// size - длина пакета
function Tservitem.Send(size: pointer; itms: array of TServItem): string;
var
  tmpbyte: byte;
  tmpword: word;
  tmpdword: dword;
  tmpfloat: double;
begin
  case SIType of
    1:
      begin
        if not varisnull(itms[id].value) then
        begin
          tmpbyte := itms[id].value;
        end
        else
          tmpbyte := 0;
        if tmpbyte <> 0 then
          tmpbyte := 1;
        Send := chr(hi(id)) + chr(lo(id)) + chr(tmpbyte);
        pinteger(size)^ := pinteger(size)^ + 3;
      end;
    2:
      begin
        if not varisnull(itms[id].value) then
        begin
          tmpbyte := itms[id].value;
        end
        else
          tmpbyte := 0;
        Send := chr(hi(id)) + chr(lo(id)) + chr(tmpbyte);
        pinteger(size)^ := pinteger(size)^ + 3;
      end;
    3:
      begin
        if not varisnull(itms[id].value) then
        begin
          tmpword := itms[id].value;
        end
        else
          tmpword := 0;
        Send := chr(hi(id)) + chr(lo(id)) + chr(lo(tmpword)) +
          chr(hi(tmpword));
        pinteger(size)^ := pinteger(size)^ + 4;
      end;
    4:
      begin
        if not varisnull(itms[id].value) then
        begin
          tmpdword := itms[id].value;
        end
        else
          tmpdword := 0;
        Send := chr(hi(id)) + chr(lo(id)) + chr(pbyte(@tmpdword)^) +
          chr(pbyte(cardinal(@tmpdword) + 1)^) + chr(pbyte(cardinal(@tmpdword) +
          2)^) + chr(pbyte(cardinal(@tmpdword) + 3)^);
        pinteger(size)^ := pinteger(size)^ + 6;
      end;
    5:
      begin
        if not varisnull(itms[id].value) then
        begin
          tmpfloat := itms[id].value;
        end
        else
          tmpfloat := 0;
        Send := chr(hi(id)) + chr(lo(id)) + chr(pbyte(@tmpfloat)^) +
          chr(pbyte(cardinal(@tmpfloat) + 1)^) + chr(pbyte(cardinal(@tmpfloat) +
          2)^) + chr(pbyte(cardinal(@tmpfloat) + 3)^) +
          chr(pbyte(cardinal(@tmpfloat) + 4)^) + chr(pbyte(cardinal(@tmpfloat) +
          5)^) + chr(pbyte(cardinal(@tmpfloat) + 6)^) +
          chr(pbyte(cardinal(@tmpfloat) + 7)^);
        pinteger(size)^ := pinteger(size)^ + 10;
      end;
  end;
end;

{ class ClientUnit ------------------------------------------------------------}

constructor ClientUnit.create(ClientHandle: TIdSocketHandle);
begin
  RequestedItems := TObjectList.Create;
  Self.ClientHandle := ClientHandle;
end;

// собирает пакет для клиента. тип пакета #2
procedure ClientUnit.SendDataToClient(itms: TItems);
var
  sendstr: string;
  size: integer;
  i: integer;
begin
  sendstr := '';
  for i := 0 to RequestedItems.Count - 1 do
  begin
    sendstr := sendstr + itms[TServItem(RequestedItems.Items[i]).id].send(@size, itms);
  end;

  size := size + 5;
  sendstr := #2 + chr(hi(size)) + chr(lo(size)) + chr(hi(RequestedItems.Count))
    + chr(lo(RequestedItems.Count)) + sendstr + '  ';
  ClientHandle.SendTo(ClientHandle.PeerIP, ClientHandle.PeerPort, sendstr[1],
    Length(sendstr));
end;

procedure ClientUnit.AddItem(i: integer; itype: byte; name: string; parent: TRealDev; tagWrite: Boolean = False);
begin
  RequestedItems.Add(TServItem.Create(i, itype, name, parent, tagWrite));
end;

procedure ClientUnit.ClearItems();
begin
  RequestedItems.Clear;
end;


// разбирает входной поток, подписка клиента
procedure ClientUnit.SetItems(itemsstring: string; items: TItems);
var
  numitems, id: word;
  i: integer;
begin
  ClearItems;
  numitems := ord(itemsstring[4]);
  numitems := (numitems shl 8) or ord(itemsstring[5]);
  for i := 1 to numitems do
  begin
    id := ord(itemsstring[i * 2 + 4]);
    id := (id shl 8) or ord(itemsstring[i * 2 + 5]);

    //OPC тег active
    if ( id = 90 ) and ( i = 1 ) then Continue;

    if (id < 2000) and (id > 0) then
    begin
      if items[id] = nil then
      begin
        log.error('UDP ClientUnit.SetItems. Items[' + IntToStr(id) + '] = nil');
        Sleep(100);
      end
      else
        AddItem(id, items[id].SIType, items[id].name, items[id].parent, items[id].iswrite);
    end
    else
      log.error('UDP ClientUnit.SetItems. Out range (0...2000) id=' + IntToStr(id));
  end;
end;

{ end class ClientUnit }

{ class MyUdpServer ---------------------------------------------------------- }

constructor MyUdpServer.Create(dport: integer; xmldoc: TXMLDocument);
begin
  flagRead:= True;

  numitems := 1;

  clients := TObjectList.create;
  UDPServer := TIdUDPServer.Create(nil);
  UDPServer.DefaultPort := dport;
  UDPServer.OnUDPRead := MyServerUDPRead;
  devs := TObjectList.Create;
  xmlconf := xmldoc;
  if (xmlconf.Active = false) then
    if (xmlconf.FileName <> '') then
      xmlconf.Active := true
    else
      raise Exception.Create('Not find config file');

  SetLength(arrRealDev, 0);
end;

procedure MyUdpServer.start;
begin
  UDPServer.Active := true;
end;

procedure MyUdpServer.Stop;
begin
  UDPServer.Active := false;
end;

// Добавляем устройство
function MyUdpServer.AddDev(dev: TRealDev): TRealDev;
begin
  AddDev := TRealDev(devs.Items[self.devs.Add(dev)]);
end;

// передаем значения тега в устройство, через родителя
// ItemHandle - netaddress
procedure MyUdpServer.SetItemValue(ItemHandle: integer; const Value: Variant);
begin
{$IfDef UNIT_ERR}
  if (ItemHandle < 2000) and ( not( GetError() ) ) then
{$Else}
  if (ItemHandle < 2000) then
{$EndIf}
  begin
    log.Trace('SetItemValue id: ' + IntToStr(ItemHandle) + ' = ' + VariantToString(Value));
    Items[ItemHandle].Value := Value;  // устаналиваем значение !!! Значение не соответствует реальности, т.е. нет синхронизации и потверждение установки
    TRealDev(Items[ItemHandle].parent).WriteParams(itemhandle, Value);  // ище родителя тега (устройство), указываем изменвшийся тег
  end;
end;

// чтение входного потока
procedure MyUdpServer.MyServerUDPRead(Sender: TObject; AData: TStream; ABinding:
  TIdSocketHandle);
var
  DataStringStream: TStringStream;
  client: ClientUnit;
  id: integer;
  tmpword: word;
  tmpdword: dword;
  tmpfloat: Double;
begin
  flagRead:= True;

  DataStringStream := TStringStream.Create('');
  try
    DataStringStream.CopyFrom(AData, AData.Size);

    client := FindClient(ABinding.Handle); // Поиск подписки клиента
    if client = nil then
    begin
      client := AddClient(ABinding);
      log.debug('Add UDP Client ' + ABinding.PeerIP + ':' +  IntToStr(ABinding.PeerPort));
    end;

    case ord(DataStringStream.DataString[1]) of
      1:
        begin
          client.SetItems(DataStringStream.DataString, items);
          client.SendDataToClient(Items);
        end;
      2:
        begin
          client.SendDataToClient(Items);
        end;
      3:
        begin
          DeleteClient(ABinding.Handle);
          log.debug('Delete UDP Client ' + ABinding.PeerIP + ':' +
            IntToStr(ABinding.PeerPort));
        end;
      4:
        begin
          // Установка значение в устройстве
          id :=ord(DataStringStream.DataString[2]);
          id := (id shl 8) or ord(DataStringStream.DataString[3]);
          log.debug('Set value id=' + IntToStr(id) + '. UDP Client ' +
            ABinding.PeerIP + ':' + IntToStr(ABinding.PeerPort));
          case items[id].SIType of
            1:
              begin
                if DataStringStream.DataString[4] = #0 then
                  SetItemValue(id, false)
                else
                  SetItemValue(id, true);
              end;
            2:
              begin
                SetItemValue(id, ord(DataStringStream.DataString[4]));
              end;
            3:
              begin
                CopyMemory(@tmpword, @DataStringStream.DataString[4], 2);
                SetItemValue(id, tmpword);
              end;
            4:
              begin
                CopyMemory(@tmpdword, @DataStringStream.DataString[4], 4);
                SetItemValue(id, tmpdword);
              end;
            5:
              begin
                CopyMemory(@tmpfloat, @DataStringStream.DataString[4], 8);
                SetItemValue(id, tmpfloat);
              end;
          end;
        end;
    end;

  except
    on E: Exception do
      log.error('UDP Server Read ' + ABinding.PeerIP + ':' +
        IntToStr(ABinding.PeerPort) + ' ' + E.ClassName + ': ' + E.Message);
  else
    log.error('Unknow error UDP Server Read' + ABinding.PeerIP + ':' +
      IntToStr(ABinding.PeerPort));
  end;
  DataStringStream.Free;
end;

// Поиск подписки клиента
function MyUdpServer.FindClient(handle: integer): ClientUnit;
var
  i: integer;
begin
  FindClient := nil;
  for i := 0 to clients.Count - 1 do
  begin
    if ClientUnit(clients.items[i]).ClientHandle.Handle = handle then
    begin
      FindClient := ClientUnit(clients.items[i]);
      exit;
    end;
  end;
end;

// Добавление нового клиента
function MyUdpServer.AddClient(ClientHandle: TIdSocketHandle): ClientUnit;
begin
  AddClient := ClientUnit(clients.Items[clients.Add(ClientUnit.create(ClientHandle))]);
end;

// Удаление клиента
procedure MyUdpServer.DeleteClient(handle: integer);
var
  i: integer;
begin
  for i := 0 to clients.Count - 1 do
  begin
    if ClientUnit(clients.items[i]).ClientHandle.Handle = handle then
    begin
      clients.Delete(i);
      exit;
    end;
  end;
end;

// name = Устройство.тег - ex9053.dummy
// numitem = netaddress - 214
// type = тип
// parent = описание устройства

function MyUdpServer.AddItem(name: string; numitem: integer; itype: byte;
  parent: TRealDev; tagWrite: Boolean = False): integer;
begin
  if Items[numitem] <> nil then
     raise Exception.Create('Дубликат сетевого адреса. netaddress = '+ IntToStr(numitem) + '. Имя тега: '+ name);

  Items[numitem] := TServItem.create(numitem, itype, name, parent, tagWrite);
  AddItem := numitem;
  numitems := numitems + 1;
end;

// Подключаем модули
procedure MyUdpServer.LoadDevs;
var
  i: integer;
  devices, deviceNode: ixmlnode;
  loadmodule : string;

  pRealDev : Pointer;
begin
  countDevices:= 0;

  devices := xmlconf.ChildNodes[0].ChildNodes.FindNode('devices');

  SetLength(arrRealDev, devices.ChildNodes.Count);
  
  for i := 0 to devices.ChildNodes.Count - 1 do
  begin
    inc(countDevices);

    deviceNode := devices.ChildNodes[i];
    loadmodule := Trim(deviceNode.Attributes['protocol']);

    if deviceNode.HasAttribute('skip') then
     if deviceNode.Attributes['skip'] = 'true' then Continue;

    log.debug('Device address ' + deviceNode.Attributes['address'] + chr(VK_TAB) + '  protocol - ' + loadmodule);

    if loadmodule = 'modbus'  then pRealDev := Tmodbus.Create(deviceNode, countDevices);
    if loadmodule = 'dcom'    then pRealDev := TDcom.Create(deviceNode);
    if loadmodule = 'mercury' then pRealDev := TMercury.Create(deviceNode);

    arrRealDev[i] := pRealDev;

  end;

end;

destructor MyUdpServer.Destroy;
var
  i: Integer;
begin

  for i := Low(Items) to High(Items) do
    if Assigned(Items[i]) then FreeAndNil(Items[i]);

  for i := Low(arrRealDev) to High(arrRealDev) do
    if Assigned(arrRealDev[i]) then FreeAndNil(arrRealDev[i]);

  SetLength(arrRealDev, 0);

  inherited Destroy;
end;

end.


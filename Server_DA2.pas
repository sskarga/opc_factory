unit Server_DA2;

//------------------------------------------------------------------------------
                                interface
//------------------------------------------------------------------------------

uses
  SysUtils, Classes, prOpcServer, prOpcTypes, Hashes, Server_UDP;

type
  FagotDA = class(TOpcItemServer)
  private
    hashItemID: TIntegerHash;

  protected
    function Options: TServerOptions; override;
    procedure OnClientConnect(Client: TClientInfo); override;
    procedure OnClientDisconnect(Client: TClientInfo); override;

    procedure OnClientSetName(Client: TClientInfo); override;
    procedure OnAddGroup(Group: TGroupInfo); override;
    procedure OnRemoveGroup(Group: TGroupInfo); override;
    procedure OnAddItem(Item: TGroupItemInfo); override;
    procedure OnRemoveItem(Item: TGroupItemInfo); override;
  public
    function GetItemInfo(const ItemID: String; var AccessPath: string;
                         var AccessRights: TAccessRights): Integer; override;

    procedure ReleaseHandle(ItemHandle: TItemHandle); override;
    procedure ListItemIds(List: TItemIDList); override;

    function GetItemValue(ItemHandle: TItemHandle;
                          var Quality: Word): OleVariant; override;

    procedure SetItemValue(ItemHandle: TItemHandle; const Value: OleVariant); override;

    constructor Create;
    destructor Destroy;
  end;

const
  ServerGuid: TGUID = '{CAE8D0E1-117B-11D5-944B-00C0F023FA1C}';
  ServerVersion = 1;
  ServerDesc = 'Fagot OPC DA - prOpcKit';
  ServerVendor = 'Production Robots Eng. Ltd';

  {
    размер массива в UDP --> TItems = array[0..2100] of TServItem;
    от 1 до 2000 - данные
    от 2001 до 2100 - служебные теги - активность, ошибки
  }
  countItem = 2000;

var
  // TODO: Проверить необходимость блокировки переменной при могопотном доступе
  countClient: Integer = 0; // количество активных клиентов

//------------------------------------------------------------------------------
                              implementation
//------------------------------------------------------------------------------

uses
  Server_Devices, prOpcError, Windows;

constructor FagotDA.Create;
begin
  inherited Create;
  hashItemID := TIntegerHash.Create;
  countClient := 0;
end;

destructor FagotDA.Destroy;
begin
  FreeAndNil(hashItemID);
  inherited Destroy;
end;

{
  Серверы OPC не должны поддерживать просмотр в соответствии со спецификацией, 
  но большинство это делает, и многие клиенты получают отчетливое представление о не просматриваемых серверах. 
  Спецификация OPC определяет два типа просмотра: плоский и иерархический. Инструмент prOpc поддерживает как плоский, 
  так и иерархический просмотр. В этом примере мы реализуем плоское пространство имен. 

 Для поддержки просмотра нам необходимо реализовать функцию ListItemIds. 
 Это дает краткое изложение информации, предоставленной GetItemInfo, 
 но без фактического предоставления дескриптора для любого из элементов.
 
  Обратите внимание, что параметр «VarType» должен быть совместимым с COM-типом. 
  Обратитесь к документации Delphi по этому вопросу - в справочной системе OleVariant есть краткая заметка. 
  На данный момент лучше всего придерживаться следующих типов кодов:

    - varInteger
    - varDouble
    - varCurrency
    - varDate
    - varOleStr
    - varBoolean

  Предупреждение: НИКОГДА не возвращайте varString - это означает строку ANSI, которая не является совместимым с COM-типом.
  Функция GetItemValue не может возвращать этот тип в любом случае, поскольку возвращает OleVariant,
  который не может содержать строковые значения ANSI.

  Вы также можете использовать порядковый тип varSmallint и varByte, но я не могу придумать веских оснований для этого.
  Одним из способов безопасного предоставления значения параметра VarType является вызов вашего собственного «GetItemValue»,
  а затем использование функции VarType для извлечения кода его типа.
}

procedure FagotDA.ListItemIDs(List: TItemIDList);
var
  i : Integer;
  VarType: Integer;
begin
  log.Info('Browsing OPC tag');

  for i := 1 to countItem - 1 do
  begin
    if Assigned(serv.Items[i]) then
    begin

      // задаем принудительно тип данных
      case serv.Items[i].SIType of
        1: VarType := varBoolean;
        2: VarType := varByte;
        3: VarType := varWord;
        4: VarType := varInteger;
        5: VarType := varDouble;
      else
        VarType := varInteger;
      end;

      if serv.Items[i].iswrite then
        List.AddItemId( serv.Items[i].name, [iaRead, iaWrite], VarType)
      else
        List.AddItemId( serv.Items[i].name, [iaRead], VarType);

      hashItemID.Items[AnsiLowerCase(serv.Items[i].name)] := i;
    end;
  end;
end;

{
  Когда клиент пытается подключиться к серверу, инструментарий вызовет GetItemInfo 
  для получения необходимой ему информации об элементе.
  
  Когда набор инструментов вызывает GetItemInfo, он немедленно вызывает GetItemValue 
  для получения значения «sample» в качестве OleVariant. Затем он использует тип этого 
  возвращаемого значения как собственный тип элемента. По этой причине вы должны быть уверены, 
  что GetItemValue готов вернуть значение, как только GetItemInfo вернется, 
  даже если это значение является «фиктивным» значением.
}
function FagotDA.GetItemInfo(const ItemID: String; var AccessPath: string;
       var AccessRights: TAccessRights): Integer;
var
  i : Integer;
begin

  Result:= 0;

  // проверяем наполнения хеш-списка.
  // Хеш-список будет пустой если запускать ОПС без предварительного просмотра тегов OPCItemServer.InitBrowsing
  if hashItemID.ItemCount = 0 then
  begin
    hashItemID.Clear;

    for i := 1 to countItem - 1 do
    begin
      if Assigned(serv.Items[i]) then
      begin

        if serv.Items[i].iswrite then
          AccessRights := [iaRead, iaWrite]
        else
          AccessRights := [iaRead];

        hashItemID.Items[AnsiLowerCase(serv.Items[i].name)] := i;
      end;
    end;
  end;

  try
    Result:= hashItemID.Items[AnsiLowerCase(ItemID)];
    log.debug('Server_DA2-->GetItemInfo: ' + IntToStr(Result) + ' - ' + ItemID );

  except
    log.error('Module Server_DA2-->GetItemInfo: OPC_E_INVALIDITEMID - ' + ItemID);
    raise EOpcError.Create(OPC_E_INVALIDITEMID);
  end;
end;

 {
    Возможно, вам потребуется выделить память или другой ресурс, чтобы реализовать свой дескриптор элемента. 
    Если вы это сделаете, вам нужно будет знать, когда можно освободить эти ресурсы. 
    ReleaseHandle вызывается, когда набор инструментов завершен с помощью вашего дескриптора. 
    Это произойдет, когда все клиенты отключились, или все группы, использующие ваш элемент, были удалены.
  }
procedure FagotDA.ReleaseHandle(ItemHandle: TItemHandle);
begin
  {Release the handle previously returned by GetItemInfo}
end;

{
  Cледует отметить, что хотя значение, возвращаемое GetItemValue, является OleVariant, 
  тип определенного элемента (целое число, строка и т. Д.) Не должен меняться в течение всего 
  срока службы устройства , Как только GetItemInfo вернется, вы должны убедиться, что GetItemValue 
  способен возвращать значение правильного типа. Фактическое значение не имеет значения, но тип важен. 
  Если вы хотите указать, что возвращаемое значение неверно или значимо, вы можете использовать
  параметр «Качество», чтобы указать факт, вернув OPC_QUALITY_OUT_OF_SERVICE, но вы всегда должны 
  возвращать значение правильного типа. Компилятор delphi не будет жаловаться, если вы этого не сделаете,
  так как функция возврата OleVariant автоматически инициализируется на «Нераспределенный». 
  Не полагайтесь на компилятор, чтобы забрать вас.
  
  Параметр качества
  В более общем смысле параметр «Качество» является выходным параметром, который может 
  использоваться для указания того, что возвращаемое значение меньше 100% точного или актуального. 
  Для подробного обсуждения значений качества вам нужно будет обратиться к спецификации доступа к данным OPC. 
  Разрешенные значения качества объявляются в блоке prOpcDa следующим образом:
  
// Masks for extracting quality subfields
// (note 'status' mask also includes 'Quality' bits)
  
  OPC_QUALITY_MASK           = $C0;
  OPC_STATUS_MASK            = $FC;
  OPC_LIMIT_MASK             = $03;

// Values for QUALITY_MASK bit field
  OPC_QUALITY_BAD            = $00; Плохо
  OPC_QUALITY_UNCERTAIN      = $40; неуверенный
  OPC_QUALITY_GOOD           = $C0; хорошо

// STATUS_MASK Values for Quality = BAD
  OPC_QUALITY_CONFIG_ERROR   = $04; Ошибка конфигурации
  OPC_QUALITY_NOT_CONNECTED  = $08; Не подключен
  OPC_QUALITY_DEVICE_FAILURE = $0C; Отказ устройства
  OPC_QUALITY_SENSOR_FAILURE = $10; Сбой датчика
  OPC_QUALITY_LAST_KNOWN     = $14; Последний известный
  OPC_QUALITY_COMM_FAILURE   = $18; Сбой связи
  OPC_QUALITY_OUT_OF_SERVICE = $1C; Не работает

// STATUS_MASK Values for Quality = UNCERTAIN
  OPC_QUALITY_LAST_USABLE    = $44; // Последний полезный
  OPC_QUALITY_SENSOR_CAL     = $50; // Вызов датчика
  OPC_QUALITY_EGU_EXCEEDED   = $54; // Превышено egu
  OPC_QUALITY_SUB_NORMAL     = $58; // Суб нормальный

// STATUS_MASK Values for Quality = GOOD
  OPC_QUALITY_LOCAL_OVERRIDE = $D8; // Локальное переопределение


Когда вызывается GetItemValue, параметр «Качество» инициализируется OPC_QUALITY_GOOD. 
В большинстве случаев это значение, которое вы хотите вернуть, 
поэтому вы можете игнорировать этот параметр.  
}
function FagotDA.GetItemValue(ItemHandle: TItemHandle;
                           var Quality: Word): OleVariant;
begin
  {return the value of the item identified by ItemHandle}
  if Assigned(serv.Items[ItemHandle]) then
    begin
      //Quality := serv.Items[ItemHandle].quality;
      Result:= serv.Items[ItemHandle].value;
    end
  else
    begin
      Result:= 0;
      raise EOpcError.Create(OPC_E_INVALIDHANDLE);
      log.error('Module Server_DA2-->GetItemValue: OPC_E_INVALIDHANDLE - '+ IntToStr(ItemHandle));
    end;

end;

procedure FagotDA.SetItemValue(ItemHandle: TItemHandle; const Value: OleVariant);
begin
  {set the value of the item identified by ItemHandle}

  if Assigned(serv.Items[ItemHandle]) then    // проверка существования тега
    begin
      if serv.Items[ItemHandle].iswrite then   // проверка доступность на запись
      begin
        serv.SetItemValue(ItemHandle, Value);
      end
      else
      begin
        raise EOpcError.Create(OPC_E_NOTSUPPORTED);
        log.error('Module Server_DA2-->SetItemValue: OPC_E_NOTSUPPORTED - '+ IntToStr(ItemHandle));
      end;
    end
  else
  begin
    raise EOpcError.Create(OPC_E_INVALIDHANDLE);
    log.error('Module Server_DA2-->SetItemValue: OPC_E_INVALIDHANDLE - '+ IntToStr(ItemHandle));
  end;

end;

function FagotDA.Options: TServerOptions;
begin
  Result:= [soHierarchicalBrowsing, soAlwaysAllocateErrorArrays]
 //Result:= [soHierarchicalBrowsing]
end;


procedure FagotDA.OnClientConnect(Client: TClientInfo);
begin
  {Code here will execute whenever a client connects}
  try
   countClient := ClientCount;
  finally
   log.Info('OPC Client Connect: ' + Client.ClientName + '. Count clients: ' + IntToStr(countClient) );
  end;
end;

procedure FagotDA.OnClientDisconnect(Client: TClientInfo);
begin
  {Code here will execute whenever a client connects}
  try
    countClient := ClientCount;
  finally
   log.Info('OPC Client Disconnect: ' + Client.ClientName + '. Count clients: ' + IntToStr(countClient));
  end;
end;

procedure FagotDA.OnClientSetName(Client: TClientInfo);
begin
  {Code here will execute whenever a client calls IOpcCommon.SetClientName}
  log.Info( Format('Client SetName %s',[Client.ClientName]) );
end;

procedure FagotDA.OnAddGroup(Group: TGroupInfo);
begin
  {Code here will execute whenever a client adds a group}
  log.Info( Format('Add Group %s ok', [Group.Name]) );
end;

procedure FagotDA.OnRemoveGroup(Group: TGroupInfo);
begin
  {Code here will execute whenever a client removes a group}
  log.Info( Format('Remove Group %s ok', [Group.Name]) );
end;

procedure FagotDA.OnAddItem(Item: TGroupItemInfo);
begin
  {Code here will execute whenever a client adds an item to a group}
  log.Info( Format('Add Item %s ok', [Item.ItemID]) );
end;

procedure FagotDA.OnRemoveItem(Item: TGroupItemInfo);
begin
  {Code here will execute whenever a client removes an item from a group}
  log.Info( Format('Remove Item %s ok', [Item.ItemID]) );
end;

initialization
  RegisterOPCServer(ServerGUID, ServerVersion, ServerDesc, ServerVendor, FagotDA.Create)
 

end.


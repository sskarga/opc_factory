unit MainForm;

//------------------------------------------------------------------------------
                                  interface
//------------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, Menus, XMLDoc, msxmldom, XMLIntf,
  ActiveX,
  Server_UDP,
  cport,
  Server_devices,
  Server_SerialPort,
  Server_DA2,
  prOpcServer,
  TLoggerUnit,
  Dialogs, XPMan, ImgList,
  CoolTrayIcon, xmldom;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Timer1: TTimer;
    Panel1: TPanel;
    ListView1: TListView;
    XMLDoc: TXMLDocument;
    spl1: TSplitter;
    Panel2: TPanel;
    pnl2: TPanel;
    StatusBar: TStatusBar;
    xpmnfst1: TXPManifest;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TreeView1: TTreeView;
    ImageList1: TImageList;
    ImageList2: TImageList;
    CoolTrayIcon: TCoolTrayIcon;
    Timer2: TTimer;

    procedure FormShow(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Timer1Timer(Sender: TObject);
    procedure refreshData();
    procedure FormCreate(Sender: TObject);
    procedure CoolTrayIconClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure CoolTrayIconMinimizeToTray(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }

  end;

var
  Form1: TForm1;
  arrSerialPort: array of DevPort;
  workDirectory: string;

implementation


{$R *.dfm}

uses
  TConfiguratorUnit, TypInfo, Registry, ShellAPI;

// запись и чтение рабочей директории из реестра
// Т.к. призапуске из другого приложения отсутствует информация о путях к файлам конфигурации
function setRegWorkDir(pathToDir: string): Boolean;
var
 Registry: TRegistry;
begin
  try
   Registry := TRegistry.Create;
   Registry.RootKey := HKEY_LOCAL_MACHINE;
   Registry.OpenKey('SOFTWARE\Fagot',true);
   Registry.WriteString('OPCAppDir', pathToDir);
   Registry.CloseKey;
   Registry.Free;
   Result := True;
  except
   log.error('Not write to registry path opc app - HKEY_LOCAL_MACHINE\SOFTWARE\Fagot\OPCAppDir');
   Result := False;
  end;
end;

function getRegWorkDir(): string;
var
 Registry: TRegistry;
begin
  try
   Registry := TRegistry.Create;
   Registry.RootKey := HKEY_LOCAL_MACHINE;
   Registry.OpenKey('SOFTWARE\Fagot',true);
   Registry.ReadString('OPCAppDir');
   Registry.CloseKey;
   Registry.Free;
  except
   log.error('Not write to registry path opc app - HKEY_LOCAL_MACHINE\SOFTWARE\Fagot\OPCAppDir');
   Result := '';
  end;
end;

// обработчик ключей запуска приложения
procedure AppParams();
var
  i: Integer;
begin
  for i:= 1 to ParamCount do
  begin
    if SameText(ParamStr(i), '/regserver') then
    begin
      if not(setRegWorkDir(ExtractFilePath(ParamStr(0)))) then
        MessageBoxW(0, 'Ошибка записи данных в реестр. ' + #13#10 + 
          'Запустите программу от имени администратора.',
          'Ошибка регистрации', MB_OK + MB_ICONSTOP);
          ;
    end;

    if SameText(ParamStr(i), '/unregserver') then
    begin
      // prOpcServer - сам отменяет регистрацию ActiveX
    end;

  end;

end;

// создаем списки опроса для каждого COM порта
procedure CreateScanList();
var
  i: integer;
  j: integer;
  count: Integer;
begin

   // создание списков для опроса портов
  for i := Low(arrSerialPort) to High(arrSerialPort) do
    if Assigned(arrSerialPort[i]) then
    begin
      count:= 0;

      for j := 0 to serv.Devs.Count - 1 do
        if TRealDev(serv.Devs.Items[j]).dport =  arrSerialPort[i].pnum then
        begin
          count:= count + 1;
          SetLength(arrSerialPort[i].scanDevs, count);
          arrSerialPort[i].scanDevs[count-1]:= serv.Devs.Items[j];
        end;

      // Запуск потоков опроса com порта
      arrSerialPort[i].resume;
      log.debug('Run thead com port - '+IntToStr(i));
      Sleep(200);
    end;

end;

// Событие - главное окно развернуто. запускаем обновление данных на форме
procedure TForm1.FormShow(Sender: TObject);
begin
  log.info('Main windows show');
  Timer1.Enabled := True;
end;

// Событие - разукрашиваем список тегов
procedure TForm1.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Item.SubItems[0] = 'False' then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := $0084C1FF;
  end;

  if Item.SubItems[0] = 'True' then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := $008AFF8A;
  end;

  if Item.SubItems[0] = '0' then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := $00D5FFFF;
  end;

  if Item.SubItems[0] = 'NULL' then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := $00E8E8E8;
  end;

  if Item.SubItems[1] = 'bad' then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := $008484FF;
  end;

end;

//запрос на закрывание программы
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;

  if serv.flagRead then
    if MessageDlg('Завершить работу OPC?', mtWarning, [mbYes, mbNo], 0) = mrNo then
      begin
        CanClose := false;

        // если отмена закрытия, то в трей
        if not CoolTrayIcon.IconVisible then
        begin
         CoolTrayIcon.IconVisible := true;
         CoolTrayIcon.HideMainForm;
        end;
      end;

end;

// закрытие программы
procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
    StatusBar.Panels[2].Text:= 'STOPING. Please wait... ';

    log.info('STOPING. Please wait...');
    Timer1.Enabled:= False;
    Timer2.Enabled:= False;

    try
    CoUninitialize;

    // останавливаем сокет
    serv.Stop;
    log.debug('Stop UDP server');

    //  Останавливаем потоки опроса com портов
    for i := Low(arrSerialPort) to High(arrSerialPort) do
    if Assigned(arrSerialPort[i]) then
    begin
      try
        arrSerialPort[i].Terminate;
        log.debug('Terminate COM ['+IntToStr(i)+']');
      except
        on E: Exception do
          log.Fatal('no free serial port '+ E.ClassName + ': ' + E.Message);
       else
          log.Fatal('no free serial port');
      end;
    end;
    SetLength(arrSerialPort, 0);

    // освобождаем UDP
    serv.Free;
    log.debug('Free UDP server');

    except
      on E: Exception do
      begin
        log.Fatal('ERROR HALT!!! '+ E.ClassName + ': ' + E.Message);
        Application.MessageBox('Ошибка при закрытие программы. Подробности в логе.', 'Ошибка', MB_OK + MB_ICONSTOP + MB_TOPMOST);
        Halt(1);
      end

     else
      begin
        log.Fatal('ERROR HALT!!!');
        Application.MessageBox('Ошибка при закрытие программы. Подробности в логе.', 'Ошибка', MB_OK + MB_ICONSTOP + MB_TOPMOST);
        Halt(1);
      end;

    end;

  log.Info('------------------------ STOP -------------------------');
  log.Free;
end;

// обновление данных на форме
procedure TForm1.refreshData();
var
  tmpdev: TRealDev;
begin
   if  (TreeView1.Selected <> nil) and (TreeView1.Selected.ImageIndex = 1) then
   begin

    tmpdev := TRealDev(TreeView1.Selected.Data);
    tmpdev.draw(listview1);

    Label1.Caption:= 'Устройство: '+ tmpdev.name;
    Label2.Caption:= 'Порт: COM'+ IntToStr(tmpdev.dport);
    Label3.Caption:= 'Тип: '+ tmpdev.devtype;

   end;

   StatusBar.Panels[1].Text:= 'Update at '+ FormatDateTime('dd-mm-yyyy hh:nn:ss', Now);
   StatusBar.Panels[2].Text:= 'DA client: '+ IntToStr(countClient);
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
   refreshData;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  refreshData;
end;

// версия ПО
function GetMyVersion:string;
type
  TVerInfo=packed record
    Nevazhno: array[0..47] of byte; // ненужные нам 48 байт
    Minor,Major,Build,Release: word; // а тут версия
  end;
var
  s:TResourceStream;
  v:TVerInfo;
begin
  result:='';
  try
    s:=TResourceStream.Create(HInstance,'#1',RT_VERSION); // достаём ресурс
    if s.Size>0 then begin
      s.Read(v,SizeOf(v)); // читаем нужные нам байты
      result:=IntToStr(v.Major)+'.'+IntToStr(v.Minor)+'.'+ // вот и версия...
              IntToStr(v.Release)+'.'+IntToStr(v.Build);
    end;
  s.Free;
  except;
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: integer;

  serialPort: DevPort;
  serialPorts: ixmlnode;

  serialPortNum: Byte;
  serialPortBRate: TBaudRate;

  stInfo: string;
  confBaudRate: string;
  confFile: string;
  cicleTimeMs: Integer;

  TreeNode : TTreeNode;
begin
  CoInitialize(nil);

  SetLength(arrSerialPort, 0);

  AppParams;

  try

  workDirectory := getRegWorkDir;

  if workDirectory = '' then
    workDirectory := ExtractFileDir(ParamStr(0));

  SetCurrentDir(workDirectory);

  TConfiguratorUnit.doPropertiesConfiguration(
              workDirectory + '\log4delphi.properties');

  log := TLogger.getInstance;
  log.Info('------------------------ START ------------------------');
  log.Info('Config logfile: log4delphi.properties');
  log.Info('Work dir: ' + ParamStr(0));
  except
    Application.MessageBox('Ошибка инициализации системы логиирования.' +
      #13#10 + 'Проверьте настройки в файле  log4delphi.properties',
      'Ошибка инициализации', MB_OK + MB_ICONSTOP);
      
    Form1.Close;
    Exit;
  end;

{$IfDef UNIT_ERR}
  log.info('Version: '+ GetMyVersion +'f DA');
  ErrFileName := workDirectory + '\opc.log';
{$Else}
  log.info('Version: '+ GetMyVersion +' DA');
{$EndIf}

  confFile := workDirectory + '\config.xml';

  log.info('Config file: ' + confFile);

  if not FileExists(confFile) then
  begin
    raise Exception.Create('Файл конфигурации не найден!');
    log.error('Not exist config file.');
    Exit;
  end;


  try
    XMLDoc.LoadFromFile(confFile);
    XMLDoc.Active := True;

{$IfDef UNIT_ERR}
    // unit error
    try
      ErrDate:= Trim(XMLDoc.DocumentElement.ChildNodes['netadr'].Text);
      initError();
    except
      log.error('Read node netadr');
    end;
{$EndIf}


    log.info('Start UDP Server in port 666');
    serv := MyUdpServer.Create(666, XMLDoc);
    serv.loaddevs;
    serv.start;

    // port
    log.info('Config serial port');
    serialPorts := XMLDoc.ChildNodes[0].ChildNodes.FindNode('serialport');
    SetLength(arrSerialPort, serialPorts.ChildNodes.Count);

    stInfo := '';

    for i := 0 to serialPorts.ChildNodes.Count - 1 do
    begin
	  serialPortNum := StrToInt(serialPorts.ChildNodes[i].Attributes['numbercom']);
	
	  // default
      confBaudRate := '9600';
      cicleTimeMs  := 1000;

      if serialPorts.ChildNodes[i].HasAttribute('baudrate') then
        confBaudRate := Trim(serialPorts.ChildNodes[i].Attributes['baudrate'])
      else
      begin
        log.error('- COM' + IntToStr(serialPortNum) + ' = Not set attribute "baudrate".');
        raise Exception.Create('Not set attribute "baudrate"');
      end;

      if serialPorts.ChildNodes[i].HasAttribute('cycletime') then
        cicleTimeMs := StrToInt(serialPorts.ChildNodes[i].Attributes['cycletime'])
      else
        log.Warn('- COM' + IntToStr(serialPortNum) + ' = Not set attribute "cycletime". Default cycletime = 1000');
		
      if cicleTimeMs <= 100 then cicleTimeMs := 100;

      serialPortBRate := TBaudRate(GetEnumValue(TypeInfo(TBaudRate), 'br' + confBaudRate ));

      if (serialPortNum > 0) and (serialPortNum < 9) then
      begin
        serialPort := DevPort.Create(serialPortNum, serialPortBRate, sbOneStopBit, cicleTimeMs);
        serialPort.FreeOnTerminate := True;

        arrSerialPort[i] := serialPort;
        log.info('- COM' + IntToStr(serialPortNum) + ' = OK. Cycle '+ IntToStr(cicleTimeMs) + ' ms');
        stInfo := stInfo + 'COM' + IntToStr(serialPortNum) + ' - ' + confBaudRate + ' | ';
      end
      else
      begin
        log.error('- COM' + IntToStr(serialPortNum) + ' = NOT CONFIG. Cycle '+ IntToStr(cicleTimeMs) + ' ms' );
        MessageDlg('Ошибка настроки COM порта. Подробности в лог файле.',
          mtError, [mbOK], 0);
        stInfo := stInfo + 'COM' + IntToStr(serialPortNum) + ' - ERROR | ';
      end;

      TreeNode:= TreeView1.Items.AddChildObject(nil, 'COM' + IntToStr(serialPortNum), nil );
      TreeNode.ImageIndex:= 0;
    end;

    log.info('Successfully configured');

  except
    on E: EInOutError do
    begin
      log.error('Ошибка начальной конфигурации EInOutError: ' + XMLDoc.FileName + ' ' + E.ClassName + ': ' + E.Message);
      Application.MessageBox('Ошибка начальной конфигурации EInOutError. Подробности в лог файле.', 'Ошибка', MB_OK + MB_ICONSTOP + MB_TOPMOST);
      Application.Terminate;
    end;

    on E: Exception do
    begin
      log.error('Ошибка начальной конфигурации: ' + XMLDoc.FileName + ' ' + E.ClassName + ': ' + E.Message);
      Application.MessageBox('Ошибка начальной конфигурации. Подробности в лог файле.', 'Ошибка', MB_OK + MB_ICONSTOP + MB_TOPMOST);
      Application.Terminate;
    end;

  else
    begin
      log.error('Неизвестная ошибка начальной конфигурации. ' + XMLDoc.FileName);
      Application.MessageBox('Неизвестная ошибка начальной конфигурации. Подробности в лог файле.'
      , 'Ошибка', MB_OK + MB_ICONSTOP + MB_TOPMOST);
      Application.Terminate;
    end;
  end;

  CreateScanList();

  // информация
  TreeView1.Items.BeginUpdate;
  for i := 0 to serv.Devs.Count - 1 do
  begin
    for j:= 0 to TreeView1.Items.Count-1 do
    begin
      if TreeView1.Items[j].Text = 'COM'+ IntToStr(TRealDev(serv.Devs.Items[i]).dport) then
      begin
        TreeNode:= TreeView1.Items.AddChildObject(TreeView1.Items[j] , TRealDev(serv.Devs.Items[i]).name , serv.Devs.Items[i] );
        TreeNode.ImageIndex:= 1;
        TreeNode.SelectedIndex:= 2;
        Break;
      end;
    end;
  end;
  TreeView1.Items.EndUpdate;

  log.info('Load devices: ' + IntToStr(serv.Devs.Count));

  StatusBar.Panels.Items[0].Text:= stInfo;

{$IfDef UNIT_ERR}
  // unit error
  GetError();
{$EndIf}

end;

procedure TForm1.CoolTrayIconClick(Sender: TObject);
begin
  CoolTrayIcon.ShowMainForm;
  CoolTrayIcon.IconVisible := false;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  if serv.flagRead then
    serv.flagRead := False
  else
   begin
   
    if ( countClient = 0 ) then
    begin
      log.info('process IDLE - programm close');
      Form1.Close;
    end;

   end;

 {$IfDef UNIT_ERR}
  // unit error
  GetError();
 {$EndIf}
end;

// Управление обновление данных на форме по таймеру. Когда форма не активна данные не обновляются.
procedure TForm1.FormActivate(Sender: TObject);
begin
    Timer1.Enabled := True;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
    Timer1.Enabled := False;
end;

procedure TForm1.CoolTrayIconMinimizeToTray(Sender: TObject);
begin
   Timer1.Enabled := False;
end;

end.

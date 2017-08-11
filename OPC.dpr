program OPC;



uses
  Forms,
  Windows,
  Dialogs,
  MainForm in 'MainForm.pas' {Form1},
  Server_SerialPort in 'Server_SerialPort.pas',
  Server_Devices in 'Server_Devices.pas',
  Server_Protocol_Modbus in 'Server_Protocol_Modbus.pas',
  Server_UDP in 'Server_UDP.pas',
  Server_protocol_DCOM in 'Server_Protocol_DCOM.pas',
  UHelper in 'UHelper.pas',
  Server_Protocol_Mercury in 'Server_Protocol_Mercury.pas',
  Server_DA2 in 'Server_DA2.pas';

{$R *.res}

var
 H: THandle;

begin
  // Запрет на запуск копии
  H := CreateMutex(nil, True, 'opc3857439824239');
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    showmessage('Программа уже работает!');
    Exit;
  end;

  Application.Title := 'OPC BSU';
  Application.CreateForm(TForm1, Form1);
  Application.Initialize;
  Application.Run;
end.

unit Server_SerialPort;

//------------------------------------------------------------------------------
                                interface
//------------------------------------------------------------------------------

uses Windows, Messages, cport, sysutils, SyncObjs, Server_Devices, Classes;

type
  DevPort = class(TThread)
    port: TComPort;
    lock: TCriticalSection;
    Events: TComEvents;
    pnum: integer;
    scanDevs: array of Pointer;
    cycleSetTime: cardinal;
    procedure execute(); override;
    constructor Create(portnum: byte; brate: TBaudRate; stopbits: TStopBits; cycleTime: Integer);
    destructor Destroy; override;
  end;

var
  port: DevPort;

//------------------------------------------------------------------------------
                            implementation
//------------------------------------------------------------------------------

procedure DevPort.execute();
var
  i: integer;
  tmpitem: TRealDev;


  tagTime: cardinal;   // время опроса тега или групового тега
  pollTime: cardinal;  // время опроса всех устройств на порту
  cycleTime: cardinal;  // периодичность опроса
  cycleDelay: cardinal;  // задержка потока

  scanStartItems : Integer;
  scanStopItems: Integer;
begin
  scanStartItems:= Low(scanDevs);
  scanStopItems:= High(scanDevs);

  while not Terminated do
  begin

    cycleTime:= GetTickCount;

    try
      if port.Connected then
      begin
        for i := scanStartItems to scanStopItems do
        begin
          if Terminated then Break;

          tmpitem := TRealDev(scanDevs[i]);
          tagTime := GetTickCount;
          tmpitem.Write(port);  // сначало пишем в порт, затем читаем с порта
          tmpitem.Read(port);
          tmpitem.timer := GetTickCount - tagTime;

        end;
      end;

    except
       on E: EInOutError do
        begin
          log.Error('TThread.DevPort.execute EInOutError. serial port: '+ IntToStr(pnum)
                     + ' ' + E.ClassName + ': ' + E.Message);
        end;

       on E: Exception do
        begin
          log.Error('TThread.DevPort.execute serial port: ' + IntToStr(pnum)
                     + ' ' + E.ClassName + ': ' + E.Message);
        end;

      else
        begin
          log.Error('Unknown error. TThread.DevPort.execute serial port: ' + IntToStr(pnum));
        end;

    end;

    pollTime:= GetTickCount - cycleTime;

    if (pollTime < 10) then
     log.error('Port #' + IntToStr(pnum) + '. Loop DevPort is empty. No devices.')
    else
     log.debug('Port #' + IntToStr(pnum) + '. Polling time: ' + IntToStr(pollTime)+ ' ms.');

    // delay cycle
    pollTime:= GetTickCount - cycleTime;

    if (pollTime < cycleSetTime) and (not Terminated) then
    begin
      cycleDelay:= cycleSetTime - pollTime;

      if cycleDelay > 10 then
      begin
        Sleep(cycleDelay);
      end;

    end;

    log.debug('Port #' + IntToStr(pnum) + '. Cicle time: ' + IntToStr(GetTickCount - cycleTime)+ ' ms.');
  end;

end;

constructor DevPort.Create(portnum: byte; brate: TBaudRate; stopbits:
  TStopBits; cycleTime: Integer);
begin
  inherited create(true);

  pnum := portnum;
  port := TComPort.Create(nil);
  port.BaudRate := brate;
  port.Port := 'com' + inttostr(portnum);
  port.Timeouts.ReadInterval := 10; //4
  port.Timeouts.ReadTotalMultiplier := 100; //50
  port.Timeouts.ReadTotalConstant := 0;
  port.Timeouts.WriteTotalMultiplier := 0;
  port.Timeouts.WriteTotalConstant := 0;

  port.FlowControl.FlowControl := fcNone;
  port.FlowControl.ControlDTR := dtrDisable;
  port.FlowControl.ControlRTS := rtsDisable;

  port.SyncMethod := smWindowSync;

  port.StopBits := stopbits;
  //port.Events := [evRxChar];
  port.Events := [];
  port.Open;

  cycleSetTime:= cycleTime;
end;

destructor DevPort.Destroy;
begin
    SetLength(scanDevs, 0);
    inherited Destroy;
end;

end.


unit Server_Devices;

//------------------------------------------------------------------------------
                            interface
//------------------------------------------------------------------------------

uses Classes, CPort, Contnrs, Variants, ActiveX, Windows, SysUtils, 
     TLoggerUnit, ComCtrls, USWMRGuard, UTheadFIFOQueue;

type
  TDevItem = class(TObject)
    private
      wrLock: TSingleWriterMultipleReaderGuard;

    protected
      _handle: integer;    // id netaddress
      _name: string;       // имя
      _address: integer;   // сетевой адрес
      _value: variant;     // значение
      _iswrite: boolean;   // запись значения или только чтение

     procedure SetValue(svalue: variant);
     function GetValue():Variant;

    public
      property handle:integer  read _handle;
      property name: string read _name;
      property adress: integer read _address;
      property iswrite: boolean read _iswrite;
      property value: variant read GetValue write SetValue;

      constructor Create; overload;
      constructor Create(handle: Integer; name:string; address: Integer;
                          value: Variant; iswrite: Boolean ); overload;
      destructor Destroy; override;
  end;

type
  TRealDev = class
  public
    name: string;
    adress: string;
    itemlist: TObjectList;
    dport: integer;
    cmdQueue: TTheadQueue; // Команды на выполнения
    timer: cardinal;
    devtype: string;

    procedure WriteParams(itemname: Integer; const Value: Variant); virtual; abstract; // интерфейс для потомков, передача параметров в порт
    procedure Read(port: TComPort); virtual; abstract;  // интерфейс для потомков, чтение параметров с порта
    procedure Write(port: TComPort); virtual; abstract; // интерфейс для потомков, запись параметров в порт

    function AddItem(iname: string; vtype: TVarType; netaddress: integer; tagWrite : Boolean = False):integer;
    procedure draw(inlist: tlistview); virtual; abstract; // интерфейс для потомков, отображение значений тегов устройства на главной форме

    Constructor Create(dname: string; portnum: integer); overload;
    destructor Destroy; override;
  end;

var
  log : TLogger;

//------------------------------------------------------------------------------
                          implementation
//------------------------------------------------------------------------------

uses
 Server_UDP;

{TDevItem}
constructor TDevItem.Create;
begin
  _value := Null;
  Self.wrLock := TSingleWriterMultipleReaderGuard.Create;
end;

constructor TDevItem.Create(handle: Integer; name:string; address: Integer;
                          value: Variant; iswrite: Boolean);
begin
  _handle := handle;

  if Length(name) > 30 then
    _name := Copy(name, 1, 30)
  else
    _name := name;

  _address := address;
  _value := value;
  _iswrite := iswrite;
  Self.wrLock := TSingleWriterMultipleReaderGuard.Create;
end;

destructor TDevItem.Destroy;
begin
  Self.wrLock.Free;

  inherited Destroy;
end;

procedure TDevItem.SetValue(svalue: variant);
begin
  wrLock.WaitToWrite;
    _value:= svalue;
  wrLock.Done;
end;

function TDevItem.GetValue():Variant;
begin
  wrLock.WaitToRead;
    Result:= _value;
  wrLock.Done;
end;

{RealDev}

// добавляем новые теги
function TRealDev.AddItem(iname: string; vtype: TVarType; netaddress: integer; tagWrite : Boolean = False):
  integer;
begin
  result := serv.AddItem(self.name + '.' + iname, netaddress, vtype, self, tagWrite);
end;

constructor TRealDev.Create(dname: string; portnum: integer);
begin
  dport := portnum;
  self.name := dname;
  serv.adddev(self);

  cmdQueue := TTheadQueue.Create;
end;

destructor TRealDev.Destroy;
begin
  cmdQueue.Free;
  inherited Destroy;
end;

end.


unit UTheadFIFOQueue;

//------------------------------------------------------------------------------
                                 interface
//------------------------------------------------------------------------------
uses
  Classes, Windows, SysUtils;

type
  TElement = string[20];

  TNodePointer = ^TNode;
  TNode = record
    value: TElement;
    next: TNodePointer;
  end;

  TTheadQueue = class(TObject)
  private
    fHead: TNodePointer;
    fTail: TNodePointer;
    FLock: TRTLCriticalSection;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Enqueue(item: TElement);
    function Dequeue: TElement;
    function isEmpty: boolean;
  end;

//------------------------------------------------------------------------------
                               implementation
//------------------------------------------------------------------------------

  constructor TTheadQueue.Create;
  begin
    inherited Create;
    InitializeCriticalSection(FLock);
    fHead:= nil;
    fTail:= nil;
  end;
  
  destructor TTheadQueue.Destroy;
  begin
    EnterCriticalSection(FLock);
      try
        while not isEmpty do
          Dequeue;
      finally
        fHead:= nil;
        fTail:= nil;
        LeaveCriticalSection(FLock);
      end;
    
    DeleteCriticalSection(FLock);
  end;
  
  procedure TTheadQueue.Enqueue(item: TElement);
  var
    temp: TNodePointer;
  begin
    EnterCriticalSection(FLock);
    
    New(temp);
    temp^.value:= item;
    temp^.next:= nil;
    if (fHead = nil) then
    begin
      fHead:= temp;
      fTail:= temp;
    end
    else
    begin
      fTail^.next:= temp;
      fTail:= temp;
    end;
    
    LeaveCriticalSection(FLock);
  end;
  
  function TTheadQueue.Dequeue: TElement;
  var
    temp: TNodePointer;
  begin
    EnterCriticalSection(FLock);
    
    if not (fHead = nil) then
    begin
      temp:= fHead;
      fHead:= fHead^.next;
      result:= temp^.value;
      Dispose(temp);
    end
    else
      result:= '';
      
    LeaveCriticalSection(FLock);
  end;
  
  function TTheadQueue.isEmpty: boolean;
  begin
    EnterCriticalSection(FLock);
      result:= fHead = nil;
    LeaveCriticalSection(FLock);
  end;



end.

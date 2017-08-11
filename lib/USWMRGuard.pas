unit USWMRGuard;

//------------------------------------------------------------------------------
                                interface
//------------------------------------------------------------------------------

uses Windows;

type
 TSingleWriterMultipleReaderGuard  = class
 private
  m_cs : TRTLCriticalSection;// Permits exclusive access to other members
  m_hsemReaders : THandle;// Readers wait on this if a writer has access
  m_hsemWriters : THandle;// Writers wait on this if a reader has access
  m_nWaitingReaders : Integer;// Number of readers waiting for access
  m_nWaitingWriters : Integer;// Number of writers waiting for access
  m_nActive : Integer;// Number of threads currently with access (0=no threads, >0=# of readers, -1=1 writer)
 public
  procedure WaitToRead;// Call this to gain shared read access
  procedure WaitToWrite;// Call this to gain exclusive write access
  procedure Done;// Call this when done accessing the resource
  constructor Create;
  destructor Destroy; override;
 end;

//------------------------------------------------------------------------------
                            implementation
//------------------------------------------------------------------------------

{ TSingleWriterMultipleReaderGuard }

constructor TSingleWriterMultipleReaderGuard.Create;
begin
 inherited;
 m_hsemReaders := CreateSemaphore(nil, 0, MAXLONG, nil);
 m_hsemWriters := CreateSemaphore(nil, 0, MAXLONG, nil);
 InitializeCriticalSection(m_cs);
end;

destructor TSingleWriterMultipleReaderGuard.Destroy;
begin
 DeleteCriticalSection(m_cs);
 CloseHandle(m_hsemReaders);
 CloseHandle(m_hsemWriters);
 inherited;
end;

procedure TSingleWriterMultipleReaderGuard.Done;
var
 hsem : THandle;
 lCount : Integer;
begin
 // Ensure exclusive access to the member variables
 EnterCriticalSection(m_cs);
 if (m_nActive > 0) then
  begin
   // Readers have control so a reader must be done
   Dec(m_nActive);
  end
 else
  begin
   // Writers have control so a writer must be done
   Inc(m_nActive);
  end;
 hsem := 0;// Assume no threads are waiting
 lCount := 1;// Assume only 1 waiter wakes; always true for writers
 if (m_nActive = 0) then
  begin
   // No thread has access, who should wake up?
   // NOTE: It is possible that readers could never get access
   //       if there are always writers wanting to write
   if (m_nWaitingWriters > 0) then
    begin
     // Writers are waiting and they take priority over readers
     m_nActive := -1;         // A writer will get access
     Dec(m_nWaitingWriters);    // One less writer will be waiting
     hsem := m_hsemWriters;   // Writers wait on this semaphore
     // NOTE: The semaphore will release only 1 writer thread
    end
   else
   if (m_nWaitingReaders > 0) then
    begin
     // Readers are waiting and no writers are waiting
     m_nActive := m_nWaitingReaders;   // All readers will get access
     m_nWaitingReaders := 0;           // No readers will be waiting
     hsem := m_hsemReaders;            // Readers wait on this semaphore
     lCount := m_nActive;              // Semaphore releases all readers
    end
   else
    begin
      // There are no threads waiting at all; no semaphore gets released
    end;
  end;
 // Allow other threads to attempt reading/writing
 LeaveCriticalSection(m_cs);
 if (hsem <> 0) then
  begin
   // Some threads are to be released
   ReleaseSemaphore(hsem, lCount, nil);
  end;
end;


procedure TSingleWriterMultipleReaderGuard.WaitToRead;
var
 fResourceWritePending : Boolean;
begin
 // Ensure exclusive access to the member variables
 EnterCriticalSection(m_cs);
 // Are there writers waiting or is a writer writing?
 fResourceWritePending := (m_nWaitingWriters <> 0) or (m_nActive < 0);
 if (fResourceWritePending) then
  begin
    // This reader must wait, increment the count of waiting readers
    Inc(m_nWaitingReaders);
  end
 else
  begin
   // This reader can read, increment the count of active readers
    Inc(m_nActive);
  end;
 // Allow other threads to attempt reading/writing
 LeaveCriticalSection(m_cs);
 if (fResourceWritePending) then
  begin
   // This thread must wait
   WaitForSingleObject(m_hsemReaders, INFINITE);
  end;
end;

procedure TSingleWriterMultipleReaderGuard.WaitToWrite;
var
 fResourceOwned : Boolean;
begin
 // Ensure exclusive access to the member variables
 EnterCriticalSection(m_cs);
 // Are there any threads accessing the resource?
 fResourceOwned := (m_nActive <> 0);
 if (fResourceOwned) then
  begin
    // This writer must wait, increment the count of waiting writers
    Inc(m_nWaitingWriters);
  end
 else
  begin
   // This writer can write, decrement the count of active writers
   m_nActive := -1;
  end;
 // Allow other threads to attempt reading/writing
 LeaveCriticalSection(m_cs);

 if (fResourceOwned) then
  begin
   // This thread must wait
   WaitForSingleObject(m_hsemWriters, INFINITE);
  end;
end;

end.

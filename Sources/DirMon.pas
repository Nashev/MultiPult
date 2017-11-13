unit DirMon;

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, SyncObjs;

type
  // Компонент сделан на основании примера, опубликованного 15.10.2013
  // участником Mawrat в форуме http://www.cyberforum.ru/post5183723.html

  {Тип записи с данными об изменении файла (или папки).
  http://msdn.microsoft.com/en-us/library/windows/desktop/aa364391%28v=vs.85%29.aspx}
  FILE_NOTIFY_INFORMATION = packed record
    //Смещение (в байтах) до следующей записи типа FILE_NOTIFY_INFORMATION.
    NextEntryOffset,
    //Код действия (изменения).
    Action,
    //Размер области в байтах, в которой записано имя файла.
    FileNameLength: Integer;
    //Первый символ в имени файла. Имя файла представлено массивом двухбайтных символов.
    FileName: WideChar;
  end;
  PFileNotifyInfo = ^FILE_NOTIFY_INFORMATION;

  TDirMonitorAction = (dmaAdded, dmaRemoved, dmaModified, dmaNewName, dmaOldName, dmaUnknown);
  TDirMonitorActions = set of TDirMonitorAction;

  TNotificationList = class(TStringList)
  private
    function GetAction(Index: Integer): TDirMonitorActions;
    procedure SetAction(Index: Integer; const Value: TDirMonitorActions);
  public
    property Actions[Index: Integer]: TDirMonitorActions read GetAction write SetAction;
    function AddAction(const AName: string; AActions: TDirMonitorActions): Integer;
  end;

  TDirMonitor = class(TThread)
  private
    //Путь к папке, которая будет наблюдаться.
    FPath: string;
    // защита FNotifications от чтения во время записи и наоборот
    FCriticalSection: TCriticalSection;
    {Прикладной буфер. В него будут записываться сведения об отслеживаемых
    изменениях в папках и файлах. Данные этого буфера будет использовать
    основной поток.}
    FNotifications: TNotificationList;
    //Код ошибки, которая может произойти при вызове API функций.
    FLastOSError: Integer;
    FOnNewNotification: TNotifyEvent;
    procedure CallEventHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string; AOnNewNotification, AOnTerminate: TNotifyEvent);
    destructor Destroy; override;
    // Путь к папке, которая будет наблюдаться.
    property Path: string read FPath;
    property LastOSError: Integer read FLastOSError;
    property Notifications: TNotificationList read FNotifications;
    // Notifications можно читать и чистить только из обработчика OnNewNotification, он под защитой FCriticalSection
    property OnNewNotification: TNotifyEvent read FOnNewNotification write FOnNewNotification;
  end;

implementation

{ TDirMonitor }

constructor TDirMonitor.Create(const APath: string; AOnNewNotification, AOnTerminate: TNotifyEvent);
begin
  inherited Create(True);
  FNotifications := TNotificationList.Create;
  FCriticalSection := TCriticalSection.Create;

  FPath := APath;
  FOnNewNotification := AOnNewNotification;
  OnTerminate := AOnTerminate;

  // FreeOnTerminate := True; //Самоуничтожение экземпляра потока при завершении.
end;

destructor TDirMonitor.Destroy;
begin
  FCriticalSection.Acquire;
  try
    FreeAndNil(FNotifications);
  finally
    FCriticalSection.Release;
  end;
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TDirMonitor.Execute;
const
  {Размер буефера в байтах. Это размер того буфера, в который система будет
  записывать сведения об отслеживаемых изменениях.
  Наибольший допустимый размер такого буефера = High(Word) = 65536 байт.}
  SizeBuff = High(Word);
var
  hDir, hEv: THandle;
  LocalThreadBuff: TBytes;
  SizeRet, ResWait: Cardinal;
  Ovlp: _OVERLAPPED;
  NotifyInfoAddr: PFileNotifyInfo;
  s: string;
  i: Integer;
  Action: TDirMonitorAction;
begin
  hEv := 0;
  //Получаем описатель папки.
  hDir := CreateFile(PChar(FPath), GENERIC_READ, FILE_SHARE_READ
    or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
  //Проверка описателя.
  if hDir = INVALID_HANDLE_VALUE then begin
    FLastOSError := GetLastError;
    Exit;
  end;
  try
    {Выделяем память для буфера. В этот буфер система будет записывать
    сведения об отслеживаемых изменениях в файлах и папках.}
    SetLength(LocalThreadBuff, SizeBuff);
    {Создаём сигнальный объект (объект-событие) с автоматическим сбросом,
    в бессигнальном (занятом) состоянии и получаем его описатель.}
    hEv := CreateEvent(nil, False, False, nil);
    if hEv = 0 then begin
      FLastOSError := GetLastError;
      Exit; //При этом управление перейдёт в раздел finally - end.
    end;
    //Записываем описатель объекта-события в структуру Ovlp.
    Ovlp.hEvent := hEv;
    //Наблюдение.
    while not Terminated do begin
      {Наблюдение за папкой.
      При первом вызове ReadDirectoryChangesW() система создаёт буфер в памяти,
      связывает его с файловым описателем hDir и запускает наблюдение. В этот
      буфер система записывает сведения об изменениях файлов и папок. Система
      будет продолжать наблюдение до момента, когда будет освобождён описатель
      папки - через вызов CloseHandle(hDir).
      При каждом вызове ReadDirectoryChangesW() (и при первом вызове - тоже)
      подаётся запрос на получение сведений об изменениях и на запись их в
      буфер Buff.
      Если произошли отслеживаемые изменения и система записала сведения о них
      в буфер Buff, то система записывает в структуру Ovlp сведения о переданных
      данных и устанавливает объект-событие Ovlp.hEvent в сигнальное состояние.}
      if not ReadDirectoryChangesW(hDir, LocalThreadBuff, SizeBuff, True,
        FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME
        or FILE_NOTIFY_CHANGE_ATTRIBUTES or FILE_NOTIFY_CHANGE_SIZE
        or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_LAST_ACCESS
        or FILE_NOTIFY_CHANGE_CREATION or FILE_NOTIFY_CHANGE_SECURITY,
        nil, @Ovlp, nil)
      then begin
        FLastOSError := GetLastError;
        Exit;
      end;
      {Здесь происходит следующее. На каждой итерации цикла выполняется вызов
      WaitForSingleObject(). Эта функция 500 миллисекунд ждёт момента, когда
      объект-событие Ovlp.hEvent перейдёт в сигнальное состояние. Если за это
      время объект-событие не перешёл в сигнальное состояние, то функция
      WaitForSingleObject() завершит свою работу и возвратит значение: WAIT_TIMEOUT.
      Если WaitForSingleObject() обнаружит, что объект-событие оказался
      в сигнальном состоянии, то WaitForSingleObject() завершит свою работу
      и возвратит значение: WAIT_OBJECT_0. В этом случае система автоматически
      переведёт объект-событие в несигнальное состояние (состояние ожидаения).
      В конце каждой итерации цикла выполняется проверка флага завершения потока
      Terminated и результат вызова функции WaitForSingleObject().
      Выход из цикла происходит, если Terminated = True - т. е., установлен флаг
      завершения потока. Или если ResWait <> WAIT_TIMEOUT - т. е., функция
      WaitForSingleObject() обнаружила, что объект-событие перешёл в сигнальное
      состояние или, если произошла ошибка.}
      repeat
        ResWait := WaitForSingleObject(hEv, 500);
      until Terminated or (ResWait <> WAIT_TIMEOUT);

      {Если объект-событие перешёл в сигнальное состояние. В этом случае нам надо
      забрать данные из буфера Buff.}
      if ResWait = WAIT_OBJECT_0 then begin
        {Опередяем размер данных, которые записаны в буфер Buff. Для этого
        вызываем GetOverlappedResult(). Размер будет записан в переменную SizeRet.
        Через четвертый параметр задаётся режим ожидания. True - ожидать до
        момента, когда операция передачи данных завершится. False - не ожидать.
        В нашем случае известно, что операция уже завершилась, поэтому четвёртый
        параметр устанавливаем в False.}
        if not GetOverlappedResult(hDir, Ovlp, SizeRet, False) then begin
          FLastOSError := GetLastError;
          Exit;
        end;
        {Забираем данные из буфера Buff и записываем их в прикладной буфер.
        Нам надо обеспечить режим, при котором доступ к прикладному буферу
        получает только один поток - основной или дополнительный. Пока буфер
        используется первым потоком, второй поток должен ожидать. Когда
        первый поток завершит работу с буфером, тогда доступ к буферу может
        получить второй поток. И теперь уже первый поток, если пожелает обратиться
        к буферу, должен будет ожидать, пока буфер используется вторым потоком.
        Для обеспечения такого режима применим мьютекс (MUTEX). Мьютекс создан
        в основном потоке и его описатель записан в поле TThr.FHMtxBuff.
        Ждём момента, когда мьютекс станет свободным и захватываем его.}
        NotifyInfoAddr := @LocalThreadBuff[0];
        FCriticalSection.Acquire;
        try
          repeat
            SetLength(s, NotifyInfoAddr^.FileNameLength div SizeOf(WideChar));
            CopyMemory(Pointer(@s[1]), @NotifyInfoAddr^.FileName, NotifyInfoAddr^.FileNameLength);

            case NotifyInfoAddr^.Action of
              FILE_ACTION_ADDED    : Action := dmaAdded;
              FILE_ACTION_REMOVED  : Action := dmaRemoved;
              FILE_ACTION_MODIFIED : Action := dmaModified;
              FILE_ACTION_RENAMED_OLD_NAME : Action := dmaNewName;
              FILE_ACTION_RENAMED_NEW_NAME : Action := dmaOldName;
            else
              Action := dmaUnknown;
            end;

            i := FNotifications.IndexOf(s);
            if i = -1 then
              FNotifications.AddAction(s, [Action])
            else
              FNotifications.Actions[i] := FNotifications.Actions[i] + [Action];

            if NotifyInfoAddr^.NextEntryOffset = 0 then
              Break
            else
              NotifyInfoAddr := Pointer(PByte(NotifyInfoAddr) + NotifyInfoAddr^.NextEntryOffset);
          until False;
        finally
          FCriticalSection.Release;
        end;
        Synchronize(CallEventHandler);
      //Если произошла ошибка.
      end else begin
        FLastOSError := GetLastError;
        Exit;
      end;
    end;
  finally
    CloseHandle(hEv);
    CloseHandle(hDir);
  end;
end;

procedure TDirMonitor.CallEventHandler;
begin
  if Assigned(OnNewNotification) then
    begin
      FCriticalSection.Acquire;
      try
        OnNewNotification(Self);
        Notifications.Clear;
      finally
        FCriticalSection.Release;
      end;
    end;
end;

{ TNotificationList }
type
  TFourBytes = array [0..3] of byte;

function TNotificationList.AddAction(const AName: string;
  AActions: TDirMonitorActions): Integer;
var
  i: Integer;
begin
  i := 0;
  TFourBytes(i)[0] := Byte(AActions);
  Result := AddObject(AName, Pointer(i));
end;

function TNotificationList.GetAction(Index: Integer): TDirMonitorActions;
begin
  Result := TDirMonitorActions(TFourBytes(Integer(Objects[Index]))[0]);
end;

procedure TNotificationList.SetAction(Index: Integer;
  const Value: TDirMonitorActions);
var
  i: Integer;
begin
  i := 0;
  TFourBytes(i)[0] := Byte(Value);
  Objects[Index] := Pointer(i);
end;

end.

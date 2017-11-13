unit DirMon;

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, SyncObjs;

type
  // ��������� ������ �� ��������� �������, ��������������� 15.10.2013
  // ���������� Mawrat � ������ http://www.cyberforum.ru/post5183723.html

  {��� ������ � ������� �� ��������� ����� (��� �����).
  http://msdn.microsoft.com/en-us/library/windows/desktop/aa364391%28v=vs.85%29.aspx}
  FILE_NOTIFY_INFORMATION = packed record
    //�������� (� ������) �� ��������� ������ ���� FILE_NOTIFY_INFORMATION.
    NextEntryOffset,
    //��� �������� (���������).
    Action,
    //������ ������� � ������, � ������� �������� ��� �����.
    FileNameLength: Integer;
    //������ ������ � ����� �����. ��� ����� ������������ �������� ����������� ��������.
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
    //���� � �����, ������� ����� �����������.
    FPath: string;
    // ������ FNotifications �� ������ �� ����� ������ � ��������
    FCriticalSection: TCriticalSection;
    {���������� �����. � ���� ����� ������������ �������� �� �������������
    ���������� � ������ � ������. ������ ����� ������ ����� ������������
    �������� �����.}
    FNotifications: TNotificationList;
    //��� ������, ������� ����� ��������� ��� ������ API �������.
    FLastOSError: Integer;
    FOnNewNotification: TNotifyEvent;
    procedure CallEventHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string; AOnNewNotification, AOnTerminate: TNotifyEvent);
    destructor Destroy; override;
    // ���� � �����, ������� ����� �����������.
    property Path: string read FPath;
    property LastOSError: Integer read FLastOSError;
    property Notifications: TNotificationList read FNotifications;
    // Notifications ����� ������ � ������� ������ �� ����������� OnNewNotification, �� ��� ������� FCriticalSection
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

  // FreeOnTerminate := True; //��������������� ���������� ������ ��� ����������.
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
  {������ ������� � ������. ��� ������ ���� ������, � ������� ������� �����
  ���������� �������� �� ������������� ����������.
  ���������� ���������� ������ ������ ������� = High(Word) = 65536 ����.}
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
  //�������� ��������� �����.
  hDir := CreateFile(PChar(FPath), GENERIC_READ, FILE_SHARE_READ
    or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
  //�������� ���������.
  if hDir = INVALID_HANDLE_VALUE then begin
    FLastOSError := GetLastError;
    Exit;
  end;
  try
    {�������� ������ ��� ������. � ���� ����� ������� ����� ����������
    �������� �� ������������� ���������� � ������ � ������.}
    SetLength(LocalThreadBuff, SizeBuff);
    {������ ���������� ������ (������-�������) � �������������� �������,
    � ������������� (�������) ��������� � �������� ��� ���������.}
    hEv := CreateEvent(nil, False, False, nil);
    if hEv = 0 then begin
      FLastOSError := GetLastError;
      Exit; //��� ���� ���������� ������� � ������ finally - end.
    end;
    //���������� ��������� �������-������� � ��������� Ovlp.
    Ovlp.hEvent := hEv;
    //����������.
    while not Terminated do begin
      {���������� �� ������.
      ��� ������ ������ ReadDirectoryChangesW() ������� ������ ����� � ������,
      ��������� ��� � �������� ���������� hDir � ��������� ����������. � ����
      ����� ������� ���������� �������� �� ���������� ������ � �����. �������
      ����� ���������� ���������� �� �������, ����� ����� ��������� ���������
      ����� - ����� ����� CloseHandle(hDir).
      ��� ������ ������ ReadDirectoryChangesW() (� ��� ������ ������ - ����)
      ������� ������ �� ��������� �������� �� ���������� � �� ������ �� �
      ����� Buff.
      ���� ��������� ������������� ��������� � ������� �������� �������� � ���
      � ����� Buff, �� ������� ���������� � ��������� Ovlp �������� � ����������
      ������ � ������������� ������-������� Ovlp.hEvent � ���������� ���������.}
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
      {����� ���������� ���������. �� ������ �������� ����� ����������� �����
      WaitForSingleObject(). ��� ������� 500 ����������� ��� �������, �����
      ������-������� Ovlp.hEvent ������� � ���������� ���������. ���� �� ���
      ����� ������-������� �� ������� � ���������� ���������, �� �������
      WaitForSingleObject() �������� ���� ������ � ��������� ��������: WAIT_TIMEOUT.
      ���� WaitForSingleObject() ���������, ��� ������-������� ��������
      � ���������� ���������, �� WaitForSingleObject() �������� ���� ������
      � ��������� ��������: WAIT_OBJECT_0. � ���� ������ ������� �������������
      �������� ������-������� � ������������ ��������� (��������� ���������).
      � ����� ������ �������� ����� ����������� �������� ����� ���������� ������
      Terminated � ��������� ������ ������� WaitForSingleObject().
      ����� �� ����� ����������, ���� Terminated = True - �. �., ���������� ����
      ���������� ������. ��� ���� ResWait <> WAIT_TIMEOUT - �. �., �������
      WaitForSingleObject() ����������, ��� ������-������� ������� � ����������
      ��������� ���, ���� ��������� ������.}
      repeat
        ResWait := WaitForSingleObject(hEv, 500);
      until Terminated or (ResWait <> WAIT_TIMEOUT);

      {���� ������-������� ������� � ���������� ���������. � ���� ������ ��� ����
      ������� ������ �� ������ Buff.}
      if ResWait = WAIT_OBJECT_0 then begin
        {��������� ������ ������, ������� �������� � ����� Buff. ��� �����
        �������� GetOverlappedResult(). ������ ����� ������� � ���������� SizeRet.
        ����� ��������� �������� ������� ����� ��������. True - ������� ��
        �������, ����� �������� �������� ������ ����������. False - �� �������.
        � ����� ������ ��������, ��� �������� ��� �����������, ������� ��������
        �������� ������������� � False.}
        if not GetOverlappedResult(hDir, Ovlp, SizeRet, False) then begin
          FLastOSError := GetLastError;
          Exit;
        end;
        {�������� ������ �� ������ Buff � ���������� �� � ���������� �����.
        ��� ���� ���������� �����, ��� ������� ������ � ����������� ������
        �������� ������ ���� ����� - �������� ��� ��������������. ���� �����
        ������������ ������ �������, ������ ����� ������ �������. �����
        ������ ����� �������� ������ � �������, ����� ������ � ������ �����
        �������� ������ �����. � ������ ��� ������ �����, ���� �������� ����������
        � ������, ������ ����� �������, ���� ����� ������������ ������ �������.
        ��� ����������� ������ ������ �������� ������� (MUTEX). ������� ������
        � �������� ������ � ��� ��������� ������� � ���� TThr.FHMtxBuff.
        ��� �������, ����� ������� ������ ��������� � ����������� ���.}
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
      //���� ��������� ������.
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

unit MainFormUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$IFDEF VER140} // Delphi 6
    {$DEFINE Delphi6}
  {$ELSE}
    {$DEFINE DelphiXE+}
  {$ENDIF}
{$ENDIF}

interface
{$WARN UNIT_PLATFORM OFF}
uses
{$IFNDEF FPC}
  jpeg,
{$ELSE}
  lclproc, fileutil, JwaWinBase, lMessages, FPReadJPEG,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ExtCtrls, ImgList, ExtDlgs, StdCtrls, Contnrs,
  Gauges, Buttons, Math, ComCtrls, FileCtrl, mmSystem,
  WaveUtils, WaveStorage, WaveOut, WavePlayers, WaveIO, WaveIn, WaveRecorders, WaveTimer,
  ToolWin, ExtActns, Vcl.StdActns, System.Actions, Vcl.AppEvnts,
  System.ImageList, Vcl.Imaging.pngimage, Vcl.Imaging.GIFimg{$IFDEF Delphi6}, Actions{$ENDIF};

const
  ControlActionStackDeep = 10;
type
  TControlAction = (caNone, caStepBackward, caStepForward, caPlayBackward, caPlayForward);
  TFrameTipMode = (ftmWorkingSet, ftmRecord);
const
  FrameTipW = 120;
  FrameTipH = 90;
  FrameTipD = 5;
  FrameTipT = 2;

type
  TFrameInfo = class
  private
    FPath, FFileName: string;
  public
    Preview: TBitmap;
    Teleport: Integer; // TODO: move to ResultFrame (?)
    Loaded: Boolean;
    function ImageFromDisc: TGraphic;
    function GenerateStubFrame(ErrorMessage: string): TGraphic;
    property FileName: string read FFileName;
    property Path: string read FPath;
    function FullFileName: string;
    function RelativeFileName: string;
    constructor Create(APath, AFileName: string);
    destructor Destroy; override;
  end;

  TRecordedFrameList = class;
  TRecordedFrame = class(TCollectionItem)
  private
    FFrameInfoIndex: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AFrameList: TRecordedFrameList; AFrameInfoIndex: Integer); reintroduce;
    property FrameInfoIndex: Integer read FFrameInfoIndex write FFrameInfoIndex;
    // TODO: FlipHorisontally etc
  end;

  TRecordedFrameList = class(TCollection)
  private
    function GetItems(AIndex: Integer): TRecordedFrame;
  protected
    // procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    property Items[AIndex: Integer]: TRecordedFrame read GetItems; default;
    function FindByFrameIndex(AFrameInfoIndex: Integer): TRecordedFrame;
    // TODO: Audio, AudioOffset
  end;

  // TODO: Snippets or clips or pieces
  // TODO: FrameSources
  // TODO: AudioSources

  TMainForm = class(TForm)
    ApplicationEvents: TApplicationEvents;
    pnlDisplay: TPanel;
    pbDisplay: TPaintBox;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    actOpen: TAction;
    actNew: TAction;
    actSaveAs: TAction;
    actSelectPhotoFolder: TAction;
    mmiSelectPhotoFolder: TMenuItem;
    mmiFiles: TMenuItem;
    mmiNew: TMenuItem;
    mmiOpen: TMenuItem;
    mmiSaveAs: TMenuItem;
    mmiNavigation: TMenuItem;
    actStepPrev: TAction;
    actStepNext: TAction;
    actToggleBookmark0: TAction;
    actNewBookmark: TAction;
    actRecord: TAction;
    actExport: TAction;
    pnlWorkingSet: TPanel;
    pbWorkingSet: TPaintBox;
    OpenPictureDialog: TOpenPictureDialog;
    mmiNext: TMenuItem;
    mmiPrev: TMenuItem;
    mmiSeparatorBookmarkManagement: TMenuItem;
    mmiNewBookmark: TMenuItem;
    mmiToggleBookmark0: TMenuItem;
    mmiExport: TMenuItem;
    mmiRecord: TMenuItem;
    actGotoBookmark0: TAction;
    mmiGotoBookmark0: TMenuItem;
    mmiSeparatorBookmarks: TMenuItem;
    MultimediaTimer: TMultimediaTimer;
    actPlayForward: TAction;
    mmiPlayingForward: TMenuItem;
    actExit: TAction;
    mmiExitSeparator: TMenuItem;
    mmiExit: TMenuItem;
    WaveStorage: TWaveStorage;
    AudioRecorder: TAudioRecorder;
    pbRecord: TPaintBox;
    mmiPlayBackward: TMenuItem;
    actPlayBackward: TAction;
    StockAudioPlayer: TStockAudioPlayer;
    pnlToolls: TPanel;
    LevelGauge: TGauge;
    actPlay: TAction;
    mmiSeparatorSteps: TMenuItem;
    mmiPlay: TMenuItem;
    dlgSaveMovie: TSaveDialog;
    dlgOpenMovie: TOpenDialog;
    mmiHelp: TMenuItem;
    mmiAbout: TMenuItem;
    pbIndicator: TPaintBox;
    actExportToAVI: TAction;
    mmiExportToAVI: TMenuItem;
    actForwardWhilePressed: TAction;
    mmiForwardWhilePressed: TMenuItem;
    actBackwardWhilePressed: TAction;
    mmiBackwardWhilePressed: TMenuItem;
    SaveToAVIDialog: TSaveDialog;
    RecordSplitter: TSplitter;
    mmiN1: TMenuItem;
    actToggleTeleport0: TAction;
    mmiToggleTeleport0: TMenuItem;
    mmiMode: TMenuItem;
    mmiDoubleFramerate: TMenuItem;
    LiveAudioRecorder: TLiveAudioRecorder;
    mmiPreviewMode: TMenuItem;
    ilActions: TImageList;
    tlbNavigation: TToolBar;
    btnPlayBackward: TToolButton;
    btnPlayForward: TToolButton;
    btnPlay: TToolButton;
    btnRecord: TToolButton;
    btnPrev: TToolButton;
    btnNext: TToolButton;
    btnBackwardWhilePressed: TToolButton;
    btnForwardWhilePressed : TToolButton;
    actPreviewMode: TAction;
    actDoubleFramerate: TAction;
    actAbout: TAction;
    actFullScreenMode: TAction;
    mmiFullScreenMode: TMenuItem;
    ilActionsDisabled: TImageList;
    actStretchImages: TAction;
    mmiStretchImages: TMenuItem;
    pbFrameTip: TPaintBox;
    mmiShowIssuesPage: TMenuItem;
    actShowIssuesPage: TBrowseURL;
    actShowControllerForm: TAction;
    mmiShowControllerForm: TMenuItem;
    actScreenWindow: TAction;
    mmiScreenWindow: TMenuItem;
    actShowMultiStudiaPage: TBrowseURL;
    mmiShowMultiStudiaPage: TMenuItem;
    imgBackgroundSource: TImage;
    StatusBar: TStatusBar;
    imgAdvertisement: TImage;
    imgAdvertisementThumbnail: TImage;
    imgLeftRightByMouseDownController: TImage;
    mmiExportResolution: TMenuItem;
    mmiExportResolution43: TMenuItem;
    mmiExportResolutionQVGA: TMenuItem;
    mmiExportResolutionVGA: TMenuItem;
    mmiExportResolutionCustom: TMenuItem;
    mmiExportResolutionFirstFrame: TMenuItem;
    actExportResolutionCustom: TAction;
    actExportResolutionFirstFrame: TAction;
    N1: TMenuItem;
    mmiExportSeparator: TMenuItem;
    mmiSelectAudioFile: TMenuItem;
    actSelectAudioFile: TAction;
    dlgOpenAudio: TOpenDialog;
    dlgSaveAudio: TSaveDialog;
    lblAudioFileName: TLabel;
    N2: TMenuItem;
    mmiUseMicrophone: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    mmiWorkingSetManagement: TMenuItem;
    mmiHideFrame: TMenuItem;
    mmiMoveFrameRight: TMenuItem;
    mmiMoveFrameLeft: TMenuItem;
    mmiOpenFrameFileInDefaultProgram: TMenuItem;
    mmiDuplicateFrame: TMenuItem;
    mmiShowFrameInExplorer: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    mmiShowCameraForm: TMenuItem;
    N19: TMenuItem;
    actMoveFrameLeft: TAction;
    actMoveFrameRight: TAction;
    actDuplicateFrame: TAction;
    actHideFrame: TAction;
    actShowFrameInExplorer: TAction;
    actOpenFrameFileInDefaultProgram: TAction;
    actWorkingSetManagement: TAction;
    actRefreshPreview: TAction;
    actOpenHelp: TBrowseURL;
    mmiRefreshPreview: TMenuItem;
    mmiOpenHelp: TMenuItem;
    actShowCameraForm: TAction;
    mmiStopRecordingOnSoundtrackFinish: TMenuItem;
    lblWorkPath: TLabel;
    procedure actSelectPhotoFolderClick(Sender: TObject);
    procedure actStepNextExecute(Sender: TObject);
    procedure actStepPrevExecute(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure pbWorkingSetPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure actPlayForwardExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actRecordExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure pbRecordPaint(Sender: TObject);
    procedure actPlayBackwardExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actToggleBookmarkExecute(Sender: TObject);
    procedure actGotoBookmark0Execute(Sender: TObject);
    procedure actPlayUpdate(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure AudioRecorderFilter(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal);
    procedure actOpenExecute(Sender: TObject);
    procedure actNavigate_Update(Sender: TObject);
    procedure actUpdate_HaveRecorded(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure pbIndicatorPaint(Sender: TObject);
    procedure actForwardWhilePressedExecute(Sender: TObject);
    procedure mmiBackwardWhilePressedClick(Sender: TObject);
    procedure actExportToAVIExecute(Sender: TObject);
    procedure actToggleTeleport0Execute(Sender: TObject);
    procedure pbIndicatorClick(Sender: TObject);
    procedure actDoubleFramerateExecute(Sender: TObject);
    procedure AudioRecorderActivate(Sender: TObject);
    procedure AudioRecorderDeactivate(Sender: TObject);
    procedure LiveAudioRecorderData(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal; var FreeIt: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StockAudioPlayerActivate(Sender: TObject);
    procedure StockAudioPlayerDeactivate(Sender: TObject);
    procedure pbRecordMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbRecordMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure actPreviewModeExecute(Sender: TObject);
    procedure actFullScreenModeExecute(Sender: TObject);
    procedure btnBackwardWhilePressedMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnBackwardWhilePressedMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnBackwardWhilePressedMouseLeave(Sender: TObject);
    procedure btnForwardWhilePressedMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnForwardWhilePressedMouseLeave(Sender: TObject);
    procedure btnForwardWhilePressedMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnNavigationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkingSetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkingSetMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbFrameTipPaint(Sender: TObject);
    procedure pbWorkingSetMouseLeave(Sender: TObject);
    procedure pbRecordMouseLeave(Sender: TObject);
    procedure actShowControllerFormExecute(Sender: TObject);
    procedure actShowControllerFormUpdate(Sender: TObject);
    procedure tlbNavigationMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actStretchImagesExecute(Sender: TObject);
    procedure actScreenWindowExecute(Sender: TObject);
    procedure pbDisplayDblClick(Sender: TObject);
    procedure mmiNewBookmarkClick(Sender: TObject);
    procedure pbDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgLeftRightByMouseDownControllerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgLeftRightByMouseDownControllerMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mmiExportResolutionClick(Sender: TObject);
    procedure actExportResolutionCustomExecute(Sender: TObject);
    procedure actExportResolutionFirstFrameExecute(Sender: TObject);
    procedure actExportResolutionFirstFrameUpdate(Sender: TObject);
    procedure actSelectAudioFileExecute(Sender: TObject);
    procedure mmiUseMicrophoneClick(Sender: TObject);
    procedure actHideFrameClick(Sender: TObject);
    procedure actDuplicateFrameClick(Sender: TObject);
    procedure actMoveFrameRightClick(Sender: TObject);
    procedure actMoveFrameLeftClick(Sender: TObject);
    procedure actHaveCurrentFrame(Sender: TObject);
    procedure actShowFrameInExplorerExecute(Sender: TObject);
    procedure actOpenFrameFileInDefaultProgramExecute(Sender: TObject);
    procedure actWorkingSetManagementExecute(Sender: TObject);
    procedure actRefreshPreviewExecute(Sender: TObject);
    procedure actHaveDisplayedFrame(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure actShowCameraFormExecute(Sender: TObject);
    procedure actShowCameraFormUpdate(Sender: TObject);
    procedure actSelectAudioFileUpdate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnBackwardWhilePressedClick(Sender: TObject);
    procedure pnlToollsResize(Sender: TObject);
  private
    NextControlActionStack: array [1..ControlActionStackDeep] of TControlAction;
    NextControlActionStackPosition: Integer;
    function NextControlAction: TControlAction;
    procedure PopControlAction;
    procedure PushControlAction(Value: TControlAction);
    procedure ReplaceControlActions(Value: TControlAction);
    procedure ClearSound;
    function FrameIndexToTimeStamp(AFrameIndex: Integer): string;
    procedure Stop;
  private
    VersionNameString: string; // '0.9.34' - задаётся в ProjectOptions
    VersionCopyrightString: string; // '2017' - тоже в ProjectOptions
    ProjectFileName: string;
    FFrameInfoList: TObjectList;
    FWorkingSetFrames: TRecordedFrameList;
    FRecordedFrames: TRecordedFrameList;
    KeyPressBlocked: Boolean;
    Interval, CurrentSpeedInterval: Integer;
    Recording, RecordingMustBeChanged, Playing, Exporting: Boolean;
    LoopMode: Boolean;
    FrameTipMode: TFrameTipMode;
    FrameTipArrow: Integer;
    const
      BookmarkKey: array [0..19] of Char = (
        '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
        'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
      );
  private
    Bookmarks: array [0..19] of Integer; // TODO: move to ResultFrameList (?)
    Saved: Boolean;
     // RecordedAudioCopy - рисуемая копия звука. По мере записи мульта пополняется из AudioRecorder
     // копиями очередных порций записываемого им себе звука.
     // При открытии старой записи инициализируется копией загруженного в WaveStorage звука.
     // По завершении записи в WaveStorage копируется звук, записанный
     // в AudioRecorder-е и так WaveStorage догоняет RecordedAudioCopy.
     // При воспроизведении проигрывается WaveStorage при помощи StockAudioPlayer
     // Паралельно всему этому всё время работает LiveAudioRecorder
     // на отображение уровня звука в статусе.
    RecordedAudioCopy: TMemoryStream;
    pbRecordOffset: Integer;
    OutOfMemoryRaised: Boolean;
    PreviousBounds: TRect;
    FFrameTipRecordedFrame: TRecordedFrame;
    FCurrentRecordPosition: Integer;
    FCurrentWorkingSetFrame: TRecordedFrame;
    AdvertisementFrameBottom: Integer;
    AdvertisementFrameImage: TBitmap;
    AdvertisementFrameTipShowing: Boolean;
    ExportSize: TSize;
    FirstFrameSize: TSize;
    FExternalAudioFileName: string;
    FDisplayedFrameIndex: Integer;
    procedure CaptureFirstFrameSizes;
    procedure SetCurrentRecordPosition(const Value: Integer);
    procedure SetCurrentWorkingSetFrame(const Value: TRecordedFrame);
    procedure UpdatePlayActions;
    procedure StopRecording;
    procedure StopPlaying;
    procedure SetDisplayedFrameIndex(Value: Integer);
    procedure InitMicrophoneUsage(AKeepOpenedWave: Boolean);
    procedure RepaintAll;
    procedure ClearBookmarks;
    procedure SaveBeforeClose(const APurpose: string);
    function CalculateSoundFramesCount: Integer;
    property CurrentRecordPosition: Integer read FCurrentRecordPosition write SetCurrentRecordPosition;
    procedure SetFrameTipRecordedFrame(const Value: TRecordedFrame);
    function TeleportEnabled: Boolean;
    procedure OpenMovie(AFileName: string);
    procedure OpenAudio(AFileName: string);
    property FrameTipRecordedFrame: TRecordedFrame read FFrameTipRecordedFrame write SetFrameTipRecordedFrame;
    function GetFrameInfo(Index: Integer): TFrameInfo;
    function GetFrameInfoCount: Integer;
    procedure UnloadFrames;
    procedure ClearRecorded;
    function WorkingFrameToWorkingSetX(AWorkingSetFrame: TRecordedFrame): Integer;
    function WorkingSetXToWorkingFrame(X: Integer): TRecordedFrame;
//    Drawing: Boolean;
    procedure LoadPhotoFolder(ANewPhotoFolder: string);
    procedure RecalculatePreview;
    procedure CreateAdvertisementFrame;
    procedure ShowTimes;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
  public
    PhotoFolder: string;
    AdvertisementFrameImagePreview: TBitmap;
    AdvertisementShowing: Boolean;
    AdvertisementDuration: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
    procedure SetCaption(const Value: TCaption);
    procedure SetStatus(const Value: string);
    property DisplayedFrameIndex: Integer read FDisplayedFrameIndex write SetDisplayedFrameIndex;
    property CurrentWorkingSetFrame: TRecordedFrame read FCurrentWorkingSetFrame write SetCurrentWorkingSetFrame;
    function FindWorkingSetFrameByOffset(AOffset: Integer): TRecordedFrame;
    property WorkingSetFrames: TRecordedFrameList read FWorkingSetFrames;
    property RecordedFrames: TRecordedFrameList read FRecordedFrames;
    property FrameInfoCount: Integer read GetFrameInfoCount;
    property FrameInfoList[Index: Integer]: TFrameInfo read GetFrameInfo;
    // Used in ScreenForm
    procedure LoadPhoto(AFrameInfoIndex: Integer);
    procedure AddNewFrame(AFileName: string);
  end;

var
  MainForm: TMainForm;

var
  FrameRate: byte = 25;
function StretchSize(AWidth, AHeight, ABoundsWidth, ABoundsHeight: Integer): TRect;

implementation
uses AVICompression, ControllerFormUnit, ScreenFormUnit,
  ExportSizeCustomRequestDialogUnit, ShellAPI, WorkingSetManagementFormUnit,
  CameraFormUnit, MP3ConvertFormUnit;
{$R *.dfm}

function Size(AX, AY: Integer): TSize;
begin
  Result.cx := AX;
  Result.cy := AY;
end;

function StretchSize(AWidth, AHeight, ABoundsWidth, ABoundsHeight: Integer): TRect;
begin
  Result.Left   := 0;
  Result.Top    := 0;
  Result.Right  := MulDiv(AWidth, ABoundsHeight, AHeight);
  Result.Bottom := MulDiv(AHeight, ABoundsWidth, AWidth);
  if Result.Right > ABoundsWidth then
    begin
      Result.Right := ABoundsWidth;
      Result.Top := (ABoundsHeight - Result.Bottom) div 2;
      Result.Bottom := Result.Bottom + Result.Top;
    end;
  if Result.Bottom > ABoundsHeight then
    begin
      Result.Bottom := ABoundsHeight;
      Result.Left := (ABoundsWidth - Result.Right) div 2;
      Result.Right := Result.Right + Result.Left;
    end;
end;

procedure TMainForm.actSelectAudioFileExecute(Sender: TObject);
resourcestring
  rs_SaveAudioBeforeOpenRequest = 'Хотите сохранить записанную озвучку перед подключением готовой?';
begin
  Stop;
  if (WaveStorage.Wave.Length > 0) and (FExternalAudioFileName = '') then
    case MessageBox(
      0,
      PChar(rs_SaveAudioBeforeOpenRequest),
      PChar(Application.Title),
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    of
      IDCANCEL: Exit;
      IDYES:
        begin
          if not dlgSaveAudio.Execute then
            Exit;
          WaveStorage.Wave.SaveToFile(dlgSaveAudio.FileName);
        end;
      IDNO: ;
    end;

  if not dlgOpenAudio.Execute then
    Abort;
  OpenAudio(dlgOpenAudio.FileName);
end;

procedure TMainForm.SaveBeforeClose(const APurpose: string);
resourcestring
  rs_SaveMovieBeforeSomethingRequest = 'Хотите сохранить текущий мульт перед ';
begin
  if (RecordedFrames.Count > 0) and not Saved then
    case MessageBox(
      0,
      PChar(rs_SaveMovieBeforeSomethingRequest + APurpose + '?'),
      PChar(Application.Title),
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    of
      IDCANCEL: Abort;
      IDYES:
        begin
          actSaveAs.Execute;
          if not Saved then
            Abort;
        end;
      IDNO: ;
    end;
end;

procedure TMainForm.actSelectPhotoFolderClick(Sender: TObject);
var
  NewPhotoFolder: string;
resourcestring
  rs_SelectPhotoFolderCaption = 'С какой папки начинать искать кадры?';
  rs_SaveBeforeChooseFolderRequest = 'созданием нового';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeChooseFolderRequest);

  NewPhotoFolder := ExtractFilePath(ExpandFileName('..\testdata\'));
  if SelectDirectory(
    rs_SelectPhotoFolderCaption, '', NewPhotoFolder
    {$IFDEF DelphiXE}
    , [sdNewFolder, sdShowFiles, sdShowEdit, (*sdShowShares, *) sdValidateDir, sdNewUI]
    {$ENDIF}
  ) then
    LoadPhotoFolder(NewPhotoFolder + '\');
end;

procedure TMainForm.actShowCameraFormExecute(Sender: TObject);
resourcestring
  CamFolder = 'FromCam\';
begin
  Stop;
  CameraForm.Execute(PhotoFolder + CamFolder, AddNewFrame);
end;

procedure TMainForm.actShowCameraFormUpdate(Sender: TObject);
begin
  actShowCameraForm.Enabled := PhotoFolder <> '';
end;

procedure TMainForm.actShowControllerFormExecute(Sender: TObject);
begin
  ControllerForm.Visible := not ControllerForm.Visible;
end;

procedure TMainForm.actShowControllerFormUpdate(Sender: TObject);
begin
  actShowControllerForm.Checked := ControllerForm.Visible;
end;

procedure InfoMsg(AText: string);
begin
  MessageBox(
    MainForm.Handle,
    PChar(AText),
    PChar(Application.Title),
    MB_OK + MB_ICONINFORMATION + MB_SETFOREGROUND
  );
end;

procedure SafeShellExecute(hWnd: HWND; const Operation, FileName, Parameters, Directory: string; ShowCmd: Integer = SW_NORMAL);
var
  res: DWORD;
begin
  res := ShellExecute(hWnd, PChar(Operation), PChar(FileName), PChar(Parameters), PChar(Directory), ShowCmd);
  if Res <= 32 then
    case Res of
      0: InfoMsg('The operating system is out of memory or resources ('+FileName+').');
      ERROR_FILE_NOT_FOUND: InfoMsg('The specified file "'+FileName+'" was not found.');
      ERROR_PATH_NOT_FOUND: InfoMsg('The specified path was not found. ('+FileName+')');
      ERROR_BAD_FORMAT: InfoMsg('The .EXE ('+FileName+') file is invalid (non-Win32 .EXE or error in .EXE image).');
      SE_ERR_ACCESSDENIED: InfoMsg('The operating system denied access to the specified file ('+FileName+').');
      SE_ERR_ASSOCINCOMPLETE: InfoMsg('The FileName association is incomplete or invalid ('+FileName+').');
      SE_ERR_DDEBUSY: InfoMsg('The DDE transaction could not be completed because other DDE transactions were being processed.');
      SE_ERR_DDEFAIL: InfoMsg('The DDE transaction failed ('+FileName+').');
      SE_ERR_DDETIMEOUT: InfoMsg('The DDE transaction could not be completed because the request timed out ('+FileName+').');
      SE_ERR_DLLNOTFOUND: InfoMsg('The specified dynamic-link library was not found ('+FileName+').');
      SE_ERR_NOASSOC: InfoMsg('There is no application associated with the given FileName extension ('+FileName+').');
      SE_ERR_OOM: InfoMsg('There was not enough memory to complete the operation ('+FileName+').');
      SE_ERR_SHARE: InfoMsg('A sharing violation occurred ('+FileName+').');
    else
      InfoMsg('Unknown error ('+FileName+')');
    end;
end;

procedure SafeDeleteFile(const FileName: string);
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
end;

procedure SafeLocateFile(const FileName: string);
begin
  if FileExists(FileName) then
    SafeShellExecute(HWND(nil), 'open', 'explorer', '/select,"' + FileName + '"', '', SW_SHOW)
  else
    InfoMsg('Файл "' + FileName + '" не существует.');
end;

procedure TMainForm.actShowFrameInExplorerExecute(Sender: TObject);
begin
  Stop;
  SafeLocateFile(FrameInfoList[DisplayedFrameIndex].FullFileName);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CurrentSpeedInterval := 3;
  AdvertisementDuration := 2000; // msec
  Saved := True;
  FFrameInfoList := TObjectList.Create(True);
  FRecordedFrames := TRecordedFrameList.Create;
  FWorkingSetFrames := TRecordedFrameList.Create;
  RecordedAudioCopy := TMemoryStream.Create;
  DisplayedFrameIndex := -1;
  ClearBookmarks;
  MultimediaTimer.Enabled := True;
end;

procedure TMainForm.ClearBookmarks;
var
  i: Integer;
begin
  for i := Low(Bookmarks) to High(Bookmarks) do
    Bookmarks[i] := -1;
end;

procedure TMainForm.CreateAdvertisementFrame;

resourcestring
  rs_AdFrame1 = 'Фильм собран из отдельных кадров и озвучен';
  rs_AdFrame2 = 'при помощи общедоступной программы МультиПульт';
  rs_AdFrame3 = 'версии %s (%s)';
  rs_AdFrame4 = 'http://MultiStudia.ru';

var
  TextTop: Integer;
  Image: TGraphic;

  procedure DrawCentredText(ABitmap: TBitmap; AText: string);
  var
    TextSize: TSize;
    TextLeft: Integer;
  begin
    TextSize := ABitmap.Canvas.TextExtent(AText);
    TextLeft := (ABitmap.Width - TextSize.cx) div 2;
    ABitmap.Canvas.TextOut(TextLeft, TextTop, AText);
    Inc(TextTop, Round(TextSize.cy * 1.5));
  end;

begin
  if Assigned(AdvertisementFrameImage) then
    Exit;

  if FrameInfoCount = 0 then
    Exit;

  Image := FrameInfoList[0].ImageFromDisc;

  AdvertisementFrameImage := TBitmap.Create;
  with AdvertisementFrameImage do
    begin
      {$IFDEF DelphiXE+}
      SetSize(Image.Width, Image.Height);
      {$ELSE}
      Width := Image.Width;
      Height := Image.Height;
      {$ENDIF}
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Height := Height div 20;
      TextTop := Canvas.Font.Height * 2;
      DrawCentredText(AdvertisementFrameImage, rs_AdFrame1);
      DrawCentredText(AdvertisementFrameImage, rs_AdFrame2);
      DrawCentredText(AdvertisementFrameImage, Format(rs_AdFrame3, [VersionNameString, VersionCopyrightString]));
      DrawCentredText(AdvertisementFrameImage, rs_AdFrame4);
      DrawCentredText(AdvertisementFrameImage, DateToStr(Now));

      Canvas.Draw(
        TextTop + (Height - TextTop - imgAdvertisement.Height) div 2,
        (Width - imgAdvertisement.Width) div 2,
        imgAdvertisement.Picture.Graphic
      );
    end;

  FreeAndNil(Image);

  AdvertisementFrameImagePreview := TBitmap.Create;
  if actPreviewMode.Checked then
    with AdvertisementFrameImagePreview do
      begin
        {$IFDEF DelphiXE+}
        SetSize(640,480);
        {$ELSE}
        Width := 640;
        Height := 480;
        {$ENDIF}
        Canvas.StretchDraw(Rect(0, 0, Width, Height), AdvertisementFrameImage);
      end
  else
    AdvertisementFrameImagePreview.Assign(AdvertisementFrameImage);
end;

procedure TMainForm.actHideFrameClick(Sender: TObject);
var
  OldCurrentFrame: TRecordedFrame;
begin
  Stop;
  OldCurrentFrame := CurrentWorkingSetFrame;
  CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(1);
  if OldCurrentFrame = CurrentWorkingSetFrame then // last frame
    CurrentWorkingSetFrame := nil;
  OldCurrentFrame.Free;
end;

procedure TMainForm.actMoveFrameLeftClick(Sender: TObject);
begin
  Stop;
  if CurrentWorkingSetFrame.Index > 0 then
    CurrentWorkingSetFrame.Index := CurrentWorkingSetFrame.Index - 1
  else
    CurrentWorkingSetFrame.Index := WorkingSetFrames.Count - 1;
  RepaintAll;
end;

procedure TMainForm.actMoveFrameRightClick(Sender: TObject);
begin
  Stop;
  if CurrentWorkingSetFrame.Index < (WorkingSetFrames.Count - 1) then
    CurrentWorkingSetFrame.Index := CurrentWorkingSetFrame.Index + 1
  else
    CurrentWorkingSetFrame.Index := 0;
  RepaintAll;
end;

function TMainForm.NextControlAction: TControlAction;
begin
  if NextControlActionStackPosition >= 1 then
    Result := NextControlActionStack[NextControlActionStackPosition]
  else
    Result := caNone;
end;

destructor TMainForm.Destroy;
begin
  MultimediaTimer.Enabled := False;
  FreeAndNil(RecordedAudioCopy);
  FreeAndNil(FWorkingSetFrames);
  FreeAndNil(FRecordedFrames);
  FreeAndNil(FFrameInfoList);
  inherited Destroy;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  rs_SaveBeforeExit =
    'Вы закрываете программу, в то время как записанный Вами мультик ещё не сохранён.'#13#10+
    'Если его не сохранить сейчас, то он пропадёт.'#13#10+
    'Желаете его сохранить, прежде чем закрыть программу?';
begin
  Stop;

  actSaveAs.Update;
  if actSaveAs.Enabled and not Saved then
    case MessageBox(
      0,
      PChar(rs_SaveBeforeExit),
      PChar(Application.Title),
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1
    ) of
      IDYES: begin actSaveAs.Execute; CanClose := Saved; end;
      IDNO: CanClose := True;
      IDCANCEL: CanClose := False;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;

  procedure AppendBookmarkMenu(Index: Integer);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Action := actToggleBookmark0;
    MenuItem.Tag := Index;
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', BookmarkKey[Index], []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut('Alt+' + BookmarkKey[Index]);
    mmiToggleBookmark0.Parent.Insert(mmiToggleBookmark0.Parent.IndexOf(mmiToggleBookmark0), MenuItem);
    MenuItem.Action := nil;

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Action := actGotoBookmark0;
    MenuItem.Tag := Index;
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', BookmarkKey[Index], []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut(BookmarkKey[Index]);
    mmiGotoBookmark0.Parent.Insert(mmiGotoBookmark0.Parent.IndexOf(mmiGotoBookmark0), MenuItem);
    MenuItem.Action := nil;

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Action := actToggleTeleport0;
    MenuItem.Tag := Index;
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', BookmarkKey[Index], []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut('Shift+' + BookmarkKey[Index]);
    mmiToggleTeleport0.Parent.Insert(mmiToggleTeleport0.Parent.IndexOf(mmiToggleTeleport0), MenuItem);
    MenuItem.Action := nil;
  end;

  procedure TakeVersionInfo;

    function AppFileName: string;
    var
      FileName: array [0 .. 255] of Char;
    begin
      if IsLibrary then
        begin
          GetModuleFileName(HInstance, FileName, SizeOf(FileName) - 1);
          Result := StrPas(FileName);
        end
      else
        Result := ParamStr(0);
    end;

  type
    TLongVersion = record
      case Integer of
      0: (All: array[1..4 ] of Word);
      1: (MS, LS: LongInt);
    end;
  var
    FileName: string;
    VersionInfoHandle: DWORD;
    VersionInfoSize: DWORD;
    VersionInfoBuffer: PByte;
    Len: UINT;
    FixedFileInfo: PVSFixedFileInfo;
    V: TLongVersion;
    Translation: Pointer;
    TranslationString: string;
    Copyright: Pointer;
  begin
    // на случай, если что-то пойдёт не так
    VersionNameString := '0.9.???';
    VersionCopyrightString := 'МультиСтудия, Москва, 20??';
    VersionInfoBuffer := nil;
    // пробуем получить от файла:
    FileName := AppFileName;
    VersionInfoSize := GetFileVersionInfoSize(PWideChar(FileName), VersionInfoHandle);
    if VersionInfoSize > 0 then
      try
        GetMem(VersionInfoBuffer, VersionInfoSize);
        if not GetFileVersionInfo(PWideChar(FileName), VersionInfoHandle, VersionInfoSize, VersionInfoBuffer) then
          Exit;
        VerQueryValue(VersionInfoBuffer, '\', Pointer(FixedFileInfo), Len);
        V.MS := FixedFileInfo^.dwFileVersionMS;
        V.LS := FixedFileInfo^.dwFileVersionLS;
        with V do
          VersionNameString := Format('%d.%d.%d', [All[2], All[1], All[4]]);

        if VerQueryValue(VersionInfoBuffer, '\VarFileInfo\Translation', Translation, Len) then
          begin
            TranslationString := IntToHex(MakeLong(HiWord(LongInt(Translation^)), LoWord(LongInt(Translation^))), 8);
            if VerQueryValue(VersionInfoBuffer, PWideChar('\StringFileInfo\' + TranslationString + '\LegalCopyright'), Copyright, Len) then
              VersionCopyrightString := StrPas(PChar(Copyright));
          end;
      finally
        FreeMem(VersionInfoBuffer, VersionInfoSize);
      end;
  end;

begin
  TakeVersionInfo;
  for i := Low(Bookmarks) to High(Bookmarks) do
    AppendBookmarkMenu(i);

  mmiToggleBookmark0.Free;
  mmiGotoBookmark0.Free;
  mmiToggleTeleport0.Free;

  LiveAudioRecorder.Active := True;

  pnlDisplay.DoubleBuffered := True;
  pnlWorkingSet.DoubleBuffered := True;
  pnlToolls.DoubleBuffered := True;

  FrameTipRecordedFrame := nil;

  {$IFDEF DelphiXE+}
  pnlDisplay.ParentBackground := False;
  pnlWorkingSet.ParentBackground := False;
  pnlToolls.ParentBackground := False;
  {$ENDIF}

  with btnBackwardWhilePressed do ControlStyle := ControlStyle - [csCaptureMouse];
  with btnForwardWhilePressed  do ControlStyle := ControlStyle - [csCaptureMouse];
  with btnPrev         do ControlStyle := ControlStyle - [csClickEvents];
  with btnNext         do ControlStyle := ControlStyle - [csClickEvents];
  with btnPlayBackward do ControlStyle := ControlStyle - [csClickEvents];
  with btnPlayForward  do ControlStyle := ControlStyle - [csClickEvents];
//  DoubleBuffered := True;
  if ParamCount >= 1 then
    if DirectoryExists(ParamStr(1)) then
      LoadPhotoFolder(ParamStr(1))
    else
      OpenMovie(ParamStr(1));

  // инициализируем разрешение при экспорте по умолчанию.
  mmiExportResolutionVGA.Click;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  LiveAudioRecorder.Active := False;
  LiveAudioRecorder.WaitForStop;
end;

procedure TMainForm.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.DestroyWindowHandle;
begin
  DragAcceptFiles(Handle, False);
  inherited DestroyWindowHandle;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  i: Integer;
  FileCount: Integer;
  FileName: array [0..MAX_PATH] of Char;
resourcestring
  rs_SaveBeforeChooseFolderRequest = 'созданием нового в другой папке кадров';
  rs_SaveBeforeOpenRequest = 'открытием другого';
begin
  Stop;

  FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0); // Get count of files
  for i := 0 to FileCount-1 do
    begin
      DragQueryFile(Msg.Drop, i, @FileName, SizeOf(FileName)); // Get N file
      if DirectoryExists(FileName) then
        begin
          SaveBeforeClose(rs_SaveBeforeChooseFolderRequest);
          LoadPhotoFolder(string(FileName) + '\')
        end
      else if LowerCase(ExtractFileExt(FileName)) = '.mp' then
        begin
          SaveBeforeClose(rs_SaveBeforeOpenRequest);
          OpenMovie(FileName)
        end
      else
        Beep;
    end;
  DragFinish(Msg.Drop); // end drag handling, release drag source window
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      vk_Left:
        if NextControlAction <> caPlayBackward then
          ReplaceControlActions(caPlayBackward)
        else
          ReplaceControlActions(caNone);
      vk_Right:
        if NextControlAction <> caPlayForward then
          ReplaceControlActions(caPlayForward)
        else
          ReplaceControlActions(caNone);
      vk_Up:   begin Inc(CurrentSpeedInterval); pbWorkingSet.Repaint; end;
      vk_Down: begin Dec(CurrentSpeedInterval); if CurrentSpeedInterval < 1  then CurrentSpeedInterval := 1; pbWorkingSet.Repaint; end;
    end
  else
    if not KeyPressBlocked then
      case Key of
        vk_Left:   begin PushControlAction(caStepBackward); KeyPressBlocked := True; end;
        vk_Right:  begin PushControlAction(caStepForward); KeyPressBlocked := True; end;
        VK_ESCAPE: if actFullScreenMode.Checked then actFullScreenMode.Execute; // TODO: что ещё тут стоит прерывать эскейпом?
        vk_Shift:  pbWorkingSet.Invalidate; // телепорты выключаются
      end;

  UpdatePlayActions;
  pbIndicator.Repaint;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressBlocked := False; // TODO: разблокировать не любую клавишу, а ту, что блокировали. Чтоб не разблокировать, например, шаг вправо при отпускании чего-то другого.
  case Key of
    vk_Shift: pbWorkingSet.Invalidate; // телепорты включаются
  end;
//  pbIndicator.Refresh;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Playing then
    Exit;

  if WheelDelta > 0 then
    actStepPrev.Execute
  else
    actStepNext.Execute;
  Handled := True;
end;

function TMainForm.GetFrameInfo(Index: Integer): TFrameInfo;
begin
  Result := TFrameInfo(FFrameInfoList[Index]);
end;

function TMainForm.GetFrameInfoCount: Integer;
begin
  Result := FFrameInfoList.Count;
end;

procedure TMainForm.imgLeftRightByMouseDownControllerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if X < 40 then
    if ssLeft in Shift then
      actStepPrev.Execute
    else
      actStepNext.Execute
  else if X < 104 then
    begin
      ReplaceControlActions(caNone);
      UpdatePlayActions;
      if ssLeft in Shift then
        actBackwardWhilePressed.Checked := True
      else
        actForwardWhilePressed.Checked := True
    end
  else
    if ssLeft in Shift then
      actPlayBackward.Execute
    else
      actPlayForward.Execute;
end;

procedure TMainForm.imgLeftRightByMouseDownControllerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actBackwardWhilePressed.Checked := False;
  actForwardWhilePressed.Checked := False;
end;

procedure TMainForm.UnloadFrames;
begin
  ClearRecorded;
  ClearSound;

  DisplayedFrameIndex := -1;
  FCurrentWorkingSetFrame := nil;
  FFrameTipRecordedFrame := nil;
  WorkingSetFrames.Clear;
  FFrameInfoList.Clear;
  OutOfMemoryRaised := False;
end;

function CompareFramesFileName(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(
    TFrameInfo(Item1).RelativeFileName,
    TFrameInfo(Item2).RelativeFileName
  )
end;

procedure TMainForm.LoadPhotoFolder(ANewPhotoFolder: string);

resourcestring
  rs_ScaningStatus = 'Чтение папки: ';

  procedure InternalLoadDirectory(ARelativePath: string);
  var
    Rec: TSearchRec;
    ext: string;
  begin
    SetStatus(rs_ScaningStatus + PhotoFolder + ARelativePath);
    if {$IFDEF FPC}FindFirstUTF8{$ELSE}FindFirst{$ENDIF}(PhotoFolder + ARelativePath + '*.*', faAnyFile, Rec) = 0 then
      begin
        repeat
          ext := AnsiLowerCase(ExtractFileExt(Rec.Name));
          if (ext = '.jpg') or
             (ext = '.jpeg') or
             (ext = '.bmp') or
             (ext = '.png') or
             (ext = '.gif') or
             (ext = '.wmf') or
             (ext = '.emf')
          then
            begin
              FFrameInfoList.Add(TFrameInfo.Create(ARelativePath, Rec.Name));
            end;
          if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and (Rec.Name <> '..') then
            InternalLoadDirectory(ARelativePath + Rec.Name + '\');
        until {$IFDEF FPC}FindNextUTF8{$ELSE}FindNext{$ENDIF}(Rec) <> 0;
        {$IFDEF FPC}FindCloseUTF8{$ELSE}FindClose{$ENDIF}(Rec);
      end;
  end;
var
  i: Integer;
//  i: Integer;
begin
  actNew.Execute;
  ClearBookmarks;
  PhotoFolder := ANewPhotoFolder;
  if PhotoFolder[Length(PhotoFolder)] <> '\' then
    PhotoFolder := PhotoFolder + '\';

  lblWorkPath.Caption := PhotoFolder;

  SetCaption('');

  UnloadFrames;

  InternalLoadDirectory('');

  FFrameInfoList.Sort(CompareFramesFileName);
  for i := 0 to FFrameInfoList.Count - 1 do
    TRecordedFrame.Create(WorkingSetFrames, i);

  DisplayedFrameIndex := 0; // same as WorkingSetFrames[0].FFrameInfoIndex;
  Saved := True;
  CaptureFirstFrameSizes;

  UpdateActions;
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
resourcestring
  rs_AboutText =
    'Версия %s'#13#10 +
    'Автор: Илья Ненашев (http://innenashev.narod.ru)'#13#10 +
    'по заказу МультиСтудии (http://multistudia.ru)'#13#10 +
    'в лице Евгения Генриховича Кабакова'#13#10 +
    ''#13#10 +
    'Исходный код программы доступен для просмотра и доработок'#13#10 +
    'по адресу https://github.com/Nashev/MultiPult'#13#10 +
    ''#13#10 +
    '(А Вы знаете, что Ctrl+C в подобных окошках работает?)';
begin
  InfoMsg(
    Application.Title + #13#10 +
    Format(rs_AboutText, [VersionNameString])
  );
end;


procedure TMainForm.mmiBackwardWhilePressedClick(Sender: TObject);
begin
 //
end;

procedure TMainForm.mmiNewBookmarkClick(Sender: TObject);
var
  i: Integer;
begin
  // сначала попробуем снять закладку, если она тут уже есть. Пусть Enter тоже выключателем работает
  for i := Low(Bookmarks) to High(Bookmarks) do
    if Bookmarks[i] = DisplayedFrameIndex then
      begin
        Bookmarks[i] := -1;
        pbWorkingSet.Invalidate;
        Saved := False;
        Exit;
      end;

  // если не было закладки - то ставим первую свободную
  for i := Low(Bookmarks) to High(Bookmarks) do
    if Bookmarks[i] = -1 then
      begin
        Bookmarks[i] := DisplayedFrameIndex;
        pbWorkingSet.Invalidate;
        Saved := False;
        Exit;
      end;

  Beep; // если свободных закладок не нашлось - гудим.
end;

procedure TMainForm.mmiUseMicrophoneClick(Sender: TObject);
begin
  actNew.Execute;
  InitMicrophoneUsage(False);
end;

procedure TMainForm.InitMicrophoneUsage(AKeepOpenedWave: Boolean);
begin
  FExternalAudioFileName := '';
  actSelectAudioFile.Checked := False;
  mmiUseMicrophone.Checked := True;
  if not AKeepOpenedWave then
    begin
      WaveStorage.Wave.Clear;
      RecordedAudioCopy.Clear;
    end;
  LiveAudioRecorder.Active := True;
  lblAudioFileName.Visible := False;
  pbRecord.Invalidate;
  ShowTimes;
end;

procedure TMainForm.btnBackwardWhilePressedClick(Sender: TObject);
begin
  ReplaceControlActions(caNone);
  UpdatePlayActions;
end;

procedure TMainForm.actDoubleFramerateExecute(Sender: TObject);
resourcestring
  rs_ConfirmNewBeforeChangeFramerate =
    'Вы меняете базовую частоту кадров. Наиграть запись придётся с чистого листа.'#13#10+
    'Поменять базовую частоту кадров и начать новый мульт?';
begin
  Stop;
  actSaveAs.Update;
  if actSaveAs.Enabled and Saved then // на случай not saved спросит actNew.Execute
    case MessageBox(
      0,
      PChar(rs_ConfirmNewBeforeChangeFramerate),
      PChar(Application.Title),
      MB_ICONQUESTION or MB_OKCANCEL or MB_APPLMODAL or MB_DEFBUTTON1
    ) of
      IDCANCEL: Exit;
    end;

  actNew.Execute;

  if FrameRate = 25 then
    FrameRate := 50
  else
    FrameRate := 25;
  actDoubleFramerate.Checked := (FrameRate = 50);

  MultimediaTimer.Interval := 1000 div FrameRate;
  RepaintAll;
end;

procedure TMainForm.actPreviewModeExecute(Sender: TObject);
begin
  RecalculatePreview;
  FreeAndNil(AdvertisementFrameImage);
  FreeAndNil(AdvertisementFrameImagePreview);
  RepaintAll;
end;

type
  LPVOID = Pointer;

function CopyProgressHandler(
  TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: LARGE_INTEGER;
  dwStreamNumber, dwCallbackReason: Integer;
  hSourceFile, hDestinationFile: THANDLE;
  lpData: LPVOID
): Integer; stdcall;
begin
//  TMainForm(lpData).SetStatus('Копирование кадра: ' + FloatToStrF(TotalBytesTransferred / TotalFileSizeTotalFileSize) * 100, ffFixed, 0, 2) + '%';
  Result := PROGRESS_CONTINUE;
end;

procedure SetCurrentCreateDatetime(const FileName: string);
var
  f: THandle;
  LocalFileTime, FileTime: TFileTime;
//  SystemTime: TSystemTime;
begin
  f := FileOpen(FileName, fmOpenWrite);
  if f <> THandle(-1) then
  begin
    GetSystemTimeAsFileTime(LocalFileTime);
    LocalFileTimeToFileTime(LocalFileTime, FileTime);
    SetFileTime(f, @FileTime, nil, nil);
    FileClose(f);
  end;
end;


procedure TMainForm.actExportExecute(Sender: TObject);
var
  Dir, Ext: string;
  i: Integer;
  Cancel: BOOL;
  NewFileName: string;
resourcestring
  rs_ExportSelectFolderCaption = 'Выберите папку для экспорта';
  rs_FramesExportingCaption = 'Экспорт. Копирование кадра %0:d из %1:d';
begin
  Stop;
  Dir := GetCurrentDir;
  if SelectDirectory(
    rs_ExportSelectFolderCaption, '', Dir
    {$IFDEF DelphiXE}
    , [sdNewFolder, sdShowFiles, sdShowEdit, (*sdShowShares, *) sdValidateDir, sdNewUI]
    {$ENDIF}
  )
  then
    begin
//      SetCurrentDir(Dir);
      Exporting := True;
      try
        dir := IncludeTrailingPathDelimiter(dir);
        WaveStorage.Wave.SaveToFile(Dir + 'AudioTrack.wav');
        AdvertisementShowing := False;
        for i := 0 to RecordedFrames.Count - 1 do
          begin
            CurrentRecordPosition := i; // preview
            DisplayedFrameIndex := RecordedFrames[i].FrameInfoIndex;
            pbRecord.Invalidate;
            SetStatus(Format(rs_FramesExportingCaption, [i + 1, RecordedFrames.Count]));
            Application.ProcessMessages;
            Cancel := False;
            Ext := ExtractFileExt(FrameInfoList[RecordedFrames[i].FrameInfoIndex].FullFileName);
            NewFileName := Dir + Format('Frame%.5d.', [i]) + Ext;
            if not CopyFileEx(
              PChar(FrameInfoList[RecordedFrames[i].FrameInfoIndex].FullFileName),
              PChar(NewFileName),
              @CopyProgressHandler,
              @Self,
              {$IFNDEF FPC}@{$ENDIF}Cancel,
              {$IFDEF DelphiXE}COPY_FILE_ALLOW_DECRYPTED_DESTINATION or {$ENDIF}
              COPY_FILE_RESTARTABLE
            ) then
              RaiseLastOSError;
            SetCurrentCreateDatetime(NewFileName);
          end;
        AdvertisementShowing := True;
        RepaintAll;
        Application.ProcessMessages;
        AdvertisementFrameImage.SaveToFile(Dir + Format('Frame%.5d.bmp', [RecordedFrames.Count]));
        InfoMsg('Экспорт завершён.');
      finally
        Exporting := False;
      end;
    end;
end;

// procedure CheckOSError(RetVal: Integer);
// begin
//   Win32Check(LongBool(RetVal));
// end;

procedure TMainForm.actExportToAVIExecute(Sender: TObject);
var
  Compressor: TAVICompressor;
  Options: TAVIFileOptions;
  i: Integer;
  WaveToSave: TWave;
  Bmp: Graphics.TBitmap;
  Image: TGraphic;
  Dir: string;
  PreparedFrameInfoIndex: Integer;
  OldCurrentWorkingFrame: TRecordedFrame;
//  Cancel: BOOL;
  R: TRect;
resourcestring
  rs_AVIExportingAudioStore = 'Экспорт в AVI. Сохранение звука.';
  rs_AVIExportingCompressorInit = 'Экспорт в AVI. Инициализация экспорта видео.';
  rs_AVIExportingCaption = 'Экспорт в AVI. Запись кадра %0:d из %1:d.';
  rs_AVIExportingAudioMerge = 'Экспорт в AVI. Объединение со звуком.';
  rs_ExportFinished = 'Экспорт завершён. Открыть созданый файл?';

begin
  Stop;
  Dir := GetCurrentDir;
  OldCurrentWorkingFrame := CurrentWorkingSetFrame;
  if SaveToAVIDialog.Execute then
    begin
//      SetCurrentDir(Dir);
      Dir := ExtractFilePath(SaveToAVIDialog.FileName);
      Exporting := True;
      try
        SetStatus(rs_AVIExportingAudioStore);
        if WaveStorage.Wave.Length = MulDiv(RecordedFrames.Count, 1000, FrameRate) then
          WaveToSave := WaveStorage.Wave
        else
          begin
            WaveToSave := TWave.Create;
            WaveToSave.Copy(WaveStorage.Wave, 0, MulDiv(RecordedFrames.Count, 1000, FrameRate));
          end;

        WaveToSave.SaveToFile(Dir + '~Audio.wav');
        if WaveToSave <> WaveStorage.Wave then
          WaveToSave.Free;

        SetStatus(rs_AVIExportingCompressorInit);
        Compressor := TAVICompressor.Create;
        Options.Init;
        Options.FrameRate := FrameRate;
        Options.Width := ExportSize.cx;
        Options.Height := ExportSize.cy;
        Options.Handler := 'DIB '; // без компрессии
        CheckAVIError(Compressor.Open(Dir + '~Video.avi', Options));
        Bmp := TBitmap.Create;
        Bmp.Canvas.Brush.Color := clBlack;
        {$IFDEF DelphiXE+}
        Bmp.SetSize(ExportSize.cx, ExportSize.cy);
        {$ELSE}
        Bmp.Width := ExportSize.cx;
        Bmp.Height := ExportSize.cy;
        {$ENDIF}
        AdvertisementShowing := False;
        PreparedFrameInfoIndex := -1;
        for i := 0 to RecordedFrames.Count - 1 do
          begin
            CurrentRecordPosition := i; // preview
            DisplayedFrameIndex := RecordedFrames[i].FrameInfoIndex;
            pbRecord.Invalidate;
            SetStatus(Format(rs_AVIExportingCaption, [i + 1, RecordedFrames.Count]));
            Application.ProcessMessages;
            if PreparedFrameInfoIndex <> RecordedFrames[i].FrameInfoIndex then // skip already preparing
              begin
                Image := FrameInfoList[RecordedFrames[i].FrameInfoIndex].ImageFromDisc;
                R := StretchSize(Image.Width, Image.Height, Bmp.Width, Bmp.Height);
                Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
                Bmp.Canvas.StretchDraw(R, Image);
                Image.Free;
                Bmp.PixelFormat := pf24bit;
                PreparedFrameInfoIndex := RecordedFrames[i].FrameInfoIndex;
              end;
            CheckAVIError(Compressor.WriteFrame(Bmp));
          end;
        Bmp.Assign(AdvertisementFrameImagePreview);
        Bmp.PixelFormat := pf24bit;
        for i := 1 to AdvertisementDuration div FrameRate do
          CheckAVIError(Compressor.WriteFrame(Bmp));
        Bmp.Free;
        Compressor.Close;
        SetStatus(rs_AVIExportingAudioMerge);

        Compressor.MergeFilesAndSaveAs(Dir + '~Video.avi', Dir + '~Audio.wav', SaveToAVIDialog.FileName);

        Compressor.Destroy;
        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Audio.wav');
        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Video.avi');
        if (MessageBox(0,
          PWideChar(rs_ExportFinished),
          PWideChar(Application.Title),
          MB_ICONINFORMATION or MB_YESNO or MB_TASKMODAL) = idYes)
        then
          ShellExecute(0, 'play', PWideChar(SaveToAVIDialog.FileName), nil, nil, SW_SHOWNORMAL);
      finally
        SetStatus('');
        Exporting := False;
        CurrentWorkingSetFrame := OldCurrentWorkingFrame;
      end;
    end;
end;

procedure TMainForm.actForwardWhilePressedExecute(Sender: TObject);
begin
  ReplaceControlActions(caNone);
  UpdatePlayActions;
end;

procedure TMainForm.actFullScreenModeExecute(Sender: TObject);
begin
  if ScreenForm.Visible then
    ScreenForm.UpdateFullScreen
  else
    if actFullScreenMode.Checked then
      begin
        BorderStyle := bsNone;
        PreviousBounds := BoundsRect;
        BoundsRect := Self.Monitor.BoundsRect;
        // FormStyle := fsStayOnTop;
        pnlDisplay.Align := alNone;
        pnlDisplay.BringToFront;
        Menu := nil;
        with Self.ClientRect do
          pnlDisplay.SetBounds(Left, Top, Right, Bottom);
        pbRecord.Hide;
        StatusBar.Hide;
        RecordSplitter.Hide;
        AdvertisementFrameTipShowing := False;
        FrameTipRecordedFrame := nil;  // hide
      end
    else
      begin
        pnlDisplay.Align := alClient;
        BorderStyle := bsSizeable;
        BoundsRect := PreviousBounds;
        FormStyle := fsNormal;
        pbRecord.Show;
        StatusBar.Show;
        RecordSplitter.Left := pbRecord.Left - 5;
        RecordSplitter.Show;
        Menu := MainMenu;
      end;
end;

procedure TMainForm.actNewExecute(Sender: TObject);
resourcestring
  rs_SaveBeforeOpenRequest = 'созданием нового';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeOpenRequest);

  ClearRecorded;
  if FExternalAudioFileName = '' then
    ClearSound;
  ProjectFileName := '';
  SetCaption('');
  Saved := True;
  AdvertisementShowing := False;
  RepaintAll;
  UpdateActions;
end;

const // do not localise
  FilesSectionStart      = '-------------------- Files: ----------------------';
  WorkingSetSectionStart = '------------------ Working set: -------------------';
  BookmarkSectionStart   = '------------------- Bookmarks: --------------------';
  FrameSectionStart      = '-------------------- Frames: ----------------------';
  TeleportsSectionStart  = '------------------- Teleports: --------------------';

resourcestring
  rs_CustomSize = 'Свой (%d, %d)';

procedure TMainForm.OpenAudio(AFileName: string);
begin
  if LowerCase(ExtractFileExt(AFileName)) = '.mp3' then
    begin
      FExternalAudioFileName := PhotoFolder + ChangeFileExt(ExtractFileName(AFileName), '.wav');
      if not TMP3ConvertForm.Execute(AFileName, FExternalAudioFileName) then
        begin
          FExternalAudioFileName := '';
          Exit;
        end;
    end
  else
    FExternalAudioFileName := AFileName;

  actSelectAudioFile.Checked := True;
  mmiUseMicrophone.Checked := False;
  LiveAudioRecorder.Active := False;
  LiveAudioRecorder.WaitForStop;
  LevelGauge.Progress := 0;
  lblAudioFileName.Visible := True;
  lblAudioFileName.Caption := MinimizeName(StringReplace(FExternalAudioFileName, PhotoFolder, '', []), Canvas, lblAudioFileName.Width);
  WaveStorage.Wave.LoadFromFile(FExternalAudioFileName);
  WaveStorage.Wave.Stream.Position := WaveStorage.Wave.DataOffset;
  RecordedAudioCopy.Clear;
  RecordedAudioCopy.CopyFrom(WaveStorage.Wave.Stream, WaveStorage.Wave.DataSize);
  pbRecord.Invalidate;
  WaveStorage.Wave.Position := 0;
  ShowTimes;
end;

procedure TMainForm.OpenMovie(AFileName: string);
var
  i: Integer;
  FrameInfoIndex: Integer;
  WaveFileName: string;
  s: string;
  LastDelimiterPos: Integer;

  procedure ReadTeleport(s: string);
  var
    FrameInfoIndex: Integer;
    SeparatorPos: Integer;
    TargetIndex: Integer;
  begin
    SeparatorPos := pos('=', s);
    FrameInfoIndex := StrToInt(Trim(Copy(s, 1, SeparatorPos-1)));
    TargetIndex := StrToInt(Trim(Copy(s, SeparatorPos+1, Length(s))));
    FrameInfoList[FrameInfoIndex].Teleport := TargetIndex;
  end;

begin
  UnloadFrames;
  ClearBookmarks;
  PhotoFolder := ExtractFilePath(AFileName);
  lblWorkPath.Caption := PhotoFolder;
  ProjectFileName := ExtractFileName(AFileName);

  with TStringList.Create do
    try
      LoadFromFile(AFileName);
      i := 0;
      s := Strings[i];
      // Сейчас файл читается в порядке регистрации, и максимум что может быть
      // - это очередная секция отсутствовать. Переставить местами их нельзя.
      // TODO: сделать гибче (хотя, зачем ?)
      if (Copy(s, 1, Length('Wave = ')) = 'Wave = ') then
        begin
          WaveFileName := Copy(s, Length('Wave = ') + 1, MaxInt);
          if {$IFDEF FPC}FileExistsUTF8{$ELSE}FileExists{$ENDIF}(PhotoFolder + WaveFileName) then
            begin
              OpenAudio(PhotoFolder + WaveFileName);
            end;
          inc(i);
          s := Strings[i];
        end;
      if (Copy(s, 1, Length('ExportWidth = ')) = 'ExportWidth = ') then
        begin
          ExportSize.cx := StrToInt(Copy(s, Length('ExportWidth = ') + 1, MaxInt));
          inc(i);
          s := Strings[i];
        end;
      if (Copy(s, 1, Length('ExportHeight = ')) = 'ExportHeight = ') then
        begin
          ExportSize.cy := StrToInt(Copy(s, Length('ExportHeight = ') + 1, MaxInt));
          mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);
          inc(i);
          s := Strings[i];
        end;
      if (Copy(s, 1, Length('FrameRate = ')) = 'FrameRate = ') then
        begin
          FrameRate := StrToInt(Copy(s, Length('FrameRate = ') + 1, 25));
          if not FrameRate in [50, 25] then
            FrameRate := 25;
          inc(i);
          s := Strings[i];
        end;
      if s = FilesSectionStart then
        begin
          while i < (Count - 1) do
            begin
              inc(i);
              s := Strings[i];
              if s = BookmarkSectionStart then // for compatibility with prev. saves without Working set
                Break;
              if s = WorkingSetSectionStart then
                Break;
              LastDelimiterPos := LastDelimiter(PathDelim + DriveDelim, s);
              FFrameInfoList.Add(TFrameInfo.Create(Copy(s, 1, LastDelimiterPos), Copy(s, LastDelimiterPos + 1, MaxInt)));
            end;
          if s = WorkingSetSectionStart then
            while i < (Count - 1) do
              begin
                inc(i);
                s := Strings[i];
                if s = BookmarkSectionStart then
                  Break;
                TRecordedFrame.Create(WorkingSetFrames, StrToInt(s));
              end
          else  // for compatibility with prev. saves without Working set
            for FrameInfoIndex := 0 to FFrameInfoList.Count - 1 do
              TRecordedFrame.Create(WorkingSetFrames, FrameInfoIndex);
        end;
      if s = BookmarkSectionStart then
        while i < (Count - 1) do
          begin
            inc(i);
            s := Strings[i];
            if s = FrameSectionStart then // for compatibility with prev. saves without Teleports
              Break;
            if s = TeleportsSectionStart then
              Break;
            Bookmarks[StrToInt(Copy(s, Length('Bookmark') + 1, Pos(' = ', s) - Length('Bookmark') - 1))] := StrToInt(Copy(s, Length('Bookmark0 = ') + 1, MaxInt));
          end;
      if s = TeleportsSectionStart then
        while i < (Count - 1) do
          begin
            inc(i);
            s := Strings[i];
            if s = FrameSectionStart then
              Break;
            ReadTeleport(s);
          end;
      if s = FrameSectionStart then
        while i < (Count - 1) do
          begin
            inc(i);
            s := Strings[i];
            TRecordedFrame.Create(RecordedFrames, StrToInt(s));
          end;
    finally
      Free;
    end;
  if WorkingSetFrames.Count > 0 then
    DisplayedFrameIndex := WorkingSetFrames[0].FrameInfoIndex;
  CaptureFirstFrameSizes;

  if (WaveFileName = ExtractFileName(AFileName) + '.wav') and (CalculateSoundFramesCount = RecordedFrames.Count) then // TODO: сделать более надёжный и явный признак
    InitMicrophoneUsage(True);

  Saved := True;
  pbRecord.Invalidate;
  UpdateActions;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
resourcestring
  rs_SaveBeforeOpenRequest = 'открытием другого';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeOpenRequest);

  if not dlgOpenMovie.Execute then
    Abort;
  OpenMovie(dlgOpenMovie.FileName);
end;

procedure TMainForm.actHaveCurrentFrame(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(CurrentWorkingSetFrame);
end;

procedure TMainForm.actHaveDisplayedFrame(Sender: TObject);
begin
  TAction(Sender).Enabled := DisplayedFrameIndex <> -1;
end;

procedure TMainForm.actDuplicateFrameClick(Sender: TObject);
begin
  Stop;
  WorkingSetFrames.Insert(CurrentWorkingSetFrame.Index).Assign(CurrentWorkingSetFrame);
  pbWorkingSet.Refresh;
end;

procedure TMainForm.actOpenFrameFileInDefaultProgramExecute(Sender: TObject);
begin
  Stop;
  SafeShellExecute(HWND(nil), 'edit', FrameInfoList[DisplayedFrameIndex].FullFileName, '', '', SW_SHOW);
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
var
  i: Integer;
  NewPath: string;
  AudioName: string;
begin
  Stop;
  dlgSaveMovie.InitialDir := PhotoFolder;
  if not dlgSaveMovie.Execute then
    Abort;

  NewPath := ExtractFilePath(dlgSaveMovie.FileName);
  ProjectFileName := ExtractFileName(dlgSaveMovie.FileName);
  SetCaption('');
  if UpperCase(NewPath) <> UpperCase(PhotoFolder) then
    // TODO: не уведомлять, а исправлять относительные пути по возможности
    InfoMsg(
      'Внимание!'#13#10+
      'Пути к файлам кадров в файле проекта будут всё равно сохранены относительно '#13#10+
      '  "' + PhotoFolder + '", '#13#10+
      'а не относительно '#13#10+
      '  "' + NewPath + '".'#13#10+
      'При загрузке проекта придётся восстанавливать структуру каталогов с изображениями относительно файла проекта.'
    );
  if FExternalAudioFileName <> '' then
    AudioName := ExtractFileName(FExternalAudioFileName)
  else
    AudioName := ExtractFileName(dlgSaveMovie.FileName) + '.wav';
  // если внешний файл под другим именем уже хранится в папке проекта, не будем его перезаписывать.
  if ExpandFileName(NewPath + AudioName) <> ExpandFileName(FExternalAudioFileName) then
    WaveStorage.Wave.SaveToFile(NewPath + AudioName);
  with TStringList.Create do
    try
      Add('Wave = ' + AudioName);
      Add(Format('ExportWidth = %d', [ExportSize.cx]));
      Add(Format('ExportHeight = %d', [ExportSize.cy]));
      Add(Format('FrameRate = %d', [FrameRate]));
      Add(FilesSectionStart);
      for i := 0 to FrameInfoCount - 1 do
        Add(FrameInfoList[i].RelativeFileName);
      Add(WorkingSetSectionStart);
      for i := 0 to WorkingSetFrames.Count - 1 do
        Add(IntToStr(WorkingSetFrames[i].FrameInfoIndex));
      Add(BookmarkSectionStart);
      for i := Low(Bookmarks) to High(Bookmarks) do
        Add('Bookmark' + IntToStr(i) + ' = ' + IntToStr(Bookmarks[i]));
      Add(TeleportsSectionStart);
      for i := 0 to FrameInfoCount - 1 do
        if FrameInfoList[i].Teleport <> -1 then
          Add(IntToStr(i) + ' = ' + IntToStr(FrameInfoList[i].Teleport));
      Add(FrameSectionStart);
      for i := 0 to RecordedFrames.Count - 1 do
        Add(IntToStr(RecordedFrames[i].FrameInfoIndex));
      SaveToFile(dlgSaveMovie.FileName);
      Saved := True;
    finally
      Free;
    end;
end;

procedure TMainForm.actSaveAsUpdate(Sender: TObject);
begin
  actSaveAs.Enabled := not Exporting and (WorkingSetFrames.Count > 0);// and not Saved;
end;

procedure TMainForm.actScreenWindowExecute(Sender: TObject);
begin
  if actFullScreenMode.Checked then
    actFullScreenMode.Execute;

  ScreenForm.Visible := actScreenWindow.Checked;
end;

procedure TMainForm.SetCaption(const Value: TCaption);
begin
  Caption := ProjectFileName + ' — ' + Value + ' — ' + Application.Title;
end;

procedure TMainForm.SetCurrentRecordPosition(const Value: Integer);
begin
  FCurrentRecordPosition := Value;
  if not Recording then
    StockAudioPlayer.Position := MulDiv(CurrentRecordPosition, 1000, FrameRate);
  ShowTimes;
end;

procedure TMainForm.SetFrameTipRecordedFrame(const Value: TRecordedFrame);
begin
  FFrameTipRecordedFrame := Value;
  pbFrameTip.Visible := (FFrameTipRecordedFrame <> nil);
end;

procedure TMainForm.SetStatus(const Value: string);
begin
  StatusBar.SimpleText := Value;
end;

function TMainForm.CalculateSoundFramesCount: Integer;
var
  WaveFormat: TWaveFormatEx;
begin
  SetPCMAudioFormatS(@WaveFormat, AudioRecorder.PCMFormat);
  Result := MulDiv(RecordedAudioCopy.Size, FrameRate, WaveFormat.nSamplesPerSec * 4); // 2 channels by 2 byte per sample
end;

procedure TMainForm.ShowTimes;
resourcestring
  rsPlayingFrameStatus = '%d:%d';
var
  s: string;
  SoundFramesCount: Integer;
begin
  SoundFramesCount := 0; // hint
  if FExternalAudioFileName <> '' then
    begin
      SoundFramesCount := CalculateSoundFramesCount;
      s := ' Длина озвучки ' + FrameIndexToTimeStamp(SoundFramesCount) + '. ';
    end;

  if Recording then
    begin
      if FExternalAudioFileName <> '' then
        s := s + 'Записывается кадр ' + FrameIndexToTimeStamp(CurrentRecordPosition) + ' (' + IntToStr(MulDiv(CurrentRecordPosition, 100, SoundFramesCount)) + '%)'
      else
        s := s + 'Записывается кадр ' + FrameIndexToTimeStamp(CurrentRecordPosition)
    end
  else //if Playing then
    begin
      if FrameInfoCount = 0 then
        s := s + 'Кадров для мульта пока не указано.'
      else if RecordedFrames.Count = 0 then
        s := s + 'Кадров пока в мульт не записано.'
      else
        s := s + 'Кадр ' + FrameIndexToTimeStamp(CurrentRecordPosition) + ' из ' + FrameIndexToTimeStamp(RecordedFrames.Count) + ' (' + IntToStr(MulDiv(CurrentRecordPosition, 100, RecordedFrames.Count)) + '%)';
    end;

  if FExternalAudioFileName <> '' then
    begin
      if mmiStopRecordingOnSoundtrackFinish.Checked then
        s := s + ', запись будет остановлена по достижении конца озвучки.'
      else
        s := s + ', по достижении конца озвучки запись будет продолжена в тишине.';
    end
  else if FrameInfoCount <> 0 then
    s := s + ' Озвучка будет записываться вместе с кадрами.';

  SetStatus(s);
end;

procedure TMainForm.SetCurrentWorkingSetFrame(const Value: TRecordedFrame);
begin
  FCurrentWorkingSetFrame := Value;
  if Assigned(Value) then
    DisplayedFrameIndex := Value.FrameInfoIndex; // else do not change for calls from SetDisplayedFrameIndex
  RepaintAll;
  ShowTimes;
end;

procedure TMainForm.SetDisplayedFrameIndex(Value: Integer);
resourcestring
  rs_FrameNotInWorkingSet = ', скрыт из рабочего набора..';
begin
  if Value >= GetFrameInfoCount then
    Value := GetFrameInfoCount - 1;

  if FDisplayedFrameIndex = Value then // break recursion
    Exit;

  FDisplayedFrameIndex := Value;
  // При вызовах из SetCurrentWorkingSetFrame оставляем тот экземпляр,
  // какой был установлен и не ищем первый аналогичный. В других случаях - ищем.
  if not Assigned(CurrentWorkingSetFrame) or (CurrentWorkingSetFrame.FrameInfoIndex <> Value) then
    CurrentWorkingSetFrame := WorkingSetFrames.FindByFrameIndex(Value);

  if Value >= 0 then
    begin
      LoadPhoto(Value);
      if not Exporting then
        if Assigned(CurrentWorkingSetFrame) then
          SetCaption(FrameInfoList[Value].RelativeFileName)
        else
          SetCaption(FrameInfoList[Value].RelativeFileName + rs_FrameNotInWorkingSet);
    end;

  RepaintAll;
end;

procedure TMainForm.RepaintAll;
begin
  pbDisplay.Repaint;

  if Assigned(ScreenForm) and ScreenForm.Visible then
    ScreenForm.Repaint;

  pbWorkingSet.Repaint;

  pbRecord.Repaint;

  if Assigned(ControllerForm) and ControllerForm.Visible then
    ControllerForm.Repaint;
end;

procedure TMainForm.StockAudioPlayerActivate(Sender: TObject);
begin
  if RecordingMustBeChanged then // значит, начали играть для записи мультика
    begin
      Recording := True;
      RecordingMustBeChanged := False;
      actRecord.Checked := True;
    end
  else
    begin  // значит, начали играть для проигрывания записанного мультика
      // запускаем воспроизведение только после того, как звук будет готов воспроизводиться
      actPlay.Checked := True;
      ReplaceControlActions(caNone);
      UpdatePlayActions;
      // на всякий случай. TODO надо ли, когда в actPlayExecute есть ?
      if CurrentRecordPosition >= RecordedFrames.Count - 1 then
        CurrentRecordPosition := 0;
      Interval := 0;
      Playing := True;
    end;
end;

procedure TMainForm.StockAudioPlayerDeactivate(Sender: TObject);
begin
  if Recording and mmiStopRecordingOnSoundtrackFinish.Checked then
    begin
      StopRecording;
      AdvertisementShowing := True;
      RepaintAll;
      InfoMsg('Конец файла озвучки, запись остановлена'#13#10'согласно выбранному режиму.');
    end;
//  TODO: понять, надо ли останавливать воспроизведение видео при завершении звука
//  actPlay.Checked := False;
//
//  //CurrentRecordPosition := 0;
//  Interval := 0;
//  Playing := False;
end;

procedure TMainForm.StopPlaying;
begin
  if not Playing then
    Exit;
  StockAudioPlayer.Active := False;
end;

procedure TMainForm.StopRecording;
begin
  if not Recording then
    Exit;

  if FExternalAudioFileName <> '' then
    begin
      Recording := False;
      actRecord.Checked := False;
      ReplaceControlActions(caNone);
      UpdatePlayActions;

      RecordingMustBeChanged := False;
      StockAudioPlayer.Active := False;
      ShowTimes;
    end
  else // через деактивацию записи
    begin
      RecordingMustBeChanged := True;
      AudioRecorder.Active := False;
    end;
end;

procedure TMainForm.LoadPhoto(AFrameInfoIndex: Integer);
var
  Image: TGraphic;
  R: TRect;
  i, j, UnloadCounter: Integer;
  NearestNeighbour: Boolean;
begin
  if (FrameInfoCount = 0) or FrameInfoList[AFrameInfoIndex].Loaded then
    Exit;

  if OutOfMemoryRaised then // unload first loaded frame info
    for i := 0 to FrameInfoCount - 1 do
      if FrameInfoList[i].Loaded then
        begin
          // skip nearest neighbours in both lists
          NearestNeighbour := False;
          for j := Max(0, CurrentWorkingSetFrame.Index - 5) to Min(WorkingSetFrames.Count - 1, CurrentWorkingSetFrame.Index + 5) do
            if i = WorkingSetFrames[j].FrameInfoIndex then
              begin
                NearestNeighbour := True;
                Break;
              end;
          if NearestNeighbour then
            Continue;
          for j := CurrentRecordPosition to Min(RecordedFrames.Count - 1, CurrentRecordPosition + 10) do
            if i = RecordedFrames[j].FrameInfoIndex then
            begin
              NearestNeighbour := True;
              Break;
            end;
          if NearestNeighbour then
            Continue;
          // unload
          FrameInfoList[i].Loaded := False;
          FreeAndNil(FrameInfoList[i].Preview);
          Break;
        end;

  try
    with FrameInfoList[AFrameInfoIndex] do
      try
        try
          Image := ImageFromDisc;
        except
          on e: Exception do
            Image := GenerateStubFrame(e.Message);
        end;
        Preview := TBitmap.Create;
        if actPreviewMode.Checked then
          begin
            R := StretchSize(Image.Width, Image.Height, 640, 480);
            {$IFDEF DelphiXE+}
            Preview.SetSize(640,480);
            {$ELSE}
            Preview.Width := 640;
            Preview.Height := 480;
            {$ENDIF}
            Preview.Canvas.StretchDraw(R, Image);
          end
        else
          Preview.Assign(Image);
        Loaded := True;
      finally
        FreeAndNil(Image);
      end;
  except
    on EOutOfResources do
      begin
        OutOfMemoryRaised := True;
        // only first time, on mass loading frames from first to last at working set
        // unload twenty percents for leave a some memory back
        // TODO: use WorkingSet frames order
        UnloadCounter := AFrameInfoIndex div 5;
        for i := 0 to FrameInfoCount - 1 do
          if FrameInfoList[i].Loaded then
            begin
              FrameInfoList[i].Loaded := False;
              FreeAndNil(FrameInfoList[i].Preview);
              Dec(UnloadCounter);
              if UnloadCounter <= 0 then
                Break;
            end;
        raise;
      end;
  end;
end;

procedure TMainForm.actToggleBookmarkExecute(Sender: TObject);
begin
  if Bookmarks[TMenuItem(Sender).Tag] = DisplayedFrameIndex then
    Bookmarks[TMenuItem(Sender).Tag] := -1
  else
    Bookmarks[TMenuItem(Sender).Tag] := DisplayedFrameIndex;
  pbWorkingSet.Invalidate;
  Saved := False;
end;

procedure TMainForm.actToggleTeleport0Execute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    if Teleport = TMenuItem(Sender).Tag then
      Teleport := -1
    else
      Teleport := TMenuItem(Sender).Tag;
  pbWorkingSet.Invalidate;
  Saved := False;
end;

procedure TMainForm.actGotoBookmark0Execute(Sender: TObject);
begin
  if Bookmarks[TMenuItem(Sender).Tag] <> -1 then
    DisplayedFrameIndex := Bookmarks[TMenuItem(Sender).Tag];
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Stop;
  Close;
end;

procedure TMainForm.actRecordExecute(Sender: TObject);
begin
  Saved := False;
  if FExternalAudioFileName <> '' then
    begin
      if not Recording then
        begin
          CurrentRecordPosition := RecordedFrames.Count; // заодно пинаем позиционирование звука
          // Запуск самой записи и последующая остановка - через обработчики
          // StockAudioPlayerActivate и StockAudioPlayerDeactivate:
          // запускаем запись кадров только после того, как звук будет готов воспроизводиться.
          RecordingMustBeChanged := True;
          StockAudioPlayer.Active := True;
        end
      else
        StopRecording;
    end
  else
    // если звук с микрофона - то запись кадров начнётся в
    // AudioRecorderActivate
    // а закончится - в AudioRecorderDeactivate
    AudioRecorder.Active := not Recording
end;

procedure TMainForm.actRefreshPreviewExecute(Sender: TObject);
begin
  Stop;
  FrameInfoList[DisplayedFrameIndex].Loaded := False;
  FreeAndNil(FrameInfoList[DisplayedFrameIndex].Preview);
  pbDisplay.Refresh;
end;

procedure TMainForm.pbDisplayMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FrameInfoCount = 0 then
    if (ssRight in Shift) or ((ssLeft in Shift) and (ssShift in Shift)) then // открытие вешаем не только на шифт+левую кнопку мыши, но и просто на правую кнопку мыши.
      actOpen.Execute
    else if (ssLeft in Shift) then
      actSelectPhotoFolder.Execute;
end;

procedure TMainForm.pbDisplayDblClick(Sender: TObject);
begin
  actFullScreenMode.Execute;
end;

procedure TMainForm.pbDisplayPaint(Sender: TObject);
const
  MainScreenBorder = 10;
  PrevNextPreviewBorder = 5;
  PrevNextPreviewMaxSize = 120;
var
  R_MainScreen: TRect;
  R_PrevNextPreview: TRect;
  Offset: Integer;
  Image: TBitmap;
  WorkSetFrame: TRecordedFrame;
begin
  try
    pbDisplay.Canvas.Brush.Color := clBlack;
    pbDisplay.Canvas.FillRect(pbDisplay.ClientRect);
    pbDisplay.Canvas.Brush.Color := clBtnFace;

    if (FrameInfoCount = 0) or (DisplayedFrameIndex = -1) then
      begin
        R_MainScreen.Left := (pbDisplay.Width  - imgBackgroundSource.Picture.Width ) div 2;
        R_MainScreen.Top  := (pbDisplay.Height - imgBackgroundSource.Picture.Height) div 2;
        pbDisplay.Canvas.Draw(R_MainScreen.Left, R_MainScreen.Top, imgBackgroundSource.Picture.Graphic);
        Exit;
      end;

    CreateAdvertisementFrame; // на всякий случай
    LoadPhoto(DisplayedFrameIndex); // на всякий случай
    // основной кадр.
    // Сначала ищем смещение экрана, нужное, чтоб он по возможности не подлазил под миниатюры.
    if FrameInfoList[DisplayedFrameIndex].Loaded then
      begin
        Image := FrameInfoList[DisplayedFrameIndex].Preview;
        R_PrevNextPreview := StretchSize(Image.Width, Image.Height, PrevNextPreviewMaxSize, PrevNextPreviewMaxSize);
      end;

    if FrameInfoList[DisplayedFrameIndex].Loaded or AdvertisementShowing then
      begin
        if AdvertisementShowing then
          Image := AdvertisementFrameImagePreview
        else
          Image := FrameInfoList[DisplayedFrameIndex].Preview;
        if (not AdvertisementShowing and actStretchImages.Checked) or (Image.Width > (pbDisplay.Width - MainScreenBorder * 2)) or (Image.Height > (pbDisplay.Height - MainScreenBorder * 2)) then
          begin
            R_MainScreen := StretchSize(Image.Width, Image.Height, pbDisplay.Width - MainScreenBorder * 2, pbDisplay.Height - MainScreenBorder * 2);
            R_MainScreen.Left   := R_MainScreen.Left   + MainScreenBorder;
            R_MainScreen.Top    := R_MainScreen.Top    + MainScreenBorder;
            R_MainScreen.Right  := R_MainScreen.Right  + MainScreenBorder;
            R_MainScreen.Bottom := R_MainScreen.Bottom + MainScreenBorder;

            Offset := Min(pbDisplay.Height - MainScreenBorder * 2 - R_MainScreen.Bottom, (R_PrevNextPreview.Bottom - R_PrevNextPreview.Top - MainScreenBorder) div 2);

            R_MainScreen.Top    := R_MainScreen.Top    + Offset + MainScreenBorder;
            R_MainScreen.Bottom := R_MainScreen.Bottom + Offset + MainScreenBorder;

            with R_MainScreen do
              pbDisplay.Canvas.RoundRect(
                Left   - MainScreenBorder,
                Top    - MainScreenBorder,
                Right  + MainScreenBorder,
                Bottom + MainScreenBorder,
                MainScreenBorder,
                MainScreenBorder
              );
            pbDisplay.Canvas.StretchDraw(R_MainScreen, Image);
          end
        else
          begin
            R_MainScreen.Left := (pbDisplay.Width  - Image.Width ) div 2;
            R_MainScreen.Top  := (pbDisplay.Height - Image.Height) div 2;
            R_MainScreen.Right := R_MainScreen.Left + Image.Width;
            R_MainScreen.Bottom := R_MainScreen.Top + Image.Height;
            with R_MainScreen do
              pbDisplay.Canvas.RoundRect(
                Left   - MainScreenBorder,
                Top    - MainScreenBorder,
                Right  + MainScreenBorder,
                Bottom + MainScreenBorder,
                MainScreenBorder,
                MainScreenBorder
              );
            pbDisplay.Canvas.Draw(R_MainScreen.Left, R_MainScreen.Top, Image);
          end;
      end;

    if Assigned(CurrentWorkingSetFrame) then
      begin
        // левая миниатюра
        WorkSetFrame := FindWorkingSetFrameByOffset(-1);
        LoadPhoto(WorkSetFrame.FrameInfoIndex); // на всякий случай
        if FrameInfoList[WorkSetFrame.FrameInfoIndex].Loaded then
          begin
            Image := FrameInfoList[WorkSetFrame.FrameInfoIndex].Preview;
            R_PrevNextPreview.Bottom := R_PrevNextPreview.Bottom - R_PrevNextPreview.Top;
            R_PrevNextPreview.Top    := 0;
            R_PrevNextPreview.Left   := R_PrevNextPreview.Left   + PrevNextPreviewBorder;
            R_PrevNextPreview.Top    := R_PrevNextPreview.Top    + PrevNextPreviewBorder;
            R_PrevNextPreview.Right  := R_PrevNextPreview.Right  + PrevNextPreviewBorder;
            R_PrevNextPreview.Bottom := R_PrevNextPreview.Bottom + PrevNextPreviewBorder;
            with R_PrevNextPreview do
              pbDisplay.Canvas.RoundRect(
                Left   - PrevNextPreviewBorder,
                Top    - PrevNextPreviewBorder,
                Right  + PrevNextPreviewBorder,
                Bottom + PrevNextPreviewBorder,
                PrevNextPreviewBorder,
                PrevNextPreviewBorder
              );
            pbDisplay.Canvas.StretchDraw(R_PrevNextPreview, Image);
          end;
        // правая миниатюра
        WorkSetFrame := FindWorkingSetFrameByOffset(+1);
        LoadPhoto(WorkSetFrame.FrameInfoIndex); // на всякий случай
        if FrameInfoList[WorkSetFrame.FrameInfoIndex].Loaded then
          begin
            Image := FrameInfoList[WorkSetFrame.FrameInfoIndex].Preview;
            R_PrevNextPreview := StretchSize(Image.Width, Image.Height, PrevNextPreviewMaxSize, PrevNextPreviewMaxSize);
            R_PrevNextPreview.Left := pbDisplay.Width - PrevNextPreviewMaxSize + R_PrevNextPreview.Left;
            R_PrevNextPreview.Right := pbDisplay.Width - PrevNextPreviewMaxSize + R_PrevNextPreview.Right;
            R_PrevNextPreview.Bottom := R_PrevNextPreview.Bottom - R_PrevNextPreview.Top;
            R_PrevNextPreview.Top := 0;
            R_PrevNextPreview.Left   := R_PrevNextPreview.Left   - PrevNextPreviewBorder;
            R_PrevNextPreview.Top    := R_PrevNextPreview.Top    + PrevNextPreviewBorder;
            R_PrevNextPreview.Right  := R_PrevNextPreview.Right  - PrevNextPreviewBorder;
            R_PrevNextPreview.Bottom := R_PrevNextPreview.Bottom + PrevNextPreviewBorder;
            with R_PrevNextPreview do
              pbDisplay.Canvas.RoundRect(
                Left   - PrevNextPreviewBorder,
                Top    - PrevNextPreviewBorder,
                Right  + PrevNextPreviewBorder,
                Bottom + PrevNextPreviewBorder,
                PrevNextPreviewBorder,
                PrevNextPreviewBorder
              );
            pbDisplay.Canvas.StretchDraw(R_PrevNextPreview, Image);
          end;
      end;
  except
    ; // на всякий случай глушим ошибки рисования, потому что они непонятно откуда лезут
  end;
end;

procedure TMainForm.pbFrameTipPaint(Sender: TObject);
begin
  if AdvertisementFrameTipShowing then
    begin
      pbFrameTip.Canvas.Polygon([
        Point(0, 0),
        Point(0, FrameTipH+FrameTipT+FrameTipT),
        Point(FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT),
        Point(FrameTipW+FrameTipT+FrameTipT, FrameTipArrow + FrameTipD),
        Point(FrameTipW+FrameTipT+FrameTipT+FrameTipD+FrameTipD, FrameTipArrow),
        Point(FrameTipW+FrameTipT+FrameTipT, FrameTipArrow - FrameTipD),
        Point(FrameTipW+FrameTipT+FrameTipT, 0)
      ]);
      pbFrameTip.Canvas.StretchDraw(Rect(FrameTipT, FrameTipT, FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT), AdvertisementFrameImagePreview);
    end
  else
    if FrameTipRecordedFrame <> nil then
      if FrameTipMode = ftmWorkingSet then
        begin
          pbFrameTip.Canvas.Polygon([
            Point(0, 0),
            Point(0, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipArrow-FrameTipD, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipArrow, FrameTipH+FrameTipT+FrameTipT+FrameTipD+FrameTipD),
            Point(FrameTipArrow+FrameTipD, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipW+FrameTipT+FrameTipT, 0)
          ]);
          pbFrameTip.Canvas.StretchDraw(Rect(FrameTipT, FrameTipT, FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT), FrameInfoList[FrameTipRecordedFrame.FrameInfoIndex].Preview);
        end
      else
        begin
          pbFrameTip.Canvas.Polygon([
            Point(0, 0),
            Point(0, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT),
            Point(FrameTipW+FrameTipT+FrameTipT, FrameTipArrow + FrameTipD),
            Point(FrameTipW+FrameTipT+FrameTipT+FrameTipD+FrameTipD, FrameTipArrow),
            Point(FrameTipW+FrameTipT+FrameTipT, FrameTipArrow - FrameTipD),
            Point(FrameTipW+FrameTipT+FrameTipT, 0)
          ]);
          pbFrameTip.Canvas.StretchDraw(Rect(FrameTipT, FrameTipT, FrameTipW+FrameTipT+FrameTipT, FrameTipH+FrameTipT+FrameTipT), FrameInfoList[FrameTipRecordedFrame.FrameInfoIndex].Preview);
        end;
end;

var
  OddTick: Boolean;

procedure TMainForm.pbIndicatorPaint(Sender: TObject);
resourcestring
  rs_Framerate = '- по %d, при %d кадров в секунду';
var
//  X: TPoint;
  a: Double;
  c: Byte;
begin
  with pbIndicator, Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
      FillRect(pbIndicator.ClientRect);

      Pen.Style := psClear;
      Brush.Style := bsClear;
      SetTextAlign(Handle, TA_TOP + TA_LEFT);
      TextOut(58, 8, Format(rs_Framerate, [CurrentSpeedInterval, FrameRate]));

      Brush.Style := bsSolid;
      Brush.Color := clWhite;
        Ellipse(15-10, 15-10, 15+10+1, 15+10+1);

      c := 255 - MulDiv(255, Interval - 1, CurrentSpeedInterval - 1);
      Brush.Color := RGB(c, c, c);
      if not Recording then
        a := -(CurrentRecordPosition mod FrameRate)* 2 * Pi / FrameRate
      else
        a := -(RecordedFrames.Count mod FrameRate)* 2 * Pi / FrameRate;
      with Point(15 + Round(10 * Cos(a)), 15 - Round(10 * Sin(a))) do
        Ellipse(X-2, Y-2, X+3, Y+3);

      a := -(GetTickCount mod 1000)* 2 * Pi / 1000; // текущая милисекунда. Просто FrameRate раз в секунду перерисовывается.
      with Point(15 + Round(10 * Cos(a)), 15 - Round(10 * Sin(a))) do
        FillRect(Rect(X-1, Y-1, X+2, Y+2));

      Pen.Style := psSolid;
      Pen.Color := clBlack;
      Pen.Width := 2;
      if OddTick then
        begin
          MoveTo(45 - 1, 25);
          LineTo(45 - 5, 5)
        end
      else
        begin
          MoveTo(45 + 1, 25);
          LineTo(45 + 5, 5);
        end;
      if Interval = CurrentSpeedInterval then
        OddTick := not OddTick;
      Pen.Width := 1;
      Polyline([
        Point(45-2, 7),
        Point(45+2, 7),
        Point(45+5, 30-3),
        Point(45-5, 30-3),
        Point(45-2, 7)
      ]);

      Brush.Color := clBlack;
      FillRect(Rect(0, Height - 2, NextControlActionStackPosition * 2, Height));
    end;
end;

procedure TMainForm.pbRecordMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewPosition: Integer;
begin
  NewPosition := Y + pbRecordOffset;
  if NewPosition > RecordedFrames.Count then
    NewPosition := RecordedFrames.Count;
  if NewPosition < 0 then
    NewPosition := 0;

  CurrentRecordPosition := NewPosition;
  AdvertisementShowing := (NewPosition >= RecordedFrames.Count);
  if not AdvertisementShowing and (NewPosition < RecordedFrames.Count) then
    begin
      DisplayedFrameIndex := RecordedFrames[CurrentRecordPosition].FrameInfoIndex;
    end;

  RepaintAll;
  UpdateActions;
end;

procedure TMainForm.pbRecordMouseLeave(Sender: TObject);
begin
  FrameTipRecordedFrame := nil;
end;

procedure TMainForm.pbRecordMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  LocalCurrentRecordPosition: Integer;
begin
  if ssLeft in Shift then
    pbRecordMouseDown(Sender, mbLeft, Shift, X, Y);

  LocalCurrentRecordPosition := Y + pbRecordOffset;

  AdvertisementFrameTipShowing := (LocalCurrentRecordPosition >= RecordedFrames.Count) and (Y <= AdvertisementFrameBottom);
  if not AdvertisementFrameTipShowing and ((LocalCurrentRecordPosition < 0) or (LocalCurrentRecordPosition > RecordedFrames.Count)) then
    FrameTipRecordedFrame := nil
  else
    begin
      if AdvertisementFrameTipShowing then
        FrameTipRecordedFrame := nil
      else
        FrameTipRecordedFrame := RecordedFrames[LocalCurrentRecordPosition];
      FrameTipMode := ftmRecord;
      pbFrameTip.Width  := FrameTipT + FrameTipW + FrameTipD + FrameTipD + FrameTipT + 1;
      pbFrameTip.Height := FrameTipT + FrameTipH + FrameTipT + 1;
      pbFrameTip.Left := pbDisplay.Width - pbFrameTip.Width;
      pbFrameTip.Top  := Min(Max(0, Y - pbFrameTip.Height div 2), pbDisplay.Height - pbFrameTip.Height);
      FrameTipArrow := Y - pbFrameTip.Top;
    end;
  pbFrameTip.Invalidate;
end;

procedure TMainForm.pbRecordPaint(Sender: TObject);
const
  FramesBorderSize = 5;
  FragmentsGap = 1;
  ScrollBarWidth = 2;
type
  TSampleArray = array [0..256] of SmallInt;
  PSampleArray = ^TSampleArray;
var
  y: Integer;
  RecordedFrameIndex: Integer;
  R: TRect;
  TopOfMainRecord: Integer;
  DataWidth: Integer;
  DataWidthHalf: Integer;
  DataAreaHeight: Integer;

  SamplesPerFrame: Integer;
  WaveFormat: TWaveFormatEx;

  procedure DrawSoundLine;
  var
    FirstSample: Integer;
    pSample: PSmallInt;
    MaxData, MinData: SmallInt;
    i: Integer;
  begin
    FirstSample := (pbRecordOffset + y - TopOfMainRecord) * SamplesPerFrame;
    if (FirstSample + SamplesPerFrame) > (RecordedAudioCopy.Size div 2) then
      Exit;

    pSample := Pointer(PSampleArray(RecordedAudioCopy.Memory));
    inc(pSample, FirstSample);
    MaxData := PWordArray(pSample)^[0];
    MinData := pSample^;
    for i := 0 to SamplesPerFrame - 1 do
      begin
        MaxData := Max(MaxData, pSample^);
        MinData := Min(MinData, pSample^);
        Inc(pSample);
        Inc(pSample); // stereo
      end;
    pbRecord.Canvas.MoveTo(R.Left + DataWidthHalf + MulDiv(MinData, DataWidthHalf, $FFFF div 2),     y);
    pbRecord.Canvas.LineTo(R.Left + DataWidthHalf + MulDiv(MaxData, DataWidthHalf, $FFFF div 2) + 1, y);
  end;

  procedure DrawScaleMark;
  begin
    if RecordedFrameIndex mod FrameRate        = 0 then pbRecord.Canvas.FillRect(Rect(R.Left, y, R.Right, y + 1));
    if RecordedFrameIndex mod (FrameRate * 10) = 0 then pbRecord.Canvas.FillRect(Rect(R.Left, y, R.Right, y + 2));
    if RecordedFrameIndex mod (FrameRate * 60) = 0 then pbRecord.Canvas.FillRect(Rect(R.Left, y, R.Right, y + 3));
  end;

begin
  DataWidth := pbRecord.ClientWidth - FramesBorderSize - FramesBorderSize - ScrollBarWidth - 1;
  DataWidthHalf := DataWidth div 2;
  DataAreaHeight := pbRecord.ClientHeight - imgAdvertisementThumbnail.Height - FramesBorderSize * 4 - FragmentsGap;

  // Automatic scrollbar position
  pbRecordOffset := CurrentRecordPosition - (DataAreaHeight div 2);
  if (pbRecordOffset + DataAreaHeight) > RecordedFrames.Count then
    pbRecordOffset := RecordedFrames.Count - DataAreaHeight;
  if pbRecordOffset < 0 then
    pbRecordOffset := 0;

  SetPCMAudioFormatS(@WaveFormat, AudioRecorder.PCMFormat);

  SamplesPerFrame := 2 * (WaveFormat.nSamplesPerSec div FrameRate);
  Assert(WaveFormat.wBitsPerSample = 16, '{F90018C7-D187-41DE-A30C-CB8D15A72149}');
  Assert(WaveFormat.nChannels = 2,       '{9655AC01-69A1-456D-B55E-027A9B04CA93}');

  // background
  pbRecord.Canvas.Brush.Color := clBtnFace;
  pbRecord.Canvas.FillRect(pbRecord.ClientRect);

  // scrollbar
  pbRecord.Canvas.Brush.Color := clMaroon;
  with pbRecord.ClientRect do
    pbRecord.Canvas.FillRect(Rect(
      Right - ScrollBarWidth, MulDiv(pbRecordOffset,                  pbRecord.ClientHeight, RecordedFrames.Count),
      Right,                  MulDiv(pbRecordOffset + DataAreaHeight, pbRecord.ClientHeight, RecordedFrames.Count)
    ));

  // border of main record
  R := pbRecord.ClientRect;
  R.Bottom := Min(RecordedFrames.Count - pbRecordOffset + FramesBorderSize + FramesBorderSize, R.Bottom);
  R.Right := R.Left + DataWidth + FramesBorderSize + FramesBorderSize;
  pbRecord.Canvas.Pen.Color := clBlack;
  if not AdvertisementShowing then
    pbRecord.Canvas.Pen.Width := 2
  else
    pbRecord.Canvas.Pen.Width := 1;
  pbRecord.Canvas.Pen.Style := psSolid;
  pbRecord.Canvas.Brush.Color := clBtnFace;
  pbRecord.Canvas.RoundRect(R.Left + pbRecord.Canvas.Pen.Width div 2, R.Top + pbRecord.Canvas.Pen.Width div 2, R.Right, R.Bottom, FramesBorderSize, FramesBorderSize);
  R.Left   := R.Left   + FramesBorderSize;
  R.Top    := R.Top    + FramesBorderSize;
  R.Right  := R.Right  - FramesBorderSize;
  R.Bottom := R.Bottom - FramesBorderSize;
  pbRecord.Canvas.Brush.Color := clWindow;
  pbRecord.Canvas.FillRect(R);

  pbRecord.Canvas.Brush.Color := clLtGray;

  pbRecord.Canvas.Pen.Color := clSkyBlue;
  pbRecord.Canvas.Pen.Width := 1;
  pbRecord.Canvas.Pen.Style := psSolid;
  TopOfMainRecord := R.Top;
  // content of main record
  RecordedFrameIndex := pbRecordOffset;
  for y := R.Top to R.Bottom - 1 do
    begin
      DrawSoundLine;

      DrawScaleMark;

      pbRecord.Canvas.Pixels[R.Left + MulDiv(RecordedFrames[RecordedFrameIndex].FrameInfoIndex, DataWidth, FrameInfoCount - 1), y] := clBlack;

      Inc(RecordedFrameIndex);
    end;

  // current position in main record
  pbRecord.Canvas.Brush.Color := clWhite;
  if not AdvertisementShowing and (CurrentRecordPosition >= 0) and (CurrentRecordPosition < RecordedFrames.Count) then
    with Point(
      MulDiv(RecordedFrames[CurrentRecordPosition].FrameInfoIndex, DataWidth, FrameInfoCount - 1) + R.Left,
      CurrentRecordPosition - pbRecordOffset + R.Top
    ) do
      pbRecord.Canvas.Ellipse(x-2, y-2, x+3, y+3);

  // border of advertisement frames
  R.Left   := R.Left   - FramesBorderSize;
  R.Right  := R.Right  + FramesBorderSize;
  R.Top := R.Bottom + FramesBorderSize + FragmentsGap;
  R.Bottom := R.Top + imgAdvertisementThumbnail.Height + FramesBorderSize + FramesBorderSize;
  pbRecord.Canvas.Pen.Color := clBlack;

  if AdvertisementShowing then
    pbRecord.Canvas.Pen.Width := 2
  else
    pbRecord.Canvas.Pen.Width := 1;

  pbRecord.Canvas.Pen.Style := psSolid;
  pbRecord.Canvas.Brush.Color := clBtnFace;
  Inc(TopOfMainRecord, R.Height);
  pbRecord.Canvas.RoundRect(R.Left + pbRecord.Canvas.Pen.Width div 2, R.Top + pbRecord.Canvas.Pen.Width div 2, R.Right, R.Bottom, FramesBorderSize, FramesBorderSize);
  R.Left   := R.Left   + FramesBorderSize;
  R.Top    := R.Top    + FramesBorderSize;
  R.Right  := R.Right  - FramesBorderSize;
  R.Bottom := R.Bottom - FramesBorderSize;
  AdvertisementFrameBottom := R.Bottom;
  pbRecord.Canvas.Brush.Color := clWindow;
  pbRecord.Canvas.FillRect(R);
  pbRecord.Canvas.Draw(
    R.Left + Max(0, (DataWidth - imgAdvertisementThumbnail.Width) div 2),
    R.Top + 1,
    imgAdvertisementThumbnail.Picture.Graphic
  );

  // Tail of sound
  pbRecord.Canvas.Pen.Color := clMoneyGreen;
  pbRecord.Canvas.Pen.Width := 1;
  pbRecord.Canvas.Pen.Style := psSolid;

  for y := R.Bottom + FramesBorderSize to pbRecord.Height do
    DrawSoundLine;
end;

procedure TMainForm.ListBoxClick(Sender: TObject);
begin
//  CurrentFrameIndex := ListBox.ItemIndex;
end;

procedure TMainForm.LiveAudioRecorderData(Sender: TObject; const Buffer: Pointer; BufferSize: Cardinal; var FreeIt: Boolean);
type
  TVolumeArray = array [0..1600] of Byte;
var
  MaxVolume: Integer;
  i: Integer;
  Volumes: ^TVolumeArray;
begin
  // Готовы очередные 200 милисекунд звука с микрофона.
  // Посчитаем максимум, и нарисуем его.
  Volumes := Buffer;
  MaxVolume := 0;
  for i := 0 to BufferSize - 1 do
    if MaxVolume < Abs(Volumes^[i] - 128) then
      MaxVolume := Abs(Volumes^[i] - 128);
  LevelGauge.Progress := Round(MaxVolume / 128 * 100);
end;

procedure TMainForm.pbWorkingSetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if WorkingSetFrames.Count = 0 then
    Exit;

  CurrentWorkingSetFrame := WorkingSetXToWorkingFrame(X);
  AdvertisementShowing := False;

  UpdateActions;
end;

procedure TMainForm.pbWorkingSetMouseLeave(Sender: TObject);
begin
  FrameTipRecordedFrame := nil;
end;

procedure TMainForm.pbWorkingSetMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    pbWorkingSetMouseDown(Sender, mbLeft, Shift, X, Y);

  FrameTipRecordedFrame := WorkingSetXToWorkingFrame(X);
  FrameTipMode := ftmWorkingSet;
  AdvertisementFrameTipShowing := False;
  pbFrameTip.Width  := FrameTipT + FrameTipW + FrameTipT + 1;
  pbFrameTip.Height := FrameTipT + FrameTipH + FrameTipD + FrameTipD + FrameTipT + 1;
  pbFrameTip.Top  := pnlDisplay.Height - pbFrameTip.Height;
  pbFrameTip.Left := Min(Max(0, X - pbFrameTip.Width div 2), pnlDisplay.Width - pbFrameTip.Width);
  FrameTipArrow := WorkingFrameToWorkingSetX(FrameTipRecordedFrame) - pbFrameTip.Left;

  pbFrameTip.Invalidate;
end;

function TMainForm.WorkingFrameToWorkingSetX(AWorkingSetFrame: TRecordedFrame): Integer;
begin
  if AWorkingSetFrame = nil then
    Result := -1
  else
    Result := 4 + MulDiv(AWorkingSetFrame.Index, pbWorkingSet.Width - 8, WorkingSetFrames.Count - 1);
end;

function TMainForm.WorkingSetXToWorkingFrame(X: Integer): TRecordedFrame;
var
  FrameIndex: Integer;
begin
  Result := nil;
  if WorkingSetFrames.Count > 0 then
    begin
      FrameIndex := MulDiv(X - 4, WorkingSetFrames.Count, pbWorkingSet.Width - 8);

      if FrameIndex >= WorkingSetFrames.Count then
        Result := WorkingSetFrames[WorkingSetFrames.Count - 1]
      else
        if FrameIndex < 0 then
          Result := WorkingSetFrames[0]
      else
        Result := WorkingSetFrames[FrameIndex];
    end;
end;

procedure TMainForm.pbWorkingSetPaint(Sender: TObject);
var
  SecondIndex: Integer;
  WorkingFrameIndex: integer;
  CurrentWorkingFrameIndex: Integer;
  BookmarkIndex: Integer;
  x, y1, y2: Integer;
  BookmarkText: string;
  BookmarkRect: TRect;
  TextSize: TSize;
begin
  y1 := 4;
  if Assigned(CurrentWorkingSetFrame) then
    CurrentWorkingFrameIndex := CurrentWorkingSetFrame.Index
  else
    CurrentWorkingFrameIndex := -1;

  for SecondIndex := 0 to MulDiv(WorkingSetFrames.Count, CurrentSpeedInterval, FrameRate) - 1 do
    begin
      x := 4 + MulDiv(pbWorkingSet.Width - 8, SecondIndex, MulDiv(WorkingSetFrames.Count, CurrentSpeedInterval, FrameRate));
      pbWorkingSet.Canvas.Brush.Style := bsSolid;
      pbWorkingSet.Canvas.Rectangle(x, y1 + 8, x+1, y1 + 16);
    end;

  for WorkingFrameIndex := 0 to WorkingSetFrames.Count - 1 do
    begin
      x := WorkingFrameToWorkingSetX(WorkingSetFrames[WorkingFrameIndex]);
      if FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].Loaded then
        y2 := y1 + 8
      else
        y2 := y1 + 4;

      pbWorkingSet.Canvas.Brush.Style := bsSolid;
      if WorkingFrameIndex <> CurrentWorkingFrameIndex then
        pbWorkingSet.Canvas.Rectangle(x, y1, x+1, y2)
      else
        begin
          pbWorkingSet.Canvas.Brush.Color := clHighlight;
          pbWorkingSet.Canvas.Rectangle(x-1, y1, x+2, y2);
        end;

      for BookmarkIndex := Low(Bookmarks) to High(Bookmarks) do
        if WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex = Bookmarks[BookmarkIndex] then
        begin
          BookmarkText := BookmarkKey[BookmarkIndex];
          BookmarkRect := Rect(x - 20, y1, x + 21, pbWorkingSet.Height);
//          pbWorkingSet.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop, tfCalcRect]);
//          BookmarkRect := Rect(x - (BookmarkRect.Right - BookmarkRect.Left) div 2 - 2, y1 - 2, x + (BookmarkRect.Right - BookmarkRect.Left) div 2 + 2, BookmarkRect.Bottom);
          TextSize := pbWorkingSet.Canvas.TextExtent(BookmarkText);
          BookmarkRect := Rect(x - TextSize.cx div 2 - 2, y1 - 2, x + TextSize.cx div 2 + 2, y1 + TextSize.cy);
          pbWorkingSet.Canvas.Brush.Style := bsSolid;
          if WorkingFrameIndex = CurrentWorkingFrameIndex then
          begin
            pbWorkingSet.Canvas.Brush.Color := clHighlight;
            pbWorkingSet.Canvas.Font.Color := clHighlightText;
          end
          else
          begin
            pbWorkingSet.Canvas.Brush.Color := clBtnFace;
            pbWorkingSet.Canvas.Brush.Style := bsSolid;
            pbWorkingSet.Canvas.Font.Color := clBtnText;
          end;
          with BookmarkRect do
            pbWorkingSet.Canvas.RoundRect(Left, Top, Right, Bottom, 2, 2);
          Inc(BookmarkRect.Left, 2);
          pbWorkingSet.Canvas.Brush.Style := bsClear;
//          pbWorkingSet.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop]);
          pbWorkingSet.Canvas.TextOut(BookmarkRect.Left, BookmarkRect.Top, BookmarkText);
        end;

      if FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].Teleport <> -1 then
        begin
          BookmarkText := BookmarkKey[FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].Teleport];
          BookmarkRect := Rect(x - 20, y1, x + 21, pbWorkingSet.Height);
//          pbWorkingSet.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop, tfCalcRect]);
//          BookmarkRect := Rect(x - (BookmarkRect.Right - BookmarkRect.Left) div 2 - 2, y1 - 2, x + (BookmarkRect.Right - BookmarkRect.Left) div 2 + 2, BookmarkRect.Bottom);
          TextSize := pbWorkingSet.Canvas.TextExtent(BookmarkText);
          BookmarkRect := Rect(x - TextSize.cx div 2 - 2, y1 - 2, x + TextSize.cx div 2 + 2, y1 + TextSize.cy);
          pbWorkingSet.Canvas.Brush.Style := bsSolid;
          if not TeleportEnabled or (Bookmarks[FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].Teleport] = -1) then
            begin
              pbWorkingSet.Canvas.Brush.Color := clLtGray;
              pbWorkingSet.Canvas.Font.Color := clDkGray;
            end
          else if WorkingFrameIndex = CurrentWorkingFrameIndex then
            begin
              pbWorkingSet.Canvas.Brush.Color := clLime;
              pbWorkingSet.Canvas.Font.Color := clBlack;
            end
          else
            begin
              pbWorkingSet.Canvas.Brush.Color := clGreen;
              pbWorkingSet.Canvas.Brush.Style := bsSolid;
              pbWorkingSet.Canvas.Font.Color := clWhite;
            end;
          with BookmarkRect do
            pbWorkingSet.Canvas.RoundRect(Left, Top + TextSize.cy, Right, Bottom + TextSize.cy, 2, 2);
          Inc(BookmarkRect.Left, 2);
          pbWorkingSet.Canvas.Brush.Style := bsClear;
//          pbWorkingSet.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop]);
          pbWorkingSet.Canvas.TextOut(BookmarkRect.Left, BookmarkRect.Top + TextSize.cy, BookmarkText);
        end;
    end;
end;

procedure TMainForm.pnlToollsResize(Sender: TObject);
begin
  imgLeftRightByMouseDownController.Left := tlbNavigation.Left + tlbNavigation.Width + 16;
end;

procedure TMainForm.PopControlAction;
begin
  Assert(NextControlActionStackPosition >=1, '{EBDE522B-9ED0-474F-9F3F-4C5CAEDBC757}');
  Dec(NextControlActionStackPosition);
end;

procedure TMainForm.PushControlAction(Value: TControlAction);
begin
  // Если была автоматика, то не отодвигаем её, а стираем.
  if NextControlAction in [caPlayBackward, caPlayForward] then
    NextControlActionStackPosition := 0;

  if NextControlActionStackPosition = ControlActionStackDeep then
    Beep
  else
    Inc(NextControlActionStackPosition);
  NextControlActionStack[NextControlActionStackPosition] := Value;
end;

procedure TMainForm.RecalculatePreview;
var
  i: Integer;
begin
  for i := 0 to FrameInfoCount - 1 do
    if FrameInfoList[i].Loaded then
      begin
        FrameInfoList[i].Loaded := False;
        FreeAndNil(FrameInfoList[i].Preview);
        OutOfMemoryRaised := False;
      end;
end;

procedure TMainForm.ReplaceControlActions(Value: TControlAction);
begin
  NextControlActionStackPosition := 1;
  NextControlActionStack[NextControlActionStackPosition] := Value;
end;

procedure TMainForm.mmiExportResolutionClick(Sender: TObject);
begin
  Stop;
  case (Sender as TMenuItem).Tag of
  //3:4
    1: ExportSize := Size(320, 240);
    2: ExportSize := Size(640, 480);
    3: ExportSize := Size(800, 600);
    4: ExportSize := Size(1024, 768);
    5: ExportSize := Size(1600, 1200);
    6: ExportSize := Size(2048, 1536);
    7: ExportSize := Size(3200, 2400);
    8: ExportSize := Size(6400, 4800);
  // 16:9
     9: ExportSize := Size(640, 360);
    10: ExportSize := Size(854, 480);
    11: ExportSize := Size(960, 540);
    12: ExportSize := Size(1280, 720);
    13: ExportSize := Size(1600, 900);
    14: ExportSize := Size(1920, 1080);
    15: ExportSize := Size(2048, 1152);
    16: ExportSize := Size(2560, 1440);
    17: ExportSize := Size(7680, 4320);
  end;
  mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + TrimLeft(TMenuItem(Sender).Caption);
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.actExportResolutionCustomExecute(Sender: TObject);
begin
  Stop;
  if TExportSizeCustomRequestDialog.Execute(ExportSize) then
    mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.CaptureFirstFrameSizes;
begin
  if FrameInfoCount > 0 then
    with FrameInfoList[0].ImageFromDisc do
      begin
        FirstFrameSize := Size(Width, Height);
        actExportResolutionFirstFrame.Caption := Copy(actExportResolutionFirstFrame.Caption, 1, Pos('-', mmiExportResolution.Caption)) + Format('(%d, %d)', [FirstFrameSize.cx, FirstFrameSize.cy]);
        Free;
      end
  else
    actExportResolutionFirstFrame.Caption := Copy(actExportResolutionFirstFrame.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + '(?, ?)';
end;

procedure TMainForm.actExportResolutionFirstFrameExecute(Sender: TObject);
begin
  Stop;
  ExportSize := FirstFrameSize;
  mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.actExportResolutionFirstFrameUpdate(Sender: TObject);
begin
  actExportResolutionFirstFrame.Enabled := (FrameInfoCount > 0);
end;

function TMainForm.TeleportEnabled: Boolean;
begin
  Result := not ((GetAsyncKeyState(vk_Shift) < 0) xor (GetKeyState(VK_CAPITAL) and 1 = 1)); // либо то, либо другое...
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if (csDestroying in ComponentState) or Exporting then
    Exit;

  if Playing then
    begin
      AdvertisementShowing := False;
      if CurrentRecordPosition < RecordedFrames.Count then
        begin
          DisplayedFrameIndex := RecordedFrames[CurrentRecordPosition].FrameInfoIndex;
          pbRecord.Invalidate;
          Inc(FCurrentRecordPosition);
          ShowTimes;
        end
      else
        if LoopMode then
          CurrentRecordPosition := 0
        else
          begin
            Playing := False;
            StockAudioPlayer.Active := False;
            AdvertisementShowing := True;
            RepaintAll;
          end;
    end
  else
    begin
      case NextControlAction of
        caStepBackward:
          begin
            CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(-1);
            PopControlAction;
          end;
        caStepForward:
          begin
            CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(1);
            PopControlAction;
          end;
        caPlayBackward, caPlayForward, // должны перекрываться нажатыми actBackwardWhilePressed и actForwardWhilePressed
        caNone:
          if Interval <= 1 then
            begin
              if actBackwardWhilePressed.Checked or (GetAsyncKeyState(Ord('A')) < 0) or (GetAsyncKeyState(Ord('C')) < 0) then //  эти буквы ещё упомянуты в меню и в блокировщике горячих клавиш IsShortCut
                CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(-1)
              else if actForwardWhilePressed.Checked or (GetAsyncKeyState(Ord('D')) < 0)  or (GetAsyncKeyState(Ord('M')) < 0) then
                CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(1)
              else
                case NextControlAction of
                  caPlayBackward:
                    CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(-1);
                  caPlayForward:
                    CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(1);
                end;
            end;
      end;

      if Recording then
        begin
          if not Assigned(CurrentWorkingSetFrame) then
            CurrentWorkingSetFrame := WorkingSetFrames[0];
          TRecordedFrame.Create(RecordedFrames, CurrentWorkingSetFrame.FrameInfoIndex);
          pbRecord.Invalidate;
          CurrentRecordPosition := RecordedFrames.Count - 1;
        end;

      if Interval <= 1 then
        begin
          Interval := CurrentSpeedInterval;
          pbIndicator.Repaint;
        end
      else
        begin
          dec(Interval);
          pbIndicator.Repaint;
        end;
    end;
end;

procedure TMainForm.tlbNavigationMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // fix auto chande Down in ToolbarButtons with style Check
  btnPlayForward.Down  := actPlayForward.Checked;
  btnPlayBackward.Down := actPlayBackward.Checked;
end;

procedure TMainForm.actPlayBackwardExecute(Sender: TObject);
begin
  if NextControlAction <> caPlayBackward then
    ReplaceControlActions(caPlayBackward)
  else
    ReplaceControlActions(caNone);
  UpdatePlayActions;
end;

procedure TMainForm.actNavigate_Update(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and (FrameInfoCount > 0);
end;

procedure TMainForm.actUpdate_HaveRecorded(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and (RecordedFrames.Count > 0);
end;

procedure TMainForm.actWorkingSetManagementExecute(Sender: TObject);
begin
  WorkingSetManagementForm.Execute;
end;

procedure TMainForm.actStepNextExecute(Sender: TObject);
begin
  PushControlAction(caStepForward);
  UpdatePlayActions;
end;

procedure TMainForm.actStepPrevExecute(Sender: TObject);
begin
  PushControlAction(caStepBackward);
  UpdatePlayActions;
end;

procedure TMainForm.actStretchImagesExecute(Sender: TObject);
begin
  pbDisplay.Invalidate;
end;

procedure TMainForm.actPlayForwardExecute(Sender: TObject);
begin
  if NextControlAction <> caPlayForward then
    ReplaceControlActions(caPlayForward)
  else
    ReplaceControlActions(caNone);
  UpdatePlayActions;
end;

procedure TMainForm.UpdatePlayActions;
begin
  actPlayForward.Checked := (NextControlAction = caPlayForward);
  actPlayBackward.Checked := (NextControlAction = caPlayBackward);
end;

procedure TMainForm.actPlayUpdate(Sender: TObject);
begin
  actPlay.Enabled := not Exporting and (RecordedFrames.Count > 0);
  actPlay.Checked := Playing;
end;

procedure TMainForm.actPlayExecute(Sender: TObject);
begin
  if Recording then
    begin
      actRecord.Execute;
      AudioRecorder.WaitForStop;
    end;

  Playing := not Playing; // это перед установкой CurrentRecordPosition, чтобы с ним установилась и позиция звука.
  if (CurrentRecordPosition >= RecordedFrames.Count) or AdvertisementShowing then
    CurrentRecordPosition := 0
  else
    CurrentRecordPosition := CurrentRecordPosition; // пинаем позиционирование звука. TODO: это кривовато как-то...

  StockAudioPlayer.Active := Playing;
  // Запуск самого воспроизведения и последующая остановка -
  // через обработчики StockAudioPlayerActivate и StockAudioPlayerDeactivate:
  // запускаем воспроизведение кадров только после того, как звук будет готов воспроизводиться.
  // Но вот останавливать вручную, на всякий случай, будем сразу, а не когда звук соизволит.
  if not Playing then
    begin
      actPlay.Checked := False;

      //CurrentRecordPosition := 0;
      Interval := 0;
    end;
end;

procedure TMainForm.AddNewFrame(AFileName: string);
begin
  DisplayedFrameIndex := FFrameInfoList.Add(
    TFrameInfo.Create(
      ExtractRelativePath(PhotoFolder, ExtractFilePath(AFileName)),
      ExtractFileName(AFileName)
    )
  );
  CurrentWorkingSetFrame := TRecordedFrame.Create(WorkingSetFrames, DisplayedFrameIndex);
end;

procedure TMainForm.Stop;
begin
  if Recording then
    StopRecording;
  if Playing then
    StopPlaying;
end;

function TMainForm.FrameIndexToTimeStamp(AFrameIndex: Integer): string;
var
  d, d2: Integer;
begin
  d := AFrameIndex;
  d2 := d mod FrameRate;
  // frames in last second
  Result := Format('%2.2d', [d2]);
  d := d div FrameRate;
  // seconds
  if d < 60 then
    Result := Format('00:%2.2d', [d]) + ':' + Result
  else
    begin
      d2 := d mod 60;
      // seconds in a last minute
      Result := Format('%2.2d', [d2]) + ':' + Result;
      d := d div 60;
      // minutes
      if d < 60 then
        Result := Format('00:%2.2d', [d]) + ':' + Result
      else
      begin
        d2 := d mod 60;
        // minutes in a last hour
        Result := Format('%2.2d', [d2]) + ':' + Result;
        d := d div 60;
        // hours
        if d < 24 then
          Result := Format('00:%2.2d', [d]) + ':' + Result
        else
        begin
          d2 := d mod 24;
          // hours in a last day
          Result := Format('%2.2d', [d2]) + ':' + Result;
          d := d div 24;
          // days
          Result := Format('%dd', [d]) + ':' + Result;
        end;
      end;
    end;
end;

procedure TMainForm.ClearSound;
begin
  WaveStorage.Wave.Clear;
  RecordedAudioCopy.Clear;
end;

procedure TMainForm.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
resourcestring
  rs_PreloadStatus = 'Загрузка кадра: ';
var
  i: integer;
  FrameInfo: TFrameInfo;
begin
  Done := True;
  if not OutOfMemoryRaised then
    for i := 0 to WorkingSetFrames.Count - 1 do
      begin
        FrameInfo := FrameInfoList[WorkingSetFrames[i].FrameInfoIndex];
        if not FrameInfo.Loaded then
          begin
            SetStatus(rs_PreloadStatus + FrameInfo.RelativeFileName);
            LoadPhoto(WorkingSetFrames[i].FrameInfoIndex);
            pbWorkingSet.Repaint;
            ShowTimes;
            Done := False;
            Exit;
          end;
      end;
end;

procedure TMainForm.AudioRecorderActivate(Sender: TObject);
begin
  Recording := True;
  actRecord.Checked := True;
end;

procedure TMainForm.AudioRecorderDeactivate(Sender: TObject);
begin
  Recording := False;
  actRecord.Checked := False;
  ReplaceControlActions(caNone);

  UpdatePlayActions;

  if WaveStorage.Wave.Empty then
    WaveStorage.Wave.Assign(AudioRecorder.Wave)
  else
    WaveStorage.Wave.Insert(MAXDWORD, AudioRecorder.Wave);
  AudioRecorder.Wave.Clear;
  Saved := False;
  ShowTimes;
end;

procedure TMainForm.AudioRecorderFilter(Sender: TObject; const Buffer: Pointer;
  BufferSize: Cardinal);
begin
  RecordedAudioCopy.Write(Buffer^, BufferSize);
end;

procedure TMainForm.btnBackwardWhilePressedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actBackwardWhilePressed.Checked := True;
end;

procedure TMainForm.btnBackwardWhilePressedMouseLeave(Sender: TObject);
begin
  actBackwardWhilePressed.Checked := False;
end;

procedure TMainForm.btnBackwardWhilePressedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actBackwardWhilePressed.Checked := False;
end;

procedure TMainForm.btnForwardWhilePressedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actForwardWhilePressed.Checked := True;
end;

procedure TMainForm.btnForwardWhilePressedMouseLeave(Sender: TObject);
begin
  actForwardWhilePressed.Checked := False;
end;

procedure TMainForm.btnForwardWhilePressedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actForwardWhilePressed.Checked := False;
end;

procedure TMainForm.btnNavigationMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TToolButton(Sender).Click;
end;

procedure TMainForm.pbIndicatorClick(Sender: TObject);
begin
  mmiDoubleFramerate.Click;
end;

function TMainForm.FindWorkingSetFrameByOffset(AOffset: Integer): TRecordedFrame;
var
  Count, Direction: Integer;
  i: Integer;
  ResultIndex: Integer;
  TargetFrame: TRecordedFrame;
begin
  Result := CurrentWorkingSetFrame;
  if AOffset = 0 then
    Exit;

  if WorkingSetFrames.Count = 0 then
    begin
      Result := nil;
      Exit;
    end;
  if CurrentWorkingSetFrame = nil then
    Exit;

  // TODO: Stop frames. Stop frames must break this incrementing
  ResultIndex := CurrentWorkingSetFrame.Index;
  Count := Abs(AOffset);
  Direction := AOffset div Count;
  for i := 1 to Count do
    begin
      ResultIndex := ResultIndex + Direction;
      while ResultIndex < 0 do
        ResultIndex := WorkingSetFrames.Count + ResultIndex;
      while ResultIndex > (WorkingSetFrames.Count - 1) do
        ResultIndex := ResultIndex - WorkingSetFrames.Count;

      Result := WorkingSetFrames[ResultIndex];

      if TeleportEnabled and
        (FrameInfoList[Result.FrameInfoIndex].Teleport <> -1) and
        (Bookmarks[FrameInfoList[Result.FrameInfoIndex].Teleport] <> -1)
      then
        begin
          TargetFrame := WorkingSetFrames.FindByFrameIndex(Bookmarks[FrameInfoList[WorkingSetFrames[ResultIndex].FrameInfoIndex].Teleport]);
          if Assigned(TargetFrame) then // if can work - it work. if not - not.
            begin
              ResultIndex := TargetFrame.Index;
              Result := TargetFrame;
            end;
        end;
    end;
end;

function TMainForm.IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean;
begin
  if (Message.CharCode <> vk_Left)
    and (Message.CharCode <> vk_Right)
    and (Message.CharCode <> ord('A')) and (Message.CharCode <> ord('C'))
    and (Message.CharCode <> ord('D')) and (Message.CharCode <> ord('M'))
  then
    begin
      Result := inherited IsShortCut(Message);
      // Full Screen menu shortcuts (bookmarks)
      if not Result and (Menu = nil) then
        Result := MainMenu.IsShortCut(Message);
    end
  else
    Result := False;
end;

procedure TMainForm.ClearRecorded;
begin
  RecordedFrames.Clear;
  FCurrentRecordPosition := 0;
  FreeAndNil(AdvertisementFrameImage);
  FreeAndNil(AdvertisementFrameImagePreview);
end;

{ TFrame }

constructor TFrameInfo.Create(APath, AFileName: string);
begin
  FPath := APath;
  FFileName := AFileName;
  Teleport := -1;
end;

destructor TFrameInfo.Destroy;
begin
  FreeAndNil(Preview);
  inherited;
end;

function TFrameInfo.RelativeFileName: string;
begin
  Result := Path + FileName;
end;

function TFrameInfo.FullFileName: string;
begin
  Result := MainForm.PhotoFolder + RelativeFileName;
end;

function TFrameInfo.ImageFromDisc: TGraphic;
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  pic.LoadFromFile(FullFileName);
  try
    Result := TGraphicClass(pic.Graphic.ClassType).Create;
    Result.Assign(pic.Graphic);
  finally
    pic.Free;
  end;
  if Result is TJPEGImage then
    with TJPEGImage(Result) do
      begin
        Performance := jpBestSpeed;
        {$IFNDEF FPC}DIBNeeded;{$ENDIF}
      end;
end;

function TFrameInfo.GenerateStubFrame(ErrorMessage: string): TGraphic;
begin
  Result := TBitmap.Create;
  with TBitmap(Result) do
    begin
      {$IFDEF DelphiXE+}
      SetSize(640,480);
      {$ELSE}
      Width := 640;
      Height := 480;
      {$ENDIF}
      Canvas.TextOut(20, 20, RelativeFileName);
      Canvas.TextOut(20, 80, ErrorMessage);
    end;
end;

{ TRecordedFrameList }

constructor TRecordedFrameList.Create;
begin
  inherited Create(TRecordedFrame);
end;

function TRecordedFrameList.FindByFrameIndex(AFrameInfoIndex: Integer): TRecordedFrame;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FrameInfoIndex = AFrameInfoIndex then
      Exit(Items[i]);
end;

function TRecordedFrameList.GetItems(AIndex: Integer): TRecordedFrame;
begin
  Result := inherited Items[AIndex] as TRecordedFrame;
end;

{ TRecordedFrame }

procedure TRecordedFrame.AssignTo(Dest: TPersistent);
begin
  if Dest is TRecordedFrame then
    TRecordedFrame(Dest).FFrameInfoIndex := FFrameInfoIndex
  else
    inherited;
end;

constructor TRecordedFrame.Create(AFrameList: TRecordedFrameList; AFrameInfoIndex: Integer);
begin
  inherited Create(AFrameList);
  FFrameInfoIndex := AFrameInfoIndex;
end;

procedure TMainForm.actSelectAudioFileUpdate(Sender: TObject);
begin
  actSelectAudioFile.Enabled := PhotoFolder <> '';
  mmiStopRecordingOnSoundtrackFinish.Enabled := actSelectAudioFile.Checked;
end;

end.

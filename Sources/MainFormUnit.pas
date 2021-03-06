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
  Windows, Messages, FileCtrl, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ExtCtrls, ImgList, ExtDlgs, StdCtrls, Contnrs,
  AVICompression, Gauges, Buttons, Math, ComCtrls, mmSystem,
  WaveUtils, WaveStorage, WaveOut, WavePlayers, WaveIO, WaveIn, WaveRecorders, WaveTimer,
  ToolWin, ExtActns, Vcl.StdActns, System.Actions, Vcl.AppEvnts,
  System.Generics.Collections,
  Vcl.Imaging.pngimage, Vcl.Imaging.GIFimg, System.ImageList{$IFDEF Delphi6}, Actions{$ENDIF},
  System.IOUtils, // TPath
  System.JSON;

const
  ControlActionStackDeep = 10;
type
  TControlAction = (caNone, caStepBackward, caStepForward, caPlayBackward, caPlayForward, caPrevFlag, caNextFlag);
  TFrameTipMode = (ftmWorkingSet, ftmRecord);
const
  FrameTipW = 120;
  FrameTipH = 90;
  FrameTipD = 5;
  FrameTipT = 2;

  MaxAudioLineWidth = 1024;

type
  TAfterFrameFlag = (affNone, affTeleport, affStopper, affBouncer, affLooper);

  TFrameInfo = class
  private
    FRelativePath, FFileName: string;
    FTeleportTargetBookmark: Integer;
    FFlagAfter: TAfterFrameFlag;
    procedure SetFlagAfter(const Value: TAfterFrameFlag);
    procedure SetTeleportTargetBookmark(const Value: Integer);
    function GetHaveBouncerAfter: Boolean;
    function GetHaveStopperAfter: Boolean;
    procedure SetHaveBouncerAfter(const Value: Boolean);
    procedure SetHaveStopperAfter(const Value: Boolean);
    function GetHaveLooperAfter: Boolean;
    procedure SetHaveLooperAfter(const Value: Boolean);
  public
    Preview: TBitmap;
    Iconic: TBitmap;
    PreviewLoaded: Boolean;
    property FlagAfter: TAfterFrameFlag read FFlagAfter write SetFlagAfter;
    property TeleportTargetBookmark: Integer read FTeleportTargetBookmark write SetTeleportTargetBookmark; // TODO: move to TRecordedFrame(List) (?)
    function ImageFromDisc: TGraphic;
    function GenerateStubFrame(ErrorMessage: string): TGraphic;
    property HaveStopperAfter: Boolean read GetHaveStopperAfter write SetHaveStopperAfter;
    property HaveBouncerAfter: Boolean read GetHaveBouncerAfter write SetHaveBouncerAfter;
    property HaveLooperAfter: Boolean read GetHaveLooperAfter write SetHaveLooperAfter;
    property FileName: string read FFileName;
    property RelativePath: string read FRelativePath;
    function FullFileName: string;
    function RelativeFileName: string;
    constructor Create(ARelativePath, AFileName: string);
    destructor Destroy; override;
    procedure Unload;
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
    actSave: TAction;
    actSaveAs: TAction;
    actSelectPhotoFolder: TAction;
    mmiSelectPhotoFolder: TMenuItem;
    mmiFiles: TMenuItem;
    mmiNew: TMenuItem;
    mmiOpen: TMenuItem;
    mmiSave: TMenuItem;
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
    mmiSeparatorRecordNavigation: TMenuItem;
    mmiNewBookmark: TMenuItem;
    mmiToggleBookmark0: TMenuItem;
    mmiExport: TMenuItem;
    mmiRecord: TMenuItem;
    actGotoBookmark0: TAction;
    mmiGotoBookmark0: TMenuItem;
    mmiGoToBookmarks: TMenuItem;
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
    mmiTeleports: TMenuItem;
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
    mniSuspendFlags: TMenuItem;
    mmiExportSeparator: TMenuItem;
    mmiSelectAudioFile: TMenuItem;
    actSelectAudioFile: TAction;
    dlgOpenAudio: TOpenDialog;
    dlgSaveAudio: TSaveDialog;
    lblAudioFileName: TLabel;
    N2: TMenuItem;
    mmiUseMicrophone: TMenuItem;
    mmiBookmarks: TMenuItem;
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
    mmiStopRecordingOnSoundtrackFinish: TMenuItem;
    lblWorkPath: TLabel;
    mmiRemoveRecordedFrame: TMenuItem;
    actRemoveRecordedFrame: TAction;
    N5: TMenuItem;
    actReplaceInMovie: TAction;
    mmiReplaceInMovie: TMenuItem;
    mmiSeparatorBookmarkManagement: TMenuItem;
    mmiPrevRecordFrame: TMenuItem;
    mmiNextRecordFrame: TMenuItem;
    mmiIncCurrentStepInterval: TMenuItem;
    mmiDecCurrentStepInterval: TMenuItem;
    mmiShowNeighbourFrames: TMenuItem;
    lvFrameset: TListView;
    ilFrameset: TImageList;
    pnlFramesetMode: TPanel;
    btnFrameSetExpand: TSpeedButton;
    btnFrameSetZoom: TSpeedButton;
    actReloadPhotoFolder: TAction;
    actClearBookmarks: TAction;
    mmiClearFlags: TMenuItem;
    mmiReloadPhotoFolder: TMenuItem;
    actNextRecordFrame: TAction;
    actPrevRecordFrame: TAction;
    imgCamPreview: TImage;
    mmiFramesFromCameraMode: TMenuItem;
    actFramesFromCameraMode: TAction;
    imgOverlay: TImage;
    actShowCameraControl: TAction;
    mmiShowCameraControl: TMenuItem;
    actExportToGif: TAction;
    mmiExportToGif: TMenuItem;
    mmiFillByAllFrames: TMenuItem;
    actFillByAllFrames: TAction;
    actToggleStopper: TAction;
    mmiToggleStopper: TMenuItem;
    actToggleBouncer: TAction;
    mmiToggleBouncer: TMenuItem;
    actToggleLooper: TAction;
    mmToggleLooper: TMenuItem;
    actPrevFlag: TAction;
    mmiPrevFlag: TMenuItem;
    actNextFlag: TAction;
    mmiNextFlag: TMenuItem;
    btnWebCam: TToolButton;
    btnWebCamSetttings: TToolButton;
    btnOnionSkin: TToolButton;
    actToggleOnionSkin: TAction;
    mmiToggleOnionSkin: TMenuItem;
    mmiDisableAdvertisement: TMenuItem;
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
    procedure actToggleStopperExecute(Sender: TObject);
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
    procedure pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgLeftRightByMouseDownControllerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgLeftRightByMouseDownControllerMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mmiExportResolutionClick(Sender: TObject);
    procedure lvFramesetKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure actFramesFromCameraModeUpdate(Sender: TObject);
    procedure actSelectAudioFileUpdate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnBackwardWhilePressedClick(Sender: TObject);
    procedure pnlToollsResize(Sender: TObject);
    procedure actReplaceInMovieUpdate(Sender: TObject);
    procedure actReplaceInMovieExecute(Sender: TObject);
    procedure mmiIncCurrentStepIntervalClick(Sender: TObject);
    procedure actPrevRecordFrameClick(Sender: TObject);
    procedure actNextRecordFrameClick(Sender: TObject);
    procedure mmiDecCurrentStepIntervalClick(Sender: TObject);
    procedure mmiShowNeighbourFramesClick(Sender: TObject);
    procedure btnFrameSetExpandClick(Sender: TObject);
    procedure lvFramesetDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvFramesetDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure actReloadPhotoFolderExecute(Sender: TObject);
    procedure actReloadPhotoFolderUpdate(Sender: TObject);
    procedure actClearBookmarksClick(Sender: TObject);
    procedure actWorkingSetManagementUpdate(Sender: TObject);
    procedure actHaveRecordedFrame(Sender: TObject);
    procedure actFramesFromCameraModeExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actShowCameraControlExecute(Sender: TObject);
    procedure actShowCameraControlUpdate(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actExportToGifClick(Sender: TObject);
    procedure actRecordUpdate(Sender: TObject);
    procedure actFillByAllFramesExecute(Sender: TObject);
    procedure actToggleBouncerExecute(Sender: TObject);
    procedure actToggleLooperExecute(Sender: TObject);
    procedure lvFramesetDblClick(Sender: TObject);
    procedure actPrevFlagUpdate(Sender: TObject);
    procedure actPrevFlagExecute(Sender: TObject);
    procedure actRemoveRecordedFrameExecute(Sender: TObject);
    procedure actRemoveRecordedFrameUpdate(Sender: TObject);
    procedure actToggleOnionSkinExecute(Sender: TObject);
    procedure actToggleOnionSkinUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mmiDisableAdvertisementClick(Sender: TObject);
  private
    NextControlActionStack: array [1..ControlActionStackDeep] of TControlAction;
    NextControlActionStackPosition: Integer;
    ExportCancelled: Boolean;
    //CameraWaiting: Boolean; // TODO: ������� ������� ��� ��������� ����� � ������ � ���������� ��� ��� ����
    function NextControlAction: TControlAction;
    procedure PopControlAction;
    procedure PushControlAction(Value: TControlAction);
    procedure ReplaceControlActions(Value: TControlAction);
    procedure ClearSound;
    function FrameIndexToTimeStamp(AFrameIndex: Integer; AShowZeroFrameIndex: Boolean = True): string;
    procedure Stop;
    procedure SaveWorkingSet(ANewProjectFileName: string = '');
    function CheckBeforeOpenAudio: Boolean;
    procedure SetCameraPhotoFolder;
  private
    ProjectFileName: string;
    FFrameInfoList: TObjectList;
    FWorkingSetFrames: TRecordedFrameList;
    FRecordedFrames: TRecordedFrameList;
    KeyPressBlocked: Boolean;
    Interval, CurrentSpeedInterval: Integer;
    Recording, RecordingMustBeChanged, Playing, Exporting: Boolean;
    Options: TAVIFileOptions;
    Compressor: TAVICompressor;
    OptionsInitialized: Boolean;
    LoopMode: Boolean;
    FrameTipMode: TFrameTipMode;
    FrameTipArrow: Integer;
    const
      BookmarkKey: array [0..19] of Char = (
        '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
        'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
      );
  private
    Bookmarks: array [0..19] of Integer; // TODO: move to TRecordedFrame(List) (?)
     // RecordedAudioCopy - �������� ����� �����. �� ���� ������ ������ ����������� �� AudioRecorder
     // ������� ��������� ������ ������������� �� ���� �����.
     // ��� �������� ������ ������ ���������������� ������ ������������ � WaveStorage �����.
     // �� ���������� ������ � WaveStorage ���������� ����, ����������
     // � AudioRecorder-� � ��� WaveStorage �������� RecordedAudioCopy.
     // ��� ��������������� ������������� WaveStorage ��� ������ StockAudioPlayer
     // ���������� ����� ����� �� ����� �������� LiveAudioRecorder
     // �� ����������� ������ ����� � �������.
    RecordedAudioCopy: TMemoryStream;
    // ����������� ������������ ������� ������ ��������� ��� ��������� �����.
    // ��� ��� RecordedAudioCopy, ������ ��� ����������� �� �������� �������,
    // ������� ��� ���������
    RecordedAudioLines: TList<Integer>;
    pbRecordOffset: Integer;
    OutOfMemoryRaised: Boolean;
    PreviousBounds: TRect;
    FFrameTipRecordedFrame: TRecordedFrame;
    FCurrentRecordPosition: Integer;
    FCurrentWorkingSetFrame: TRecordedFrame;
    AdvertisementFrameBottom: Integer;
    AdvertisementFrameImage: TBitmap;
    AdvertisementFrameTipShowing: Boolean;
    FirstFrameSize: TSize;
    FExternalAudioFileName: string;
    FDisplayedFrameIndex: Integer;
    FPhotoFolder: string;
    FSaved: Boolean;
    procedure SetSaved(const Value: Boolean);
    procedure CheckStopCamera;
    function OnSaveAsCloseQuery(const ANewMovieName: string): Boolean;
    function FindFrameInfo(const ARelativePath, AFileName: string): Integer;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure DoCancelExport(Sender: TObject);
    procedure MakeRecordedAudioLines;
    function WrapFrameIndex(AIndex: Integer): Integer;
    function FindNearestLooper(AIndex, ADirection: Integer): Integer;
    property Saved: Boolean read FSaved write SetSaved;
    procedure CaptureFirstFrameSizes;
    procedure SetCurrentRecordPosition(const Value: Integer);
    procedure SetCurrentWorkingSetFrame(const Value: TRecordedFrame);
    procedure UpdatePlayActions;
    procedure StopRecording;
    procedure StopPlaying;
    procedure SetDisplayedFrameIndex(Value: Integer);
    procedure SwitchToMicrophoneUsage(AKeepOpenedWave: Boolean);
    procedure RepaintAll;
    procedure ClearBookmarks;
    procedure SaveBeforeClose(const APurpose: string);
    function CalculateSoundFramesCount: Integer;
    procedure ChangeCurrentRecordPosition(ANewRecordPosition: Integer; AChangeDisplayedFrame: Boolean = True);
    procedure FreeAdvertisementFrame;
    procedure LoadPhotoFolder;
    procedure ClearFlags;
    procedure SetPhotoFolder(const Value: string);
    procedure CameraFormActiveChanged(Sender: TObject);
    property CurrentRecordPosition: Integer read FCurrentRecordPosition write SetCurrentRecordPosition;
    procedure SetFrameTipRecordedFrame(const Value: TRecordedFrame);
    function FlagsEnabled: Boolean;
    procedure OpenMovie(AFileName: string);
    procedure OpenAudio(AFileName: string);
    property FrameTipRecordedFrame: TRecordedFrame read FFrameTipRecordedFrame write SetFrameTipRecordedFrame;
    function GetFrameInfo(Index: Integer): TFrameInfo;
    function GetFrameInfoCount: Integer;
    procedure UnloadFrames;
    procedure ClearRecorded;
    function WorkingFrameIndexToWorkingSetX(AWorkingSetFrameIndex: Integer): Integer;
    function WorkingSetXToWorkingFrame(X: Integer): TRecordedFrame;
//    Drawing: Boolean;
    procedure OpenNewPhotoFolder(ANewPhotoFolder: string);
    procedure RecalculatePreview;
    procedure CreateAdvertisementFrame;
    procedure ShowTimes;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    function MoveToFrameByOffset(AInitialIndex, AOffset: Integer; ASkipAutomovementFlags, AFollowFlags: Boolean): TRecordedFrame;
  public
    AdvertisementFrameImagePreview: TBitmap;
    AdvertisementShowing: Boolean;
    AdvertisementEnabled: Boolean;
    AdvertisementDuration: Integer;
    ExportSize: TSize;
    AutoMovementStopped: Boolean;
    SkipFirstStopper: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
    procedure UpdateCaption;
    procedure SetStatus(const Value: string);
    property PhotoFolder: string read FPhotoFolder write SetPhotoFolder;
    property DisplayedFrameIndex: Integer read FDisplayedFrameIndex write SetDisplayedFrameIndex;
    property CurrentWorkingSetFrame: TRecordedFrame read FCurrentWorkingSetFrame write SetCurrentWorkingSetFrame;
    function FindWorkingSetFrameByOffset(AOffset: Integer): TRecordedFrame;
    property WorkingSetFrames: TRecordedFrameList read FWorkingSetFrames;
    property RecordedFrames: TRecordedFrameList read FRecordedFrames;
    property FrameInfoCount: Integer read GetFrameInfoCount;
    property FrameInfoList[Index: Integer]: TFrameInfo read GetFrameInfo;
    procedure AutoMove(AOffset: Integer);
    // Used in ScreenForm
    procedure LoadPhoto(AFrameInfoIndex: Integer; AWorkingSetIndex: Integer);
    procedure AddNewFrame(AFileName: string);
  end;

var
  MainForm: TMainForm;
  SettingsChanged: Boolean;

var
  FrameRate: byte = 25;

implementation
uses
  ControllerFormUnit, ScreenFormUnit, Vcl.Imaging.JConsts,
  ExportSizeCustomRequestDialogUnit, ShellAPI, WorkingSetManagementFormUnit,
  CameraFormUnit, MP3ConvertFormUnit, MovieNameDialogUnit, IniFiles,
  ProgressFormUnit, UtilsUnit, GifPreviewUnit;
{$R *.dfm}

function Size(AX, AY: Integer): TSize;
begin
  Result.cx := AX;
  Result.cy := AY;
end;

function TMainForm.CheckBeforeOpenAudio: Boolean;
resourcestring
  rs_SaveAudioBeforeOpenRequest = '������ ��������� ���������� ������� ����� ������������ �������?';
begin
  Stop;
  Result := True;
  if (WaveStorage.Wave.Length > 0) and (FExternalAudioFileName = '') then
    case MessageDlg(rs_SaveAudioBeforeOpenRequest, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrCANCEL:
        Result := False;
      mrYES:
        begin
          if not dlgSaveAudio.Execute then
            Result := False
          else
            WaveStorage.Wave.SaveToFile(dlgSaveAudio.FileName);
        end;
      IDNO:
        ;
    end;
end;

procedure TMainForm.actSelectAudioFileExecute(Sender: TObject);
begin
  if not CheckBeforeOpenAudio then
    Abort;

  if not dlgOpenAudio.Execute then
    Abort;
  OpenAudio(dlgOpenAudio.FileName);
end;

procedure TMainForm.SaveBeforeClose(const APurpose: string);
resourcestring
  rs_SaveMovieBeforeSomethingRequest = '������ ��������� ������� ����� ����� ';
begin
  if (RecordedFrames.Count > 0) and not Saved then
    case MessageDlg(
      rs_SaveMovieBeforeSomethingRequest + APurpose + '?',
      mtConfirmation,
      [mbYes, mbNo, mbCancel],
      0
    ) of
      mrCancel: Abort;
      mrYes:
        begin
          actSave.Execute;
          if not Saved then
            Abort;
        end;
    end;
end;

procedure TMainForm.actSelectPhotoFolderClick(Sender: TObject);
var
  NewPhotoFolder: string;
  Directories: TArray<string>;
resourcestring
  rs_SelectPhotoFolderCaption = '� ����� ����� ������� �����, ������� � ������/��������� �����?';
  rs_SaveBeforeChooseFolderRequest = '��������� ������';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeChooseFolderRequest);

  NewPhotoFolder := TPath.GetDocumentsPath + '\MultiPult\';
  ForceDirectories(NewPhotoFolder);
  if SelectDirectory(
      NewPhotoFolder,
      Directories,
      [],
      rs_SelectPhotoFolderCaption
//     rs_SelectPhotoFolderCaption, '', NewPhotoFolder
//     {$IFDEF DelphiXE}
//     , [sdNewFolder, sdShowFiles, sdShowEdit, (*sdShowShares, *) sdValidateDir, sdNewUI]
//     {$ENDIF}
  ) then begin
    NewPhotoFolder := Directories[0];
    OpenNewPhotoFolder(NewPhotoFolder + '\');
  end;
end;

procedure TMainForm.actFramesFromCameraModeUpdate(Sender: TObject);
begin
  actFramesFromCameraMode.Enabled := not Exporting;
  actFramesFromCameraMode.Checked := CameraForm.Active;
end;

procedure TMainForm.actShowCameraControlExecute(Sender: TObject);
begin
  CameraForm.Visible := not CameraForm.Visible;
end;

procedure TMainForm.actShowCameraControlUpdate(Sender: TObject);
begin
  actShowCameraControl.Checked := CameraForm.Visible;
  actShowCameraControl.Enabled := CameraForm.Active and not Exporting and (PhotoFolder <> '');
end;

procedure TMainForm.actShowControllerFormExecute(Sender: TObject);
begin
  ControllerForm.Visible := not ControllerForm.Visible;
end;

procedure TMainForm.actShowControllerFormUpdate(Sender: TObject);
begin
  actShowControllerForm.Checked := ControllerForm.Visible;
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
    InfoMsg('���� "' + FileName + '" �� ����������.');
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
  AdvertisementEnabled := True;
  Saved := True;
  FFrameInfoList := TObjectList.Create(True);
  FRecordedFrames := TRecordedFrameList.Create;
  FWorkingSetFrames := TRecordedFrameList.Create;
  RecordedAudioCopy := TMemoryStream.Create;
  RecordedAudioLines := TList<Integer>.Create;
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

procedure TMainForm.ClearFlags;
var
  i: Integer;
begin
  for i := 0 to FrameInfoCount - 1 do
    FrameInfoList[i].FlagAfter := affNone;
end;

procedure TMainForm.CreateAdvertisementFrame;

resourcestring
  rs_AdFrame1 = '����� ������ �� ��������� ������ � �������';
  rs_AdFrame2 = '��� ������ ������������� ��������� �����������';
  rs_AdFrame3 = '������ %s (%s)';
  rs_AdFrame4 = 'http://MultiStudia.ru';

var
  TextTop: Integer;

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

  AdvertisementFrameImage := TBitmap.Create;
  with AdvertisementFrameImage do
    begin
      {$IFDEF DelphiXE+}
      SetSize(ExportSize.Width, ExportSize.Height);
      {$ELSE}
      Width := ExportSize.Width;
      Height := ExportSize.Height;
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
  Saved := False;
  OldCurrentFrame.Free;
end;

procedure TMainForm.actFillByAllFramesExecute(Sender: TObject);
var
  i, k: Integer;
begin
  actNew.Execute;
  for i := 0 to FWorkingSetFrames.Count - 1 do
    for k := 1 to CurrentSpeedInterval do
      TRecordedFrame.Create(RecordedFrames, FWorkingSetFrames[i].FrameInfoIndex);
  Saved := False;
  RepaintAll;
end;

procedure TMainForm.actMoveFrameLeftClick(Sender: TObject);
begin
  Stop;
  if CurrentWorkingSetFrame.Index > 0 then
    CurrentWorkingSetFrame.Index := CurrentWorkingSetFrame.Index - 1
  else
    CurrentWorkingSetFrame.Index := WorkingSetFrames.Count - 1;
  Saved := False;
  RepaintAll;
end;

procedure TMainForm.actMoveFrameRightClick(Sender: TObject);
begin
  Stop;
  if CurrentWorkingSetFrame.Index < (WorkingSetFrames.Count - 1) then
    CurrentWorkingSetFrame.Index := CurrentWorkingSetFrame.Index + 1
  else
    CurrentWorkingSetFrame.Index := 0;
  Saved := False;
  RepaintAll;
end;

procedure TMainForm.actPrevRecordFrameClick(Sender: TObject);
begin
  ChangeCurrentRecordPosition(CurrentRecordPosition - 1);
end;

procedure TMainForm.mmiShowNeighbourFramesClick(Sender: TObject);
begin
  RepaintAll;
end;

procedure TMainForm.actNextRecordFrameClick(Sender: TObject);
begin
  ChangeCurrentRecordPosition(CurrentRecordPosition + 1);
end;

procedure TMainForm.mmiIncCurrentStepIntervalClick(Sender: TObject);
begin
  Inc(CurrentSpeedInterval);
  pbWorkingSet.Repaint;
end;

procedure TMainForm.mmiDecCurrentStepIntervalClick(Sender: TObject);
begin
  Dec(CurrentSpeedInterval);
  if CurrentSpeedInterval < 1  then CurrentSpeedInterval := 1;
  pbWorkingSet.Repaint;
end;

procedure TMainForm.mmiDisableAdvertisementClick(Sender: TObject);
begin
  AdvertisementEnabled := False;
  if AdvertisementShowing then
    ChangeCurrentRecordPosition(RecordedFrames.Count - 1)
  else
    pbRecord.Repaint;
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
  FreeAndNil(RecordedAudioLines);
  FreeAndNil(FWorkingSetFrames);
  FreeAndNil(FRecordedFrames);
  FreeAndNil(FFrameInfoList);
  inherited Destroy;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CameraForm.Active := False;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  rs_SaveBeforeExit =
    '�� ���������� ���������, � �� ����� ��� ���������� ���� ������� ��� �� ��������.'#13#10+
    '���� ��� �� ��������� ������, �� �� �������.'#13#10+
    '������� ��� ���������, ������ ��� ������� ���������?';
begin
  Stop;

  actSaveAs.Update;
  if actSaveAs.Enabled and not Saved then
    case MessageDlg(rs_SaveBeforeExit, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYES: begin actSaveAs.Execute; CanClose := Saved; end;
      mrNO: CanClose := True;
      mrCANCEL: CanClose := False;
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

begin
  // �� ������, ���� ���-�� ����� �� ���
  VersionNameString := '0.9.???';
  VersionCopyrightString := '������������, ������, 20??';
  TakeVersionInfo;

  for i := Low(Bookmarks) to High(Bookmarks) do
    AppendBookmarkMenu(i);

  mmiToggleBookmark0.Free;
  mmiGotoBookmark0.Free;
  mmiToggleTeleport0.Free;

  try
    LiveAudioRecorder.Active := True;
    LiveAudioRecorder.WaitForStart;
  except
    // ������ ������, ���� ��������� ���
  end;

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
      OpenNewPhotoFolder(ParamStr(1))
    else
      OpenMovie(ParamStr(1));
  imgCamPreview.Picture.Graphic.Width := 1;
  // �������������� ���������� ��� �������� �� ���������.
  mmiExportResolutionVGA.Click;
  Saved := True;
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
  rs_SaveBeforeChooseFolderRequest = '��������� ������ � ������ ����� ������';
  rs_SaveBeforeOpenRequest = '��������� �������';
begin
  Stop;

  FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0); // Get count of files
  for i := 0 to FileCount-1 do
    begin
      DragQueryFile(Msg.Drop, i, @FileName, SizeOf(FileName)); // Get N file
      if DirectoryExists(FileName) then
        begin
          SaveBeforeClose(rs_SaveBeforeChooseFolderRequest);
          OpenNewPhotoFolder(string(FileName) + '\')
        end
      else if LowerCase(ExtractFileExt(FileName)) = '.mp' then
        begin
          SaveBeforeClose(rs_SaveBeforeOpenRequest);
          OpenMovie(FileName)
        end
      else if LowerCase(ExtractFileExt(FileName)) = '.wav' then
        begin
          if PhotoFolder <> '' then
            if CheckBeforeOpenAudio then
              OpenAudio(FileName)
        end
      else if LowerCase(ExtractFileExt(FileName)) = '.mp3' then
        begin
          if PhotoFolder <> '' then
            if CheckBeforeOpenAudio then
              OpenAudio(FileName)
        end
      else
        Beep;
    end;
  DragFinish(Msg.Drop); // end drag handling, release drag source window
end;

procedure TMainForm.WMMove(var Msg: TWMMove);
begin
  inherited;
  SettingsChanged := True;
end;

procedure TMainForm.ChangeCurrentRecordPosition(ANewRecordPosition: Integer; AChangeDisplayedFrame: Boolean = True);
begin
  if AdvertisementEnabled then
  begin
    if ANewRecordPosition > RecordedFrames.Count then
      ANewRecordPosition := RecordedFrames.Count;
  end else begin
    if ANewRecordPosition >= RecordedFrames.Count then
      ANewRecordPosition := RecordedFrames.Count - 1;
  end;
  if ANewRecordPosition < 0 then
    ANewRecordPosition := 0;

  if AChangeDisplayedFrame then
    begin
      AdvertisementShowing := (ANewRecordPosition >= RecordedFrames.Count);
      if not AdvertisementShowing and (ANewRecordPosition < RecordedFrames.Count) then
        begin
          DisplayedFrameIndex := RecordedFrames[ANewRecordPosition].FrameInfoIndex;
        end;
    end;

  CurrentRecordPosition := ANewRecordPosition;
  RepaintAll;
  UpdateActions;
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
    end
  else
    if not KeyPressBlocked then
      case Key of
        vk_Left:   begin PushControlAction(caStepBackward); KeyPressBlocked := True; end;
        vk_Right:  begin PushControlAction(caStepForward); KeyPressBlocked := True; end;
        VK_ESCAPE: if actFullScreenMode.Checked then actFullScreenMode.Execute; // TODO: ��� ��� ��� ����� ��������� ��������?
        VK_SHIFT, VK_CAPITAL:  pbWorkingSet.Repaint; // ��������� �����������
      end;

  UpdatePlayActions;
  pbIndicator.Repaint;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressBlocked := False; // ��������� ���������� ��������� ����������� ������� �������. TODO: �������������� �� ����� �������, � ��, ��� �����������. ���� �� ��������������, ��������, ��� ������ ��� ���������� ����-�� �������.
  case Key of
    VK_SHIFT, VK_CAPITAL: pbWorkingSet.Repaint; // ��������� ����������
  end;
//  pbIndicator.Refresh;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Playing or lvFrameset.Visible then
    Exit;

  if ssCtrl in Shift then
    if WheelDelta > 0 then
      ChangeCurrentRecordPosition(CurrentRecordPosition - 1)
    else
      ChangeCurrentRecordPosition(CurrentRecordPosition + 1)
  else
    if WheelDelta > 0 then
      actStepPrev.Execute
    else
      actStepNext.Execute;
  Handled := True;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CameraForm.OnNewFrame := AddNewFrame;
  CameraForm.OnActiveChanged := CameraFormActiveChanged;
  CameraForm.imgCamPreview := imgCamPreview;
  CameraForm.imgOverlay := imgOverlay;
  CameraForm.DisablePhotoFolderLookup;
  if PhotoFolder <> '' then
    SetCameraPhotoFolder;

  ScreenForm.OnKeyDown := FormKeyDown;
  ScreenForm.OnKeyUp := FormKeyUp;
  ScreenForm.OnDblClick := actFullScreenModeExecute;

  LoadSettings;
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
  AutoMovementStopped := False;
  SkipFirstStopper := True;
end;

procedure TMainForm.UnloadFrames;
begin
  ClearRecorded;
  ClearSound;

  WorkingSetFrames.Clear;
  FFrameInfoList.Clear;
  DisplayedFrameIndex := -1;
  FCurrentWorkingSetFrame := nil;
  FFrameTipRecordedFrame := nil;
  OutOfMemoryRaised := False;
end;

function CompareFramesFileName(Item1, Item2: Pointer): Integer;

  function ExtractInt(const s: string; var PosDif: Integer): Integer;
  begin
    Result := 0;
    while CharInSet(s[PosDif], ['0'..'9']) do
      begin
        Result := Result * 10 + (Ord(s[PosDif])-ord('0'));
        Inc(PosDif);
      end;
  end;

  function CopyTillSlash(const s: string; APos: Integer): string;
  var
    i: Integer;
  begin
    i := Pos('\', s);
    if i = 0 then
      i := length(s);
    Result := Copy(s, APos, i - APos);
  end;

  function CompareStringWithInt(const s1, s2: string): Integer;
  var
    i, PosDif, PosDif1, PosDif2: Integer;
  begin
    PosDif := 0;
    i := 1;
    while (s1[i] <> #0) and (s2[i] <> #0) do
      begin
        // ��������������� �� ������ �� ���������� �������
        if (s1[i] <> s2[i]) then
          begin
            PosDif := i;
            // ���� ���� ���� �� ������������ �������� - �����, ������ �������� ������ � �����, � �������� �� ����������
            if CharInSet(s1[PosDif], ['0'..'9']) or CharInSet(s2[PosDif], ['0'..'9']) then
              // ���� ���������� ���������� ������� ���� ����� - ������������ � ������ ����� ����� ����
              // �� ����, � ������ ������� ������������ ������������� �����.
              // ������ �� ������ ����� ���� ������, ����������, �����, �� ��� � ���� ��������� ���� �� �������.
              // ������� ������:
              //   img1001.jpg > img101.jpg ���������� 3-� ���� �����, � ������� �� ������, �� ������ ������ �����
              //   img0002.jpg > img001.jpg ���������� 3-� ���� �����, � ������� �� ������, �� ������ ������ �����
              //   img1012.jpg > img101.jpg ���������� 4-� ���� �����, � ������� ����� �� ���� ����� �� �����
              //   img10a.jpg > img10b.jpg ����� �� ����������, �� ���������� ���������� �����
              //   img010a.jpg > img10b.jpg ����� �� �������� �� ����������, �� ���������� ���������� �����
              //   img010a1.jpg > img10a02.jpg ������ ����� �� ���������� �� ����, �� �� �� ��������, � � ���������� ������ ���������� �����.
              //   img010.jpg > img10.jpg ����� �� �������� �� ����������, � �� ���������� ���������� �����, �� ������� ������ ���� �� ���� ���-��, �� ��������� � ����������
              // ��-�� �������� fld010\img1.jpg > fld10\img02.jpg, ��� ������ �����
              // ���������� �� ����, �� �� �� ��������, � � ���������� ������ ���������� �����,
              // �� ����� ��� ������, � ��������� �� ���������� �� ���� ��,
              // ���� ���������� ����� ��� ��� �� �����������, ������� ��� ����� CopyTillSlash.
              while (PosDif > 1) and CharInSet(s1[PosDif - 1], ['0'..'9']) do
                dec(PosDif);
            Break;
          end
        else
          Inc(i);
      end;

    // ���� �� ����� ����� �� ����� �� ������� ��������, �� PosDif ������� 0.
    if PosDif = 0 then
    begin
      if (s1[i] = #0) and (s2[i] = #0) then // ��������� ��� ������������ - ������, �����
        Exit(0)
      else if s1[i] = #0 then // ��������� ������, ������ ��� ���, ������ ������ ������
        Exit(-1)
      else
        Exit(1); // ��������� ������, ������ ������ ������.
    end;

    // ���� ���� ����-�� ������� � ���-�� ������������, �� �������, �� ����� �� ������ ��������
    if CharInSet(s1[PosDif], ['0'..'9']) and CharInSet(s2[PosDif], ['0'..'9']) then
      begin
        PosDif1 := PosDif;
        PosDif2 := PosDif;
        Result := ExtractInt(s1, PosDif1) - ExtractInt(s2, PosDif2);
        // ����� ����� ��������� ����������, �� ��-������� ����������,
        // ��������, � ������ ����������� ������� �����.
        // � ���� ������ ����� �������� ������� ������ (�� ���������� '\', ���� �� ����
        //   - ��. ���� ������ ��� fld010\img1.jpg > fld10\img02.jpg!),
        // � ���� ��� ������� �� ������, �� �������� ����� ����� ������ ������� � ��������� ������.
        // ����� �������� � ��� ���������� ������, ��� ������.
        // ������� ���������� �����:
        // 1��
        // 01�� // ����� ��� � 1��, ����� �� �����������, �� ������ ������ � �����
        // 1��  // ����� ��� � 1�� � 01��, �� ������ ������ - ������, ��� ��
        // 01�� // ����� ��� � ����, � ������ - ��� ������, ��� � ����.
        if Result = 0 then
          Result := CompareStringWithInt(CopyTillSlash(s1, PosDif1), CopyTillSlash(s2, PosDif2));
        if Result = 0 then
          Result := PosDif1 - PosDif2;
        Assert(Result <> 0, 'Need to find more differences in "' + s1 + '" and "' + s2 + '"');
      end
    else
      // ���� ���� ���� �� �����, �� ������ ��� ������, �� �������� ���������
      // ��� ������, � ������ ������� ������� �������� (�� ��� ������������ �������������������,
      // ��� ��� ��� �� ����� ��� �������� AnsUpperCase)
      // � ����� � ������ �����, �� ������� ������� � ���� �������� �������,
      // ��� ���� ��� ���������� ������ �������� � ���� �����
      // ������, ����������� ����� � ���� ������ ����� ��������� ����� ������
      // ���������, ���� ��������, ��������, ������� ����� �� �� 1-�� ������.
      Result := AnsiCompareFileName(StringReplace(s1, '\', #1, [rfReplaceAll]), StringReplace(s2, '\', #1, [rfReplaceAll]));
  end;

begin
  if Item1 = Item2 then
    Exit(0);
  Result := CompareStringWithInt(AnsiUpperCase(TFrameInfo(Item1).RelativeFileName), AnsiUpperCase(TFrameInfo(Item2).RelativeFileName));
end;

function TMainForm.FindFrameInfo(const ARelativePath, AFileName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FrameInfoCount - 1 do
    with FrameInfoList[i] do
      if (RelativePath = ARelativePath) and (FileName = AFileName) then
        Exit(i);
end;


procedure TMainForm.LoadPhotoFolder;

resourcestring
  rs_ScaningStatus = '������ �����: ';

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
             (ext = '.jpe') or
             (ext = '.jpeg') or
             (ext = '.bmp') or
             (ext = '.png') or
             (ext = '.gif') or
             (ext = '.wmf') or
             (ext = '.emf')
          then
            begin
              if FindFrameInfo(ARelativePath, Rec.Name) = -1 then // ��� ��� ������������ ����� ���������
                FFrameInfoList.Add(TFrameInfo.Create(ARelativePath, Rec.Name));
            end;
          if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and (Rec.Name <> '..') then
            InternalLoadDirectory(ARelativePath + Rec.Name + '\');
        until {$IFDEF FPC}FindNextUTF8{$ELSE}FindNext{$ENDIF}(Rec) <> 0;
        {$IFDEF FPC}FindCloseUTF8{$ELSE}FindClose{$ENDIF}(Rec);
      end;
  end;

  procedure FillWorkingSet;
  var
    PrevFramePath: string;
    NextBookmarkIndex: Integer;
    i: Integer;
  begin
    if FFrameInfoList.Count > 0 then
      PrevFramePath := TFrameInfo(FFrameInfoList[0]).RelativePath;
    NextBookmarkIndex := 0;
    for i := 0 to FFrameInfoList.Count - 1 do
    begin
      if (PrevFramePath <> TFrameInfo(FFrameInfoList[i]).RelativePath) and (NextBookmarkIndex <= High(Bookmarks)) then
      begin
        Bookmarks[NextBookmarkIndex] := i;
        inc(NextBookmarkIndex);
        PrevFramePath := TFrameInfo(FFrameInfoList[i]).RelativePath;
      end;
      TRecordedFrame.Create(WorkingSetFrames, i);
    end;
  end;

begin
  WorkingSetFrames.Clear;
  InternalLoadDirectory('');
  FFrameInfoList.Sort(CompareFramesFileName);
  FillWorkingSet;
  ShowTimes;
end;

procedure TMainForm.OpenNewPhotoFolder(ANewPhotoFolder: string);
begin
  ExportCancelled := True;
  actNew.Execute;
  ClearBookmarks;
  PhotoFolder := ANewPhotoFolder;
  if PhotoFolder[Length(PhotoFolder)] <> '\' then
    PhotoFolder := PhotoFolder + '\';

  lblWorkPath.Caption := PhotoFolder;

  UpdateCaption;

  UnloadFrames;
  LoadPhotoFolder;

  DisplayedFrameIndex := 0; // same as WorkingSetFrames[0].FFrameInfoIndex;
  Saved := True;
  CaptureFirstFrameSizes;

  UpdateActions;
  RepaintAll;
end;

procedure TMainForm.actReloadPhotoFolderExecute(Sender: TObject);
resourcestring
  rs_RelopadPhotoFolder =
    '��� ������������ ����� ����� ����������� �� ������ ������,'+#13#10+
    '� �������� ����� ���������� �� ������ ������ �������� �����.'+#13#10+
    '�������������?';
begin
  Stop;
  case MessageDlg(rs_RelopadPhotoFolder, mtConfirmation, [mbOk, mbCancel], 0) of
    IDCANCEL: Abort;
  end;
  ClearBookmarks;
  UnloadFrames;
  LoadPhotoFolder;
  if lvFrameset.Visible then begin
    lvFrameset.Visible := False;
    btnFrameSetExpand.Click;
  end;
end;

procedure TMainForm.actReloadPhotoFolderUpdate(Sender: TObject);
begin
  actReloadPhotoFolder.Enabled := not Exporting and (PhotoFolder <> '');
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
resourcestring
  rs_AboutText =
    '��������� ��� ������, ������ � ������� ���������. ������ � �������� �������!'#13#10 +
    '������ %s'#13#10 +
    '�����: ���� ������� (http://innenashev.narod.ru)'#13#10 +
    '�� ������ ������������ (http://multistudia.ru)'#13#10 +
    '� ���� ������� ����������� ��������'#13#10 +
    ''#13#10 +
    '�������� ��� ��������� �������� ��� ��������� � ���������'#13#10 +
    '�� ������ https://github.com/Nashev/MultiPult'#13#10 +
    ''#13#10 +
    '(� �� ������, ��� Ctrl+C � �������� ������� ��������?)';
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

procedure TMainForm.actClearBookmarksClick(Sender: TObject);
begin
  ClearBookmarks;
  ClearFlags;
  pbWorkingSet.Repaint;
end;

procedure TMainForm.mmiNewBookmarkClick(Sender: TObject);
var
  i: Integer;
begin
  // ������� ��������� ����� ��������, ���� ��� ��� ��� ����. ����� Enter ���� ������������ ��������
  for i := Low(Bookmarks) to High(Bookmarks) do
    if Bookmarks[i] = DisplayedFrameIndex then
      begin
        Bookmarks[i] := -1;
        pbWorkingSet.Repaint;
        Saved := False;
        Exit;
      end;

  // ���� �� ���� �������� - �� ������ ������ ���������
  for i := Low(Bookmarks) to High(Bookmarks) do
    if Bookmarks[i] = -1 then
      begin
        Bookmarks[i] := DisplayedFrameIndex;
        pbWorkingSet.Repaint;
        Saved := False;
        Exit;
      end;

  Beep; // ���� ��������� �������� �� ������� - �����.
end;

procedure TMainForm.mmiUseMicrophoneClick(Sender: TObject);
begin
  actNew.Execute;
  SwitchToMicrophoneUsage(False);
end;

procedure TMainForm.SwitchToMicrophoneUsage(AKeepOpenedWave: Boolean);
resourcestring
  rs_MicInitError = '�� ������� �������� ��������';
begin
  if not AKeepOpenedWave then
    begin
      WaveStorage.Wave.Clear;
      RecordedAudioCopy.Clear;
      RecordedAudioLines.Clear;
    end;
  try
    LiveAudioRecorder.Active := True;
    LiveAudioRecorder.WaitForStart;

    FExternalAudioFileName := '';
    actSelectAudioFile.Checked := False;
    mmiUseMicrophone.Checked := True;
    lblAudioFileName.Visible := False;
    pbRecord.Repaint;
    ShowTimes;
  except
    InfoMsg(rs_MicInitError)// ������ ������, ���� ��������� ���
  end;
end;

procedure TMainForm.btnBackwardWhilePressedClick(Sender: TObject);
begin
  ReplaceControlActions(caNone);
  UpdatePlayActions;
end;

procedure TMainForm.actDoubleFramerateExecute(Sender: TObject);
resourcestring
  rs_ConfirmNewBeforeChangeFramerate =
    '�� ������� ������� ������� ������. �������� ������ ������� � ������� �����.'#13#10+
    '�������� ������� ������� ������ � ������ ����� �����?';
begin
  Stop;
  actSaveAs.Update;
  if actSaveAs.Enabled and Saved then // �� ������ not saved ������� actNew.Execute
    case MessageDlg(rs_ConfirmNewBeforeChangeFramerate, mtConfirmation, [mbOk, mbCancel], 0) of
      mrCANCEL: Exit;
    end;

  actNew.Execute;

  if FrameRate = 25 then
    FrameRate := 50
  else
    FrameRate := 25;
  actDoubleFramerate.Checked := (FrameRate = 50);

  MultimediaTimer.Interval := 1000 div FrameRate;
  RepaintAll;
  ShowTimes;
end;

procedure TMainForm.actPrevFlagExecute(Sender: TObject);
begin
  PushControlAction(caPrevFlag);
  UpdatePlayActions;
end;

procedure TMainForm.actPrevFlagUpdate(Sender: TObject);
begin
  PushControlAction(caNextFlag);
  UpdatePlayActions;
end;

procedure TMainForm.actPreviewModeExecute(Sender: TObject);
begin
  RecalculatePreview;
  FreeAdvertisementFrame;
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
//  TMainForm(lpData).SetStatus('����������� �����: ' + FloatToStrF(TotalBytesTransferred / TotalFileSizeTotalFileSize) * 100, ffFixed, 0, 2) + '%';
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
  rs_ExportSelectFolderCaption = '�������� ����� ��� ��������';
  rs_FramesExportingCaption = '�������. ����������� ����� %0:d �� %1:d';
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
            pbRecord.Repaint;
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
        if AdvertisementEnabled then
          AdvertisementFrameImage.SaveToFile(Dir + Format('Frame%.5d.bmp', [RecordedFrames.Count]));
        AdvertisementEnabled := True;
        InfoMsg('������� ��������.');
      finally
        Exporting := False;
      end;
    end;
end;

// procedure CheckOSError(RetVal: Integer);
// begin
//   Win32Check(LongBool(RetVal));
// end;

procedure TMainForm.DoCancelExport(Sender: TObject);
begin
  ExportCancelled := True;
end;

procedure TMainForm.actExportToAVIExecute(Sender: TObject);
var
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
  rs_AVIExporting = '������� � AVI';
  rs_AVIExportingAudioStore = '���������� �����.';
  rs_AVIExportingCompressorInit = '������������� �������� �����.';
  rs_AVIExportingCaption = '������ ����� %0:s (%1:s, %2:d �� %3:d).';
  rs_AVIExportingAudioMerge = '����������� �� ������.';
  rs_ExportFinished = '������� �������� ����?';

begin
  Stop;
  CheckStopCamera;
  Dir := GetCurrentDir;
  OldCurrentWorkingFrame := CurrentWorkingSetFrame;
  if SaveToAVIDialog.Execute then
    begin
//      SetCurrentDir(Dir);
      Dir := ExtractFilePath(SaveToAVIDialog.FileName);
      Exporting := True;
      mmiExportResolution.Enabled := False;
      SetStatus(rs_AVIExporting);
      with TProgressForm.Create(rs_AVIExporting, SaveToAVIDialog.FileName, '', DoCancelExport) do
      try
        ExportCancelled := False;
        Show;
        SetProgressStatus(rs_AVIExportingAudioStore);
        if WaveStorage.Wave.Length = DWORD(MulDiv(RecordedFrames.Count, 1000, FrameRate)) then
          WaveToSave := WaveStorage.Wave
        else
          begin
            WaveToSave := TWave.Create;
            WaveToSave.Copy(WaveStorage.Wave, 0, MulDiv(RecordedFrames.Count, 1000, FrameRate));
          end;

        if FileExists(Dir + '~Audio.wav') then
          DeleteFile(Dir + '~Audio.wav');

        if FileExists(Dir + '~Video.wav') then
          DeleteFile(Dir + '~Video.wav');

        WaveToSave.SaveToFile(Dir + '~Audio.wav');
        if WaveToSave <> WaveStorage.Wave then
          WaveToSave.Free;

        SetProgressStatus(rs_AVIExportingCompressorInit);
        if not Assigned(Compressor) then
          Compressor := TAVICompressor.Create;
        if not OptionsInitialized then begin
          Options.Init;
          Options.Handler := 'DIB '; // �� ��������� - ��� ����������
          OptionsInitialized := True;
          Compressor.LoadOptionsFromFile(Application.ExeName + '.AviParam');
        end;
        Options.FrameRate := FrameRate;
        Options.Width := ExportSize.cx;
        Options.Height := ExportSize.cy;
        Hide; // progress form �� ����� ������ �������� ������
        CheckAVIError(Compressor.Open(Dir + '~Video.avi', False, Options));
        Compressor.SaveOptionsToFile(Application.ExeName + '.AviParam');
        Show; // progress form
        try
          Bmp := TBitmap.Create;
          try
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
                SetProgress(i + 1, RecordedFrames.Count + AdvertisementDuration div FrameRate);
                CurrentRecordPosition := i; // preview
                DisplayedFrameIndex := RecordedFrames[i].FrameInfoIndex;
                pbRecord.Repaint;
                SetProgressStatus(Format(rs_AVIExportingCaption, [
                  FrameInfoList[DisplayedFrameIndex].RelativeFileName,
                  FrameIndexToTimeStamp(CurrentRecordPosition),
                  i + 1,
                  RecordedFrames.Count
                ]));
                Application.ProcessMessages;
                if ExportCancelled then
                  Abort;
                try
                  if PreparedFrameInfoIndex <> DisplayedFrameIndex then // skip preparing for already prepared
                    begin
                      Image := FrameInfoList[DisplayedFrameIndex].ImageFromDisc;
                      R := StretchSize(Image.Width, Image.Height, Bmp.Width, Bmp.Height);
                      Bmp.Canvas.Brush.Color := clBlack;
                      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
                      Bmp.Canvas.StretchDraw(R, Image);
                      Image.Free;
                      Bmp.PixelFormat := pf24bit;
                      PreparedFrameInfoIndex := DisplayedFrameIndex;
                    end;
                  CheckAVIError(Compressor.WriteFrame(Bmp));
                except
                  on e: Exception do
                    begin
                      InfoMsg(
                        Format(rs_AVIExportingCaption, [
                          FrameInfoList[DisplayedFrameIndex].RelativeFileName,
                          FrameIndexToTimeStamp(CurrentRecordPosition),
                          i + 1,
                          RecordedFrames.Count
                        ]) + #13#10 +
                        e.Message
                      );
                      Abort;
                    end;
                end;
              end;
            if AdvertisementEnabled then
            begin
              SetProgress(RecordedFrames.Count, RecordedFrames.Count + AdvertisementDuration div FrameRate);
              Image := AdvertisementFrameImage;
              R := StretchSize(Image.Width, Image.Height, Bmp.Width, Bmp.Height);
              Bmp.Canvas.Brush.Color := clBlack;
              Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
              Bmp.Canvas.StretchDraw(R, Image);
              Bmp.PixelFormat := pf24bit;
              CurrentRecordPosition := RecordedFrames.Count;
              pbRecord.Repaint;
              SetProgressStatus(Format(rs_AVIExportingCaption, ['��������� ����', FrameIndexToTimeStamp(CurrentRecordPosition), RecordedFrames.Count, RecordedFrames.Count]));
              Application.ProcessMessages;
              if ExportCancelled then
                Abort;
              for i := 1 to AdvertisementDuration div FrameRate do
              begin
                SetProgress(RecordedFrames.Count + i, RecordedFrames.Count + AdvertisementDuration div FrameRate);
                CheckAVIError(Compressor.WriteFrame(Bmp));
                Application.ProcessMessages;
                if ExportCancelled then
                  Abort;
              end;
            end;
            AdvertisementEnabled := True;
          finally
            Bmp.Free;
          end;
        finally
          Compressor.Close;
        end;
        SetProgressStatus(rs_AVIExportingAudioMerge);

        Application.ProcessMessages;
        if ExportCancelled then
          Abort;

        Compressor.MergeFilesAndSaveAs(Dir + '~Video.avi', Dir + '~Audio.wav', SaveToAVIDialog.FileName);

        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Audio.wav');
        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Video.avi');
        SetProgressStatus(rs_ExportFinished);
        Hide; // progress
        if MessageDlg(rs_ExportFinished, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          ShellExecute(0, 'open', PWideChar(SaveToAVIDialog.FileName), nil, nil, SW_SHOWNORMAL);
      finally
        Free;
        mmiExportResolution.Enabled := True;
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

procedure TMainForm.actFramesFromCameraModeExecute(Sender: TObject);
var
  CameraWasNonActive: Boolean;
  FirstTime: Boolean;
begin
  FirstTime := PhotoFolder = '';
  if PhotoFolder = '' then begin
    actSelectPhotoFolder.Execute;
    if PhotoFolder = '' then
      Exit;
  end;

  CameraWasNonActive := not CameraForm.Active;
  if CameraWasNonActive then begin
    Cursor := crHourGlass;
    Screen.Cursor := Cursor;
  end;
  CameraForm.Active := CameraWasNonActive;
  if not CameraForm.Active and CameraForm.Visible then
    CameraForm.Visible := False;
  if FirstTime and CameraForm.Active and not CameraForm.Visible then
    CameraForm.Visible := True;
  RepaintAll;
end;

procedure TMainForm.CameraFormActiveChanged(Sender: TObject);
resourcestring
  rsStartRecording = '������';
  rsMakeCameraFrame = '�������� ���� � ������ � ����� ������';
begin
  Stop;
  if CameraForm.Active then
  begin
    actRecord.Caption := rsMakeCameraFrame;
    actRecord.ImageIndex := 9;
  end
  else
  begin
    actRecord.Caption := rsStartRecording;
    actRecord.ImageIndex := 6;
  end;

  btnRecord.Hint := actRecord.Caption;

  imgCamPreview.Visible := CameraForm.Active;
end;

procedure TMainForm.actFullScreenModeExecute(Sender: TObject);
begin
  if ScreenForm.Visible then
  begin
    if (Sender <> actFullScreenMode) then
      actFullScreenMode.Checked := not actFullScreenMode.Checked;
    ScreenForm.FullScreen := actFullScreenMode.Checked
  end
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
  rs_SaveBeforeOpenRequest = '��������� ������';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeOpenRequest);

  ClearRecorded;
  if FExternalAudioFileName = '' then
    ClearSound;
  ProjectFileName := '';
  UpdateCaption;
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
  StopperSectionStart    = '------------------- Stoppers: --------------------';

resourcestring
  rs_CustomSize = '���� (%d, %d)';

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
  RecordedAudioLines.Clear;
  MakeRecordedAudioLines;
  pbRecord.Repaint;
  WaveStorage.Wave.Position := 0;
  ShowTimes;
end;

procedure TMainForm.OpenMovie(AFileName: string);
var
  i: Integer;
  FrameInfoIndex, FoundFrameInfoIndex: Integer;
  RelativePath, FileName: string;
  WaveFileName: string;
  s: string;
  LastDelimiterPos: Integer;
  JSONValue: TJSONValue;
  RootJSONObject, JSONObject: TJSONObject;
  JSONArray: TJSONArray;

  procedure ReadTeleport(s: string);
  var
    FrameInfoIndex: Integer;
    SeparatorPos: Integer;
    TargetBookmarkIndex: Integer;
  begin
    SeparatorPos := pos('=', s);
    FrameInfoIndex := StrToInt(Trim(Copy(s, 1, SeparatorPos-1)));
    TargetBookmarkIndex := StrToInt(Trim(Copy(s, SeparatorPos+1, Length(s))));
    FrameInfoList[FrameInfoIndex].TeleportTargetBookmark := TargetBookmarkIndex;
    // � ��� ������ ��������� ������ �� �����, � ������ ����� ����� ������� � �������,
    // ��� �������� �������� ���������, ������ ���� �� ���� � ����� ����
    Dec(FrameInfoIndex);
    if FrameInfoIndex < 0 then
      FrameInfoIndex := FrameInfoCount - 1;
    FrameInfoList[FrameInfoIndex].TeleportTargetBookmark := TargetBookmarkIndex;
  end;

begin
  ExportCancelled := True;
  ClearBookmarks;
  ClearFlags;
  PhotoFolder := ExtractFilePath(AFileName);
  ProjectFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  UpdateCaption;

  with TStringList.Create do
    try
      LoadFromFile(AFileName);

      JSONValue := TJSONObject.ParseJSONValue(Text, True);
      if Assigned(JSONValue) then try
        Assert(JSONValue is TJSONObject);
        RootJSONObject := TJSONObject(JSONValue);
        WaveFileName := RootJSONObject.GetValue('Wave').Value;
        ExportSize.cx := RootJSONObject.GetValue('ExportSize').GetValue<Integer>('x');
        ExportSize.cy := RootJSONObject.GetValue('ExportSize').GetValue<Integer>('y');
        FrameRate := RootJSONObject.GetValue('FrameRate').GetValue<Integer>('');

        JSONArray := RootJSONObject.GetValue('FrameFiles') as TJSONArray;
        FrameInfoIndex := 0;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do begin
            s := JSONArray.Items[i].Value;
            LastDelimiterPos := LastDelimiter(PathDelim + DriveDelim, s);
            RelativePath := Copy(s, 1, LastDelimiterPos);
            FileName := Copy(s, LastDelimiterPos + 1, MaxInt);
            FoundFrameInfoIndex := FindFrameInfo(RelativePath, FileName);  // �� �������������� �����, ���� ��� ��� ���� �������
            if FoundFrameInfoIndex <> -1 then
              FFrameInfoList.Move(FoundFrameInfoIndex, FrameInfoIndex)
            else
              FFrameInfoList.Insert(FrameInfoIndex, TFrameInfo.Create(RelativePath, FileName));
            Inc(FrameInfoIndex);
          end;
        FFrameInfoList.Count := FrameInfoIndex;

        WorkingSetFrames.Clear;
        JSONObject := RootJSONObject.GetValue('WorkingSet') as TJSONObject;
        JSONArray := JSONObject.GetValue('Frames') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            TRecordedFrame.Create(WorkingSetFrames, JSONArray.Items[i].GetValue<Integer>);

        JSONArray := JSONObject.GetValue('Bookmarks') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            Bookmarks[JSONArray.Items[i].GetValue<Integer>('BookMarkIndex')] :=
                      JSONArray.Items[i].GetValue<Integer>('SitAtFrame');

        JSONArray := JSONObject.GetValue('Teleports') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            FrameInfoList[JSONArray.Items[i].GetValue<Integer>('SitAfterFrame')].TeleportTargetBookmark :=
                          JSONArray.Items[i].GetValue<Integer>('TargetBookmarkIndex');

        JSONArray := JSONObject.GetValue('Stoppers') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            FrameInfoList[JSONArray.Items[i].GetValue<Integer>('SitAfterFrame')].HaveStopperAfter := True;

        JSONArray := JSONObject.GetValue('Bouncers') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            FrameInfoList[JSONArray.Items[i].GetValue<Integer>('SitAfterFrame')].HaveBouncerAfter := True;

        JSONArray := JSONObject.GetValue('Loopers') as TJSONArray;
        if Assigned(JSONArray) then
          for i := 0 to JSONArray.Count - 1 do
            FrameInfoList[JSONArray.Items[i].GetValue<Integer>('SitAfterFrame')].HaveLooperAfter := True;

        RecordedFrames.Clear;
        JSONObject := RootJSONObject.GetValue('RecordedSet') as TJSONObject;
        if Assigned(JSONObject) then begin
          JSONArray := JSONObject.GetValue('Frames') as TJSONArray;
          if Assigned(JSONArray) then
            for i := 0 to JSONArray.Count - 1 do
              TRecordedFrame.Create(RecordedFrames, JSONArray.Items[i].GetValue<Integer>);
        end;

      finally
        JSONValue.Free;
      end else begin // ������ ������� ������� �����

        i := 0;
        s := Strings[i];
        // �� ����� �������� � ������� �����������, � �������� ��� ����� ����
        // - ��� ��������� ������ �������������. ����������� ������� �� ������.
        if (Copy(s, 1, Length('Wave = ')) = 'Wave = ') then
          begin
            WaveFileName := Copy(s, Length('Wave = ') + 1, MaxInt);
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
            inc(i);
            s := Strings[i];
          end;
        if (Copy(s, 1, Length('FrameRate = ')) = 'FrameRate = ') then
          begin
            FrameRate := StrToInt(Copy(s, Length('FrameRate = ') + 1, 25));
            inc(i);
            s := Strings[i];
          end;
        if s = FilesSectionStart then
          begin
            FrameInfoIndex := 0;
            while i < (Count - 1) do
              begin
                inc(i);
                s := Strings[i];
                if s = BookmarkSectionStart then // for compatibility with prev. saves without Working set
                  Break;
                if s = WorkingSetSectionStart then
                  Break;
                LastDelimiterPos := LastDelimiter(PathDelim + DriveDelim, s);
                RelativePath := Copy(s, 1, LastDelimiterPos);
                FileName := Copy(s, LastDelimiterPos + 1, MaxInt);
                FoundFrameInfoIndex := FindFrameInfo(RelativePath, FileName);  // �� �������������� �����, ���� ��� ��� ���� �������
                if FoundFrameInfoIndex <> -1 then
                  FFrameInfoList.Move(FoundFrameInfoIndex, FrameInfoIndex)
                else
                  FFrameInfoList.Insert(FrameInfoIndex, TFrameInfo.Create(RelativePath, FileName));
                Inc(FrameInfoIndex);
              end;
            FFrameInfoList.Count := FrameInfoIndex;
            WorkingSetFrames.Clear;
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
              if s = FrameSectionStart then // for compatibility with old saves without Teleports
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
              if s = FrameSectionStart then // for compatibility with old saves without stoppers
                Break;
              if s = StopperSectionStart then
                Break;
              ReadTeleport(s);
            end;
        if s = StopperSectionStart then
          while i < (Count - 1) do
            begin
              inc(i);
              s := Strings[i];
              if s = FrameSectionStart then
                Break;
              FrameInfoIndex := StrToInt(s);
              FrameInfoList[FrameInfoIndex].HaveStopperAfter := True;
              // � ��� ������ �������� ������ �� �����, � ������ ����� ����� ������� � �������,
              // ��� �������� �������� ���������, ������ ���� �� ���� � ����� ����
              Dec(FrameInfoIndex);
              if FrameInfoIndex < 0 then
                FrameInfoIndex := FrameInfoCount - 1;
              FrameInfoList[FrameInfoIndex].HaveStopperAfter := True;
            end;
        if s = FrameSectionStart then
          while i < (Count - 1) do
            begin
              inc(i);
              s := Strings[i];
              TRecordedFrame.Create(RecordedFrames, StrToInt(s));
            end;
      end;
    finally
      Free;
    end;

  mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);

  if not FrameRate in [50, 25] then
    FrameRate := 25;

  if WorkingSetFrames.Count > 0 then
    DisplayedFrameIndex := WorkingSetFrames[0].FrameInfoIndex;
  CaptureFirstFrameSizes;

  if {$IFDEF FPC}FileExistsUTF8{$ELSE}FileExists{$ENDIF}(PhotoFolder + WaveFileName) then
    OpenAudio(PhotoFolder + WaveFileName); // ��. ���� ������� ��� SwitchToMicrophoneUsage

  if (WaveFileName = ExtractFileName(AFileName) + '.wav') and
     (CalculateSoundFramesCount = RecordedFrames.Count)
  then // TODO: ������� ����� ������� � ����� �������
    SwitchToMicrophoneUsage(True);

  Saved := True;
  RepaintAll;
  UpdateActions;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
resourcestring
  rs_SaveBeforeOpenRequest = '��������� �������';
begin
  Stop;
  SaveBeforeClose(rs_SaveBeforeOpenRequest);

  if not dlgOpenMovie.Execute then
    Abort;
  OpenMovie(dlgOpenMovie.FileName);
end;

procedure TMainForm.actHaveCurrentFrame(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and Assigned(CurrentWorkingSetFrame);
end;

procedure TMainForm.actHaveDisplayedFrame(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and (DisplayedFrameIndex <> -1);
end;

procedure TMainForm.actHaveRecordedFrame(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and not Recording and (RecordedFrames.Count > 0);
end;

procedure TMainForm.actDuplicateFrameClick(Sender: TObject);
var
  OriginalFileName, NewFileName, Suffix: string;
  SuffixIndex: Integer;
  CurrentWorkingSetFrameIndex, NewDisplayedFrameIndex: Integer;
begin
  Stop;
  if not Assigned(CurrentWorkingSetFrame) then
    Exit;

  CurrentWorkingSetFrameIndex := CurrentWorkingSetFrame.Index;
  OriginalFileName := FrameInfoList[CurrentWorkingSetFrame.FrameInfoIndex].FileName;
  if FrameInfoList[CurrentWorkingSetFrame.FrameInfoIndex].RelativePath = 'Duplicates\' then
    begin
      NewFileName := ChangeFileExt(OriginalFileName, '');
      while (Length(NewFileName) > 0) and CharInSet(NewFileName[Length(NewFileName)], ['0'..'9']) do
        SetLength(NewFileName, Length(NewFileName) - 1);
      if (Length(NewFileName) > 1) and (NewFileName[Length(NewFileName)] = '_') then
        begin
          SetLength(NewFileName, Length(NewFileName) - 1);
          OriginalFileName := NewFileName + ExtractFileExt(OriginalFileName);
        end;
    end;

  Suffix := '';
  SuffixIndex := 0;
  ForceDirectories(PhotoFolder + 'Duplicates\');
  repeat
    NewFileName := OriginalFileName;
    NewFileName := ChangeFileExt(ExtractFileName(NewFileName), Suffix + ExtractFileExt(NewFileName));
    NewFileName := PhotoFolder + 'Duplicates\' + NewFileName;
    inc(SuffixIndex);
    Suffix := '_' + IntToStr(SuffixIndex);
  until not FileExists(NewFileName);

  CopyFile(PWideChar(FrameInfoList[CurrentWorkingSetFrame.FrameInfoIndex].FullFileName), PWideChar(NewFileName), True);

  NewDisplayedFrameIndex := FFrameInfoList.Add(
    TFrameInfo.Create(
      ExtractRelativePath(PhotoFolder, ExtractFilePath(NewFileName)),
      ExtractFileName(NewFileName)
    )
  );
  CurrentWorkingSetFrame := TRecordedFrame.Create(WorkingSetFrames, NewDisplayedFrameIndex);
  CurrentWorkingSetFrame.Index := CurrentWorkingSetFrameIndex + 1;
  pbWorkingSet.Refresh;

  Saved := False;
end;

procedure TMainForm.actOpenFrameFileInDefaultProgramExecute(Sender: TObject);
begin
  Stop;
  SafeShellExecute(HWND(nil), 'edit', FrameInfoList[DisplayedFrameIndex].FullFileName, '', '', SW_SHOW);
end;

function TMainForm.OnSaveAsCloseQuery(const ANewMovieName: string): Boolean;
resourcestring
  rs_DoYouWantReplaceMovie = '� ����� "%s"'#13#10'��� ���� ����� � ������ "%s".'#13#10'��������?';
begin
  if FileExists(PhotoFolder + ANewMovieName + '.mp') then
    Result := MessageDlg(
      Format(rs_DoYouWantReplaceMovie, [PhotoFolder, ANewMovieName]),
      mtConfirmation,
      [mbOk, mbCancel],
      0
    ) = mrOK
  else
    Result := True;

  if Result then
    SaveWorkingSet(ANewMovieName);
end;

// Originally coped from REST.Json.pas function TJson.Format
function FormatJSON(AJsonValue: TJsonValue): string;
var
  s: string;
  LBytes: TBytes;
  c: char;
  EOL: string;
  INDENT: string;
  LIndent: string;
  isInString: boolean;
  isEscape: boolean;
  isStartOfObject: Boolean;
begin
  Result := '';
  EOL := #13#10;
  INDENT := '  ';
  isInString := false;
  isEscape := false;
  isStartOfObject := False;

  SetLength(LBytes,AJsonValue.ToString.Length*6); //Length can not be predicted. Worst case: every single char gets escaped
  SetLength(LBytes, AJsonValue.ToBytes(LBytes, 0)); //adjust Array to actual length
  s := TEncoding.UTF8.GetString(LBytes);

  for c in s do
  begin
    if not isInString and ((c = '{') or (c = '[')) then
    begin
      if isStartOfObject then begin
        LIndent := LIndent + INDENT;
        Result := Result + EOL + LIndent;
      end;
      Result := Result + c;
      isStartOfObject := True;
    end
    else if not isInString and (c = ',') then
    begin
      Result := Result + c + EOL + LIndent;
    end
    else if not isInString and (c = ':') then
    begin
      Result := Result + c + ' ';
    end
    else if not isInString and ((c = '}') or (c = ']')) then
    begin
      if not isStartOfObject then begin
        Delete(LIndent, 1, Length(INDENT));
      end;
      isStartOfObject := False;
      Result := Result + EOL + LIndent;
      Result := Result + c;
    end
    else
    begin
      if isStartOfObject then begin
        isStartOfObject := False;
        LIndent := LIndent + INDENT;
        Result := Result + EOL + LIndent;
      end;
      Result := Result + c;
    end;
    isEscape := (c = '\') and not isEscape;
    if not isEscape and (c = '"') then
      isInString := not isInString;
  end;
end;

procedure TMainForm.SaveWorkingSet(ANewProjectFileName: string = '');
var
  i: Integer;
  AudioName: string;
  JSONArray: TJsonArray;
  RootJSONObject, JSONObject: TJSONObject;
  OutputFile : TextFile;
begin
  if ANewProjectFileName <> '' then
    ProjectFileName := ANewProjectFileName;
  UpdateCaption;
  if FExternalAudioFileName <> '' then
    AudioName := ExtractFileName(FExternalAudioFileName)
  else
    AudioName := ProjectFileName + '.wav';
  // ���� ������� ���� ��� ������ ������ ��� �������� � ����� �������, �� ����� ��� ��������������.
  if ExpandFileName(PhotoFolder + AudioName) <> ExpandFileName(FExternalAudioFileName) then
    WaveStorage.Wave.SaveToFile(PhotoFolder + AudioName);
  RootJSONObject := TJSONObject.Create;
  try
    RootJSONObject.
      AddPair('Wave', AudioName).
      AddPair('ExportSize', TJSONObject.Create.
        AddPair('x', TJSONNumber.Create(ExportSize.cx)).
        AddPair('y', TJSONNumber.Create(ExportSize.cy))
      ).
      AddPair('FrameRate', TJSONNumber.Create(FrameRate));

    JSONArray := TJsonArray.Create;
    for i := 0 to FrameInfoCount - 1 do
      JSONArray.Add(FrameInfoList[i].RelativeFileName);
    RootJSONObject.AddPair('FrameFiles', JSONArray);

    JSONObject := TJSONObject.Create;
    RootJSONObject.AddPair('WorkingSet', JSONObject);

      JSONArray := TJsonArray.Create;
      for i := 0 to WorkingSetFrames.Count - 1 do
        JSONArray.Add(WorkingSetFrames[i].FrameInfoIndex);
      JSONObject.AddPair('Frames', JSONArray);

      JSONArray := TJsonArray.Create;
      JSONObject.AddPair('Bookmarks', JSONArray);
      for i := Low(Bookmarks) to High(Bookmarks) do
        if Bookmarks[i] <> -1 then
          JSONArray.Add(TJSONObject.Create.
            AddPair('BookMarkIndex', TJSONNumber.Create(i)).
            AddPair('SitAtFrame', TJSONNumber.Create(Bookmarks[i]))
          );

      JSONArray := TJsonArray.Create;
      JSONObject.AddPair('Teleports', JSONArray);
      for i := 0 to FrameInfoCount - 1 do
        if FrameInfoList[i].TeleportTargetBookmark <> -1 then
          JSONArray.Add(TJSONObject.Create.
            AddPair('SitAfterFrame', TJSONNumber.Create(i)).
            AddPair('TargetBookmarkIndex', TJSONNumber.Create(FrameInfoList[i].TeleportTargetBookmark))
          );

      JSONArray := TJsonArray.Create;
      JSONObject.AddPair('Stoppers', JSONArray);
      for i := 0 to FrameInfoCount - 1 do
        if FrameInfoList[i].HaveStopperAfter then
          JSONArray.Add(TJSONObject.Create.
            AddPair('SitAfterFrame', TJSONNumber.Create(i))
          );

      JSONArray := TJsonArray.Create;
      JSONObject.AddPair('Bouncers', JSONArray);
      for i := 0 to FrameInfoCount - 1 do
        if FrameInfoList[i].HaveBouncerAfter then
          JSONArray.Add(TJSONObject.Create.
            AddPair('SitAfterFrame', TJSONNumber.Create(i))
          );

      JSONArray := TJsonArray.Create;
      JSONObject.AddPair('Loopers', JSONArray);
      for i := 0 to FrameInfoCount - 1 do
        if FrameInfoList[i].HaveLooperAfter then
          JSONArray.Add(TJSONObject.Create.
            AddPair('SitAfterFrame', TJSONNumber.Create(i))
          );

    JSONObject := TJSONObject.Create;
    RootJSONObject.AddPair('RecordedSet', JSONObject);

      JSONArray := TJsonArray.Create;
      for i := 0 to RecordedFrames.Count - 1 do
        JSONArray.Add(RecordedFrames[i].FrameInfoIndex);
      JSONObject.AddPair('Frames', JSONArray);

    try
      AssignFile(OutputFile, PhotoFolder + ProjectFileName + '.mp');
      Rewrite(OutputFile);
      WriteLn(OutputFile, FormatJSON(RootJSONObject));
    finally
      CloseFile(OutputFile);
    end;
    Saved := True;
  finally
    RootJSONObject.Free;
  end;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
resourcestring
  rs_SaveAs = '���������� ������ ��� ����� ������';
begin
  Stop;
  if not TMovieNameDialog.Execute(PhotoFolder, ProjectFileName, rs_SaveAs, OnSaveAsCloseQuery) then
    Abort;
end;

procedure TMainForm.actSaveAsUpdate(Sender: TObject);
begin
  actSaveAs.Enabled := not Exporting and (WorkingSetFrames.Count > 0);// and not Saved;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
begin
  if ProjectFileName <> '' then
    SaveWorkingSet
  else
   actSaveAs.Execute;
end;

procedure TMainForm.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := not Exporting and (WorkingSetFrames.Count > 0) and ((ProjectFileName = '') or not Saved);
end;

procedure TMainForm.actScreenWindowExecute(Sender: TObject);
begin
  if actFullScreenMode.Checked then
    actFullScreenMode.Execute;

  ScreenForm.Visible := actScreenWindow.Checked;
end;

procedure TMainForm.UpdateCaption;
resourcestring
  rs_FrameNotInWorkingSet = ', ����� �� �������� ������..';
var
  s: string;
begin
  s := '';
  if not Exporting then
    if FDisplayedFrameIndex >= 0 then
      if Assigned(CurrentWorkingSetFrame) then
        s := ' � ' + FrameInfoList[FDisplayedFrameIndex].RelativeFileName
      else
        s := ' � ' + FrameInfoList[FDisplayedFrameIndex].RelativeFileName + rs_FrameNotInWorkingSet;
  Caption := ProjectFileName + s + ' � ' + Application.Title;
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

procedure TMainForm.SetCameraPhotoFolder;
resourcestring
  CamFolder = 'FromCam\';
begin
  CameraForm.PhotoFolder := PhotoFolder + CamFolder;
end;

procedure TMainForm.SetPhotoFolder(const Value: string);
begin
  if PhotoFolder = Value then
    Exit;
  UnloadFrames;
  FPhotoFolder := Value;
  lblWorkPath.Caption := FPhotoFolder;
  if Assigned(CameraForm) then
    SetCameraPhotoFolder;
  // TODO: PhotoFolderMonitor := TDirMonitor.Create(FPhotoFolder, PhotoFolderChanged, nil)
end;

procedure TMainForm.SetSaved(const Value: Boolean);
begin
  FSaved := Value;
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
  if Exporting then
    Exit;

  SoundFramesCount := 0; // hint
  if FExternalAudioFileName <> '' then
    begin
      SoundFramesCount := CalculateSoundFramesCount;
      s := ' ����� ������� ' + FrameIndexToTimeStamp(SoundFramesCount) + '. ';
    end;

  if Recording then
    begin
      if FExternalAudioFileName <> '' then
        s := s + '������������ ���� ' + FrameIndexToTimeStamp(CurrentRecordPosition) + ' (' + IntToStr(MulDiv(CurrentRecordPosition, 100, SoundFramesCount)) + '%)'
      else
        s := s + '������������ ���� ' + FrameIndexToTimeStamp(CurrentRecordPosition)
    end
  else //if Playing then
    begin
      if FrameInfoCount = 0 then
        s := s + '������ ��� ������ ���� �� �������.'
      else if RecordedFrames.Count = 0 then
        s := s + '������ ���� � ����� �� ��������.'
      else
        s := s + '���� ' + FrameIndexToTimeStamp(CurrentRecordPosition) + ' �� ' + FrameIndexToTimeStamp(RecordedFrames.Count) + ' (' + IntToStr(MulDiv(CurrentRecordPosition, 100, RecordedFrames.Count)) + '%)';
    end;

  if FExternalAudioFileName <> '' then
    begin
      if mmiStopRecordingOnSoundtrackFinish.Checked then
        s := s + ', ������ ����� ����������� �� ���������� ����� �������.'
      else
        s := s + ', �� ���������� ����� ������� ������ ����� ���������� � ������.';
    end
  else if PhotoFolder <> '' then
    if Recording then
      s := s + ' ������� ������������ ������ � �������.'
    else
      s := s + ' ������� ����� ������������ ������ � �������.';

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
var
  WorkingSetIndex: Integer;
begin
  if Value >= GetFrameInfoCount then
    Value := GetFrameInfoCount - 1;

  if FDisplayedFrameIndex = Value then // break recursion
    Exit;

  FDisplayedFrameIndex := Value;
  // ��� ������� �� SetCurrentWorkingSetFrame ��������� ��� ���������,
  // ����� ��� ���������� � �� ���� ������ �����������. � ������ ������� - ����.
  if not Assigned(CurrentWorkingSetFrame) or (CurrentWorkingSetFrame.FrameInfoIndex <> Value) then
    CurrentWorkingSetFrame := WorkingSetFrames.FindByFrameIndex(FDisplayedFrameIndex);

  if Value >= 0 then
    begin
      if Assigned(CurrentWorkingSetFrame) then
        WorkingSetIndex := CurrentWorkingSetFrame.Index
      else
        WorkingSetIndex := -1;
      LoadPhoto(Value, WorkingSetIndex);
      UpdateCaption;
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
  if RecordingMustBeChanged then // ������, ������ ������ ��� ������ ��������
    begin
      Recording := True;
      AdvertisementShowing := False;
      RecordingMustBeChanged := False;
      actRecord.Checked := True;
    end
  else
    begin  // ������, ������ ������ ��� ������������ ����������� ��������
      // ��������� ��������������� ������ ����� ����, ��� ���� ����� ����� ����������������
      actPlay.Checked := True;
      ReplaceControlActions(caNone);
      UpdatePlayActions;
      // �� ������ ������. TODO ���� ��, ����� � actPlayExecute ���� ?
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
      InfoMsg('����� ����� �������, ������ �����������'#13#10'�������� ���������� ������.');
    end;

  // ������������� ��������������� ��� ��������� ����� ����, ��� �������, ������
  // ��� StopPlaying �� ��� ����������.
  //  TODO: ������, ����� �� ���� ������������� ��������������� ����� ��� ���������� �����
  // (��������, ���� ����� ������, ��� ����������� ������, � ���� � ���������� � ������ ?)
  // � ���������, ��� ��� ����������.
  if Playing then
    begin
      actPlay.Checked := False;

      //CurrentRecordPosition := 0;
      Interval := 0;
      Playing := False;
    end;
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
  else // ����� ����������� ������
    begin
      RecordingMustBeChanged := True;
      AudioRecorder.Active := False;
    end;
end;

procedure TMainForm.LoadPhoto(AFrameInfoIndex: Integer; AWorkingSetIndex: Integer);
var
  Image: TGraphic;
  R: TRect;
  i, j, UnloadCounter: Integer;
  NearestNeighbour: Boolean;
begin
  if (FrameInfoCount = 0) then
    Exit;

  // �������, ���� ������ ������� �� �����.
  // � ������� ����� ���� ������, ���� ���������, � ����������� �� ��������� ������������� � ���� ������
  if not lvFrameset.Visible and FrameInfoList[AFrameInfoIndex].PreviewLoaded then
    Exit;

  if lvFrameset.Visible and (Assigned(FrameInfoList[AFrameInfoIndex].Iconic) or (AWorkingSetIndex = -1)) then
    Exit;

  if OutOfMemoryRaised then // unload first loaded frame info
    for i := 0 to FrameInfoCount - 1 do
      if FrameInfoList[i].PreviewLoaded then
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
          FrameInfoList[i].Unload;
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
        if not lvFrameset.Visible then
          begin
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
                Preview.Canvas.Brush.Color := clBlack;
                Preview.Canvas.FillRect(Rect(0, 0, 640, 480));
                Preview.Canvas.StretchDraw(R, Image);
              end
            else
              Preview.Assign(Image);
            PreviewLoaded := True;
          end
        else
          begin
            Iconic := TBitmap.Create;

            R := StretchSize(Image.Width, Image.Height, 64, 48);
            {$IFDEF DelphiXE+}
            Iconic.SetSize(64,48);
            {$ELSE}
            Iconic.Width := 64;
            Iconic.Height := 48;
            {$ENDIF}
            Iconic.Canvas.Brush.Color := clBlack;
            Iconic.Canvas.FillRect(Rect(0, 0, 64, 48));
            Iconic.Canvas.StretchDraw(R, Image);

            ilFrameset.ReplaceMasked(AWorkingSetIndex, Iconic, 0);
          end
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
          if FrameInfoList[i].PreviewLoaded then
            begin
              FrameInfoList[i].Unload;
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
  pbWorkingSet.Repaint;
  Saved := False;
end;

procedure TMainForm.actToggleBouncerExecute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    HaveBouncerAfter := not HaveBouncerAfter;
  pbWorkingSet.Repaint;
  Saved := False;
end;

procedure TMainForm.actToggleLooperExecute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    HaveLooperAfter := not HaveLooperAfter;
  pbWorkingSet.Repaint;
  Saved := False;
end;

procedure TMainForm.actToggleOnionSkinExecute(Sender: TObject);
begin
  CameraForm.OnionSkinEnabled := actToggleOnionSkin.Checked;
end;

procedure TMainForm.actToggleOnionSkinUpdate(Sender: TObject);
begin
  actToggleOnionSkin.Enabled := CameraForm.Active;
  actToggleOnionSkin.Checked := CameraForm.OnionSkinEnabled;
end;

procedure TMainForm.actToggleTeleport0Execute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    if TeleportTargetBookmark = TMenuItem(Sender).Tag then
      TeleportTargetBookmark := -1
    else
      TeleportTargetBookmark := TMenuItem(Sender).Tag;
  pbWorkingSet.Repaint;
  Saved := False;
end;

procedure TMainForm.actToggleStopperExecute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    HaveStopperAfter := not HaveStopperAfter;
  pbWorkingSet.Repaint;
  Saved := False;
end;

procedure TMainForm.actGotoBookmark0Execute(Sender: TObject);
begin
  if Bookmarks[TMenuItem(Sender).Tag] <> -1 then
    DisplayedFrameIndex := Bookmarks[TMenuItem(Sender).Tag];
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  ExportCancelled := True;
  CheckStopCamera;
  Stop;
  Close;
end;

procedure TMainForm.actRecordExecute(Sender: TObject);
begin
  Saved := False;
  if CameraForm.Active then
    CameraForm.MakePhoto
  else
    begin
      if FExternalAudioFileName <> '' then
        begin
          if not Recording then
            begin
              CurrentRecordPosition := RecordedFrames.Count; // ������ ������ ���������������� �����
              // ������ ����� ������ � ����������� ��������� - ����� �����������
              // StockAudioPlayerActivate � StockAudioPlayerDeactivate:
              // ��������� ������ ������ ������ ����� ����, ��� ���� ����� ����� ����������������.
              RecordingMustBeChanged := True;
              StockAudioPlayer.Active := True;
            end
          else
            StopRecording;
        end
      else
        // ���� ���� � ��������� - �� ������ ������ �������� �
        // AudioRecorderActivate
        // � ���������� - � AudioRecorderDeactivate
        AudioRecorder.Active := not Recording
    end;
end;

procedure TMainForm.actRecordUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and ((FrameInfoCount > 0) or CameraForm.Active);
end;

procedure TMainForm.actRefreshPreviewExecute(Sender: TObject);
begin
  Stop;
  FrameInfoList[DisplayedFrameIndex].Unload;
  pbDisplay.Refresh;
end;

procedure TMainForm.actReplaceInMovieExecute(Sender: TObject);
begin
  if CurrentRecordPosition = RecordedFrames.Count then
    TRecordedFrame.Create(RecordedFrames, CurrentWorkingSetFrame.FrameInfoIndex)
  else
    RecordedFrames[CurrentRecordPosition].FrameInfoIndex := CurrentWorkingSetFrame.FrameInfoIndex;
  ChangeCurrentRecordPosition(CurrentRecordPosition + 1, False);
  Saved := False;
  RepaintAll;
  ShowTimes;
end;

procedure TMainForm.actReplaceInMovieUpdate(Sender: TObject);
begin
  actReplaceInMovie.Enabled := not Exporting and (CurrentRecordPosition <> -1) and (CurrentRecordPosition <= RecordedFrames.Count) and Assigned(CurrentWorkingSetFrame)
end;

procedure TMainForm.pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if PhotoFolder = '' then
  begin
    if (Button = mbRight) or ((Button = mbLeft) and (ssShift in Shift)) then // �������� ������ �� ������ �� ����+����� ������ ����, �� � ������ �� ������ ������ ����.
      actOpen.Execute
    else if (Button = mbLeft) then
      actSelectPhotoFolder.Execute;
  end;
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
  Image: TGraphic;
  WorkSetFrame: TRecordedFrame;
begin
//   Image := nil;
  try
    pbDisplay.Canvas.Brush.Color := $222222;// clBlack;
    pbDisplay.Canvas.FillRect(pbDisplay.ClientRect);
    pbDisplay.Canvas.Brush.Color := clBlack;//clBtnFace;

    if PhotoFolder = '' then
      begin
        R_MainScreen.Left := (pbDisplay.Width  - imgBackgroundSource.Picture.Width ) div 2;
        R_MainScreen.Top  := (pbDisplay.Height - imgBackgroundSource.Picture.Height) div 2;
        pbDisplay.Canvas.Draw(R_MainScreen.Left, R_MainScreen.Top, imgBackgroundSource.Picture.Graphic);
        Exit;
      end
    else
      begin
        CreateAdvertisementFrame; // �� ������ ������
        if (DisplayedFrameIndex >= 0) then
          LoadPhoto(DisplayedFrameIndex, -1); // �� ������ ������
        // �������� ����.
        // ������� ���� �������� ������, ������, ���� �� �� ����������� �� �������� ��� ���������.

        if not AdvertisementShowing and
          (DisplayedFrameIndex >= 0) and
          FrameInfoList[DisplayedFrameIndex].PreviewLoaded
        then
          Image := FrameInfoList[DisplayedFrameIndex].Preview
        else
          Image := AdvertisementFrameImagePreview;

        if mmiShowNeighbourFrames.Checked and Assigned(Image) then
          R_PrevNextPreview := StretchSize(Image.Width, Image.Height, PrevNextPreviewMaxSize, PrevNextPreviewMaxSize)
        else
          R_PrevNextPreview := Rect(0,0,0,0);

        if Assigned(Image) then
          begin
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
        imgCamPreview.BoundsRect := R_MainScreen;
        imgOverlay.BoundsRect := R_MainScreen;

        ScreenForm.Image := Image;

        if Assigned(CurrentWorkingSetFrame) and mmiShowNeighbourFrames.Checked then
          begin
            // ����� ���������
            if CameraForm.Active then
              WorkSetFrame := CurrentWorkingSetFrame
            else
              WorkSetFrame := FindWorkingSetFrameByOffset(-1);
            LoadPhoto(WorkSetFrame.FrameInfoIndex, -1); // �� ������ ������
            if FrameInfoList[WorkSetFrame.FrameInfoIndex].PreviewLoaded then
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
            // ������ ���������
            WorkSetFrame := FindWorkingSetFrameByOffset(+1);
            LoadPhoto(WorkSetFrame.FrameInfoIndex, -1); // �� ������ ������
            if FrameInfoList[WorkSetFrame.FrameInfoIndex].PreviewLoaded then
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
      end;
  except
    ; // �� ������ ������ ������ ������ ���������, ������ ��� ��� ��������� ������ �����
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
  rs_Framerate = '- �� %d, ��� %d ������ � �������';
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

      a := -(GetTickCount mod 1000)* 2 * Pi / 1000; // ������� �����������. ������ FrameRate ��� � ������� ����������������.
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

procedure TMainForm.pbRecordMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ChangeCurrentRecordPosition(Y + pbRecordOffset);
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
  pbFrameTip.Repaint;
end;


procedure TMainForm.MakeRecordedAudioLines;
var
  SamplesPerFrame: Integer;
  WaveFormat: TWaveFormatEx;
  CalculatedCount, FrameIndex: Integer;
  FirstSample: Integer;
  pSample: PSmallInt;
  MaxData, MinData: SmallInt;
  i: Integer;
type
  TSampleArray = array [0..256] of SmallInt;
  PSampleArray = ^TSampleArray;
begin
  CalculatedCount := CalculateSoundFramesCount;
  if RecordedAudioLines.Count = CalculatedCount then
    Exit;

  SetPCMAudioFormatS(@WaveFormat, AudioRecorder.PCMFormat);

  SamplesPerFrame := 2 * (WaveFormat.nSamplesPerSec div FrameRate);
  Assert(WaveFormat.wBitsPerSample = 16, '{F90018C7-D187-41DE-A30C-CB8D15A72149}');
  Assert(WaveFormat.nChannels = 2,       '{9655AC01-69A1-456D-B55E-027A9B04CA93}');

  CalculatedCount := RecordedAudioLines.Count;
  RecordedAudioLines.Count := CalculateSoundFramesCount;
  for FrameIndex := CalculatedCount to RecordedAudioLines.Count - 1 do begin
    FirstSample := FrameIndex * SamplesPerFrame;
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
    RecordedAudioLines[FrameIndex] := MulDiv(MaxData - MinData, MaxAudioLineWidth, $FFFF);
  end;
end;

procedure TMainForm.pbRecordPaint(Sender: TObject);
const
  FramesBorderSize = 5;
  FragmentsGap = 1;
  ScrollBarWidth = 2;
var
  y: Integer;
  DrawingRecordedFrameIndex: Integer;
  R: TRect;
  //TopOfMainRecord: Integer;
  DataWidth: Integer;
  DataWidthHalf: Integer;
  DataAreaHeight: Integer;

  procedure DrawSoundLine;
  var
    w: Integer;
  begin
    if DrawingRecordedFrameIndex >= RecordedAudioLines.Count then
      Exit;

    w := MulDiv(RecordedAudioLines[DrawingRecordedFrameIndex], DataWidthHalf, MaxAudioLineWidth);
    pbRecord.Canvas.MoveTo(R.Left + DataWidthHalf - w,     y);
    pbRecord.Canvas.LineTo(R.Left + DataWidthHalf + w + 1, y);
  end;

  procedure DrawScaleMark;
    procedure Draw(AThickness: Integer);
    var
      Text: string;
      TextSize: TSize;
    begin
//       pbRecord.Canvas.Brush.Color := clRed;
      pbRecord.Canvas.Brush.Style := bsSolid;
      pbRecord.Canvas.FillRect(Rect(R.Left, y, R.Right, y + AThickness));
      Text := FrameIndexToTimeStamp(DrawingRecordedFrameIndex, False);
      TextSize := pbRecord.Canvas.TextExtent(Text);
      pbRecord.Canvas.Brush.Style := bsFDiagonal; // bsClear �������-�� �� ���������� ������� � �����, �������� ���� ����� FillRect, ��������� �����
      pbRecord.Canvas.Font.Color := clBlack;
      pbRecord.Canvas.TextOut(pbRecord.Width - TextSize.cx - 8, y - TextSize.cy - 3, Text);
      pbRecord.Canvas.Brush.Style := bsSolid;
    end;
  begin
    if DrawingRecordedFrameIndex mod FrameRate        = 0 then Draw(1);
    if DrawingRecordedFrameIndex mod (FrameRate * 10) = 0 then Draw(2);
    if DrawingRecordedFrameIndex mod (FrameRate * 60) = 0 then Draw(3);
  end;

begin
  DataWidth := pbRecord.ClientWidth - FramesBorderSize - FramesBorderSize - ScrollBarWidth - 1;
  DataWidthHalf := DataWidth div 2;
  DataAreaHeight := pbRecord.ClientHeight - imgAdvertisementThumbnail.Height - FramesBorderSize * 4 - FragmentsGap;

  // Automatic scrollbar position
  pbRecordOffset := CurrentRecordPosition - (DataAreaHeight div 2);
  if (pbRecordOffset + DataAreaHeight) > RecordedFrames.Count + FrameRate then //  + FrameRate ���� ���� ������� ������ ����� ����� ����� ������� ����� ���� �������
    pbRecordOffset := RecordedFrames.Count + FrameRate - DataAreaHeight;
  if pbRecordOffset < 0 then
    pbRecordOffset := 0;

  MakeRecordedAudioLines; // ����������� �������������, ����� ����������

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

  pbRecord.Canvas.Font.Color := clLtGray;

  pbRecord.Canvas.Brush.Color := clLtGray;

  pbRecord.Canvas.Pen.Color := clSkyBlue;
  pbRecord.Canvas.Pen.Width := 1;
  pbRecord.Canvas.Pen.Style := psSolid;
  // TopOfMainRecord := R.Top;

  // content of main record
  DrawingRecordedFrameIndex := pbRecordOffset;
  for y := R.Top to R.Bottom - 1 do
    begin
      DrawSoundLine;

      DrawScaleMark;

      pbRecord.Canvas.Pixels[R.Left + MulDiv(RecordedFrames[DrawingRecordedFrameIndex].FrameInfoIndex, DataWidth, FrameInfoCount - 1), y] := clBlack;

      Inc(DrawingRecordedFrameIndex);
    end;

  // current position in main record
  pbRecord.Canvas.Brush.Color := clMaroon;
  if not AdvertisementShowing and (CurrentRecordPosition >= 0) and (CurrentRecordPosition < RecordedFrames.Count) then
    with Point(
      MulDiv(RecordedFrames[CurrentRecordPosition].FrameInfoIndex, DataWidth, FrameInfoCount - 1) + R.Left,
      CurrentRecordPosition - pbRecordOffset + R.Top
    ) do
      pbRecord.Canvas.Ellipse(x-2, y-2, x+3, y+3);

  if AdvertisementEnabled then
  begin
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
    // Inc(TopOfMainRecord, R.Height);
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
  end;

  // Tail of sound
  pbRecord.Canvas.Pen.Color := clMoneyGreen;
  pbRecord.Canvas.Pen.Width := 1;
  pbRecord.Canvas.Pen.Style := psSolid;

  DrawingRecordedFrameIndex := RecordedFrames.Count;
  for y := R.Bottom + FramesBorderSize to pbRecord.Height do
    begin
      DrawSoundLine;
      Inc(DrawingRecordedFrameIndex);
    end;
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
  // ������ ��������� 200 ���������� ����� � ���������.
  // ��������� ��������, � �������� ���.
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
  if Assigned(FrameTipRecordedFrame) then
    FrameTipArrow := WorkingFrameIndexToWorkingSetX(FrameTipRecordedFrame.Index) - pbFrameTip.Left
  else
    FrameTipArrow := -1;

  pbFrameTip.Repaint;
end;

function TMainForm.WorkingFrameIndexToWorkingSetX(AWorkingSetFrameIndex: Integer): Integer;
begin
  Result := 4 + 8 + MulDiv(AWorkingSetFrameIndex, pbWorkingSet.Width - 24, WorkingSetFrames.Count - 1);
end;

function TMainForm.WorkingSetXToWorkingFrame(X: Integer): TRecordedFrame;
var
  FrameIndex: Integer;
begin
  Result := nil;
  if WorkingSetFrames.Count > 0 then
    begin
      FrameIndex := MulDiv(X - 12, WorkingSetFrames.Count, pbWorkingSet.Width - 24);

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
  i, WorkingFrameIndex, CurrentWorkingFrameIndex: integer;
  WorkingFrameInfo: TFrameInfo;
  BookmarkIndex: Integer;
  x, y1, y2: Integer;
  BookmarkText: string;
  BookmarkRect: TRect;
  TextSize: TSize;
  BoxWidth, MinBoxWidth: Integer;
  MidFrameOffset: Integer;
begin
  if lvFrameset.Visible then
    Exit;

  y1 := 4;
  MinBoxWidth := pbWorkingSet.Canvas.TextExtent('8').cx;
  if WorkingSetFrames.Count > 2 then
    MidFrameOffset := ((pbWorkingSet.Width - 24) div (WorkingSetFrames.Count - 1)) div 2 + 1
  else
    MidFrameOffset := 50;
  if Assigned(CurrentWorkingSetFrame) then
    CurrentWorkingFrameIndex := CurrentWorkingSetFrame.Index
  else
    CurrentWorkingFrameIndex := -1;

  for SecondIndex := 0 to MulDiv(WorkingSetFrames.Count, CurrentSpeedInterval, FrameRate) - 1 do
    begin
      x := 4 + 8 + MulDiv(pbWorkingSet.Width - 24, SecondIndex, MulDiv(WorkingSetFrames.Count, CurrentSpeedInterval, FrameRate));
      pbWorkingSet.Canvas.Brush.Style := bsSolid;
      pbWorkingSet.Canvas.Rectangle(x, y1 + 8, x + 1, y1 + 16);
    end;

  if WorkingSetFrames.Count > 0 then
    for i := -1 to WorkingSetFrames.Count - 1 do
      begin
        WorkingFrameIndex := i;
        if i = -1 then // -1 - ��� ����, ���� ���������� ������� ������ �������� ��� ������� ����� ������ ������ ����
          WorkingFrameIndex := WorkingSetFrames.Count - 1;

        x := WorkingFrameIndexToWorkingSetX(i);
        WorkingFrameInfo := FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex];
        if WorkingFrameInfo.PreviewLoaded then
          y2 := y1 + 8
        else
          y2 := y1 + 4;

        if i <> -1 then begin
          pbWorkingSet.Canvas.Brush.Style := bsSolid;
          if WorkingFrameIndex <> CurrentWorkingFrameIndex then
            pbWorkingSet.Canvas.Rectangle(x, y1, x + 1, y2)
          else
            begin
              pbWorkingSet.Canvas.Brush.Color := clHighlight;
              pbWorkingSet.Canvas.Rectangle(x - 1, y1, x + 2, y2);
            end;

          for BookmarkIndex := Low(Bookmarks) to High(Bookmarks) do
            if WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex = Bookmarks[BookmarkIndex] then
            begin
              BookmarkText := BookmarkKey[BookmarkIndex];
              TextSize := pbWorkingSet.Canvas.TextExtent(BookmarkText);
              BoxWidth := Max(TextSize.cx, MinBoxWidth) + 4;
              BookmarkRect := Rect(x - BoxWidth div 2, y1 - 2, x + BoxWidth div 2, y1 + TextSize.cy);
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
              Inc(BookmarkRect.Left, (BoxWidth - TextSize.cx) div 2);
              pbWorkingSet.Canvas.Brush.Style := bsClear;
    //          pbWorkingSet.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop]);
              pbWorkingSet.Canvas.TextOut(BookmarkRect.Left, BookmarkRect.Top, BookmarkText);
            end;
          end;

        x := x + MidFrameOffset;
        case WorkingFrameInfo.FlagAfter of
          affNone: Continue;
          affTeleport: BookmarkText := BookmarkKey[FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].TeleportTargetBookmark];
          affStopper: BookmarkText := 'x';
          affBouncer: BookmarkText := '�';
          affLooper:  BookmarkText := '@';
        end;
        TextSize := pbWorkingSet.Canvas.TextExtent(BookmarkText);
        BoxWidth := Max(TextSize.cx, MinBoxWidth) + 4;
        BookmarkRect := Rect(x - BoxWidth div 2, y1 - 2, x + BoxWidth div 2, y1 + TextSize.cy);
        pbWorkingSet.Canvas.Brush.Style := bsSolid;
        if not FlagsEnabled or
          ((WorkingFrameInfo.FlagAfter = affTeleport) and (Bookmarks[WorkingFrameInfo.TeleportTargetBookmark] = -1))
        then begin
          pbWorkingSet.Canvas.Brush.Color := clLtGray;
          pbWorkingSet.Canvas.Font.Color := clDkGray;
        end else if WorkingFrameIndex = CurrentWorkingFrameIndex then begin
          case WorkingFrameInfo.FlagAfter of
            affTeleport: pbWorkingSet.Canvas.Brush.Color := clLime;
            affStopper:  pbWorkingSet.Canvas.Brush.Color := $9999FF;
            affBouncer:  pbWorkingSet.Canvas.Brush.Color := $FF9999;
            affLooper:   pbWorkingSet.Canvas.Brush.Color := $99FF99;
          end;
          pbWorkingSet.Canvas.Font.Color := clBlack;
        end else begin
          case WorkingFrameInfo.FlagAfter of
            affTeleport: pbWorkingSet.Canvas.Brush.Color := clGreen;
            affStopper:  pbWorkingSet.Canvas.Brush.Color := $2222FF;
            affBouncer:  pbWorkingSet.Canvas.Brush.Color := $FF2222;
            affLooper:   pbWorkingSet.Canvas.Brush.Color := $22FF22;
          end;
          pbWorkingSet.Canvas.Brush.Style := bsSolid;
          pbWorkingSet.Canvas.Font.Color := clWhite;
        end;
        with BookmarkRect do
          pbWorkingSet.Canvas.RoundRect(Left, Top + TextSize.cy, Right, Bottom + TextSize.cy, 2, 2);
        Inc(BookmarkRect.Left, (BoxWidth - TextSize.cx) div 2);
        pbWorkingSet.Canvas.Brush.Style := bsClear;
        pbWorkingSet.Canvas.TextOut(BookmarkRect.Left, BookmarkRect.Top + TextSize.cy, BookmarkText);
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
  // ���� ���� ����������, � ������ ��������, �������� ���� ���������� ���������, �� �� ���������� �, � �������.
  if not (Value in [caPrevFlag, caNextFlag]) then
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
    if FrameInfoList[i].PreviewLoaded then
      begin
        FrameInfoList[i].Unload;
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
  FreeAdvertisementFrame;
  CreateAdvertisementFrame;
  mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + TrimLeft(TMenuItem(Sender).Caption);
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.actExportToGifClick(Sender: TObject);
begin
  GifPreviewForm.Execute(RecordedFrames);
end;

procedure TMainForm.actExportResolutionCustomExecute(Sender: TObject);
begin
  Stop;
  if TExportSizeCustomRequestDialog.Execute(ExportSize) then
    mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);
  FreeAdvertisementFrame;
  CreateAdvertisementFrame;
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.CaptureFirstFrameSizes;
begin
  if (FrameInfoCount > 0) and FileExists(FrameInfoList[0].FullFileName) then
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
  FreeAdvertisementFrame;
  CreateAdvertisementFrame;
  mmiExportResolution.Caption := Copy(mmiExportResolution.Caption, 1, Pos('-', mmiExportResolution.Caption) + 1) + Format(rs_CustomSize, [ExportSize.cx, ExportSize.cy]);
  if FrameInfoCount > 0 then
    Saved := False;
end;

procedure TMainForm.actExportResolutionFirstFrameUpdate(Sender: TObject);
begin
  actExportResolutionFirstFrame.Enabled := (FrameInfoCount > 0);
end;

function TMainForm.FlagsEnabled: Boolean;
begin
  Result := not ((GetAsyncKeyState(VK_SHIFT) < 0) xor (GetKeyState(VK_CAPITAL) and 1 = 1)); // ���� ��, ���� ������...
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
          pbRecord.Repaint;
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
        caPlayBackward, caPlayForward, // ������ ������������� �������� actBackwardWhilePressed � actForwardWhilePressed
        caNone:
          if Interval <= 1 then
            begin
              //  ��� ����� ��� ��������� � ���� � � ������������ ������� ������ IsShortCut
              if actBackwardWhilePressed.Checked or (
                (GetAsyncKeyState(VK_CONTROL) >= 0) and
                ((GetAsyncKeyState(Ord('A')) < 0) or (GetAsyncKeyState(Ord('C')) < 0))
              )
              then begin
                AutoMove(-1);
              end else if actForwardWhilePressed.Checked or (
                (GetAsyncKeyState(VK_CONTROL) >= 0) and
                ((GetAsyncKeyState(Ord('D')) < 0) or (GetAsyncKeyState(Ord('M')) < 0))
              )
              then begin
                AutoMove(1);
              end else
                case NextControlAction of
                  caPlayBackward: AutoMove(-1);
                  caPlayForward:  AutoMove( 1);
                  caNone: begin
                    AutoMovementStopped := False; // ����� ��������� ������������, ���� ������� �� ������, �� ��������� ������� ������ ����� ���������
                    SkipFirstStopper := True;
                  end;
                end;
            end;
      end;

      if Recording then
        begin
          if not Assigned(CurrentWorkingSetFrame) then
            CurrentWorkingSetFrame := WorkingSetFrames[0];
          TRecordedFrame.Create(RecordedFrames, CurrentWorkingSetFrame.FrameInfoIndex);
          pbRecord.Repaint;
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
  btnFrameSetExpandClick(nil);
  actWorkingSetManagement.Checked := lvFrameset.Visible;
end;

procedure TMainForm.actWorkingSetManagementUpdate(Sender: TObject);
begin
  actWorkingSetManagement.Enabled := PhotoFolder <> '';
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
  pbDisplay.Repaint;
  ScreenForm.StretchImages := actStretchImages.Checked;
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
  CheckStopCamera;
  if Recording then
    begin
      actRecord.Execute;
      AudioRecorder.WaitForStop;
    end;

  Playing := not Playing; // ��� ����� ���������� CurrentRecordPosition, ����� � ��� ������������ � ������� �����.
  if (CurrentRecordPosition >= RecordedFrames.Count) or AdvertisementShowing then
    CurrentRecordPosition := 0
  else
    CurrentRecordPosition := CurrentRecordPosition; // ������ ���������������� �����. TODO: ��� ��������� ���-��...

  StockAudioPlayer.Active := Playing;
  // ������ ������ ��������������� � ����������� ��������� -
  // ����� ����������� StockAudioPlayerActivate � StockAudioPlayerDeactivate:
  // ��������� ��������������� ������ ������ ����� ����, ��� ���� ����� ����� ����������������.
  // �� ��� ������������� �������, �� ������ ������, ����� �����, � �� ����� ���� ���������.
  if not Playing then
    begin
      actPlay.Checked := False;

      //CurrentRecordPosition := 0;
      Interval := 0;
    end;
end;

procedure TMainForm.AddNewFrame(AFileName: string);
var
  NewFrameInfoIndex: Integer;
  NewRecordedFrame: TRecordedFrame;
begin
  NewFrameInfoIndex := FFrameInfoList.Add(
    TFrameInfo.Create(
      ExtractRelativePath(PhotoFolder, ExtractFilePath(AFileName)),
      ExtractFileName(AFileName)
    )
  );

  if Assigned(CurrentWorkingSetFrame) then
    NewRecordedFrame := TRecordedFrame(WorkingSetFrames.Insert(CurrentWorkingSetFrame.Index + 1))
  else
    NewRecordedFrame := TRecordedFrame(WorkingSetFrames.Add);
  NewRecordedFrame.FrameInfoIndex := NewFrameInfoIndex;
  CurrentWorkingSetFrame := NewRecordedFrame;

  // FFrameInfoList � ����� ��������� ����������.
  // � ���� ����� ������� ��� ���, �� ��� ������ ��� ����� �����, ��� �� ����������� ���������.
  if ProjectFileName <> '' then
    Saved := False;
end;

procedure TMainForm.Stop;
begin
  if Recording then
    StopRecording;
  if Playing then
    StopPlaying;
// Stop ����������� ����� ������� ����������, �� ������� ����� ����������. � � ���� ������ ������� ������ ���� ������������
//  if Exporting then
//    ExportCancelled := True;
end;

procedure TMainForm.CheckStopCamera;
begin
  if Assigned(CameraForm) and CameraForm.Active then
    CameraForm.Active := False;
end;

function TMainForm.FrameIndexToTimeStamp(AFrameIndex: Integer; AShowZeroFrameIndex: Boolean = True): string;
var
  d, d2: Integer;
begin
  d := AFrameIndex;
  d2 := d mod FrameRate;
  d := d div FrameRate;
  // frames in last second
  if (d2 <> 0) or AShowZeroFrameIndex then begin
    Result := Format('[%2.2d]', [d2]);
    if d > 0 then
      Result := ':' + Result;
  end;
  // seconds
  if d < 60 then
    Result := Format('%2.2d', [d]) + Result
  else
    begin
      d2 := d mod 60;
      // seconds in a last minute
      Result := Format('%2.2d', [d2]) + Result;
      d := d div 60;
      // minutes
      if d < 60 then
        Result := Format('%2.2d', [d]) + ':' + Result
      else
      begin
        d2 := d mod 60;
        // minutes in a last hour
        Result := Format('%2.2d', [d2]) + ':' + Result;
        d := d div 60;
        // hours
        if d < 24 then
          Result := Format('%2.2d', [d]) + ':' + Result
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
  RecordedAudioLines.Clear;
end;

procedure TMainForm.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
resourcestring
  rs_PreloadStatus = '�������� �����: ';
var
  i: integer;
  FrameInfo: TFrameInfo;
begin
  Done := True;

  if SettingsChanged then
    SaveSettings;

  if not OutOfMemoryRaised then
    for i := 0 to WorkingSetFrames.Count - 1 do
      begin
        FrameInfo := FrameInfoList[WorkingSetFrames[i].FrameInfoIndex];
        if (not lvFrameset.Visible and not FrameInfo.PreviewLoaded) or (lvFrameset.Visible and not Assigned(FrameInfo.Iconic)) then
          begin
            SetStatus(rs_PreloadStatus + FrameInfo.RelativeFileName);
            LoadPhoto(WorkingSetFrames[i].FrameInfoIndex, i);
            if not lvFrameset.Visible then
              begin
                pbWorkingSet.Repaint;
                ShowTimes;
              end;
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
  AutoMovementStopped := False;
  SkipFirstStopper := True;
end;

procedure TMainForm.btnBackwardWhilePressedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actBackwardWhilePressed.Checked := False;
  AutoMovementStopped := False;
  SkipFirstStopper := True;
end;

procedure TMainForm.btnForwardWhilePressedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actForwardWhilePressed.Checked := True;
end;

procedure TMainForm.btnForwardWhilePressedMouseLeave(Sender: TObject);
begin
  actForwardWhilePressed.Checked := False;
  AutoMovementStopped := False;
  SkipFirstStopper := True;
end;

procedure TMainForm.btnForwardWhilePressedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actForwardWhilePressed.Checked := False;
  AutoMovementStopped := False;
  SkipFirstStopper := True;
end;

procedure TMainForm.btnFrameSetExpandClick(Sender: TObject);
var
  WorkingFrameIndex, ItemIndex: Integer;
  Item: TListItem;
  FrameInfo: TFrameInfo;
  FramesInOriginalOrder: array of TRecordedFrame;
begin
  CheckStopCamera;
  Stop;
  if not lvFrameset.Visible then
  begin
    lvFrameset.Items.BeginUpdate;
    ilFrameset.BeginUpdate;
    try
      lvFrameset.Clear;
      ilFrameset.Clear;
      for WorkingFrameIndex := 0 to WorkingSetFrames.Count - 1 do
      begin
        FrameInfo := FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex];
        Item := lvFrameset.Items.Add;
        Item.Caption := FrameInfo.FileName;
        Item.ImageIndex := WorkingFrameIndex;
        if Assigned(FrameInfo.Iconic) then
          ilFrameset.Add(FrameInfo.Iconic, nil)
        else
          ilFrameset.Add(nil, nil);
      end;
      if Assigned(CurrentWorkingSetFrame) then
        lvFrameset.ItemIndex := CurrentWorkingSetFrame.Index;
    finally
      lvFrameset.Items.EndUpdate;
      ilFrameset.EndUpdate;
      lvFrameset.Visible := True;
    end;
    lvFrameset.BringToFront;
    lvFrameset.SetFocus;
  end
  else
    begin
      lvFrameset.Visible := False;
      if lvFrameset.ItemIndex <> -1 then
        CurrentWorkingSetFrame := WorkingSetFrames[lvFrameset.Items[lvFrameset.ItemIndex].ImageIndex];
      SetLength(FramesInOriginalOrder, WorkingSetFrames.Count);
      for WorkingFrameIndex := 0 to WorkingSetFrames.Count - 1 do
        FramesInOriginalOrder[WorkingFrameIndex] := WorkingSetFrames[WorkingFrameIndex];
      for ItemIndex := lvFrameset.Items.Count - 1 downto 0 do
        begin
          FramesInOriginalOrder[lvFrameset.Items[ItemIndex].ImageIndex].Index := ItemIndex;
          FramesInOriginalOrder[lvFrameset.Items[ItemIndex].ImageIndex] := nil;
          Saved := False;
        end;
      // ��� �� ������� � lvFrameset.Items � �� ��� ����� �� ������ FramesInOriginalOrder - ������������.
      for WorkingFrameIndex := 0 to WorkingSetFrames.Count - 1 do
        FramesInOriginalOrder[WorkingFrameIndex].Free;
    end;
  actWorkingSetManagement.Checked := lvFrameset.Visible;
  RepaintAll;
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

procedure TMainForm.AutoMove(AOffset: Integer);
begin
  if AutoMovementStopped then
    Exit;
  if AOffset = 0 then
    Exit;
  if WorkingSetFrames.Count = 0 then
    Exit;
  if CurrentWorkingSetFrame = nil then
    Exit;
  CurrentWorkingSetFrame := MoveToFrameByOffset(CurrentWorkingSetFrame.Index, AOffset, False, FlagsEnabled);
end;

function TMainForm.FindWorkingSetFrameByOffset(AOffset: Integer): TRecordedFrame;
begin
  if AOffset = 0 then
    Exit(CurrentWorkingSetFrame);
  if WorkingSetFrames.Count = 0 then
    Exit(nil);
  if CurrentWorkingSetFrame = nil then
    Exit(nil);
  Result := MoveToFrameByOffset(CurrentWorkingSetFrame.Index, AOffset, True, FlagsEnabled);
end;

function TMainForm.WrapFrameIndex(AIndex: Integer): Integer;
begin
  Result := AIndex;
  if Result < 0 then
    Result := WorkingSetFrames.Count + Result;
  if Result > (WorkingSetFrames.Count - 1) then
    Result := Result - WorkingSetFrames.Count;
end;

function TMainForm.FindNearestLooper(AIndex, ADirection: Integer): Integer;
var
  i: Integer;
  FrameAtLeft: TRecordedFrame;
begin
  // ������������� ����������� ������ ������������ ��������� ����������� ��������
  ADirection := -ADirection;
  // ��������� � ����������� �����, � ���� ����� �������� ������� �����,
  // ���� �� �������� ���� �� ������� ����� �� ��� ��
  i := MoveToFrameByOffset(AIndex, ADirection, True, False).Index;
  repeat
    FrameAtLeft := nil; // hide warning
    // ���� ������ - �� ���� ����� ��� ���, ��� �� ����������
    if ADirection > 0 then
      FrameAtLeft := WorkingSetFrames[i];

    // ��������� � ���������� �����
    i := MoveToFrameByOffset(i, ADirection, True, False).Index;

    // ���� ����� - �� ���� ����� ��� ���, ��� ���������
    if ADirection < 0 then
      FrameAtLeft := WorkingSetFrames[i];

    if FrameInfoList[FrameAtLeft.FrameInfoIndex].HaveLooperAfter then
      // ��������� �� ����, ������������� � �������� ������������ ����� ���������� �������
      Exit(MoveToFrameByOffset(i, -ADirection, True, False).Index);
  until i = AIndex;
  Exit(AIndex);
end;

function TMainForm.MoveToFrameByOffset(AInitialIndex, AOffset: Integer; ASkipAutomovementFlags, AFollowFlags: Boolean): TRecordedFrame;
var
  Count, Direction: Integer;
  i: Integer;
  ResultIndex, TeleportBookmarkIndex: Integer;
  FrameAtLeft: TRecordedFrame;
begin
  Result := nil; // hide warning
  Count := Abs(AOffset);
  Direction := AOffset div Count;
  for i := 1 to Count do
    begin
      ResultIndex := WrapFrameIndex(AInitialIndex + Direction);
      Result := WorkingSetFrames[ResultIndex];

      if AFollowFlags then begin
        if Direction > 0 then
          FrameAtLeft := WorkingSetFrames[AInitialIndex]
        else // Direction < 0
          FrameAtLeft := Result;

        if not ASkipAutomovementFlags and FrameInfoList[FrameAtLeft.FrameInfoIndex].HaveBouncerAfter then begin
          Direction := - Direction;
          ResultIndex := WrapFrameIndex(AInitialIndex + Direction);
          Result := WorkingSetFrames[ResultIndex];
          if (NextControlAction <> caNone) then begin
            if NextControlAction = caPlayBackward then
              ReplaceControlActions(caPlayForward)
            else
              ReplaceControlActions(caPlayBackward);
            UpdatePlayActions;
          end;
        end;

        if not ASkipAutomovementFlags then begin
          if not SkipFirstStopper then
            if FrameInfoList[FrameAtLeft.FrameInfoIndex].HaveStopperAfter then begin
              Result := WorkingSetFrames[AInitialIndex];
              if not AutoMovementStopped and (NextControlAction <> caNone) then begin
                AutoMovementStopped := True;
                ReplaceControlActions(caNone);
                UpdatePlayActions;
              end;
              Exit;
            end;
          // ������ ������� ��������, ���� ���� ��������, � ������ ��������� �������� �� ������ � ���� ������.
          SkipFirstStopper := False;
        end;

        if FrameInfoList[FrameAtLeft.FrameInfoIndex].HaveLooperAfter then begin
          ResultIndex := FindNearestLooper(ResultIndex, Direction);
          Result := WorkingSetFrames[ResultIndex];
        end;

        TeleportBookmarkIndex := FrameInfoList[FrameAtLeft.FrameInfoIndex].TeleportTargetBookmark;
        if (TeleportBookmarkIndex <> -1) and (Bookmarks[TeleportBookmarkIndex] <> -1) then begin
          Result := WorkingSetFrames.FindByFrameIndex(Bookmarks[TeleportBookmarkIndex]);
          ResultIndex := Result.Index;
        end;
      end;
      AInitialIndex := ResultIndex;
    end;
end;

function TMainForm.IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean;
begin
  if not Active then
    if not (
      (Screen.ActiveForm is TControllerForm) or
      (Screen.ActiveForm is TScreenForm)
    ) then
      Exit(False);

  if not (
    // ���� �� �������, �������������� ������ ��������, �� ������������ ��-�������� ��� ������������� ShortCut
    // � ������ �������� �������������� ������� �������� �� ������ ��� �� ������� Ctrl � Alt
        (GetAsyncKeyState(VK_CONTROL) >= 0)
    and (GetAsyncKeyState(VK_MENU) >= 0)
    and (
         (Message.CharCode = vk_Left)
      or (Message.CharCode = vk_Right)
      or (Message.CharCode = ord('A'))
      or (Message.CharCode = ord('C'))
      or (Message.CharCode = ord('D'))
      or (Message.CharCode = ord('M'))
    )
  )
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
  FreeAdvertisementFrame;
end;

procedure TMainForm.FreeAdvertisementFrame;
begin
  FreeAndNil(AdvertisementFrameImage);
  FreeAndNil(AdvertisementFrameImagePreview);
end;

{ TFrame }

constructor TFrameInfo.Create(ARelativePath, AFileName: string);
begin
  FRelativePath := ARelativePath;
  FFileName := AFileName;
  TeleportTargetBookmark := -1;
end;

destructor TFrameInfo.Destroy;
begin
  Unload;
  inherited Destroy;
end;

function TFrameInfo.RelativeFileName: string;
begin
  Result := RelativePath + FileName;
end;

procedure TFrameInfo.SetFlagAfter(const Value: TAfterFrameFlag);
begin
  FFlagAfter := Value;
  if FFlagAfter <> affTeleport then
    FTeleportTargetBookmark := -1;
end;

procedure TFrameInfo.SetHaveBouncerAfter(const Value: Boolean);
begin
  if Value then
    FlagAfter := affBouncer
  else
    FlagAfter := affNone;
end;

procedure TFrameInfo.SetHaveLooperAfter(const Value: Boolean);
begin
  if Value then
    FlagAfter := affLooper
  else
    FlagAfter := affNone;
end;

procedure TFrameInfo.SetHaveStopperAfter(const Value: Boolean);
begin
  if Value then
    FlagAfter := affStopper
  else
    FlagAfter := affNone;
end;

procedure TFrameInfo.SetTeleportTargetBookmark(const Value: Integer);
begin
  FTeleportTargetBookmark := Value;
  if FTeleportTargetBookmark <> -1 then
    FFlagAfter := affTeleport
  else
    FFlagAfter := affNone;
end;

procedure TFrameInfo.Unload;
begin
  PreviewLoaded := False;
  FreeAndNil(Preview);
  FreeAndNil(Iconic);
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

function TFrameInfo.GetHaveBouncerAfter: Boolean;
begin
  Result := FFlagAfter = affBouncer;
end;

function TFrameInfo.GetHaveLooperAfter: Boolean;
begin
  Result := FFlagAfter = affLooper;
end;

function TFrameInfo.GetHaveStopperAfter: Boolean;
begin
  Result := FFlagAfter = affStopper;
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

procedure TMainForm.actRemoveRecordedFrameExecute(Sender: TObject);
begin
  RecordedFrames[RecordedFrames.Count - 1].Free;
  if CurrentRecordPosition = RecordedFrames.Count then
    ChangeCurrentRecordPosition(CurrentRecordPosition - 1, False);
  Saved := False;
  RepaintAll;
  ShowTimes;
end;

procedure TMainForm.actRemoveRecordedFrameUpdate(Sender: TObject);
begin
  actRemoveRecordedFrame.Enabled := not Exporting and (RecordedFrames.Count > 0);
end;

procedure TMainForm.actSelectAudioFileUpdate(Sender: TObject);
begin
  actSelectAudioFile.Enabled := not Exporting and (PhotoFolder <> '');
  mmiStopRecordingOnSoundtrackFinish.Enabled := actSelectAudioFile.Checked;
end;

procedure TMainForm.lvFramesetKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
if Key = VK_DELETE then
    for i := lvFrameset.Items.Count - 1 downto 0 do
      if lvFrameset.Items[i].Selected then
        lvFrameset.Items[i].Delete;
end;

procedure TMainForm.lvFramesetDblClick(Sender: TObject);
begin
  btnFrameSetExpand.Click;
end;

procedure TMainForm.lvFramesetDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  MovedItems: TList<TListItem>;
  MovedItem, TargetItem: TListItem;
  TargetIndex: Integer;
  MovedItemIndex: Integer;
  MovedItemCaption: string;
  TargetPosition: TPoint;
  i: Integer;

  procedure MoveItem;
  begin
    MovedItemIndex :=  MovedItem.ImageIndex;
    MovedItemCaption := MovedItem.Caption;
    lvFrameset.Items.Delete(MovedItem.Index);
    TargetItem := lvFrameset.Items.Insert(TargetIndex);
    TargetItem.Caption := MovedItemCaption;
    TargetItem.ImageIndex := MovedItemIndex;
    if TargetPosition.x <> -1 then
      TargetItem.Position := TargetPosition;
  end;

begin
  MovedItems := TList<TListItem>.Create;
  try
    MovedItems.Capacity := lvFrameset.SelCount;
    for i := 0 to lvFrameset.Items.Count - 1 do
      if lvFrameset.Items[i].Selected then
        MovedItems.Add(lvFrameset.Items[i]);

    TargetItem := lvFrameset.GetItemAt(X, Y);
    if Assigned(TargetItem) then
      begin
        TargetIndex := TargetItem.Index;
        TargetPosition := TargetItem.Position;
      end
    else
      begin
        TargetIndex := lvFrameset.Items.Count - 1;
        TargetPosition.x := -1;
      end;
    // ������� ����� ��, ��� ���� ����� �����
    for i := 0 to MovedItems.Count - 1 do begin
      MovedItem := MovedItems[i];
      if TargetIndex > MovedItem.Index then
        MoveItem;
    end;
    // ������� ����� ��, ��� ���� ����� ����.
    // ���� �� ������� ���� ������� � ������ �������, ��� �������� � ��������.
    // ������� ������ � ��������, ���� ���������� � ������.
    for i := MovedItems.Count - 1 downto 0 do begin
      MovedItem := MovedItems[i];
      if TargetIndex < MovedItem.Index then
        MoveItem;
    end;
    for i := 0 to MovedItems.Count - 1 do
      MovedItems[i].Selected := True;
  finally
    MovedItems.Free;
  end
end;

procedure TMainForm.lvFramesetDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(lvFrameset.ItemFocused);
end;

procedure TMainForm.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    if (WindowState = wsNormal) and not actFullScreenMode.Checked then // ���������� ������ �� ��������� ��� ����������
    begin
      IniFile.WriteInteger('LastUsed', 'MainWindowLeft', Left);
      IniFile.WriteInteger('LastUsed', 'MainWindowTop', Top);
      IniFile.WriteInteger('LastUsed', 'MainWindowWidth', Width);
      IniFile.WriteInteger('LastUsed', 'MainWindowHeight', Height);
    end;
    IniFile.WriteBool('LastUsed', 'MainWindowMaximized', WindowState = wsMaximized);
    IniFile.WriteInteger('LastUsed', 'RightPaneSize', RecordSplitter.Parent.ClientWidth - RecordSplitter.Left);
    GifPreviewForm.SaveSettings(IniFile);
  finally
    IniFile.Free;
  end;
  SettingsChanged := False;
end;

procedure TMainForm.LoadSettings;
var
  IniFile: TIniFile;
//   LastUsedCam: string;
//   LastUsedResolution: Integer;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Left := IniFile.ReadInteger('LastUsed', 'MainWindowLeft', Left);
    Top := IniFile.ReadInteger('LastUsed', 'MainWindowTop', Top);
    Width := IniFile.ReadInteger('LastUsed', 'MainWindowWidth', Width);
    Height := IniFile.ReadInteger('LastUsed', 'MainWindowHeight', Height);
    if IniFile.ReadBool('LastUsed', 'MainWindowMaximized', False) then
      WindowState := wsMaximized;
    RecordSplitter.Left  := RecordSplitter.Parent.ClientWidth - IniFile.ReadInteger('LastUsed', 'RightPaneSize', RecordSplitter.Parent.ClientWidth - RecordSplitter.Left);
    GifPreviewForm.LoadSettings(IniFile);
  finally
    IniFile.Free;
  end;
  SettingsChanged := False;
end;

initialization
  TPicture.RegisterFileFormat('jpe', sJPEGImageFile, TJPEGImage);
end.

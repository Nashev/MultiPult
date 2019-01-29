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
  Gauges, Buttons, Math, ComCtrls, mmSystem,
  WaveUtils, WaveStorage, WaveOut, WavePlayers, WaveIO, WaveIn, WaveRecorders, WaveTimer,
  ToolWin, ExtActns, Vcl.StdActns, System.Actions, Vcl.AppEvnts,
  Vcl.Imaging.pngimage, Vcl.Imaging.GIFimg, System.ImageList{$IFDEF Delphi6}, Actions{$ENDIF}, System.IOUtils;

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
    FRelativePath, FFileName: string;
  public
    Preview: TBitmap;
    Iconic: TBitmap;
    Teleport: Integer; // TODO: move to TRecordedFrame(List) (?)
    PreviewLoaded: Boolean;
    function ImageFromDisc: TGraphic;
    function GenerateStubFrame(ErrorMessage: string): TGraphic;
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
    mmiSuspendTeleports: TMenuItem;
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
    mmiClearBookmarks: TMenuItem;
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
    procedure pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
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
  private
    NextControlActionStack: array [1..ControlActionStackDeep] of TControlAction;
    NextControlActionStackPosition: Integer;
    SettingsChanged: Boolean;
    ExportCancelled: Boolean;
    //CameraWaiting: Boolean; // TODO: сделать событие про отрисовку кадра с камеры и обработчик тут для него
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
    procedure ClearTeleports;
    procedure SetPhotoFolder(const Value: string);
    procedure CameraFormActiveChanged(Sender: TObject);
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
    procedure OpenNewPhotoFolder(ANewPhotoFolder: string);
    procedure RecalculatePreview;
    procedure CreateAdvertisementFrame;
    procedure ShowTimes;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
  public
    AdvertisementFrameImagePreview: TBitmap;
    AdvertisementShowing: Boolean;
    AdvertisementDuration: Integer;
    ExportSize: TSize;
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
    // Used in ScreenForm
    procedure LoadPhoto(AFrameInfoIndex: Integer; AWorkingSetIndex: Integer);
    procedure AddNewFrame(AFileName: string);
  end;

var
  MainForm: TMainForm;

var
  FrameRate: byte = 25;

implementation
uses
  AVICompression, ControllerFormUnit, ScreenFormUnit, Vcl.Imaging.JConsts,
  ExportSizeCustomRequestDialogUnit, ShellAPI, WorkingSetManagementFormUnit,
  CameraFormUnit, MP3ConvertFormUnit, MovieNameDialogUnit, IniFiles,
  ProgressFormUnit, UtilsUnit, GifPreviewUnit, System.Generics.Collections;
{$R *.dfm}

function Size(AX, AY: Integer): TSize;
begin
  Result.cx := AX;
  Result.cy := AY;
end;

function TMainForm.CheckBeforeOpenAudio: Boolean;
resourcestring
  rs_SaveAudioBeforeOpenRequest = 'Хотите сохранить записанную озвучку перед подключением готовой?';
begin
  Stop;
  Result := True;
  if (WaveStorage.Wave.Length > 0) and (FExternalAudioFileName = '') then
    case MessageBox(0, PChar(rs_SaveAudioBeforeOpenRequest), PChar(Application.Title), MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1) of
      IDCANCEL:
        Result := False;
      IDYES:
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
          actSave.Execute;
          if not Saved then
            Abort;
        end;
      IDNO: ;
    end;
end;

procedure TMainForm.actSelectPhotoFolderClick(Sender: TObject);
var
  NewPhotoFolder: string;
  Directories: TArray<string>;
resourcestring
  rs_SelectPhotoFolderCaption = 'В какой папке хранить мульт, озвучку и искать/сохранять кадры?';
  rs_SaveBeforeChooseFolderRequest = 'созданием нового';
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
  actShowCameraControl.Enabled := not Exporting and (PhotoFolder <> '');
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

procedure TMainForm.ClearTeleports;
var
  i: Integer;
begin
  for i := 0 to FrameInfoCount - 1 do
    FrameInfoList[i].Teleport := -1;
end;

procedure TMainForm.CreateAdvertisementFrame;

resourcestring
  rs_AdFrame1 = 'Фильм собран из отдельных кадров и озвучен';
  rs_AdFrame2 = 'при помощи общедоступной программы МультиПульт';
  rs_AdFrame3 = 'версии %s (%s)';
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

begin
  // на случай, если что-то пойдёт не так
  VersionNameString := '0.9.???';
  VersionCopyrightString := 'МультиСтудия, Москва, 20??';

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
    // глушим ошибку, если микрофона нет
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
  // инициализируем разрешение при экспорте по умолчанию.
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
  if ANewRecordPosition > RecordedFrames.Count then
    ANewRecordPosition := RecordedFrames.Count;
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
        VK_ESCAPE: if actFullScreenMode.Checked then actFullScreenMode.Execute; // TODO: что ещё тут стоит прерывать эскейпом?
        VK_SHIFT, VK_CAPITAL:  pbWorkingSet.Repaint; // телепорты выключаются
      end;

  UpdatePlayActions;
  pbIndicator.Repaint;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressBlocked := False; // TODO: разблокировать не любую клавишу, а ту, что блокировали. Чтоб не разблокировать, например, шаг вправо при отпускании чего-то другого.
  case Key of
    VK_SHIFT, VK_CAPITAL: pbWorkingSet.Repaint; // телепорты включаются
  end;
//  pbIndicator.Refresh;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Playing then
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
        // останавливаемся на первом не одинаковом символе
        if (s1[i] <> s2[i]) then
          begin
            PosDif := i;
            // Если хоть один из отличающихся символов - цифра, значит возможно попали в число, и ситуация не однозначна
            if CharInSet(s1[PosDif], ['0'..'9']) or CharInSet(s2[PosDif], ['0'..'9']) then
              // Если предыдущие одинаковые символы были цифры - откатываемся к началу этого блока цифр
              // то есть, к началу первого встреченного неодинакового числа.
              // Раньше по тексту могли быть другие, одинаковые, числа, но они в этом сравнении роли не сыграют.
              // Сложные случаи:
              //   img1001.jpg > img101.jpg отличается 3-й знак числа, у первого он меньше, но меньше второе число
              //   img0002.jpg > img001.jpg отличается 3-й знак числа, у первого он меньше, но меньше второе число
              //   img1012.jpg > img101.jpg отличается 4-й знак числа, у второго имени на этом месте не цифра
              //   img10a.jpg > img10b.jpg цифры не отличаются, но отличается дальнейший текст
              //   img010a.jpg > img10b.jpg числа по значению не отличаются, но отличается дальнейший текст
              //   img010a1.jpg > img10a02.jpg первые числа по отличаются по сути, но не по значению, а в дальнейшем тексте отличаются числа.
              //   img010.jpg > img10.jpg числа по значению не отличаются, и не отличается дальнейший текст, но порядок задать надо бы хоть как-то, но стабильно и однозначно
              // Из-за варианта fld010\img1.jpg > fld10\img02.jpg, где первые числа
              // отличаются по сути, но не по значению, а в дальнейшем тексте отличаются числа,
              // но папки уже разные, и смешивать их содержимое не надо бы,
              // ниже сравниваем папки без имён их содержимого, отрезав его через CopyTillSlash.
              while (PosDif > 1) and CharInSet(s1[PosDif - 1], ['0'..'9']) do
                dec(PosDif);
            Break;
          end
        else
          Inc(i);
      end;

    // если до конца одной из строк не нашлось различий, то PosDif остался 0.
    if PosDif = 0 then
    begin
      if (s1[i] = #0) and (s2[i] = #0) then // кончились обе одновременно - значит, равны
        Exit(0)
      else if s1[i] = #0 then // кончилась первая, вторая ещё нет, значит вторая больше
        Exit(-1)
      else
        Exit(1); // кончилась вторая, значит первая больше.
    end;

    // если хоть куда-то ушагали и где-то остановились, то смотрим, не числа ли оттуда начались
    if CharInSet(s1[PosDif], ['0'..'9']) and CharInSet(s2[PosDif], ['0'..'9']) then
      begin
        PosDif1 := PosDif;
        PosDif2 := PosDif;
        Result := ExtractInt(s1, PosDif1) - ExtractInt(s2, PosDif2);
        // Числа могут оказаться одинаковые, но по-разному написанные,
        // например, с разным количеством ведущих нулей.
        // В этом случае нужно сравнить остаток строки (до ближайшего '\', если он есть
        //   - см. ниже пример про fld010\img1.jpg > fld10\img02.jpg!),
        // а если там отличия не надётся, то пытаться найти более тонкое отличие в найденных числах.
        // Можно сравнить в них количество знаков, для начала.
        // Порядок получается такой:
        // 1аа
        // 01аа // число как у 1аа, конец не различается, но больше знаков в числе
        // 1ав  // число как у 1аа и 01аа, но строка дальше - больше, чем аа
        // 01вв // число как у всех, а строка - ещё больше, чем у всех.
        if Result = 0 then
          Result := CompareStringWithInt(CopyTillSlash(s1, PosDif1), CopyTillSlash(s2, PosDif2));
        if Result = 0 then
          Result := PosDif1 - PosDif2;
        Assert(Result <> 0, 'Need to find more differences in "' + s1 + '" and "' + s2 + '"');
      end
    else
      // если хоть одно не число, то сравню как строки, по правилам сравнения
      // имён файлов, с учётом текущей кодовой страницы (но без региональной регистрозависимости,
      // так как уже на входе был применён AnsUpperCase)
      // и можно с начала строк, до отличия функция и сама доберётся быстрее,
      // чем если тут одинаковое начало отрезать у двух строк
      // однако, разделители папок в пути должны иметь приоритет перед любыми
      // символами, типа пробелов, например, поэтому меняю их на 1-ый символ.
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
             (ext = '.jpe') or
             (ext = '.jpeg') or
             (ext = '.bmp') or
             (ext = '.png') or
             (ext = '.gif') or
             (ext = '.wmf') or
             (ext = '.emf')
          then
            begin
              if FindFrameInfo(ARelativePath, Rec.Name) = -1 then // это для перезагрузки папки актуально
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
    'При перезагрузке кадры будут упорядочены по именам файлов,'+#13#10+
    'а закладки будут поставлены на первые кадрах дочерних папок.'+#13#10+
    'Перезагрузить?';
begin
  Stop;
  case MessageBox(
    0,
    PChar(rs_RelopadPhotoFolder),
    PChar(Application.Title),
    MB_ICONWARNING or MB_OKCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
  of
    IDCANCEL: Abort;
  end;
  ClearBookmarks;
  LoadPhotoFolder;
end;

procedure TMainForm.actReloadPhotoFolderUpdate(Sender: TObject);
begin
  actReloadPhotoFolder.Enabled := not Exporting and (PhotoFolder <> '');
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
resourcestring
  rs_AboutText =
    'Программа для съёмки, сборки и озвучки мультиков. Монтаж в реальном времени!'#13#10 +
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

procedure TMainForm.actClearBookmarksClick(Sender: TObject);
begin
  ClearBookmarks;
  ClearTeleports;
  pbWorkingSet.Repaint;
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
        pbWorkingSet.Repaint;
        Saved := False;
        Exit;
      end;

  // если не было закладки - то ставим первую свободную
  for i := Low(Bookmarks) to High(Bookmarks) do
    if Bookmarks[i] = -1 then
      begin
        Bookmarks[i] := DisplayedFrameIndex;
        pbWorkingSet.Repaint;
        Saved := False;
        Exit;
      end;

  Beep; // если свободных закладок не нашлось - гудим.
end;

procedure TMainForm.mmiUseMicrophoneClick(Sender: TObject);
begin
  actNew.Execute;
  SwitchToMicrophoneUsage(False);
end;

procedure TMainForm.SwitchToMicrophoneUsage(AKeepOpenedWave: Boolean);
resourcestring
  rs_MicInitError = 'Не удалось включить микрофон';
begin
  if not AKeepOpenedWave then
    begin
      WaveStorage.Wave.Clear;
      RecordedAudioCopy.Clear;
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
    InfoMsg(rs_MicInitError)// глушим ошибку, если микрофона нет
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
  ShowTimes;
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

procedure TMainForm.DoCancelExport(Sender: TObject);
begin
  ExportCancelled := True;
end;

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
  rs_AVIExporting = 'Экспорт в AVI';
  rs_AVIExportingAudioStore = 'Сохранение звука.';
  rs_AVIExportingCompressorInit = 'Инициализация экспорта видео.';
  rs_AVIExportingCaption = 'Запись кадра %0:s (%1:s, %2:d из %3:d).';
  rs_AVIExportingAudioMerge = 'Объединение со звуком.';
  rs_ExportFinished = 'Открыть созданый файл?';

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
        Compressor := TAVICompressor.Create;
        try
          Options.Init;
          Options.FrameRate := FrameRate;
          Options.Width := ExportSize.cx;
          Options.Height := ExportSize.cy;
          Options.Handler := 'DIB '; // по умолчанию - без компрессии
          Hide; // progress form на время выбора настроек кодека
          CheckAVIError(Compressor.Open(Dir + '~Video.avi', Options));
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
                        MessageBox(0,
                          PWideChar(
                            Format(rs_AVIExportingCaption, [
                              FrameInfoList[DisplayedFrameIndex].RelativeFileName,
                              FrameIndexToTimeStamp(CurrentRecordPosition),
                              i + 1,
                              RecordedFrames.Count
                            ]) + #13#10 +
                            e.Message
                          ),
                          PWideChar(Application.Title),
                          MB_ICONINFORMATION or MB_OK or MB_TASKMODAL
                        );
                        Abort;
                      end;
                  end;
                end;
              SetProgress(RecordedFrames.Count, RecordedFrames.Count + AdvertisementDuration div FrameRate);
              Image := AdvertisementFrameImage;
              R := StretchSize(Image.Width, Image.Height, Bmp.Width, Bmp.Height);
              Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
              Bmp.Canvas.StretchDraw(R, Image);
              Bmp.PixelFormat := pf24bit;
              CurrentRecordPosition := RecordedFrames.Count;
              pbRecord.Repaint;
              SetProgressStatus(Format(rs_AVIExportingCaption, ['Последний кадр', FrameIndexToTimeStamp(CurrentRecordPosition), RecordedFrames.Count, RecordedFrames.Count]));
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
        finally
          Compressor.Destroy;
        end;
        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Audio.wav');
        {$IFDEF FPC}DeleteFileUTF8{$ELSE}DeleteFile{$ENDIF}(Dir + '~Video.avi');
        SetProgressStatus(rs_ExportFinished);
        Hide; // progress
        if (MessageBox(0,
          PWideChar(rs_ExportFinished),
          PWideChar(Application.Title),
          MB_ICONINFORMATION or MB_YESNO or MB_TASKMODAL) = idYes)
        then
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
  if FirstTime then
    actShowCameraControl.Execute;
end;

procedure TMainForm.CameraFormActiveChanged(Sender: TObject);
resourcestring
  rsStartRecording = 'Запись';
  rsMakeCameraFrame = 'Записать кадр с камеры в папку мульта';
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
  rs_SaveBeforeOpenRequest = 'созданием нового';
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
  pbRecord.Repaint;
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
  ExportCancelled := True;
  UnloadFrames;
  ClearBookmarks;
  PhotoFolder := ExtractFilePath(AFileName);
  lblWorkPath.Caption := PhotoFolder;
  ProjectFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  UpdateCaption;

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
            OpenAudio(PhotoFolder + WaveFileName); // см. ниже условие про SwitchToMicrophoneUsage
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

  if (WaveFileName = ExtractFileName(AFileName) + '.wav') and
     (CalculateSoundFramesCount = RecordedFrames.Count)
  then // TODO: сделать более надёжный и явный признак
    SwitchToMicrophoneUsage(True);

  Saved := True;
  RepaintAll;
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
  rs_DoYouWantReplaceMovie = 'В папке "%s"'#13#10'уже есть мульт с именем "%s".'#13#10'Заменить?';
begin
  if FileExists(PhotoFolder + ANewMovieName + '.mp') then
    Result := (
    MessageBox(
      0,
      PChar(Format(rs_DoYouWantReplaceMovie, [PhotoFolder, ANewMovieName])),
      PChar(Application.Title),
      MB_ICONQUESTION or MB_OKCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    = IDYES)
  else
    Result := True;

  if Result then
    SaveWorkingSet(ANewMovieName);
end;

procedure TMainForm.SaveWorkingSet(ANewProjectFileName: string = '');
var
  i: Integer;
  AudioName: string;
begin
  ProjectFileName := ANewProjectFileName;
  UpdateCaption;
  if FExternalAudioFileName <> '' then
    AudioName := ExtractFileName(FExternalAudioFileName)
  else
    AudioName := ProjectFileName + '.wav';
  // если внешний файл под другим именем уже хранится в папке проекта, не будем его перезаписывать.
  if ExpandFileName(PhotoFolder + AudioName) <> ExpandFileName(FExternalAudioFileName) then
    WaveStorage.Wave.SaveToFile(PhotoFolder + AudioName);
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
      SaveToFile(PhotoFolder + ProjectFileName + '.mp');
      Saved := True;
    finally
      Free;
    end;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
resourcestring
  rs_SaveAs = 'Сохранение мульта под новым имемем';
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
  rs_FrameNotInWorkingSet = ', скрыт из рабочего набора..';
var
  s: string;
begin
  s := '';
  if not Exporting then
    if FDisplayedFrameIndex >= 0 then
      if Assigned(CurrentWorkingSetFrame) then
        s := ' — ' + FrameInfoList[FDisplayedFrameIndex].RelativeFileName
      else
        s := ' — ' + FrameInfoList[FDisplayedFrameIndex].RelativeFileName + rs_FrameNotInWorkingSet;
  Caption := ProjectFileName + s + ' — ' + Application.Title;
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
  FPhotoFolder := Value;
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
  else if PhotoFolder <> '' then
    if Recording then
      s := s + ' Озвучка записывается вместе с кадрами.'
    else
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
var
  WorkingSetIndex: Integer;
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

  // Останавливать воспроизведение при завершени звука надо, как минимум, потому
  // что StopPlaying на это рассчитана.
  //  TODO: понять, когда не надо останавливать воспроизведение видео при завершении звука
  // (например, если звука меньше, чем записанного мульта, и надо б продолжить в тишине ?)
  // и придумать, как это совместить.
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
  else // через деактивацию записи
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

  // выходим, если ничего грузить не нужно.
  // А грузить нужно либо иконку, либо превьюшку, в зависимости от видимости представления в виде иконок
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

procedure TMainForm.actToggleTeleport0Execute(Sender: TObject);
begin
  with FrameInfoList[DisplayedFrameIndex] do
    if Teleport = TMenuItem(Sender).Tag then
      Teleport := -1
    else
      Teleport := TMenuItem(Sender).Tag;
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
  RecordedFrames[CurrentRecordPosition].FrameInfoIndex := CurrentWorkingSetFrame.FrameInfoIndex;
  ChangeCurrentRecordPosition(CurrentRecordPosition + 1, False);
  Saved := False;
  RepaintAll;
  ShowTimes;
end;

procedure TMainForm.actReplaceInMovieUpdate(Sender: TObject);
begin
  actReplaceInMovie.Enabled := not Exporting and not AdvertisementShowing and (CurrentRecordPosition <> -1) and (CurrentRecordPosition < RecordedFrames.Count) and Assigned(CurrentWorkingSetFrame)
end;

procedure TMainForm.pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if PhotoFolder = '' then
  begin
    if (Button = mbRight) or ((Button = mbLeft) and (ssShift in Shift)) then // открытие вешаем не только на шифт+левую кнопку мыши, но и просто на правую кнопку мыши.
      actOpen.Execute
    else if (Button = mbLeft) then
      actSelectPhotoFolder.Execute;
  end
  else
    if CameraForm.Active then
      CameraForm.Visible := not CameraForm.Visible;
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
    pbDisplay.Canvas.Brush.Color := clBlack;
    pbDisplay.Canvas.FillRect(pbDisplay.ClientRect);
    pbDisplay.Canvas.Brush.Color := clBtnFace;

    if PhotoFolder = '' then
      begin
        R_MainScreen.Left := (pbDisplay.Width  - imgBackgroundSource.Picture.Width ) div 2;
        R_MainScreen.Top  := (pbDisplay.Height - imgBackgroundSource.Picture.Height) div 2;
        pbDisplay.Canvas.Draw(R_MainScreen.Left, R_MainScreen.Top, imgBackgroundSource.Picture.Graphic);
        Exit;
      end
    else
      begin
        CreateAdvertisementFrame; // на всякий случай
        LoadPhoto(DisplayedFrameIndex, -1); // на всякий случай
        // основной кадр.
        // Сначала ищем смещение экрана, нужное, чтоб он по возможности не подлазил под миниатюры.

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

        if Assigned(CurrentWorkingSetFrame) and mmiShowNeighbourFrames.Checked then
          begin
            // левая миниатюра
            WorkSetFrame := FindWorkingSetFrameByOffset(-1);
            LoadPhoto(WorkSetFrame.FrameInfoIndex, -1); // на всякий случай
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
            // правая миниатюра
            WorkSetFrame := FindWorkingSetFrameByOffset(+1);
            LoadPhoto(WorkSetFrame.FrameInfoIndex, -1); // на всякий случай
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
    ScreenForm.Image := Image;
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
  rs_Framerate = '- по %d, при %d кадрах в секунду';
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
    procedure Draw(AThickness: Integer);
    var
      Text: string;
      TextSize: TSize;
    begin
//       pbRecord.Canvas.Brush.Color := clRed;
      pbRecord.Canvas.Brush.Style := bsSolid;
      pbRecord.Canvas.FillRect(Rect(R.Left, y, R.Right, y + AThickness));
      Text := FrameIndexToTimeStamp(RecordedFrameIndex, False);
      TextSize := pbRecord.Canvas.TextExtent(Text);
      pbRecord.Canvas.Brush.Style := bsFDiagonal; // bsClear почпему-то не выключался обратно и линии, делаемые выше через FillRect, пропадали вовсе
      pbRecord.Canvas.TextOut(pbRecord.Width - TextSize.cx - 8, y - TextSize.cy - 3, Text);
      pbRecord.Canvas.Brush.Style := bsSolid;
    end;
  begin
    if RecordedFrameIndex mod FrameRate        = 0 then Draw(1);
    if RecordedFrameIndex mod (FrameRate * 10) = 0 then Draw(2);
    if RecordedFrameIndex mod (FrameRate * 60) = 0 then Draw(3);
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

  pbRecord.Canvas.Font.Color := clLtGray;

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

  pbFrameTip.Repaint;
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
  if lvFrameset.Visible then
    Exit;

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
      if FrameInfoList[WorkingSetFrames[WorkingFrameIndex].FrameInfoIndex].PreviewLoaded then
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

function TMainForm.TeleportEnabled: Boolean;
begin
  Result := not ((GetAsyncKeyState(VK_SHIFT) < 0) xor (GetKeyState(VK_CAPITAL) and 1 = 1)); // либо то, либо другое...
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
        caPlayBackward, caPlayForward, // должны перекрываться нажатыми actBackwardWhilePressed и actForwardWhilePressed
        caNone:
          if Interval <= 1 then
            begin
              //  эти буквы ещё упомянуты в меню и в блокировщике горячих клавиш IsShortCut
              if actBackwardWhilePressed.Checked or (
                (GetAsyncKeyState(VK_CONTROL) >= 0) and
                ((GetAsyncKeyState(Ord('A')) < 0) or (GetAsyncKeyState(Ord('C')) < 0))
              )
              then
                CurrentWorkingSetFrame := FindWorkingSetFrameByOffset(-1)
              else if actForwardWhilePressed.Checked or (
                (GetAsyncKeyState(VK_CONTROL) >= 0) and
                ((GetAsyncKeyState(Ord('D')) < 0) or (GetAsyncKeyState(Ord('M')) < 0))
              )
              then
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

  // FFrameInfoList с диска потребует сохранения.
  // А если имени проекта ещё нет, то это просто все файлы папки, это не обязательно сохранять.
  if ProjectFileName <> '' then
    Saved := False;
end;

procedure TMainForm.Stop;
begin
  if Recording then
    StopRecording;
  if Playing then
    StopPlaying;
// Stop выполняется перед всякими действиями, от которых можно отказаться. И в этом случае экспорт должен мочь продолжиться
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
  // frames in last second
  if (d2 = 0) and AShowZeroFrameIndex then
    Result := Format('[%2.2d]', [d2]);
  d := d div FrameRate;
  // seconds
  if d < 60 then
    Result := Format('%2.2d', [d]) + ':' + Result
  else
    begin
      d2 := d mod 60;
      // seconds in a last minute
      Result := Format('%2.2d', [d2]) + ':' + Result;
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
end;

procedure TMainForm.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
resourcestring
  rs_PreloadStatus = 'Загрузка кадра: ';
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

procedure TMainForm.btnFrameSetExpandClick(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
  FrameInfo: TFrameInfo;
  OriginalOrders: array of TRecordedFrame;
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
      for i := 0 to WorkingSetFrames.Count - 1 do
      begin
        FrameInfo := FrameInfoList[WorkingSetFrames[i].FrameInfoIndex];
        Item := lvFrameset.Items.Add;
        Item.Caption := FrameInfo.FileName;
        Item.ImageIndex := i;
        if Assigned(FrameInfo.Iconic) then
          ilFrameset.Add(FrameInfo.Iconic, nil)
        else
          ilFrameset.Add(nil, nil);
      end;
      pbWorkingSet.Repaint;
    finally
      lvFrameset.Items.EndUpdate;
      ilFrameset.EndUpdate;
      lvFrameset.Visible := True;
      lvFrameset.BringToFront;
    end;
  end
  else
    begin
      lvFrameset.Visible := False;
      SetLength(OriginalOrders, WorkingSetFrames.Count);
      for i := 0 to WorkingSetFrames.Count - 1 do
        OriginalOrders[i] := WorkingSetFrames[i];
      for i := WorkingSetFrames.Count - 1 downto 0 do
      begin
        if lvFrameset.Items[i].ImageIndex <> i then
          begin
            OriginalOrders[lvFrameset.Items[i].ImageIndex].Index := i;
            Saved := False;
          end;
      end;
      pbWorkingSet.Repaint;
    end;
  actWorkingSetManagement.Checked := lvFrameset.Visible;
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
  if not (
    // если не клавиша, обрабатываемая другим способом, то обрабатываем по-прежнему как потенциальный ShortCut
    // А другим способом обрабатываются клавиши движения по кадрам при не нажатых Ctrl и Alt
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
  Teleport := -1;
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
  actSelectAudioFile.Enabled := not Exporting and (PhotoFolder <> '');
  mmiStopRecordingOnSoundtrackFinish.Enabled := actSelectAudioFile.Checked;
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
    // двигаем вперёд те, что были перед целью
    for i := 0 to MovedItems.Count - 1 do begin
      MovedItem := MovedItems[i];
      if TargetIndex > MovedItem.Index then
        MoveItem;
    end;
    // Двигаем назад те, что были после цели.
    // Если их двигать этим методом в прямом порядке, они окажутся в обратном.
    // Поэтому двигаю в обратном, чтоб получались в прямом.
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
    if (WindowState = wsNormal) and not actFullScreenMode.Checked then // развёрнутый размер не интересен для сохранения
    begin
      IniFile.WriteInteger('LastUsed', 'MainWindowLeft', Left);
      IniFile.WriteInteger('LastUsed', 'MainWindowTop', Top);
      IniFile.WriteInteger('LastUsed', 'MainWindowWidth', Width);
      IniFile.WriteInteger('LastUsed', 'MainWindowHeight', Height);
    end;
    IniFile.WriteBool('LastUsed', 'MainWindowMaximized', WindowState = wsMaximized);
    IniFile.WriteInteger('LastUsed', 'RightPaneSize', RecordSplitter.Parent.ClientWidth - RecordSplitter.Left);
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
  finally
    IniFile.Free;
  end;
  SettingsChanged := False;
end;

initialization
  TPicture.RegisterFileFormat('jpe', sJPEGImageFile, TJPEGImage);
end.

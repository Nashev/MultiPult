unit MainFormUnit;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ExtCtrls, ImgList, ExtDlgs, StdCtrls, Contnrs,
  MPlayer, mmSystem, WaveUtils, WaveStorage, WaveOut, WavePlayers, WaveIO,
  WaveIn, WaveRecorders, Gauges, Buttons, WaveTimer, Math, ComCtrls, FileCtrl, jpeg;

const
  ControlActionStackDeep = 10;
type
  TControlAction = (caNone, caStepBackward, caStepForward, caPlayBackward, caPlayForward);
  TFrame = class
  private
    FFileName: string;
  public
    OriginalJpeg: TJPegImage;
    Teleport: Integer;
    Loaded: Boolean;
    property FileName: string read FFileName;
    constructor Create(AFileName: string);
    destructor Destroy; override;
  end;

  TMainForm = class(TForm)
    pnlDisplay: TPanel;
    pbDisplay: TPaintBox;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    actOpen: TAction;
    actNew: TAction;
    actSave: TAction;
    actSelectPhotoFolder: TAction;
    mmiSelectPhotoFolder: TMenuItem;
    mmiFiles: TMenuItem;
    mmiNew: TMenuItem;
    mmiOpen: TMenuItem;
    mmiSave: TMenuItem;
    mmiNavigation: TMenuItem;
    actStepPrev: TAction;
    actStepNext: TAction;
    actToggleBookmark0: TAction;
    actNewBookmark: TAction;
    actRecord: TAction;
    actExport: TAction;
    pnlTimeLine: TPanel;
    pbTimeLine: TPaintBox;
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
    Timer: TMultimediaTimer;
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
    pnlToolbar: TPanel;
    btnStepPrev: TSpeedButton;
    btnPlay: TSpeedButton;
    btnRecord: TSpeedButton;
    btnStepNext: TSpeedButton;
    btnPlayForward: TSpeedButton;
    btnPlayBackward: TSpeedButton;
    actPlay: TAction;
    mmiSeparatorSteps: TMenuItem;
    mmiPlay: TMenuItem;
    SaveDialog: TSaveDialog;
    pbAudio: TPaintBox;
    OpenDialog: TOpenDialog;
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
    Splitter1: TSplitter;
    mmiN1: TMenuItem;
    actToggleTeleport0: TAction;
    mmiToggleTeleport0: TMenuItem;
    procedure actSelectPhotoFolderClick(Sender: TObject);
    procedure actStepNextExecute(Sender: TObject);
    procedure actStepPrevExecute(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure pbTimeLinePaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure actPlayForwardExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actRecordExecute(Sender: TObject);
    procedure AudioLevelChanged(Sender: TObject; Level: Integer);
    procedure actPlayExecute(Sender: TObject);
    procedure pbRecordPaint(Sender: TObject);
    procedure actPlayBackwardExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actToggleBookmarkExecute(Sender: TObject);
    procedure actGotoBookmark0Execute(Sender: TObject);
    procedure actPlayUpdate(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure AudioRecorderFilter(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal);
    procedure pbAudioPaint(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actUpdate_HaveFiles(Sender: TObject);
    procedure actUpdate_HaveRecorded(Sender: TObject);
    procedure mmiAboutClick(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure pbIndicatorPaint(Sender: TObject);
    procedure actForwardWhilePressedExecute(Sender: TObject);
    procedure mmiBackwardWhilePressedClick(Sender: TObject);
    procedure actExportToAVIExecute(Sender: TObject);
    procedure actToggleTeleport0Execute(Sender: TObject);
    procedure pbIndicatorClick(Sender: TObject);
  private
    NextControlActionStack: array [1..ControlActionStackDeep] of TControlAction;
    NextControlActionStackPosition: Integer;
    function NextControlAction: TControlAction;
    procedure PopControlAction;
    procedure PushControlAction(Value: TControlAction);
    procedure ReplaceControlActions(Value: TControlAction);
  private
    PhotoFolder: string;
    //FileNames: TStringList;
//    BufferIndexes: TList;
    FFrames: TObjectList;

    HaveBuffers: Boolean;
    KeyPressBlocked: Boolean;
    Interval, CurrentSpeedInterval: Integer;
    Recording, Playing, Exporting: Boolean;
    LoopMode: Boolean;
    CurrentRecordPosition: Integer;
    RecordedFrames: TList;
    CurrentFrameIndex: Integer;
    Bookmarks: array [0..9] of Integer;
    Saved: Boolean;
    RecordedAudioCopy: TMemoryStream;
    function GetFrame(Index: Integer): TFrame;
    function GetFramesCount: Integer;
//    Drawing: Boolean;
    property Frames[Index: Integer]: TFrame read GetFrame;
    property FramesCount: Integer read GetFramesCount;
    procedure LoadPhotoFolder(APath: string);
    procedure LoadPhoto(Index: Integer);
    procedure ShowFrame(Index: Integer);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure ClearRecorded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    procedure SetCaption(const Value: TCaption);
  end;

var
  MainForm: TMainForm;

var
  FrameRate: Integer = 25;

implementation
uses AVICompression;
{$R *.dfm}

procedure TMainForm.actSelectPhotoFolderClick(Sender: TObject);
var
  i: Integer;
  FileName: string;
begin
  if OpenPictureDialog.Execute then
    begin
      LoadPhotoFolder(ExtractFilePath(OpenPictureDialog.FileName));
      FileName := ExtractFileName(OpenPictureDialog.FileName);
      for i := 0 to FramesCount - 1 do
        if Frames[i].FileName = FileName then
          begin
            ShowFrame(i);
            Break;
          end;
    end;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  CurrentSpeedInterval := 3;
//  BufferIndexes := TList.Create;
  FFrames := TObjectList.Create(True);
  RecordedFrames := TList.Create;
  RecordedAudioCopy := TMemoryStream.Create;
  Application.OnIdle := ApplicationIdle;
  pnlDisplay.DoubleBuffered := True;
  for i := 0 to 9 do
    Bookmarks[i] := -1;
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
  Timer.Enabled := False;
  FreeAndNil(RecordedAudioCopy);
//  FreeAndNil(BufferIndexes);
  FreeAndNil(FFrames);
  inherited Destroy;
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
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', IntToStr(Index), []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut('Ctrl+' + IntToStr(Index));
    mmiToggleBookmark0.Parent.Insert(mmiToggleBookmark0.Parent.IndexOf(mmiToggleBookmark0), MenuItem);
    MenuItem.Action := nil;

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Action := actGotoBookmark0;
    MenuItem.Tag := Index;
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', IntToStr(Index), []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut(IntToStr(Index));
    mmiGotoBookmark0.Parent.Insert(mmiGotoBookmark0.Parent.IndexOf(mmiGotoBookmark0), MenuItem);
    MenuItem.Action := nil;

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Action := actToggleTeleport0;
    MenuItem.Tag := Index;
    MenuItem.Caption := StringReplace(MenuItem.Caption, '0', IntToStr(Index), []);
    MenuItem.Hint := MenuItem.Caption;
    MenuItem.ShortCut := TextToShortCut('Shift+' + IntToStr(Index));
    mmiToggleTeleport0.Parent.Insert(mmiToggleTeleport0.Parent.IndexOf(mmiToggleTeleport0), MenuItem);
    MenuItem.Action := nil;
  end;

begin
  for i := 0 to pnlToolbar.ControlCount - 1 do
    TSpeedButton(pnlToolbar.Controls[i]).Caption := '';
  for i := 1 to 9 do
    AppendBookmarkMenu(i);

  mmiToggleBookmark0.Action.Free;
  mmiGotoBookmark0.Action.Free;
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
      vk_Up:   begin Inc(CurrentSpeedInterval); end;
      vk_Down: begin Dec(CurrentSpeedInterval); if CurrentSpeedInterval < 1  then CurrentSpeedInterval := 1; end;
    end
  else
    if not KeyPressBlocked then
      case Key of
        vk_Left:  begin PushControlAction(caStepBackward); KeyPressBlocked := True; end;
        vk_Right: begin PushControlAction(caStepForward); KeyPressBlocked := True; end;
      end;

  btnPlayForward.Down  := NextControlAction = caPlayForward;
  btnPlayBackward.Down := NextControlAction = caPlayBackward;
//  pbIndicator.Invalidate;
//  pbIndicator.Refresh;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressBlocked := False; // TODO: разблокировать не любую клавишу, а ту, что блокировали. Чтоб не разблокировать, например, шаг вправо при отпускании чего-то другого.
//  pbIndicator.Refresh;
end;

function TMainForm.GetFrame(Index: Integer): TFrame;
begin
  Result := FFrames[Index] as TFrame;
end;

function TMainForm.GetFramesCount: Integer;
begin
  Result := FFrames.Count;
end;

procedure TMainForm.LoadPhotoFolder(APath: string);
var
  Rec: TSearchRec;
  ext: string;
//  i: Integer;
begin
  SetCaption(APath);
  PhotoFolder := APath;
  FFrames.Clear;
//  BufferIndexes.Clear;
  HaveBuffers := True;

  if FindFirst(PhotoFolder + '*.*', faAnyFile, Rec) = 0 then
    begin
      repeat
        ext := AnsiLowerCase(ExtractFileExt(Rec.Name));
        if (ext = '.jpg') or (ext = '.jpeg') then
          begin
            FFrames.Add(TFrame.Create(Rec.Name));
          end;
      until FindNext(Rec) <> 0;
      FindClose(Rec);
    end;
//  ListBox.Items.Assign(FileNames);
  UpdateActions;
end;

procedure TMainForm.mmiAboutClick(Sender: TObject);
begin
  ShowMessage(
    'МультПульт'#13#10 +
    'Версия 0.9.1'#13#10 +
    'Автор: Илья Ненашев (I.N.Nenashev@gmail.com)'#13#10 +
    'по заказу МультиСтудии (http://multistudia.ru)'#13#10 +
    'в лице Евгения Генриховича Кабакова'#13#10 +
    ''#13#10 +
    '(А Вы знаете, что Ctrl+C в подобных окошках работает?)'
  );
end;


procedure TMainForm.mmiBackwardWhilePressedClick(Sender: TObject);
begin
 //
end;

function CopyProgressHandler(
  TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: LARGE_INTEGER;
  dwStreamNumber, dwCallbackReason: DWORD;
  hSourceFile, hDestinationFile: THANDLE;
  lpData: LPVOID
): DWORD; stdcall;
begin
//  TMainForm(lpData).SetCaption('Копирование кадра: ' + FloatToStrF(TotalBytesTransferred / TotalFileSizeTotalFileSize) * 100, ffFixed, 0, 2) + '%';
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
  Dir: string;
  i: Integer;
  Cancel: BOOL;
  NewFileName: string;
begin
  Dir := GetCurrentDir;
  if SelectDirectory(
    'Выберите папку для экспорта', '', Dir,
    [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI, sdShowFiles, sdValidateDir]
  )
  then
    begin
//      SetCurrentDir(Dir);
      Exporting := True;
      try
        dir := IncludeTrailingPathDelimiter(dir);
        WaveStorage.Wave.SaveToFile(Dir + 'AudioTrack.wav');
        for i := 0 to RecordedFrames.Count - 1 do
          begin
            CurrentRecordPosition := i;
            ShowFrame(Integer(RecordedFrames[CurrentRecordPosition]));
            pbRecord.Invalidate;
            pbAudio.Invalidate;
            SetCaption('Экспорт. Копирование кадра ' + IntToStr(i+1) + ' из ' + IntToStr(RecordedFrames.Count));
            Application.ProcessMessages;
            Cancel := False;
            NewFileName := Dir + Format('Frame%.5d.jpg', [i]);
            if not CopyFileEx(
              PChar(PhotoFolder + Frames[Integer(RecordedFrames[i])].FileName),
              PChar(NewFileName),
              @CopyProgressHandler,
              @Self,
              @Cancel,
              COPY_FILE_ALLOW_DECRYPTED_DESTINATION or COPY_FILE_RESTARTABLE
            ) then
              RaiseLastOSError;
            SetCurrentCreateDatetime(NewFileName);
          end;
      finally
        Exporting := False;
      end;
    end;
end;

procedure TMainForm.actExportToAVIExecute(Sender: TObject);
var
  Compressor: TAVICompressor;
  Options: TAVIFileOptions;
  i: Integer;
  Bmp: Graphics.TBitmap;
  Image: TJPEGImage;
  Dir: string;
//  Cancel: BOOL;
begin
  Dir := GetCurrentDir;
  if SaveToAVIDialog.Execute then
    begin
//      SetCurrentDir(Dir);
      Dir := ExtractFilePath(SaveToAVIDialog.FileName);
      Exporting := True;
      try
        WaveStorage.Wave.SaveToFile(Dir + 'Audio.wav');
        Compressor := TAVICompressor.Create;
        Options.Init;
        Options.FrameRate := FrameRate;
        Options.Width := 640;
        Options.Height := 480;
        CheckOsError(Compressor.Open(Dir + 'Video.avi', Options));
        Bmp := TBitmap.Create;
        for i := 0 to RecordedFrames.Count - 1 do
          begin
            CurrentRecordPosition := i;
            ShowFrame(Integer(RecordedFrames[CurrentRecordPosition]));
            pbRecord.Invalidate;
            pbAudio.Invalidate;
            SetCaption('Экспорт в AVI. Запись кадра ' + IntToStr(i+1) + ' из ' + IntToStr(RecordedFrames.Count));
            Application.ProcessMessages;
            Image := Frames[CurrentFrameIndex].OriginalJpeg;
            Bmp.Assign(Image);
            Compressor.WriteFrame(Bmp);
          end;
        Bmp.Free;
        Compressor.MergeSoundAndSaveAs(Dir + 'Audio.wav', SaveToAVIDialog.FileName);
        Compressor.Close;
        Compressor.Destroy;
        DeleteFile(Dir + 'Audio.wav');
        DeleteFile(Dir + 'Video.avi');
      finally
        Exporting := False;
      end;
    end;
end;

procedure TMainForm.actForwardWhilePressedExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.actNewExecute(Sender: TObject);
begin
  if (RecordedFrames.Count > 0) and not Saved then
    case MessageBox(
      0,
      'Хотите сохранить текущий мульт перед созданием нового?',
      'МультПульт',
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    of
      IDCANCEL: Exit;
      IDYES:
        begin
          actSave.Execute;
          if not Saved then
            Exit;
        end;
      IDNO: ;
    end;

  ClearRecorded;
  pbRecord.Invalidate;
  pbAudio.Invalidate;
  UpdateActions;
end;

const
  FilesSectionStart     = '-------------------- Files: ----------------------';
  BookmarkSectionStart  = '------------------- Bookmarks: --------------------';
  FrameSectionStart     = '-------------------- Frames: ----------------------';
  TeleportsSectionStart = '------------------- Teleports: --------------------';

procedure TMainForm.actOpenExecute(Sender: TObject);
var
  i: Integer;
  WaveFileName: string;
  s: string;

  procedure ReadTeleport(s: string);
  var
    FrameIndex: Integer;
    SeparatorPos: Integer;
    TargetIndex: Integer;
  begin
    SeparatorPos := pos('=', s);
    FrameIndex := StrToInt(Copy(s, 1, SeparatorPos-1));
    TargetIndex := StrToInt(Copy(s, SeparatorPos+1, Length(s)));
    Frames[FrameIndex].Teleport := TargetIndex;
  end;

begin
  if not OpenDialog.Execute then
    Abort;

  if (RecordedFrames.Count > 0) and not Saved then
    case MessageBox(
      0,
      'Хотите сохранить текущий мульт перед открытием другого?',
      'МультПульт',
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    of
      IDCANCEL: Exit;
      IDYES:
        begin
          actSave.Execute;
          if not Saved then
            Exit;
        end;
      IDNO: ;
    end;

  ClearRecorded;
  PhotoFolder := ExtractFilePath(OpenDialog.FileName);
  FFrames.Clear;

  with TStringList.Create do
    try
      LoadFromFile(OpenDialog.FileName);
      i := 0;
      s := Strings[i];
      if WaveStorage.Wave.Empty and (Copy(s, 1, Length('Wave = ')) = 'Wave = ') then
        begin
          WaveFileName := Copy(s, Length('Wave = ') + 1, MaxInt);
          if FileExists(PhotoFolder + WaveFileName) then
            WaveStorage.Wave.LoadFromFile(PhotoFolder + WaveFileName);
        end;
      inc(i);
      s := Strings[i];
      if s = FilesSectionStart then
        while i < (Count - 1) do
          begin
            inc(i);
            s := Strings[i];
            if s = BookmarkSectionStart then
              Break;
            FFrames.Add(TFrame.Create(s));
          end;
      if s = BookmarkSectionStart then
        while i < (Count - 1) do
          begin
            inc(i);
            s := Strings[i];
            if s = FrameSectionStart then // for compatibility with prev.saves without Teleports
              Break;
            if s = TeleportsSectionStart then
              Break;
            Bookmarks[StrToInt(s[Length('Bookmark') + 1])] := StrToInt(Copy(s, Length('Bookmark0 = ') + 1, MaxInt));
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
            RecordedFrames.Add(Pointer(StrToInt(s)));
          end;
    finally
      Free;
    end;
  if FramesCount > 0 then
    ShowFrame(0);
  pbRecord.Invalidate;
  pbAudio.Invalidate;
  UpdateActions;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
var
  i: Integer;
begin
  if not SaveDialog.Execute then
    Abort;
  WaveStorage.Wave.SaveToFile(SaveDialog.FileName + '.wav');
  with TStringList.Create do
    try
      Add('Wave = ' + ExtractFileName(SaveDialog.FileName) + '.wav');
      Add(FilesSectionStart);
      for i := 0 to FramesCount - 1 do
        Add(Frames[i].FileName);
      Add(BookmarkSectionStart);
      for i := 0 to 9 do
        Add('Bookmark' + IntToStr(i) + ' = ' + IntToStr(Integer(Bookmarks[i])));
      Add(FrameSectionStart);
      for i := 0 to RecordedFrames.Count - 1 do
        Add(IntToStr(Integer(RecordedFrames[i])));
      SaveToFile(SaveDialog.FileName);
      Saved := True;
    finally
      Free;
    end;
end;

procedure TMainForm.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := not Exporting and (RecordedFrames.Count > 0) and not Saved;
end;

procedure TMainForm.SetCaption(const Value: TCaption);
begin
  Caption := Value + ' - МультПульт';
end;

procedure TMainForm.ShowFrame(Index: Integer);
begin
  LoadPhoto(Index);
  CurrentFrameIndex := Index;
  if not Exporting then
    SetCaption(Frames[CurrentFrameIndex].FileName);
  pbDisplay.Repaint;
  pbTimeLine.Repaint;
end;

procedure TMainForm.LoadPhoto(Index: Integer);
var
  Image: TJPEGImage;
begin
  if Frames[Index].Loaded then
    Exit;

  Image := TJPEGImage.Create;
  Image.LoadFromFile(PhotoFolder + Frames[Index].FileName);
  Image.Performance := jpBestSpeed;
  Image.DIBNeeded;

  with Frames[Index] do
    begin
      OriginalJpeg := Image;
      Loaded := True;
    end;
end;

procedure TMainForm.actToggleBookmarkExecute(Sender: TObject);
begin
  if Bookmarks[TMenuItem(Sender).Tag] = CurrentFrameIndex then
    Bookmarks[TMenuItem(Sender).Tag] := -1
  else
    Bookmarks[TMenuItem(Sender).Tag] := CurrentFrameIndex;
end;

procedure TMainForm.actToggleTeleport0Execute(Sender: TObject);
begin
  with Frames[CurrentFrameIndex] do
    if Teleport = TMenuItem(Sender).Tag then
      Teleport := -1
    else
      Teleport := TMenuItem(Sender).Tag;
end;

procedure TMainForm.actGotoBookmark0Execute(Sender: TObject);
begin
  if Bookmarks[TMenuItem(Sender).Tag] <> -1 then
    ShowFrame(Bookmarks[TMenuItem(Sender).Tag]);
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  if (RecordedFrames.Count > 0) and not Saved then
    case MessageBox(
      0,
      'Хотите сохранить текущий мульт перед закрытием программы?',
      'МультПульт',
      MB_ICONQUESTION or MB_YESNOCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
    of
      IDCANCEL: Exit;
      IDYES:
        begin
          actSave.Execute;
          if not Saved then
            Exit;
        end;
      IDNO: ;
    end;

  Close;
end;

procedure TMainForm.actRecordExecute(Sender: TObject);
begin
  if not Recording then
    begin
      Recording := True;
      btnRecord.Down := True;
      AudioRecorder.Active := True;
    end
  else
    begin
      Recording := False;
      btnRecord.Down := False;
      ReplaceControlActions(caNone);

      btnPlayForward.Down := NextControlAction = caPlayForward;
      btnPlayBackward.Down := NextControlAction = caPlayBackward;

      AudioRecorder.Active := False;
      Application.ProcessMessages;

      if WaveStorage.Wave.Empty then
        WaveStorage.Wave.Assign(AudioRecorder.Wave)
      else
        WaveStorage.Wave.Insert(MAXDWORD, AudioRecorder.Wave);
      AudioRecorder.Wave.Clear;
      Saved := False;
    end;
end;

procedure TMainForm.pbDisplayPaint(Sender: TObject);
var
  R: TRect;
  Image: TJPEGImage;
begin
  if CurrentFrameIndex < FramesCount then
    begin
      Image := Frames[CurrentFrameIndex].OriginalJpeg;
      r.Left :=  0;
      r.Top := 0;
      if (Image.Width > pbDisplay.Width) or (Image.Height > pbDisplay.Height) then
        begin
          R.Right  := MulDiv(Image.Width, pbDisplay.Height, Image.Height);
          R.Bottom := MulDiv(Image.Height, pbDisplay.Width, Image.Width);
          if r.Right > pbDisplay.Width then
            begin
              r.Right := pbDisplay.Width;
              r.Top := (pbDisplay.Height - r.Bottom) div 2;
              r.Bottom := r.Bottom + r.Top;
            end;
          if r.Bottom > pbDisplay.Height then
            begin
              r.Bottom := pbDisplay.Height;
              r.Left := (pbDisplay.Width - r.Right) div 2;
              r.Right := r.Right + r.Left;
            end;
          pbDisplay.Canvas.StretchDraw(R, Image);
        end
      else
        begin
          r.Left := (pbDisplay.Width  - Image.Width ) div 2;
          r.Top  := (pbDisplay.Height - Image.Height) div 2;
          pbDisplay.Canvas.Draw(R.Left, r.Top, Image);
        end;
    end;
end;

var
  OddTick: Boolean;

procedure TMainForm.pbIndicatorPaint(Sender: TObject);
var
//  X: TPoint;
  a: Double;
begin
  with pbIndicator, Canvas do
    begin
      Pen.Style := psClear;
      Brush.Style := bsClear;
      SetTextAlign(Handle, TA_TOP + TA_CENTER);
      TextOut(15, 8, IntToStr(CurrentSpeedInterval) + '/' + IntToStr(FrameRate));
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      if Playing then
        a := -(CurrentRecordPosition mod FrameRate)* 2 * Pi / FrameRate
      else
        a := -(RecordedFrames.Count mod FrameRate)* 2 * Pi / FrameRate;
      with Point(15 + Round(10 * Cos(a)), 15 - Round(10 * Sin(a))) do
        Ellipse(X-2, Y-2, X+3, Y+3);

      a := -(GetTickCount mod 1000)* 2 * Pi / 1000; // текущая милисекунда. Просто 25 раз в секунду перерисовывается.
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
      OddTick := not OddTick;
      Pen.Width := 1;
      Polyline([
        Point(45-2, 7),
        Point(45+2, 7),
        Point(45+5, 30-3),
        Point(45-5, 30-3),
        Point(45-2, 7)
      ]);

      FillRect(Rect(0, Height - 2, NextControlActionStackPosition * 2, Height));
    end;
end;

procedure TMainForm.pbRecordPaint(Sender: TObject);
var
  y: Integer;
begin
  pbRecord.Canvas.Brush.Color := clWindow;
  pbRecord.Canvas.FillRect(pbRecord.ClientRect);
  for y := 0 to pbRecord.Height do
    if y < RecordedFrames.Count then
      pbRecord.Canvas.Pixels[MulDiv(Integer(RecordedFrames[RecordedFrames.Count - 1 - y]), pbRecord.Width, FramesCount), y] := clBlack
    else
      Break;
  if Playing then
    if CurrentRecordPosition < RecordedFrames.Count then
      with Point(MulDiv(Integer(RecordedFrames[CurrentRecordPosition]), pbRecord.Width, FramesCount), RecordedFrames.Count - 1 - CurrentRecordPosition) do
        pbRecord.Canvas.Ellipse(x-2, y-2, x+3, y+3);
end;

procedure TMainForm.pbAudioPaint(Sender: TObject);
var
  //Data: array of ShortInt;
  MaxData, MinData: SmallInt;
  SamplesPerGraph: Integer;
  FirstSample: Integer;
  DataSize: Integer;
//  Position: Int64;
  SamplesPerPixel: Integer;
  y: Integer;
//  MinY: Integer;
  I: Integer;
  pSample: PSmallInt;
  WaveFormat: TWaveFormatEx;
var
  PixelPerSecond: Integer;
begin
  pbAudio.Canvas.Brush.Color := clSkyBlue;
  pbAudio.Canvas.FillRect(pbAudio.ClientRect);

  SetPCMAudioFormatS(@WaveFormat, AudioRecorder.PCMFormat);

  PixelPerSecond := FrameRate;//trckbr1.Position;// ar1.Wave.WaveFormat.nSamplesPerSec div 100;
  SamplesPerGraph := Round(pbAudio.Height * WaveFormat.nSamplesPerSec / PixelPerSecond);
  SamplesPerPixel := Round(WaveFormat.nSamplesPerSec / PixelPerSecond);
  DataSize := SamplesPerGraph * 2;
  if DataSize > RecordedAudioCopy.Size then
    begin
      DataSize := RecordedAudioCopy.Size;
      SamplesPerGraph := DataSize div 2;
    end;

//  MinY := pbAudio.Height - (SamplesPerGraph div SamplesPerPixel);
  for y := 0 to SamplesPerGraph - 1 do
    begin
      FirstSample := round(y * WaveFormat.nSamplesPerSec / PixelPerSecond); // не пользуемся округлённым SamplesPerPixel, чтoб не множить ошибку округления.
      if (FirstSample + SamplesPerPixel) > RecordedAudioCopy.Size div 2 then
        break;
//    Wave.InternalDataOffset
//    TMemoryStream(Wave.Stream)

      pSample := RecordedAudioCopy.Memory;
      inc(pSample, FirstSample);
      MaxData := pSample^;
      MinData := pSample^;
      for I := 0 to SamplesPerPixel - 1 do
        begin
          MaxData := Max(MaxData, pSample^);
          MinData := Min(MinData, pSample^);
          Inc(pSample);
        end;
      pbAudio.Canvas.MoveTo(pbAudio.Width div 2 + MulDiv(MaxData, pbAudio.Width, MaxWord div 4), y);
      pbAudio.Canvas.LineTo(pbAudio.Width div 2 + MulDiv(MinData, pbAudio.Width, MaxWord div 4)+ 1, y);
    end;
end;

procedure TMainForm.ListBoxClick(Sender: TObject);
begin
//  ShowFrame(ListBox.ItemIndex);
end;

procedure TMainForm.pbTimeLinePaint(Sender: TObject);
var
  SecondIndex: Integer;
  FrameIndex: integer;
  BookmarkIndex: Integer;
  x, y1, y2: Integer;
  BookmarkText: string;
  BookmarkRect: TRect;
begin
  y1 := 4;

  for SecondIndex := 0 To FramesCount * CurrentSpeedInterval div FrameRate - 1 do
    begin
      x := 4 + MulDiv(pbTimeLine.Width - 8, SecondIndex, FramesCount * CurrentSpeedInterval div FrameRate);
      pbTimeLine.Canvas.Brush.Style := bsSolid;
      pbTimeLine.Canvas.Rectangle(x, y1 + 8, x+1, y1 + 16);
    end;

  for FrameIndex := 0 to FramesCount - 1 do
    begin
      x := 4 + MulDiv(pbTimeLine.Width - 8, FrameIndex, FramesCount);
      if Frames[FrameIndex].Loaded then
        y2 := y1 + 8
      else
        y2 := y1 + 4;

      pbTimeLine.Canvas.Brush.Style := bsSolid;
      if FrameIndex <> CurrentFrameIndex then
        pbTimeLine.Canvas.Rectangle(x, y1, x+1, y2)
      else
        begin
          pbTimeLine.Canvas.Brush.Color := clHighlight;
          pbTimeLine.Canvas.Rectangle(x-1, y1, x+2, y2);
        end;

      for BookmarkIndex := 0 to 9 do
        if FrameIndex = Bookmarks[BookmarkIndex] then
        begin
          BookmarkText := IntToStr(BookmarkIndex);
          BookmarkRect := Rect(x - 20, y1, x + 21, pbTimeLine.Height);
          pbTimeLine.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop, tfCalcRect]);
          BookmarkRect := Rect(x - (BookmarkRect.Right - BookmarkRect.Left) div 2 - 2, y1 - 2, x + (BookmarkRect.Right - BookmarkRect.Left) div 2 + 2, BookmarkRect.Bottom);
          pbTimeLine.Canvas.Brush.Style := bsSolid;
          if FrameIndex = CurrentFrameIndex then
          begin
            pbTimeLine.Canvas.Brush.Color := clHighlight;
            pbTimeLine.Canvas.Font.Color := clHighlightText;
          end
          else
          begin
            pbTimeLine.Canvas.Brush.Color := clBtnFace;
            pbTimeLine.Canvas.Brush.Style := bsSolid;
            pbTimeLine.Canvas.Font.Color := clBtnText;
          end;
          pbTimeLine.Canvas.RoundRect(BookmarkRect, 2, 2);
          Inc(BookmarkRect.Left, 2);
          pbTimeLine.Canvas.Brush.Style := bsClear;
          pbTimeLine.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop]);
        end;

      if Frames[FrameIndex].Teleport <> -1 then
        begin
          BookmarkText := IntToStr(Frames[FrameIndex].Teleport);
          BookmarkRect := Rect(x - 20, y1, x + 21, pbTimeLine.Height);
          pbTimeLine.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop, tfCalcRect]);
          BookmarkRect := Rect(x - (BookmarkRect.Right - BookmarkRect.Left) div 2 - 2, y1 - 2, x + (BookmarkRect.Right - BookmarkRect.Left) div 2 + 2, BookmarkRect.Bottom);
          pbTimeLine.Canvas.Brush.Style := bsSolid;
          if FrameIndex = CurrentFrameIndex then
          begin
            pbTimeLine.Canvas.Brush.Color := clLime;
            pbTimeLine.Canvas.Font.Color := clBlack;
          end
          else
          begin
            pbTimeLine.Canvas.Brush.Color := clGreen;
            pbTimeLine.Canvas.Brush.Style := bsSolid;
            pbTimeLine.Canvas.Font.Color := clWhite;
          end;
          pbTimeLine.Canvas.RoundRect(BookmarkRect, 2, 2);
          Inc(BookmarkRect.Left, 2);
          pbTimeLine.Canvas.Brush.Style := bsClear;
          pbTimeLine.Canvas.TextRect(BookmarkRect, BookmarkText, [tfNoClip, tfLeft, tfTop]);
        end;
    end;
end;

procedure TMainForm.PopControlAction;
begin
  Assert(NextControlActionStackPosition >=1, '{EBDE522B-9ED0-474F-9F3F-4C5CAEDBC757}');
  Dec(NextControlActionStackPosition);
end;

procedure TMainForm.PushControlAction(Value: TControlAction);
begin
  if NextControlActionStackPosition = ControlActionStackDeep then
    Beep
  else
    Inc(NextControlActionStackPosition);
  NextControlActionStack[NextControlActionStackPosition] := Value;
end;

procedure TMainForm.ReplaceControlActions(Value: TControlAction);
begin
  NextControlActionStackPosition := 1;
  NextControlActionStack[NextControlActionStackPosition] := Value;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if (FramesCount = 0) then
    Exit;

  if Exporting then
    Exit;

  if Playing then
    begin
      if CurrentRecordPosition < RecordedFrames.Count then
        begin
          ShowFrame(Integer(RecordedFrames[CurrentRecordPosition]));
          pbRecord.Invalidate;
          pbAudio.Invalidate;
          inc(CurrentRecordPosition);
        end
      else
        if LoopMode then
          CurrentRecordPosition := 0
        else
          Playing := False;
    end
  else
    begin
      case NextControlAction of
        caStepForward:
          begin
            inc(CurrentFrameIndex);
            PopControlAction;
          end;
        caPlayForward:
          if Interval <= 1 then
            if Frames[CurrentFrameIndex].Teleport <> -1 then
              CurrentFrameIndex := Bookmarks[Frames[CurrentFrameIndex].Teleport]
            else
              inc(CurrentFrameIndex);
        caStepBackward:
          begin
            dec(CurrentFrameIndex);
            PopControlAction;
          end;
        caPlayBackward:
          if Interval <= 1 then
            if Frames[CurrentFrameIndex].Teleport <> -1 then
              CurrentFrameIndex := Bookmarks[Frames[CurrentFrameIndex].Teleport]
            else
              dec(CurrentFrameIndex);
        caNone:
          if Interval <= 1 then
            begin
              if GetAsyncKeyState(Ord('A')) < 0 then //  эти буквы ещё упомянуты в меню и в блокировщике горячих клавиш IsShortCut
                dec(CurrentFrameIndex);
              if GetAsyncKeyState(Ord('D')) < 0 then
                inc(CurrentFrameIndex);
            end;
      end;
      if CurrentFrameIndex < 0 then
        CurrentFrameIndex := FramesCount - 1;
      if CurrentFrameIndex > (FramesCount - 1) then
        CurrentFrameIndex := 0;
      ShowFrame(CurrentFrameIndex);

      if Recording then
        begin
          RecordedFrames.Add(Pointer(CurrentFrameIndex));
          pbRecord.Invalidate;
          pbAudio.Invalidate;
        end;

      if Interval <= 1 then
        begin
          Interval := CurrentSpeedInterval;
          pbIndicator.Repaint;
        end
      else
        dec(Interval);
    end;
end;

procedure TMainForm.actPlayBackwardExecute(Sender: TObject);
begin
  if NextControlAction <> caPlayBackward then
    ReplaceControlActions(caPlayBackward)
  else
    ReplaceControlActions(caNone);
  btnPlayBackward.Down := True;
end;

procedure TMainForm.actUpdate_HaveFiles(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and (FramesCount > 0);
end;

procedure TMainForm.actUpdate_HaveRecorded(Sender: TObject);
begin
  TAction(Sender).Enabled := not Exporting and (RecordedFrames.Count > 0);
end;

procedure TMainForm.actStepNextExecute(Sender: TObject);
begin
  PushControlAction(caStepForward);
  btnPlayForward.Down := False;
  btnPlayBackward.Down := False;
end;

procedure TMainForm.actStepPrevExecute(Sender: TObject);
begin
  PushControlAction(caStepBackward);
  btnPlayForward.Down := False;
  btnPlayBackward.Down := False;
end;

procedure TMainForm.actPlayForwardExecute(Sender: TObject);
begin
  if NextControlAction <> caPlayForward then
    ReplaceControlActions(caPlayForward)
  else
    ReplaceControlActions(caNone);
  btnPlayForward.Down := True;
end;

procedure TMainForm.actPlayUpdate(Sender: TObject);
begin
  actPlay.Enabled := not Exporting and (RecordedFrames.Count > 0);
  actPlay.Checked := Playing;
end;

procedure TMainForm.actPlayExecute(Sender: TObject);
begin
  if Recording then
    actRecord.Execute;
  if Playing then
    begin
      Playing := False;
      btnPlay.Down := False;
      StockAudioPlayer.Active := False;
      CurrentRecordPosition := 0;
      Interval := 0;
    end
  else
    begin
      Playing := True;
      btnPlay.Down := True;
      ReplaceControlActions(caNone);
      btnPlayForward.Down := False;
      btnPlayBackward.Down := False;
      StockAudioPlayer.PlayStock(0);
      CurrentRecordPosition := 0;
      Interval := 0;
    end;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  i: integer;
begin
  Done := True;
  for i := 0 to FramesCount - 1 do
    if not Frames[i].Loaded then
      begin
        LoadPhoto(i);
        pbTimeLine.Repaint;
        Done := False;
        Exit;
      end;
end;

procedure TMainForm.AudioLevelChanged(Sender: TObject; Level: Integer);
begin
  LevelGauge.Progress := Level;
end;

procedure TMainForm.AudioRecorderFilter(Sender: TObject; const Buffer: Pointer;
  BufferSize: Cardinal);
begin
  //RecordedAudioCopy.Write(Buffer, BufferSize);
end;

procedure TMainForm.pbIndicatorClick(Sender: TObject);
begin
  if FrameRate = 25 then
    FrameRate := 50
  else
    FrameRate := 25;
  Timer.Interval := 1000 div FrameRate;
  pnlToolls.Invalidate;
//  Invalidate;
end;

function TMainForm.IsShortCut(var Message: TWMKey): Boolean;
begin
  if (Message.CharCode <> vk_Left)
    and (Message.CharCode <> vk_Right)
    and (Message.CharCode <> ord('A'))
    and (Message.CharCode <> ord('D'))
  then
    Result := inherited IsShortCut(Message)
  else
    Result := False;
end;

procedure TMainForm.ClearRecorded;
begin
  WaveStorage.Wave.Clear;
  RecordedAudioCopy.Clear;
  RecordedFrames.Clear;
end;

{ TFrame }

constructor TFrame.Create(AFileName: string);
begin
  FFileName := AFileName;
  Teleport := -1;
end;

destructor TFrame.Destroy;
begin
  FreeAndNil(OriginalJpeg);
  inherited;
end;

end.

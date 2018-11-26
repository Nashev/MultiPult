unit CameraFormUnit;

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

uses
  Winapi.Windows, Winapi.Messages, VCL.FileCtrl, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles, JPEG,
  VFrames, VSample, Direct3D9, DirectDraw, DirectShow9, DirectSound, DXTypes,
  Vcl.ComCtrls, Vcl.Samples.Spin, WaveTimer, Vcl.Imaging.pngimage, System.SyncObjs, DirMon;

type
  TNewFrameEvent = procedure(AFileName: string) of object;
  TOpacityChangedEvent = procedure(AOpacity: byte) of object;

  TCameraForm = class(TForm)
    btnMakePhoto: TButton;
    cbCamSelector: TComboBox;
    btnNextCam: TButton;
    lblCamSelector: TLabel;
    cbbResolution: TComboBox;
    lblResolution: TLabel;
    btnPreferences: TButton;
    btnTimeLapse: TButton;
    seInterval: TSpinEdit;
    cbbUnit: TComboBox;
    TimeLapseTimer: TMultimediaTimer;
    TimeLapseStatusTimer: TMultimediaTimer;
    lblLapseStatus: TLabel;
    lblFolder: TLabel;
    edtFolder: TEdit;
    btnFolderLookup: TButton;
    btnStart: TButton;
    edtOverlay: TEdit;
    btnSelectOverlay: TButton;
    chkOverlay: TCheckBox;
    chkMinimize: TCheckBox;
    tbOpacity: TTrackBar;
    lblOpacity: TLabel;
    btnReloadOverlay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnMakePhotoClick(Sender: TObject);
    procedure btnNextCamClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbCamSelectorChange(Sender: TObject);
    procedure cbbResolutionChange(Sender: TObject);
    procedure btnPreferencesClick(Sender: TObject);
    procedure btnTimeLapseClick(Sender: TObject);
    procedure TimeLapseTimerTimer(Sender: TObject);
    procedure TimeLapseStatusTimerTimer(Sender: TObject);
    procedure btnFolderLookupClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chkOverlayClick(Sender: TObject);
    procedure btnSelectOverlayClick(Sender: TObject);
    procedure btnReloadOverlayClick(Sender: TObject);
    procedure ToggleVisibility(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FVideoImage: TVideoImage;
    FVideoBitmap: TBitmap;
    VideoBitmapCriticalSection: TCriticalSection;
    FPhotoFolder: string;
    FOnNewFrame: TNewFrameEvent;
    LastPhotoTimeStamp: DWord;
    LastPreviewFrameTimeStamp: DWord;
    LastFileName: string;
    FActive: Boolean;
    FimgCamPreview: TImage;
    FimgOverlay: TImage;
    FCanChangePhotoFolder: Boolean;
    FOnOpacityChanged: TOpacityChangedEvent;
    FOnActiveChanged: TNotifyEvent;
    FOverlayDirMonitor: TDirMonitor;
    FFileCommanderDirMonitor: TDirMonitor;
    FPrevResolution: string;
    procedure GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
    function IntervalToString(AInterval: Integer): string;
    procedure SetPhotoFolder(Value: string);
    procedure LookupPhotoFolder(APhotoFolder: string);
    procedure SetActive(const Value: Boolean);
    procedure StartCamera;
    procedure StopCamera;
    procedure SetImgCamPreview(const Value: TImage);
    procedure SetImgOverlay(const Value: TImage);
    procedure DoActiveChanged;
    procedure OverlayDirChangedHandler(Sender: TObject);
    procedure StartFileCommander;
    procedure StopFileCommander;
    procedure FileCommanderDirChangedHandler(Sender: TObject);
    procedure LoadSettings;
    procedure SaveSettings;
  public
    property PhotoFolder: string read FPhotoFolder write SetPhotoFolder;
    property OnNewFrame: TNewFrameEvent read FOnNewFrame write FOnNewFrame;
    property OnOpacityChanged: TOpacityChangedEvent read FOnOpacityChanged write FOnOpacityChanged;
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property imgCamPreview: TImage read FimgCamPreview write SetImgCamPreview;
    property imgOverlay: TImage read FimgOverlay write SetImgOverlay;
    property Active: Boolean read FActive write SetActive;
    procedure DisablePhotoFolderLookup;
    function MakePhoto(const AFileName: string = ''; const AExtraExt: string = ''): string;
  end;

var
  CameraForm: TCameraForm;

implementation

uses
  UtilsUnit;

{$R *.dfm}

procedure TCameraForm.ToggleVisibility(Sender: TObject);
begin
  Visible := not Visible; // используется в program WebcamMultFrameCapture;
end;

procedure TCameraForm.FormCreate(Sender: TObject);
begin
  FCanChangePhotoFolder := True;
  FVideoBitmap := TBitmap.Create;
  VideoBitmapCriticalSection := TCriticalSection.Create;
  FVideoImage := TVideoImage.Create;
  FVideoImage.OnNewVideoFrame := GetNewFrame;

  edtOverlay.Text := '';
  LoadSettings;

  if LowerCase(ParamStr(2)) = '/timelapse' then
    btnTimeLapse.Click;
end;

procedure TCameraForm.LoadSettings;
var
  IniFile: TIniFile;
  LastUsedCam: string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    LastUsedCam := IniFile.ReadString('LastUsed', 'Camera', '');
    if LastUsedCam = '' then
      if cbCamSelector.ItemIndex <> -1 then
        LastUsedCam := cbCamSelector.Items[cbCamSelector.ItemIndex];
    cbCamSelector.Items.Clear;
    FVideoImage.GetListOfDevices(cbCamSelector.Items);
    if cbCamSelector.Items.Count > 0 then
    begin
      cbCamSelector.ItemIndex := cbCamSelector.Items.IndexOf(LastUsedCam);
      if cbCamSelector.ItemIndex = -1 then
        cbCamSelector.ItemIndex := 0;
    end;
    FPrevResolution := IniFile.ReadString('LastUsed', 'CamResolution', '');
    btnNextCam.Enabled := cbCamSelector.Items.Count > 0;
    chkOverlay.Checked := IniFile.ReadBool('LastUsed', 'ShowOverlay', chkOverlay.Checked);
    edtOverlay.Text := IniFile.ReadString('LastUsed', 'OverlayFile', edtOverlay.Text);
    chkOverlayClick(nil);
    tbOpacity.Position := tbOpacity.Max - (IniFile.ReadInteger('LastUsed', 'Opacity', AlphaBlendValue) - tbOpacity.Min);
    seInterval.Value := IniFile.ReadInteger('LastUsed', 'TimeLapseInterval', seInterval.Value);
    cbbUnit.ItemIndex := IniFile.ReadInteger('LastUsed', 'TimeLapseIntervalUnit', cbbUnit.ItemIndex);
    if PhotoFolder = '' then
      PhotoFolder := IniFile.ReadString('LastUsed', 'Folder', GetCurrentDir);
    Left := IniFile.ReadInteger('LastUsed', 'CamSettingsLeft', Left);
    Top := IniFile.ReadInteger('LastUsed', 'CamSettingsTop', Top);
    //    Width := IniFile.ReadInteger('LastUsed', 'CamSettingsWidth', Width);
    //    Height := IniFile.ReadInteger('LastUsed', 'CamSettingsHeight', Height);
    //
    //    WindowState := TWindowState(IniFile.ReadInteger('LastUsed', 'WindowState', Ord(WindowState)));
    chkMinimize.Checked := IniFile.ReadBool('LastUsed', 'MinimizeAfterFrame', chkMinimize.Checked);
  finally
    IniFile.Free;
  end;
end;

procedure TCameraForm.FormDestroy(Sender: TObject);
begin
  // наивные попытки выключить таймер. Но этот всё равно срабатывает в параллельном потоке и иногда падает
  TimeLapseTimer.Enabled := False;
  TimeLapseStatusTimer.Enabled := False;
  TimeLapseTimer.OnTimer := nil;
  TimeLapseStatusTimer.OnTimer := nil;
  Application.ProcessMessages;
  Sleep(300);
  // StopCamera;
  StopFileCommander;

  VideoBitmapCriticalSection.Enter;
  try
    FreeAndNil(FVideoImage);
    FreeAndNil(FVideoBitmap);
  finally
    VideoBitmapCriticalSection.Free;
  end;
end;

procedure TCameraForm.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    IniFile.WriteString('LastUsed', 'Camera', cbCamSelector.Items[cbCamSelector.ItemIndex]);
    IniFile.WriteString('LastUsed', 'CamResolution', cbbResolution.Items[cbbResolution.ItemIndex]);
    if btnFolderLookup.Visible and (PhotoFolder <> '') then
      IniFile.WriteString('LastUsed', 'Folder', PhotoFolder);
    IniFile.WriteInteger('LastUsed', 'TimeLapseInterval', seInterval.Value);
    IniFile.WriteInteger('LastUsed', 'TimeLapseIntervalUnit', cbbUnit.ItemIndex);
    IniFile.WriteBool('LastUsed', 'ShowOverlay', imgOverlay.Visible);
    IniFile.WriteString('LastUsed', 'OverlayFile', edtOverlay.Text);
    IniFile.WriteInteger('LastUsed', 'Opacity', tbOpacity.Max - (tbOpacity.Position - tbOpacity.Min));
    //    IniFile.WriteInteger('LastUsed', 'WindowState', Ord(WindowState));
    //    if WindowState = wsNormal then
    //      begin
    IniFile.WriteInteger('LastUsed', 'CamSettingsLeft', Left);
    IniFile.WriteInteger('LastUsed', 'CamSettingsTop', Top);
    //        IniFile.WriteInteger('LastUsed', 'CamSettingsWidth', Width);
    //        IniFile.WriteInteger('LastUsed', 'CamSettingsHeight', Height);
    //      end;
    IniFile.WriteBool('LastUsed', 'MinimizeAfterFrame', chkMinimize.Checked);
  finally
    IniFile.Free;
  end;
end;

procedure TCameraForm.FormHide(Sender: TObject);
begin
  SaveSettings;
  // StopCamera;
end;

procedure TCameraForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then  // пробел
    btnMakePhotoClick(nil);
end;

procedure TCameraForm.FormResize(Sender: TObject);
begin
  imgOverlay.BoundsRect := imgCamPreview.BoundsRect;
end;

procedure TCameraForm.GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
begin
  if not Active or ((GetTickCount - LastPreviewFrameTimeStamp) < 100) then
    Exit;

  VideoBitmapCriticalSection.Enter;
  try
    FVideoImage.GetBitmap(FVideoBitmap);
    if not (imgCamPreview.Picture.Graphic is TPngImage) then
      imgCamPreview.Picture.Graphic := TPngImage.CreateBlank(COLOR_RGB, 8, 1, 1);

    imgCamPreview.Picture.Graphic.Assign(FVideoBitmap);
  finally
    VideoBitmapCriticalSection.Leave;
  end;
  if not Assigned(OnOpacityChanged) and (tbOpacity.Position <> tbOpacity.Min) then
    begin
      TPngImage(imgCamPreview.Picture.Graphic).CreateAlpha;
      FillMemory(
        TPngImage(imgCamPreview.Picture.Graphic).AlphaScanline[0],
        Integer(imgCamPreview.Picture.Graphic.Width) * Integer(imgCamPreview.Picture.Graphic.Height),
        tbOpacity.Max - (tbOpacity.Position - tbOpacity.Min));
    end;
  imgCamPreview.Repaint;
  TForm(imgCamPreview.Owner).Cursor := crDefault; //  TODO: переделать на нормальное событие с обработчиком
  Screen.Cursor := TForm(imgCamPreview.Owner).Cursor;

  LastPreviewFrameTimeStamp := GetTickCount;
end;

procedure TCameraForm.btnReloadOverlayClick(Sender: TObject);
begin
  imgOverlay.Picture.LoadFromFile(edtOverlay.Text);
end;

procedure TCameraForm.btnFolderLookupClick(Sender: TObject);
begin
  LookupPhotoFolder(PhotoFolder);
end;

procedure TCameraForm.LookupPhotoFolder(APhotoFolder: string);
resourcestring
  rs_SelectPhotoFolderCaption = 'В какую папку сохранять взятые кадры?';
begin
  if SelectDirectory(
    rs_SelectPhotoFolderCaption, '', APhotoFolder
    {$IFDEF DelphiXE}
    , [sdNewFolder, sdShowFiles, sdShowEdit, sdShowShares, sdValidateDir, sdNewUI]
    {$ENDIF}
  ) then
    PhotoFolder := APhotoFolder + '\';
end;

procedure TCameraForm.btnMakePhotoClick(Sender: TObject);
begin
  MakePhoto;
  if chkMinimize.Checked then
    if FCanChangePhotoFolder then
      Application.MainForm.WindowState := wsMinimized // самостоятельным приложением
    else
      Active := False; //  в составе МультиПульта
end;

procedure TCameraForm.btnNextCamClick(Sender: TObject);
begin
  if cbCamSelector.ItemIndex < cbCamSelector.Items.Count - 1 then
    cbCamSelector.ItemIndex := cbCamSelector.ItemIndex + 1
  else
    cbCamSelector.ItemIndex := 0;
  StopCamera;
  StartCamera;
  DoActiveChanged;
end;

procedure TCameraForm.btnPreferencesClick(Sender: TObject);
begin
  if not SUCCEEDED(FVideoImage.ShowVfWCaptureDlg) then
    if not SUCCEEDED(FVideoImage.ShowProperty) then
      ShowMessage('Параметры открыть не удалось');
end;

procedure TCameraForm.btnSelectOverlayClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  begin
    Filter := '*.png; *.gif|*.png;*.gif|*.*|*.*';
    FileName := edtOverlay.Text;
    if Execute then
    begin
      edtOverlay.Text := FileName;
      imgOverlay.Picture.LoadFromFile(edtOverlay.Text);
      imgOverlay.Visible := True;
      chkOverlay.Checked := True;
      chkOverlayClick(nil);
    end;
  end;
end;

procedure TCameraForm.btnStartClick(Sender: TObject);
begin
  StartCamera;
  DoActiveChanged;
end;

procedure TCameraForm.StartCamera;
var
  NewIndex: Integer;
begin
  if FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex])) = 0 then
    begin
      if cbbResolution.Items.Count > 0 then
        FPrevResolution := cbbResolution.Items[cbbResolution.ItemIndex];
      cbbResolution.Clear;
      FVideoImage.GetListOfSupportedVideoSizes(cbbResolution.Items);
      NewIndex := cbbResolution.Items.IndexOf(FPrevResolution);
      if NewIndex = -1 then
        NewIndex := 0;
      cbbResolution.ItemIndex := NewIndex;
      FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
      FActive := True;
      StartFileCommander;
    end;
end;

procedure TCameraForm.StartFileCommander;
begin
  StopFileCommander;
  if Active then
    begin
      FFileCommanderDirMonitor := TDirMonitor.Create(PhotoFolder, FileCommanderDirChangedHandler, nil);
      FFileCommanderDirMonitor.Start;
    end;
end;

procedure TCameraForm.StopFileCommander;
begin
  if Assigned(FFileCommanderDirMonitor) then
    begin
      FFileCommanderDirMonitor.FreeOnTerminate := True;
      FFileCommanderDirMonitor.Terminate;
      FFileCommanderDirMonitor := nil;
    end;
end;

procedure TCameraForm.FileCommanderDirChangedHandler(Sender: TObject);
var
  i: Integer;
  SavedFileName: string;
begin
  if Assigned(FFileCommanderDirMonitor) then
    for i := 0 to FFileCommanderDirMonitor.Notifications.Count - 1 do
      if (LowerCase(FFileCommanderDirMonitor.Notifications[i]) = 'grab') then
        if ([dmaAdded, dmaNewName] * FFileCommanderDirMonitor.Notifications.Actions[i] <> []) then
          try
            SavedFileName := GetFileContent(PhotoFolder + 'grab');
            DeleteFile(PhotoFolder + 'grab'); // забрали исполнять

            if SavedFileName = '' then
              SavedFileName := 'Grabbed';

            SavedFileName := MakePhoto(SavedFileName, '.tmp'); // создаём файл и "постепенно" наполняем содержимым
            RenameFile(PhotoFolder + SavedFileName, PhotoFolder + ChangeFileExt(SavedFileName, '')); // делаем, чтоб файл с нужным именем появлялся уже наполненным.
          except
            ;
          end;
end;

procedure TCameraForm.btnTimeLapseClick(Sender: TObject);
resourcestring
  rs_TimeLapseStartButton = 'Брать кадр каждые';
  rs_TimeLapseStopButton = 'Не брать кадр каждые';
begin
  if not TimeLapseTimer.Enabled then
  begin
    case cbbUnit.ItemIndex of
      0: TimeLapseTimer.Interval := seInterval.Value;
      1: if seInterval.Value <= 64 then TimeLapseTimer.Interval := seInterval.Value * 1000 else begin
        ShowMessage('Поддержка интервалов более 64 секунд пока не сделана.');
        Abort;
      end;
//      2: TimeLapseTimer.Interval := seInterval.Value * 1000 * 60;
//      3: TimeLapseTimer.Interval := seInterval.Value * 1000 * 60 * 60;
//      4: TimeLapseTimer.Interval := seInterval.Value * 1000 * 60 * 60 * 24;
    end;
    TimeLapseTimer.Enabled := True;
    TimeLapseStatusTimer.Enabled := True;
    TimeLapseTimer.OnTimer(nil);
    cbbUnit.Enabled := False;
    seInterval.Enabled := False;
    btnTimeLapse.Caption := rs_TimeLapseStopButton;
  end else begin
    TimeLapseTimer.Enabled := False;
    TimeLapseStatusTimer.Enabled := False;
    lblLapseStatus.Caption := LastFileName;
    cbbUnit.Enabled := True;
    seInterval.Enabled := True;
    btnTimeLapse.Caption := rs_TimeLapseStartButton;
  end;
end;

procedure TCameraForm.cbbResolutionChange(Sender: TObject);
begin
  FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
  StopCamera;
  StartCamera;
  DoActiveChanged;
end;

procedure TCameraForm.cbCamSelectorChange(Sender: TObject);
begin
  StopCamera;
  StartCamera;
  DoActiveChanged;
end;

procedure TCameraForm.chkOverlayClick(Sender: TObject);
begin
  if Assigned(imgOverlay) then
    begin
      imgOverlay.Visible := chkOverlay.Checked;
      if Assigned(FOverlayDirMonitor) then
        begin
          FOverlayDirMonitor.FreeOnTerminate := True;
          FOverlayDirMonitor.Terminate;
          FOverlayDirMonitor := nil;
        end;
      if chkOverlay.Checked and DirectoryExists(ExtractFilePath(edtOverlay.Text)) then
        begin
          FOverlayDirMonitor := TDirMonitor.Create(ExtractFilePath(edtOverlay.Text), OverlayDirChangedHandler, nil);
          FOverlayDirMonitor.Start;
        end;
    end;
end;

procedure TCameraForm.OverlayDirChangedHandler(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FOverlayDirMonitor) then
    for i := 0 to FOverlayDirMonitor.Notifications.Count - 1 do
      if (FOverlayDirMonitor.Path + FOverlayDirMonitor.Notifications[i] = edtOverlay.Text) then
        if ([dmaAdded, dmaNewName, dmaModified] * FOverlayDirMonitor.Notifications.Actions[i] <> []) then
          try
            imgOverlay.Picture.LoadFromFile(edtOverlay.Text);
          except
            ;
          end;
end;

procedure TCameraForm.StopCamera;
resourcestring
  rsPressStart = 'Если камера не включается, '#13#10'нажмите кнопку Пуск в окне управления камерой';
var
  StubImage: TPngImage;
begin
  FActive := False;
  VideoBitmapCriticalSection.Enter;
  try
    FVideoImage.VideoStop;
  finally
    VideoBitmapCriticalSection.Leave;
  end;
  Assert(imgCamPreview.Picture.Graphic is TPngImage, '{1F9B8A82-C994-4546-ADA4-2DD3E1CCA0EF}');
  StubImage := TPngImage(imgCamPreview.Picture.Graphic);

  StubImage.SetSize(imgCamPreview.Width, imgCamPreview.Height);
  StubImage.Canvas.Brush.Color := clBlack;
  StubImage.Canvas.FillRect(Rect(0, 0, StubImage.Width, StubImage.Height));
  StubImage.Canvas.Font.Color := clWhite;
  StubImage.Canvas.TextRect(Rect(4, 4, StubImage.Width, StubImage.Height), 4, 4, rsPressStart);
  StopFileCommander;
end;

function TCameraForm.MakePhoto(const AFileName: string = ''; const AExtraExt: string = ''): string;
var
  StoringFile: TPNGImage;
  n: Integer;
  StringDate: string;
const
  Ext = '.png';
begin
  if AFileName = '' then
    Result := '%timestamp%%unique%'
  else
    Result := AFileName;

  if Result.IndexOf('%timestamp%') >= 0 then begin
    DateTimeToString(StringDate, 'yyyy.mm.dd-hh.nn.ss.zzz', Now);
    Result := Result.Replace('%timestamp%', StringDate, []);
  end;

  if Result.IndexOf('%unique%') >= 0 then begin
    n := 0;
    while FileExists(PhotoFolder + Result.Replace('%unique%', StringOfChar('_', n)) + Ext) do
      Inc(n);
    Result := Result.Replace('%unique%', StringOfChar('_', n));
  end;

  Result := Result + Ext + AExtraExt;
  StoringFile := TPNGImage.Create;
  try
    VideoBitmapCriticalSection.Enter;
    try
      StoringFile.Assign(FVideoBitmap);
    finally
      VideoBitmapCriticalSection.Leave;
    end;
    ForceDirectories(PhotoFolder);
    StoringFile.SaveToFile(PhotoFolder + Result);
  finally
    StoringFile.Free;
  end;
  if Assigned(OnNewFrame)  then
    OnNewFrame(PhotoFolder + Result);
  LastFileName := Result;
  lblLapseStatus.Caption := LastFileName;
end;

procedure TCameraForm.DoActiveChanged;
begin
  if Assigned(OnActiveChanged) then
    OnActiveChanged(Self);
end;


procedure TCameraForm.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;

  if Value then
    StartCamera
  else
    StopCamera;
  DoActiveChanged;
end;

procedure TCameraForm.DisablePhotoFolderLookup;
resourcestring
  rsAndDeactivateCamera = 'И выключить камеру';
begin
  FCanChangePhotoFolder := False;
  btnFolderLookup.Visible := False;
  edtFolder.Width := edtFolder.Width - btnFolderLookup.Width - 8;
  chkMinimize.Caption := rsAndDeactivateCamera;
end;

procedure TCameraForm.SetImgCamPreview(const Value: TImage);
begin
  FimgCamPreview := Value;
end;

procedure TCameraForm.SetImgOverlay(const Value: TImage);
begin
  FimgOverlay := Value;
  if Assigned(FimgOverlay) and FileExists(edtOverlay.Text) then
    begin
      imgOverlay.Picture.LoadFromFile(edtOverlay.Text);
      imgOverlay.Visible := chkOverlay.Checked;
      chkOverlayClick(nil);
    end;
end;

procedure TCameraForm.SetPhotoFolder(Value: string);
begin
  if Value <> '' then begin
    if Value[Length(Value)] <> '\' then
      Value := Value + '\';

    if not DirectoryExists(Value) then
      if FileExists(Value) then begin
        ShowMessage(Format('Вместо папки для сохранения кадров с камеры указан файл "%s". Нужно указать папку.', [Value]));
        // внутри, если пользователь выберет папку,
        // этот сеттер будет вызван рекурсивно и там всё сделает,
        // а если пользователь откажется - то ничего вызвано не будет
        // и значение у свойтва останется прежним.
        LookupPhotoFolder(Value);
        Exit;
      end else
        ForceDirectories(Value);

    FPhotoFolder := Value;

    StartFileCommander;
  end else begin
    FPhotoFolder := Value;
    StopFileCommander;
  end;

  edtFolder.Text := PhotoFolder;
end;

function TCameraForm.IntervalToString(AInterval: Integer): string;
var
  d, d2: Integer;
begin
  d := AInterval;
  if d <= 300 then
    Exit('');

  d2 := d mod 1000;
  // mSec in last second
  Result := Format('%2.2d', [d2]);
  d := d div 1000;
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

procedure TCameraForm.TimeLapseStatusTimerTimer(Sender: TObject);
var
  s: string;
begin
  s := '';
  if (GetTickCount > LastPhotoTimeStamp) and (TimeLapseTimer.Interval > (GetTickCount - LastPhotoTimeStamp))then
    s := IntervalToString(TimeLapseTimer.Interval - (GetTickCount - LastPhotoTimeStamp));
  if s <> '' then
    s := ' (ждём ' + s + ')';

  lblLapseStatus.Caption := LastFileName + s;
end;

procedure TCameraForm.TimeLapseTimerTimer(Sender: TObject);
begin
  if Active and not FVideoBitmap.Empty and not Application.Terminated and TimeLapseTimer.Enabled then
    begin
      MakePhoto;
      LastPhotoTimeStamp := GetTickCount;
    end;
end;

procedure TCameraForm.tbOpacityChange(Sender: TObject);
begin
  if Assigned(OnOpacityChanged) then
    OnOpacityChanged(tbOpacity.Max - (tbOpacity.Position - tbOpacity.Min));
end;

end.


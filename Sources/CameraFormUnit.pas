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
  Vcl.ComCtrls, Vcl.Samples.Spin, WaveTimer;

type
  TOnNewFrame = procedure(AFileName: string) of object;

  TCameraForm = class(TForm)
    imgPreview: TImage;
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
    procedure FormCreate(Sender: TObject);
    procedure btnMakePhotoClick(Sender: TObject);
    procedure btnNextCamClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbCamSelectorChange(Sender: TObject);
    procedure cbbResolutionChange(Sender: TObject);
    procedure btnPreferencesClick(Sender: TObject);
    procedure btnTimeLapseClick(Sender: TObject);
    procedure TimeLapseTimerTimer(Sender: TObject);
    procedure TimeLapseStatusTimerTimer(Sender: TObject);
    procedure btnFolderLookupClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    FVideoImage: TVideoImage;
    FVideoBitmap: TBitmap;
    LastPhotoTimeStamp: DWord;
    LastPreviewFrameTimeStamp: DWord;
    FPhotoFolder: string;
    FOnNewFrame: TOnNewFrame;
    CameraStopped: Boolean;
    procedure GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
    procedure MakePhoto;
    function IntervalToString(AInterval: Integer): string;
    procedure SetPhotoFolder(const Value: string);
    procedure LookupPhotoFolder;
    procedure StartCamera;
    procedure StopCamera;
  public
    property PhotoFolder: string read FPhotoFolder write SetPhotoFolder;
    property OnNewFrame: TOnNewFrame read FOnNewFrame write FOnNewFrame;
    procedure Execute(APhotoFolder: string; AOnNewFrame: TOnNewFrame);
  end;

var
  CameraForm: TCameraForm;

implementation

{$R *.dfm}

// Executa ao iniciar o programa.
procedure TCameraForm.FormCreate(Sender: TObject);
begin
  FVideoBitmap := TBitmap.Create;
  FVideoImage := TVideoImage.Create;
  FVideoImage.OnNewVideoFrame := GetNewFrame;
end;

procedure TCameraForm.FormDestroy(Sender: TObject);
begin
  FVideoImage.Free;
  FVideoBitmap.Free;
end;

procedure TCameraForm.FormShow(Sender: TObject);
var
  LastUsedCam: string;
  LastUsedResolution: Integer;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ParamStr(0) + '.ini');
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
      FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex]));
      cbbResolution.Items.Clear;
      FVideoImage.GetListOfSupportedVideoSizes(cbbResolution.Items);
      LastUsedResolution := cbbResolution.Items.IndexOf(IniFile.ReadString('LastUsed', 'Resolution', ''));
      if LastUsedResolution < 0 then
        LastUsedResolution := 0;
      cbbResolution.ItemIndex := LastUsedResolution;
      FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
    end;

    btnNextCam.Enabled := cbCamSelector.Items.Count > 0;

    if PhotoFolder = '' then // если в момент этой инициализации окна папка
    // ещё не указана, то значит вызов не из мультипульта, и можно папку дать лукапить.
      begin
        btnFolderLookup.Visible := True;
        edtFolder.Width := edtFolder.Width - btnFolderLookup.Width - 8;
      end;

    if PhotoFolder = '' then
      PhotoFolder := ParamStr(1);

    if PhotoFolder = '' then
      PhotoFolder := IniFile.ReadString('LastUsed', 'Folder', GetCurrentDir);
  
    Left := IniFile.ReadInteger('LastUsed', 'Left', Left);
    Top := IniFile.ReadInteger('LastUsed', 'Top', Top);
    Width := IniFile.ReadInteger('LastUsed', 'Width', Width);
    Height := IniFile.ReadInteger('LastUsed', 'Height', Height);
  
    WindowState := TWindowState(IniFile.ReadInteger('LastUsed', 'WindowState', Ord(WindowState)));
  finally
    IniFile.Free;
  end;
end;

procedure TCameraForm.FormHide(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ParamStr(0) + '.ini');
  try
    IniFile.WriteString('LastUsed', 'Camera', cbCamSelector.Items[cbCamSelector.ItemIndex]);
    IniFile.WriteString('LastUsed', 'Resolution', cbbResolution.Items[cbbResolution.ItemIndex]);
  
    if btnFolderLookup.Visible and (PhotoFolder <> '') then
      IniFile.WriteString('LastUsed', 'Folder', PhotoFolder);
  
    IniFile.WriteInteger('LastUsed', 'WindowState', Ord(WindowState));
    if WindowState = wsNormal then
      begin    
        IniFile.WriteInteger('LastUsed', 'Left', Left);
        IniFile.WriteInteger('LastUsed', 'Top', Top);
        IniFile.WriteInteger('LastUsed', 'Width', Width);
        IniFile.WriteInteger('LastUsed', 'Height', Height);
      end;      
  finally
    IniFile.Free;
  end;
  StopCamera;
end;

procedure TCameraForm.GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
begin
  if CameraStopped or ((GetTickCount - LastPreviewFrameTimeStamp) < 100) then
    Exit;

  FVideoImage.GetBitmap(FVideoBitmap);
  imgPreview.Picture.Bitmap.Assign(FVideoBitmap);

  LastPreviewFrameTimeStamp := GetTickCount;
end;

procedure TCameraForm.btnFolderLookupClick(Sender: TObject);
begin
  LookupPhotoFolder;
end;

procedure TCameraForm.LookupPhotoFolder;
resourcestring
  rs_SelectPhotoFolderCaption = 'В какую папку сохранять взятые кадры?';
var
  NewPhotoFolder: string;
begin
  NewPhotoFolder := PhotoFolder;
  if SelectDirectory(
    rs_SelectPhotoFolderCaption, '', NewPhotoFolder
    {$IFDEF DelphiXE}
    , [sdNewFolder, sdShowFiles, sdShowEdit, sdShowShares, sdValidateDir, sdNewUI]
    {$ENDIF}
  ) then
    PhotoFolder := NewPhotoFolder + '\';
end;

procedure TCameraForm.btnMakePhotoClick(Sender: TObject);
begin
  MakePhoto;
end;

procedure TCameraForm.btnNextCamClick(Sender: TObject);
begin
  if cbCamSelector.ItemIndex < cbCamSelector.Items.Count - 1 then
    cbCamSelector.ItemIndex := cbCamSelector.ItemIndex + 1
  else
    cbCamSelector.ItemIndex := 0;
  StopCamera;
  StartCamera;
end;

procedure TCameraForm.btnPreferencesClick(Sender: TObject);
begin
  if not SUCCEEDED(FVideoImage.ShowVfWCaptureDlg) then
    if not SUCCEEDED(FVideoImage.ShowProperty) then
      ShowMessage('Параметры открыть не удалось');
end;

procedure TCameraForm.btnStartClick(Sender: TObject);
begin
  StartCamera;
end;

procedure TCameraForm.StartCamera;
var
  PrevResolution: string;
  NewIndex: Integer;
begin
  if FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex])) = 0 then
    begin
      cbbResolution.Clear;
      PrevResolution := cbbResolution.Items[cbbResolution.ItemIndex];
      FVideoImage.GetListOfSupportedVideoSizes(cbbResolution.Items);
      NewIndex := cbbResolution.Items.IndexOf(PrevResolution);
      if NewIndex = -1 then
        NewIndex := 0;
      cbbResolution.ItemIndex := NewIndex;
      FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
      CameraStopped := False;
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
      1: if seInterval.Value <= 64 then TimeLapseTimer.Interval := seInterval.Value * 1000 else begin ShowMessage('Поддержка интервалов более 64 секунд пока не сделана.'); Abort; end;
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
    lblLapseStatus.Caption := '';
    cbbUnit.Enabled := True;
    seInterval.Enabled := True;
    btnTimeLapse.Caption := rs_TimeLapseStartButton;
  end;
end;

procedure TCameraForm.cbbResolutionChange(Sender: TObject);
begin
  FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
end;

procedure TCameraForm.cbCamSelectorChange(Sender: TObject);
begin
  StopCamera;
  StartCamera;
end;

procedure TCameraForm.Execute(APhotoFolder: string; AOnNewFrame: TOnNewFrame);
begin
  PhotoFolder := APhotoFolder;
  OnNewFrame := AOnNewFrame;
  ShowModal;
end;

procedure TCameraForm.StopCamera;
resourcestring
  rsPressStart = 'Если камера не включается, '#13#10'нажмите кнопку Пуск в этом окне';
begin
  CameraStopped := True;
  FVideoImage.VideoStop;
  imgPreview.Picture.Bitmap.SetSize(imgPreview.Width, imgPreview.Height);
  imgPreview.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  imgPreview.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, imgPreview.Picture.Bitmap.Width, imgPreview.Picture.Bitmap.Height));
  imgPreview.Picture.Bitmap.Canvas.Font.Color := clWhite;
  imgPreview.Picture.Bitmap.Canvas.TextRect(Rect(4, 4, imgPreview.Picture.Bitmap.Width, imgPreview.Picture.Bitmap.Height), 4, 4, rsPressStart);
end;

procedure TCameraForm.MakePhoto;
var
  NewFileName: string;
  StoringFile: TJPEGImage;
begin
  DateTimeToString(NewFileName, 'yyyy.mm.dd-hh.nn.ss', Now);
  NewFileName := NewFileName + '.jpg';
  StoringFile := TJPEGImage.Create;
  try
    StoringFile.Assign(FVideoBitmap);
    ForceDirectories(PhotoFolder);
    StoringFile.SaveToFile(PhotoFolder + NewFileName);
  finally
    StoringFile.Free;
  end;
  if Assigned(OnNewFrame)  then
    OnNewFrame(NewFileName);
end;


procedure TCameraForm.SetPhotoFolder(const Value: string);
begin
  FPhotoFolder := Value;
  if Value <> '' then
    if not DirectoryExists(FPhotoFolder) then
      if FileExists(PhotoFolder) then
        LookupPhotoFolder
      else
        ForceDirectories(PhotoFolder);
      
  edtFolder.Text := Value;
end;

function TCameraForm.IntervalToString(AInterval: Integer): string;
var
  d, d2: Integer;
begin
  d := AInterval;
  if d <= 1000 then
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
begin
  lblLapseStatus.Caption := IntervalToString(TimeLapseTimer.Interval - (GetTickCount - LastPhotoTimeStamp));
end;

procedure TCameraForm.TimeLapseTimerTimer(Sender: TObject);
begin
  MakePhoto;
  LastPhotoTimeStamp := GetTickCount;
end;

end.


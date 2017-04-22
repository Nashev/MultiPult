unit CameraFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles, JPEG,
  VFrames, VSample, Direct3D9, DirectDraw, DirectShow9, DirectSound, DXTypes,
  Vcl.ComCtrls, Vcl.Samples.Spin, WaveTimer;

type
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
  private
    FVideoImage: TVideoImage;
    FVideoBitmap: TBitmap;
    LastPhotoTimeStamp: DWord;
    LastPreviewFrameTimeStamp: DWord;
    procedure GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
    procedure MakePhoto;
    function IntervalToString(AInterval: Integer): string;
  public
    procedure Execute;
  end;

var
  CameraForm: TCameraForm;

implementation

uses
  MainFormUnit;

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
begin
  if cbCamSelector.ItemIndex <> -1 then
    LastUsedCam := cbCamSelector.Items[cbCamSelector.ItemIndex]
  else
    LastUsedCam := '';

  cbCamSelector.Items.Clear;
  FVideoImage.GetListOfDevices(cbCamSelector.Items);

  if cbCamSelector.Items.Count > 0 then
  begin
    cbCamSelector.ItemIndex := cbCamSelector.Items.IndexOf(LastUsedCam);
    if cbCamSelector.ItemIndex = -1 then
      cbCamSelector.ItemIndex := 0; // TODO: save/restore last used
    FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex]));
    cbbResolution.Items.Clear;
    FVideoImage.GetListOfSupportedVideoSizes(cbbResolution.Items);
    cbbResolution.ItemIndex := 0;
    FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
  end;

  btnNextCam.Enabled := cbCamSelector.Items.Count > 0;
end;

procedure TCameraForm.FormHide(Sender: TObject);
begin
  FVideoImage.VideoStop;
  imgPreview.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  imgPreview.Picture.Bitmap.Canvas.FillRect(
    Rect(
      0, 0,
      imgPreview.Picture.Bitmap.Width,
      imgPreview.Picture.Bitmap.Height
    )
  );
end;

procedure TCameraForm.GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
begin
  if (GetTickCount - LastPreviewFrameTimeStamp) < 100 then
    Exit;

  FVideoImage.GetBitmap(FVideoBitmap);
  imgPreview.Picture.Bitmap.Assign(FVideoBitmap);

  LastPreviewFrameTimeStamp := GetTickCount;
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

  FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex]));
end;

procedure TCameraForm.btnPreferencesClick(Sender: TObject);
begin
  if not SUCCEEDED(FVideoImage.ShowVfWCaptureDlg) then
    if not SUCCEEDED(FVideoImage.ShowProperty) then
      ShowMessage('Параметры открыть не удалось');
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
  FVideoImage.VideoStart(Trim(cbCamSelector.Items[cbCamSelector.ItemIndex]));
  cbbResolution.Clear;
  FVideoImage.GetListOfSupportedVideoSizes(cbbResolution.Items);
  cbbResolution.ItemIndex := 0;
  FVideoImage.SetResolutionByIndex(cbbResolution.ItemIndex);
end;

procedure TCameraForm.Execute;
begin
  ShowModal
end;

procedure TCameraForm.MakePhoto;
resourcestring
  CamFolder = 'FromCam\';
var
  NewFileName: string;
  StoringFile: TJPEGImage;
begin
  DateTimeToString(NewFileName, 'yyyy.mm.dd-hh.nn.ss', Now);
  NewFileName := NewFileName + '.jpg';
  StoringFile := TJPEGImage.Create;
  try
    StoringFile.Assign(FVideoBitmap);
    ForceDirectories(MainForm.PhotoFolder + CamFolder);
    StoringFile.SaveToFile(MainForm.PhotoFolder + CamFolder + NewFileName);
  finally
    StoringFile.Free;
  end;
  MainForm.AddNewFrame(CamFolder, NewFileName);
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


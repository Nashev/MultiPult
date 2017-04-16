unit CameraFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles, JPEG,
  VFrames, VSample, Direct3D9, DirectDraw, DirectShow9, DirectSound, DXTypes,
  Vcl.ComCtrls;

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
    procedure FormCreate(Sender: TObject);
    procedure btnMakePhotoClick(Sender: TObject);
    procedure btnNextCamClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbCamSelectorChange(Sender: TObject);
    procedure cbbResolutionChange(Sender: TObject);
    procedure btnPreferencesClick(Sender: TObject);
  private
    FVideoImage: TVideoImage;
    FVideoBitmap: TBitmap;
    procedure GetNewFrame(Sender: TObject; Width, Height: Integer; DataPtr: Pointer);
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
  FVideoImage.GetBitmap(FVideoBitmap);
  imgPreview.Picture.Bitmap.Assign(FVideoBitmap);
end;

procedure TCameraForm.btnMakePhotoClick(Sender: TObject);
var
  NewFileName: string;
  StoringFile: TJPEGImage;
resourcestring
  CamFolder = 'FromCam\';
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

end.


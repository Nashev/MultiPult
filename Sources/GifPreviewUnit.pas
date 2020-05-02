unit GifPreviewUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  MainFormUnit,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.GIFimg, Vcl.Samples.Spin, Vcl.Samples.Gauges, Vcl.AppEvnts, Vcl.ComCtrls;

type
  TGifPreviewForm = class(TForm)
    edPalette: TComboBox;
    edDithering: TComboBox;
    pbPalettePreview: TPaintBox;
    imgPreview: TImage;
    grpTransparency: TGroupBox;
    rbOpacy: TRadioButton;
    rbTransparent: TRadioButton;
    pbTransparentColor: TShape;
    edWidth: TSpinEdit;
    edHeight: TSpinEdit;
    lblSize: TLabel;
    lblSizeX: TLabel;
    lblSizePX: TLabel;
    gProgress: TGauge;
    gProgressFrame: TGauge;
    ApplicationEvents1: TApplicationEvents;
    btnSave: TButton;
    SaveToGIFDialog: TSaveDialog;
    dlgColor: TColorDialog;
    sb: TStatusBar;
    lblDittering: TLabel;
    lblPalette: TLabel;
    mmoWarnings: TMemo;
    lblBits: TLabel;
    edBits: TSpinEdit;
    lblLoopCount: TLabel;
    edLoopCount: TSpinEdit;
    lblWarnings: TLabel;
    procedure ParamChanged(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure pbTransparentColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPalettePreviewPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMovie: TRecordedFrameList;
    FGIF: TGIFImage;
    FBmp: TBitmap;
    FGenerating: Boolean;
    S: TMemoryStream;
    procedure UpdatePreview;
    procedure GifFrameProgresHandler(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure GifWarningHandler(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
  public
    procedure Execute(AMovie: TRecordedFrameList);
    procedure SaveSettings(AIniFile: TIniFile);
    procedure LoadSettings(AIniFile: TIniFile);
  end;

var
  GifPreviewForm: TGifPreviewForm;

implementation

uses
  ScreenFormUnit, UtilsUnit;

{$R *.dfm}

procedure TGifPreviewForm.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var
  CurrentFrameInfoIndex, SameFrameCount: Integer;
  GIFFrame: TGIFFrame;
  Image: TGraphic;
  R: TRect;
begin
  if not FGenerating or not Visible then
    Exit;

  if gProgress.Progress <= FMovie.Count - 1 then try
    gProgressFrame.Progress := 0;

    CurrentFrameInfoIndex := FMovie[gProgress.Progress].FrameInfoIndex;
    SameFrameCount := 1;
    while (gProgress.Progress < (FMovie.Count - 1)) and (FMovie[gProgress.Progress + 1].FrameInfoIndex = CurrentFrameInfoIndex) do
      begin
        inc(SameFrameCount);
        gProgress.Progress := gProgress.Progress + 1;
      end;

    Image := MainForm.FrameInfoList[CurrentFrameInfoIndex].ImageFromDisc;
    R := StretchSize(Image.Width, Image.Height, FBmp.Width, FBmp.Height);
    FBmp.Canvas.FillRect(FBmp.Canvas.ClipRect);
    FBmp.Canvas.StretchDraw(R, Image);
    Image.Free;
    FBmp.PixelFormat := pf24bit;
    gProgressFrame.Progress := 50;

    GIFFrame := FGIF.Add(FBmp);
    // бесконечный цикл в IE получается если записать 0,
    // 1 цикл - если вовсе не записывать,
    // и два раза проигрывание (т.е. один повтор) выходит если записать 1
    if FGIF.Images.Count = 1 then
      case edLoopCount.Value of
        0: TGIFAppExtNSLoop.Create(GIFFrame).Loops := 0;
        1: ;
      else
        TGIFAppExtNSLoop.Create(GIFFrame).Loops := edLoopCount.Value - 1;
      end;

    TGIFGraphicControlExtension.Create(GIFFrame);
    GIFFrame.GraphicControlExtension.Delay := 1000 div FrameRate div 10 * SameFrameCount;
    gProgress.Progress := gProgress.Progress + 1;
    if gProgress.Progress <= FMovie.Count - 1 then begin
      imgPreview.Picture.Bitmap.Assign(GIFFrame);
      imgPreview.Refresh;
      Done := False;
    end else begin
      imgPreview.Picture.Graphic := FGIF;
      Done := True;
      FGenerating := False;
      FGIF.Optimize([ooCrop, ooMerge, ooColorMap], FGIF.ColorReduction, FGIF.DitherMode, FGIF.ReductionBits);
      FGIF.OptimizeColorMap;
      btnSave.Enabled := True;
    end;
    if gProgress.Progress = 0 then begin
      TGIFAppExtNSLoop.Create(GIFFrame).Loops := 0;
      if rbTransparent.Checked then
        FGIF.BackgroundColor := pbTransparentColor.Brush.Color;
      FGIF.Transparent := rbTransparent.Checked;
    end;
    pbPalettePreview.Refresh;
    S.Clear;
    FGIF.SaveToStream(S);
    sb.SimpleText := FormatFileSize(S.Size);
  except
    FGenerating := False;
    raise;
  end;
end;

procedure TGifPreviewForm.btnSaveClick(Sender: TObject);
begin
  if SaveToGIFDialog.Execute then
    FGIF.SaveToFile(SaveToGIFDialog.FileName)
end;

procedure TGifPreviewForm.Execute(AMovie: TRecordedFrameList);
begin
  S := TMemoryStream.Create;
  FMovie := AMovie;
  edWidth.Value := MainForm.ExportSize.cx;
  edHeight.Value := MainForm.ExportSize.cy;
  Show;
  UpdatePreview;
end;

procedure TGifPreviewForm.FormDestroy(Sender: TObject);
begin
  S.Free;
end;

procedure TGifPreviewForm.ParamChanged(Sender: TObject);
begin
  SettingsChanged := True;
  UpdatePreview;
end;

procedure TGifPreviewForm.pbPalettePreviewPaint(Sender: TObject);
var
  ColorMap: TGIFColorMap;
  Index: Integer;
  x, y: Integer;
const
  s = 10;
begin
  if not Assigned(FGIF) then
    Exit;

  if FGenerating and (FGIF.Images.Count > 0) then
    ColorMap := FGIF.Images.Frames[FGIF.Images.Count - 1].ColorMap
  else
    ColorMap := FGIF.GlobalColorMap;

  for y := 0 to 7 do
    for x := 0 to 31 do begin
      Index := x + y * 32;
      if Index >= ColorMap.Count then
        Exit;

      pbPalettePreview.Canvas.Brush.Color := ColorMap.Colors[Index];
      pbPalettePreview.Canvas.Rectangle(
        x*s + x div 8, y*s,
        x*s + s + x div 8, y*s+s
      );
    end;
end;

procedure TGifPreviewForm.pbTransparentColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color := pbTransparentColor.Brush.Color;
  if dlgColor.Execute then begin
    pbTransparentColor.Brush.Color := dlgColor.Color;
    rbTransparent.Checked := True;
  end;
end;

procedure TGifPreviewForm.LoadSettings(AIniFile: TIniFile);
begin
  edPalette.ItemIndex := AIniFile.ReadInteger('GIF', 'Palette', edPalette.ItemIndex);
  edBits.Value := AIniFile.ReadInteger('GIF', 'BitPerPixel', edBits.Value);
  edDithering.ItemIndex := AIniFile.ReadInteger('GIF', 'Dithering', edDithering.ItemIndex);
  rbTransparent.Checked := AIniFile.ReadBool('GIF', 'Transparent', rbTransparent.Checked);
  rbOpacy.Checked := not rbTransparent.Checked;
  pbTransparentColor.Brush.Color := StringToColor(AIniFile.ReadString('GIF', 'TransparentColor', ColorToString(pbTransparentColor.Brush.Color)));
  edLoopCount.Value := AIniFile.ReadInteger('GIF', 'LoopCount', edLoopCount.Value);
end;

procedure TGifPreviewForm.SaveSettings(AIniFile: TIniFile);
begin
  AIniFile.WriteInteger('GIF', 'Palette', edPalette.ItemIndex);
  AIniFile.WriteInteger('GIF', 'BitPerPixel', edBits.Value);
  AIniFile.WriteInteger('GIF', 'Dithering', edDithering.ItemIndex);
  AIniFile.WriteBool('GIF', 'Transparent', rbTransparent.Checked);
  AIniFile.WriteString('GIF', 'TransparentColor', ColorToString(pbTransparentColor.Brush.Color));
  AIniFile.WriteInteger('GIF', 'LoopCount', edLoopCount.Value);
end;

procedure TGifPreviewForm.GifFrameProgresHandler(Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  gProgressFrame.Progress := 100 + PercentDone;
end;

procedure TGifPreviewForm.GifWarningHandler(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
const
  SeverityName: array [TGIFSeverity] of string = ('Info', 'Warning', 'Error');
begin
  mmoWarnings.Lines.Add(Format('Frame %d, %s: %s', [gProgress.Progress, SeverityName[Severity], Msg]));
end;

procedure TGifPreviewForm.UpdatePreview;
begin
  imgPreview.Picture.Graphic := nil;
  if (edDithering.ItemIndex = -1) or
     (edPalette.ItemIndex = -1) or
     (not rbOpacy.Checked and not rbTransparent.Checked) or
     (edWidth.Value = 0) or
     (edHeight.Value = 0)
  then
    Exit;
  btnSave.Enabled := False;
  FreeAndNil(FGIF);
  mmoWarnings.Lines.Clear;
  FGIF := TGifImage.Create;
  FGIF.DitherMode := TDitherMode(edDithering.ItemIndex);
  FGIF.ColorReduction := TColorReduction(edPalette.ItemIndex + 1); // skip rmNnone
  FGIF.SetSize(edWidth.Value, edHeight.Value);
  FGIF.OnProgress := GifFrameProgresHandler;
  FGIF.OnWarning := GifWarningHandler;
  FGIF.ReductionBits := edBits.Value;
  FGIF.AnimateLoop := glEnabled;
  FGIF.Animate := True;

  FBmp := TBitmap.Create;
  FBmp.Canvas.Brush.Color := clBlack;
  FBmp.SetSize(edWidth.Value, edHeight.Value);

  gProgress.Progress := 0;
  gProgress.MaxValue := FMovie.Count;
  FGenerating := FMovie.Count > 0;
end;

end.

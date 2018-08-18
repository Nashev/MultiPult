unit GifPreviewUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  MainFormUnit, Vcl.Imaging.GIFimg, Vcl.Samples.Spin, Vcl.Samples.Gauges,
  Vcl.AppEvnts, Vcl.ComCtrls;

type
  TGifPreviewForm = class(TForm)
    rgPalette: TRadioGroup;
    rgDithering: TRadioGroup;
    pbPalettePreview: TPaintBox;
    imgPreview: TImage;
    grpTransparency: TGroupBox;
    rbOpacy: TRadioButton;
    rbTransparent: TRadioButton;
    pbTransparentColor: TShape;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
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
  public
    procedure Execute(AMovie: TRecordedFrameList);
  end;

var
  GifPreviewForm: TGifPreviewForm;

implementation

uses
  ScreenFormUnit, UtilsUnit;

{$R *.dfm}

procedure TGifPreviewForm.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var
  GIFFrame: TGIFFrame;
  Image: TGraphic;
  R: TRect;
begin
  if not FGenerating or not Visible then
    Exit;

  if gProgress.Progress <= FMovie.Count - 1 then try
    gProgressFrame.Progress := 0;

    Image := MainForm.FrameInfoList[FMovie[gProgress.Progress].FrameInfoIndex].ImageFromDisc;
    R := StretchSize(Image.Width, Image.Height, FBmp.Width, FBmp.Height);
    FBmp.Canvas.FillRect(FBmp.Canvas.ClipRect);
    FBmp.Canvas.StretchDraw(R, Image);
    Image.Free;
    FBmp.PixelFormat := pf24bit;
    gProgressFrame.Progress := 50;

    GIFFrame := FGIF.Add(FBmp);
    TGIFGraphicControlExtension.Create(GIFFrame);
    GIFFrame.GraphicControlExtension.Delay := MainForm.MultimediaTimer.Interval div 10;
    gProgress.Progress := gProgress.Progress + 1;
    if gProgress.Progress <= FMovie.Count - 1 then begin
      imgPreview.Picture.Bitmap.Assign(GIFFrame);
      imgPreview.Refresh;
      Done := False;
    end else begin
      imgPreview.Picture.Graphic := FGIF;
      Done := True;
      FGenerating := False;
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
  seWidth.Value := MainForm.ExportSize.cx;
  seHeight.Value := MainForm.ExportSize.cy;
  Show;
  UpdatePreview;
end;

procedure TGifPreviewForm.FormDestroy(Sender: TObject);
begin
  S.Free;
end;

procedure TGifPreviewForm.ParamChanged(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TGifPreviewForm.pbPalettePreviewPaint(Sender: TObject);
var
  FPaletteEntries: PPalArray;
  Index, ColorsCount: Integer;
  x, y: Integer;
const
  s = 10;
begin
  if not Assigned(FGIF) then
    Exit;

  GetMem(FPaletteEntries, SizeOf(TPaletteEntry) * 256);
  try
    ColorsCount := GetPaletteEntries(FGIF.Palette, 0, 256, FPaletteEntries^);

    for y := 0 to 8 do
      for x := 0 to 31 do begin
        Index := x + y * 32;
        if Index > ColorsCount then
          Exit;

        pbPalettePreview.Canvas.Brush.Color := RGB(FPaletteEntries^[Index].peRed, FPaletteEntries^[Index].peGreen, FPaletteEntries^[Index].peBlue);
        pbPalettePreview.Canvas.Rectangle(x*s, y*s, x*s+s, y*s+s);
      end;
  finally
    FreeMem(FPaletteEntries);
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

procedure TGifPreviewForm.GifFrameProgresHandler(Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  gProgressFrame.Progress := 100 + PercentDone;
end;

procedure TGifPreviewForm.UpdatePreview;
begin
  imgPreview.Picture.Graphic := nil;
  if (rgDithering.ItemIndex = -1) or
     (rgPalette.ItemIndex = -1) or
     (not rbOpacy.Checked and not rbTransparent.Checked) or
     (seWidth.Value = 0) or
     (seHeight.Value = 0)
  then
    Exit;
  btnSave.Enabled := False;
  FreeAndNil(FGIF);
  FGIF := TGifImage.Create;
  FGIF.DitherMode := TDitherMode(rgDithering.ItemIndex);
  FGIF.ColorReduction := TColorReduction(rgPalette.ItemIndex + 1); // skip rmNnone
  FGIF.SetSize(seWidth.Value, seHeight.Value);
  FGIF.OnProgress := GifFrameProgresHandler;
  FGIF.AnimateLoop := glEnabled;
  FGIF.Animate := True;

  FBmp := TBitmap.Create;
  FBmp.Canvas.Brush.Color := clBlack;
  FBmp.SetSize(seWidth.Value, seHeight.Value);

  gProgress.Progress := 0;
  gProgress.MaxValue := FMovie.Count;
  FGenerating := FMovie.Count > 0;
end;

end.

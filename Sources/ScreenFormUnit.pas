unit ScreenFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TScreenForm = class(TForm)
    imgOverlay: TImage;
    imgCamPreview: TImage;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure imgOverlayClick(Sender: TObject);
    procedure imgOverlayDblClick(Sender: TObject);
    procedure DoCloseScreenForm(Sender: TObject; var Action: TCloseAction);
    procedure DoCloseQueryScreenForm(Sender: TObject; var CanClose: Boolean);
  private
    PreviousBounds: TRect;
    FFullScreen: Boolean;
    FStretchImages: Boolean;
    FImage: TGraphic;
    procedure SetFullScreen(const Value: Boolean);
    procedure UpdateFullScreen;
    procedure SetStretchImages(const Value: Boolean);
    procedure SetImage(const Value: TGraphic);
  protected
    procedure NCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure NCLButtonDblClk(var Message: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure NCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLButtonDown;
    procedure EraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
    property FullScreen: Boolean read FFullScreen write SetFullScreen;
    property StretchImages: Boolean read FStretchImages write SetStretchImages;
    property Image: TGraphic read FImage write SetImage;
    procedure AdjustOpacity(AOpacity: byte);
  end;

var
  ScreenForm: TScreenForm;

function StretchSize(AWidth, AHeight, ABoundsWidth, ABoundsHeight: Integer): TRect;

implementation

{$R *.dfm}

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

{ TScreenForm }

procedure TScreenForm.AdjustOpacity(AOpacity: byte);
begin
  AlphaBlendValue := AOpacity;
end;

procedure TScreenForm.EraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := Integer(True);
end;

procedure TScreenForm.NCLButtonDblClk(var Message: TWMNCLButtonDblClk);
begin
  DblClick;
end;

procedure TScreenForm.NCLButtonDown(var Message: TWMNCLButtonDown);
begin
  Click;
  inherited;
end;

procedure TScreenForm.SetFullScreen(const Value: Boolean);
begin
  FFullScreen := Value;
  UpdateFullScreen;
end;

procedure TScreenForm.SetImage(const Value: TGraphic);
begin
  FImage := Value;
end;

procedure TScreenForm.SetStretchImages(const Value: Boolean);
begin
  FStretchImages := Value;
end;

procedure TScreenForm.UpdateFullScreen;
begin
  if FFullScreen then
    begin
      PreviousBounds := BoundsRect;
      WindowState := wsMaximized;
//      BoundsRect := Self.Monitor.BoundsRect;
      // FormStyle := fsStayOnTop;
    end
  else
    begin
      WindowState := wsNormal;
//      BoundsRect := PreviousBounds;
      // FormStyle := fsNormal;
    end;
end;

procedure TScreenForm.DoCloseQueryScreenForm(Sender: TObject;
  var CanClose: Boolean);
resourcestring
  rs_DoYouWantCloseGrabber = 'Закрыть программу?';
begin
  CanClose := MessageBox(
    0,
    PChar(rs_DoYouWantCloseGrabber),
    PChar(Application.Title),
    MB_ICONQUESTION or MB_OKCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
   = IDOK
end;

procedure TScreenForm.DoCloseScreenForm(Sender: TObject; var Action: TCloseAction);
begin
  Close;
  Action := caFree;
end;

procedure TScreenForm.FormDblClick(Sender: TObject);
begin
  FullScreen := not FullScreen;
end;

procedure TScreenForm.FormPaint(Sender: TObject);
const
  ScreenBorder = 1;
var
  R: TRect;
  ScreenSize: TPoint;
begin
  R := Rect(0,0,0,0);
  if FullScreen then
    ScreenSize := Point(Self.ClientWidth, Self.ClientHeight)
  else
    ScreenSize := Point(Self.ClientWidth - ScreenBorder * 2, Self.ClientHeight - ScreenBorder * 2);

  if Assigned(Image) then
    try
      if StretchImages or (Image.Width > ScreenSize.X) or (Image.Height > ScreenSize.Y) then
        begin
          R := StretchSize(Image.Width, Image.Height, ScreenSize.X, ScreenSize.Y);
          R.Left   := R.Left   + ScreenBorder;
          R.Top    := R.Top    + ScreenBorder;
          R.Right  := R.Right  + ScreenBorder;
          R.Bottom := R.Bottom + ScreenBorder;
          Self.Canvas.StretchDraw(R, Image);
        end
      else
        begin
          R.Left := (ScreenSize.X - Image.Width ) div 2 + ScreenBorder;
          R.Top  := (ScreenSize.Y - Image.Height) div 2 + ScreenBorder;
          R.Right := R.Left + Image.Width;
          R.Bottom := R.Top + Image.Height;
          Self.Canvas.Draw(R.Left, R.Top, Image);
        end;

      Self.Canvas.Brush.Color := clBlack;
      Self.Canvas.FillRect(Rect(0,       0,        R.Left,           Self.ClientHeight));
      Self.Canvas.FillRect(Rect(R.Right, 0,        Self.ClientWidth, Self.ClientHeight));
      Self.Canvas.FillRect(Rect(0,       0,        Self.ClientWidth, R.Top            ));
      Self.Canvas.FillRect(Rect(0,       R.Bottom, Self.ClientWidth, Self.ClientHeight));
    except
      ; // на всякий случай глушим ошибки рисования, потому что они непонятно откуда лезут
    end
  else
    begin
      Self.Canvas.Brush.Color := clBlack;
      Self.Canvas.FillRect(Rect(0,       0,        Self.ClientWidth, Self.ClientHeight));
    end;
end;

procedure TScreenForm.FormResize(Sender: TObject);
begin
  Invalidate;
end;

procedure TScreenForm.imgOverlayClick(Sender: TObject);
begin
  Click;
end;

procedure TScreenForm.imgOverlayDblClick(Sender: TObject);
begin
  DblClick;
end;

function TScreenForm.IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean;
begin
  if Application.MainForm <> Self then
    Result := Application.MainForm.IsShortCut(Message)
  else
    Result := inherited IsShortCut(Message);
end;

procedure TScreenForm.NCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if not FullScreen and (Message.Result = HTCLIENT) then
    begin
      Message.Result := HTCAPTION;

      P := ScreenToClient(Point(Message.XPos, Message.YPos));
      if P.X < 5 then
        Message.Result := HTLEFT;

      if P.X > (ClientWidth - 5) then
        Message.Result := HTRIGHT;

      if P.Y < 5 then
        begin
          if Message.Result = HTLEFT then
            Message.Result := HTTOPLEFT
          else if Message.Result = HTRIGHT then
            Message.Result := HTTOPRIGHT
          else
            Message.Result := HTTOP;
        end;

      if P.Y > (ClientHeight - 5) then
        begin
          if Message.Result = HTLEFT then
            Message.Result := HTBOTTOMLEFT
          else if Message.Result = HTRIGHT then
            Message.Result := HTBOTTOMRIGHT
          else
            Message.Result := HTBOTTOM;
        end;
    end
  else
    Message.Result := HTCLIENT;
end;

end.

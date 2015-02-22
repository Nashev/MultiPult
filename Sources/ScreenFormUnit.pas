unit ScreenFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TScreenForm = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    PreviousBounds: TRect;
  protected
    procedure NCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure NCLButtonDblClk(var Message: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure EraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
    procedure UpdateFullScreen;
  end;

var
  ScreenForm: TScreenForm;

implementation

uses MainFormUnit;

{$R *.dfm}

{ TScreenForm }

procedure TScreenForm.EraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := Integer(True);
end;

procedure TScreenForm.FormDblClick(Sender: TObject);
begin
  MainForm.actFullScreenMode.Execute;
end;

procedure TScreenForm.NCLButtonDblClk(var Message: TWMNCLButtonDblClk);
begin
  DblClick;
end;

procedure TScreenForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainForm.FormKeyDown(Sender, Key, Shift);
end;

procedure TScreenForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainForm.FormKeyUp(Sender, Key, Shift);
end;

procedure TScreenForm.UpdateFullScreen;
begin
  if MainForm.actFullScreenMode.Checked then
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

procedure TScreenForm.FormPaint(Sender: TObject);
const
  ScreenBorder = 1;
var
  R: TRect;
  Image: TBitmap;
  ScreenSize: TPoint;
begin
  with MainForm do
    begin
      LoadPhoto(DisplayedFrameIndex); // на всякий случай
      R := Rect(0,0,0,0);
      if MainForm.actFullScreenMode.Checked then
        ScreenSize := Point(Self.ClientWidth, Self.ClientHeight)
      else
        ScreenSize := Point(Self.ClientWidth - ScreenBorder * 2, Self.ClientHeight - ScreenBorder * 2);

      if FrameInfoCount > 0 then
        try
          if AdvertisementShowing then
            Image := AdvertisementFrameImagePreview
          else
            if FrameInfoList[DisplayedFrameIndex].Loaded then
              Image := FrameInfoList[DisplayedFrameIndex].Preview
            else
              Image := nil;
          if Image <> nil then
            begin
              if actStretchImages.Checked or (Image.Width > ScreenSize.X) or (Image.Height > ScreenSize.Y) then
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
    end
end;

procedure TScreenForm.FormResize(Sender: TObject);
begin
  Invalidate;
end;

function TScreenForm.IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean;
begin
  Result := MainForm.IsShortCut(Message);
end;

procedure TScreenForm.NCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if not MainForm.actFullScreenMode.Checked and (Message.Result = HTCLIENT) then
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

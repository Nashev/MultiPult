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
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
  public
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
var
  R: TRect;
  Image: TBitmap;
begin
  with MainForm do
  try
    LoadPhoto(CurrentFrameIndex); // на всякий случай
    if Frames[CurrentFrameIndex].Loaded then
      begin
        Image := Frames[CurrentFrameIndex].Preview;
        if actStretchImages.Checked or (Image.Width > (Self.Width)) or (Image.Height > (Self.Height)) then
          begin
            R := StretchSize(Image.Width, Image.Height, Self.Width, Self.Height);
            Self.Canvas.StretchDraw(R, Image);
          end
        else
          begin
            R.Left := (Self.Width  - Image.Width ) div 2;
            R.Top  := (Self.Height - Image.Height) div 2;
            R.Right := R.Left + Image.Width;
            R.Bottom := R.Top + Image.Height;
            Self.Canvas.Draw(R.Left, R.Top, Image);
          end;
      end;
    Self.Canvas.FillRect(Rect(0,       0,        R.Left,           Self.ClientHeight));
    Self.Canvas.FillRect(Rect(R.Right, 0,        Self.ClientWidth, Self.ClientHeight));
    Self.Canvas.FillRect(Rect(0,       0,        Self.ClientWidth, R.Top            ));
    Self.Canvas.FillRect(Rect(0,       R.Bottom, Self.ClientWidth, Self.ClientHeight));
  except
    ; // на всякий случай глушим ошибки рисования, потому что они непонятно откуда лезут
  end;
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

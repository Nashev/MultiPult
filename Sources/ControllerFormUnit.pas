unit ControllerFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TScrollHandleMode = (shmInactive, shmManual, shmAuto);

  TControllerForm = class(TForm)
    pbScrollHandle: TPaintBox;
    lblFramerate: TLabel;
    tmrAutoMode: TTimer;
    procedure pbScrollHandlePaint(Sender: TObject);
    procedure pbScrollHandleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbScrollHandleMouseEnter(Sender: TObject);
    procedure pbScrollHandleMouseLeave(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tmrAutoModeTimer(Sender: TObject);
  private
    ScrollHandleCenterPoint, ScrollHandleTargetPoint: TPoint;
    AutoAreaRadius: Integer;
    ThumbmailAreaRadius: Integer;
    ThumbnailSize: TPoint;
    ScrollHandleTimePerFrame: Integer;
    ScrollHandleAutoDirection: Integer;
    ScrollHandleFramerate: Integer;
    FScrollHandleMode: TScrollHandleMode;
    ManualModeStarting: Boolean;
    FScrollHandlePosition: Integer;
    PreviousTickCount: DWORD;
    procedure SetScrollHandleMode(const Value: TScrollHandleMode);
    procedure SetScrollHandlePosition(const Value: Integer);
    property ScrollHandlePosition: Integer read FScrollHandlePosition write SetScrollHandlePosition;
    property ScrollHandleMode: TScrollHandleMode read FScrollHandleMode write SetScrollHandleMode;
  public
    { Public declarations }
  end;

var
  ControllerForm: TControllerForm;
const
  ScrollHandlePositionCount = 25;

implementation
uses Math, MainFormUnit;

{$R *.dfm}

procedure TControllerForm.FormActivate(Sender: TObject);
begin
  MainForm.SetFocus;
end;

procedure TControllerForm.FormResize(Sender: TObject);
begin
  ScrollHandleCenterPoint := Point(pbScrollHandle.ClientWidth div 2, pbScrollHandle.ClientHeight div 2);
  AutoAreaRadius := ClientWidth div 8;
  ThumbnailSize.X := ClientWidth div 8;
  ThumbnailSize.Y := MulDiv(ThumbnailSize.X, 480, 640);
  ThumbmailAreaRadius := (ClientWidth - ThumbnailSize.X) div 2;
end;

procedure TControllerForm.pbScrollHandleMouseEnter(Sender: TObject);
begin
  if MainForm.WorkingSetFrames.Count = 0 then
    Exit;
  ScrollHandleMode := shmManual;
end;

procedure TControllerForm.pbScrollHandleMouseLeave(Sender: TObject);
begin
  ScrollHandleMode := shmInactive;
end;

procedure TControllerForm.pbScrollHandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Distance: Integer;
  Angle: Double;
begin
  if MainForm.WorkingSetFrames.Count = 0 then
    Exit;

  ScrollHandleTargetPoint := pbScrollHandle.ScreenToClient(Mouse.CursorPos);
  Angle := ArcTan2(ScrollHandleTargetPoint.X - ScrollHandleCenterPoint.X, - (ScrollHandleTargetPoint.Y - ScrollHandleCenterPoint.Y));
  if abs(ScrollHandleTargetPoint.X - ScrollHandleCenterPoint.X) > abs(ScrollHandleTargetPoint.Y - ScrollHandleCenterPoint.Y) then
    Distance := Round((ScrollHandleTargetPoint.X - ScrollHandleCenterPoint.X) / Sin(Angle))
  else
    Distance := Round(-(ScrollHandleTargetPoint.Y - ScrollHandleCenterPoint.Y) / Cos(Angle));

  if Distance > AutoAreaRadius then
    begin
      ScrollHandleMode := shmManual; // Set Manual mode before set ScrollHandlePosition
      ScrollHandlePosition := Round(ScrollHandlePositionCount * Angle / (2*Pi) );
    end
  else
    begin
      // last set ScrollHandlePosition before go to Auto mode, while we in Manual mode yet
      if ScrollHandleMode = shmManual then
        ScrollHandlePosition := Round(ScrollHandlePositionCount * Angle / (2*Pi));
      // TODO: continuosly update framerate when mouse stay over manual area to use this speed when go into auto area
      ScrollHandleMode := shmAuto;
    end;
end;

procedure TControllerForm.pbScrollHandlePaint(Sender: TObject);
var
  AutoAreaBorderPoint: TPoint;
  ThumbmailRect: TRect;
  EndPoint: TPoint;
  Angle: Double;
  I: Integer;
  NextWorkingFrame: TRecordedFrame;
begin
  case ScrollHandleMode of
    shmInactive: pbScrollHandle.Canvas.Brush.Color := clDkGray;
    shmManual: pbScrollHandle.Canvas.Brush.Color := clBlack;
    shmAuto: pbScrollHandle.Canvas.Brush.Color := clGreen;
  end;
  pbScrollHandle.Canvas.Brush.Style := bsSolid;

  pbScrollHandle.Canvas.Pen.Color := pbScrollHandle.Canvas.Brush.Color;
  pbScrollHandle.Canvas.Pen.Style := psSolid;

  if not Assigned(MainForm.CurrentWorkingSetFrame) then
    Exit;

  if ScrollHandleMode = shmManual then
    begin
      Angle := Pi * 2 * FScrollHandlePosition / ScrollHandlePositionCount;
      AutoAreaBorderPoint.X := ScrollHandleCenterPoint.X + Round(Sin(Angle) * AutoAreaRadius);
      AutoAreaBorderPoint.Y := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * AutoAreaRadius);

      EndPoint.X := ScrollHandleCenterPoint.X + Round(Sin(Angle) * ((ClientWidth div 2) - 4));
      EndPoint.Y := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * ((ClientWidth div 2) - 4));

      with ScrollHandleCenterPoint do pbScrollHandle.Canvas.MoveTo(X, Y);
      pbScrollHandle.Canvas.Pen.Width := 3;
      with AutoAreaBorderPoint do pbScrollHandle.Canvas.LineTo(X, Y);
      pbScrollHandle.Canvas.Pen.Width := 1;
      with EndPoint do pbScrollHandle.Canvas.LineTo(X, Y);

      pbScrollHandle.Canvas.Brush.Style := bsClear;
      with ScrollHandleCenterPoint do pbScrollHandle.Canvas.Ellipse(X - AutoAreaRadius, Y - AutoAreaRadius, X + AutoAreaRadius + 1, Y + AutoAreaRadius + 1);

      for I := ScrollHandlePositionCount div 3 downto 1 do
        begin
          NextWorkingFrame := MainForm.FindWorkingSetFrameByOffset(-I);
          // Stop Frames: if NextFrameIndex(i) = NextFrameIndex(i+1) then not draw previous preview
          if NextWorkingFrame <> MainForm.FindWorkingSetFrameByOffset(- I + 1) then
            begin
              Angle := Pi * 2 * (FScrollHandlePosition - I) / ScrollHandlePositionCount;
              ThumbmailRect.Left := ScrollHandleCenterPoint.X + Round(Sin(Angle) * ThumbmailAreaRadius) - ThumbnailSize.X div 2;
              ThumbmailRect.Top := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * ThumbmailAreaRadius) - ThumbnailSize.Y div 2;
              ThumbmailRect.Right := ThumbmailRect.Left + ThumbnailSize.X;
              ThumbmailRect.Bottom := ThumbmailRect.Top + ThumbnailSize.Y;

              pbScrollHandle.Canvas.StretchDraw(ThumbmailRect, MainForm.FrameInfoList[NextWorkingFrame.FrameInfoIndex].Preview);
            end;

          NextWorkingFrame := MainForm.FindWorkingSetFrameByOffset(I);
          // Stop Frames: if NextFrameIndex(i) = NextFrameIndex(i-1) then not draw next preview
          if NextWorkingFrame <> MainForm.FindWorkingSetFrameByOffset(I - 1) then
            begin
              Angle := Pi * 2 * (FScrollHandlePosition + I) / ScrollHandlePositionCount;
              ThumbmailRect.Left := ScrollHandleCenterPoint.X + Round(Sin(Angle) * ThumbmailAreaRadius) - ThumbnailSize.X div 2;
              ThumbmailRect.Top := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * ThumbmailAreaRadius) - ThumbnailSize.Y div 2;
              ThumbmailRect.Right := ThumbmailRect.Left + ThumbnailSize.X;
              ThumbmailRect.Bottom := ThumbmailRect.Top + ThumbnailSize.Y;

              pbScrollHandle.Canvas.StretchDraw(ThumbmailRect, MainForm.FrameInfoList[NextWorkingFrame.FrameInfoIndex].Preview);
            end;
        end;

      Angle := Pi * 2 * (FScrollHandlePosition) / ScrollHandlePositionCount;
      ThumbmailRect.Left := ScrollHandleCenterPoint.X + Round(Sin(Angle) * ThumbmailAreaRadius) - ThumbnailSize.X div 2;
      ThumbmailRect.Top := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * ThumbmailAreaRadius) - ThumbnailSize.Y div 2;
      ThumbmailRect.Right := ThumbmailRect.Left + ThumbnailSize.X;
      ThumbmailRect.Bottom := ThumbmailRect.Top + ThumbnailSize.Y;

      pbScrollHandle.Canvas.StretchDraw(ThumbmailRect, MainForm.FrameInfoList[MainForm.CurrentWorkingSetFrame.FrameInfoIndex].Preview);
    end;

  if ScrollHandleMode = shmAuto then
  begin
    pbScrollHandle.Canvas.Brush.Style := bsDiagCross;
    pbScrollHandle.Canvas.Pen.Width := 1;
    with ScrollHandleCenterPoint do pbScrollHandle.Canvas.Ellipse(X - AutoAreaRadius, Y - AutoAreaRadius, X + AutoAreaRadius + 1, Y + AutoAreaRadius + 1);

    pbScrollHandle.Canvas.Pen.Width := 2;
    Angle := Pi * 2 * FScrollHandlePosition / ScrollHandlePositionCount;

    AutoAreaBorderPoint.X := ScrollHandleCenterPoint.X + Round(Sin(Angle) * AutoAreaRadius);
    AutoAreaBorderPoint.Y := ScrollHandleCenterPoint.Y - Round(Cos(Angle) * AutoAreaRadius);

    with ScrollHandleCenterPoint do pbScrollHandle.Canvas.MoveTo(X, Y);
    with AutoAreaBorderPoint do pbScrollHandle.Canvas.LineTo(X, Y);

    AutoAreaBorderPoint.X := ScrollHandleCenterPoint.X + Round(Sin(Angle + 2 * Pi / 3) * AutoAreaRadius);
    AutoAreaBorderPoint.Y := ScrollHandleCenterPoint.Y - Round(Cos(Angle + 2 * Pi / 3) * AutoAreaRadius);

    with ScrollHandleCenterPoint do pbScrollHandle.Canvas.MoveTo(X, Y);
    with AutoAreaBorderPoint do pbScrollHandle.Canvas.LineTo(X, Y);

    AutoAreaBorderPoint.X := ScrollHandleCenterPoint.X + Round(Sin(Angle - 2 * Pi / 3) * AutoAreaRadius);
    AutoAreaBorderPoint.Y := ScrollHandleCenterPoint.Y - Round(Cos(Angle - 2 * Pi / 3) * AutoAreaRadius);

    with ScrollHandleCenterPoint do pbScrollHandle.Canvas.MoveTo(X, Y);
    with AutoAreaBorderPoint do pbScrollHandle.Canvas.LineTo(X, Y);
  end;

  pbScrollHandle.Canvas.Brush.Style := bsSolid;
  with ScrollHandleCenterPoint do pbScrollHandle.Canvas.Ellipse(X - 3, Y - 3, X + 4, Y + 4);
end;

procedure TControllerForm.SetScrollHandleMode(const Value: TScrollHandleMode);
begin
  if FScrollHandleMode <> Value then
    begin
      ManualModeStarting := (Value = shmManual);
      tmrAutoMode.Interval := ScrollHandleTimePerFrame;
      tmrAutoMode.Enabled := (Value = shmAuto) and (ScrollHandleTimePerFrame <> 0);
      FScrollHandleMode := Value;
      if tmrAutoMode.Enabled then
        tmrAutoMode.OnTimer(tmrAutoMode);
      pbScrollHandle.Invalidate;
    end;
end;

procedure TControllerForm.SetScrollHandlePosition(const Value: Integer);
var
  NewTickCount: DWORD;
  ScrollHandleFramesOffset: Integer;
begin
  Assert(ScrollHandleMode = shmManual, 'Set HandlePosition in non manual mode are useless!');
  NewTickCount := GetTickCount;

  if ManualModeStarting then
    begin
      ManualModeStarting := False;
      // For auto mode:
      ScrollHandleAutoDirection := 0;
      ScrollHandleTimePerFrame := 0;
      // for display:
      ScrollHandleFramerate := 0;
      lblFramerate.Caption := '-- /' + IntToStr(FrameRate);
    end
  else
    begin
      ScrollHandleFramesOffset := Value - FScrollHandlePosition;
      if ScrollHandleFramesOffset = 0 then
        Exit;

      if ScrollHandleFramesOffset < -(ScrollHandlePositionCount div 2) then
        ScrollHandleFramesOffset := ScrollHandleFramesOffset + ScrollHandlePositionCount
      else if ScrollHandleFramesOffset > (ScrollHandlePositionCount div 2) then
        ScrollHandleFramesOffset := ScrollHandleFramesOffset - ScrollHandlePositionCount;

      MainForm.CurrentWorkingSetFrame := MainForm.FindWorkingSetFrameByOffset(ScrollHandleFramesOffset);

      // For auto mode:
      if ScrollHandleFramesOffset > 0 then
        ScrollHandleAutoDirection := 1
      else // if ScrollHandleFramesOffset < 0 then
        ScrollHandleAutoDirection := -1;

      // For auto mode:
      ScrollHandleTimePerFrame := (NewTickCount - PreviousTickCount) div Abs(ScrollHandleFramesOffset);
      // for display:
      ScrollHandleFramerate := Round(FrameRate * Round(ScrollHandleTimePerFrame) / 1000);
      lblFramerate.Caption := IntToStr(ScrollHandleFramerate) + '/' + IntToStr(FrameRate);
    end;

  PreviousTickCount := NewTickCount;
  FScrollHandlePosition := Value;
  pbScrollHandle.Invalidate;
end;

procedure TControllerForm.tmrAutoModeTimer(Sender: TObject);
begin
  Assert(ScrollHandleMode = shmAuto, 'Timer working in non auto mode are useless!');
  MainForm.CurrentWorkingSetFrame := MainForm.FindWorkingSetFrameByOffset(ScrollHandleAutoDirection);

  if MainForm.FrameInfoList[MainForm.CurrentWorkingSetFrame.FrameInfoIndex].Stopper then
    ScrollHandleMode := shmManual;

  FScrollHandlePosition := FScrollHandlePosition + ScrollHandleAutoDirection;
  if FScrollHandlePosition < 0 then
    FScrollHandlePosition := ScrollHandlePositionCount - 1
  else if FScrollHandlePosition >= ScrollHandlePositionCount then
    FScrollHandlePosition := 0;

  pbScrollHandle.Invalidate;
end;

end.

unit ScreenFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.AppEvnts, Vcl.StdCtrls,
  Vcl.Buttons;

type
  TScreenForm = class(TForm)
    imgOverlay: TImage;
    imgCamPreview: TImage;
    ApplicationEvents: TApplicationEvents;
    tmrHideControl: TTimer;
    btnControlPanel: TBitBtn;
    btnExit: TBitBtn;
    btnAbout: TBitBtn;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure imgClick(Sender: TObject);
    procedure imgDblClick(Sender: TObject);
    procedure DoCloseScreenForm(Sender: TObject; var Action: TCloseAction);
    procedure DoCloseQueryScreenForm(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormShow(Sender: TObject);
    procedure tmrHideControlTimer(Sender: TObject);
    procedure imgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnExitClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    PreviousBounds: TRect;
    FFullScreen: Boolean;
    FStretchImages: Boolean;
    FImage: TGraphic;
    SettingsChanged: Boolean;
    SettingsLoaded: Boolean;
    LastMousePos: TPoint;
    procedure SetFullScreen(const Value: Boolean);
    procedure UpdateFullScreen;
    procedure SetStretchImages(const Value: Boolean);
    procedure SetImage(const Value: TGraphic);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetOnControlButtonClick(const Value: TNotifyEvent);
    procedure ShowControlPanelButton;
  protected
    procedure NCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure NCLButtonDblClk(var Message: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure EraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMNCMouseMove(var Msg: TWMNCMouseMove); message WM_NCMOUSEMOVE;
  public
    function IsShortCut(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF}): Boolean; override;
    property FullScreen: Boolean read FFullScreen write SetFullScreen;
    property StretchImages: Boolean read FStretchImages write SetStretchImages;
    property Image: TGraphic read FImage write SetImage;
    procedure AdjustOpacity(AOpacity: byte);
    property OnControlButtonClick: TNotifyEvent write SetOnControlButtonClick;
  end;

var
  ScreenForm: TScreenForm;

function StretchSize(AWidth, AHeight, ABoundsWidth, ABoundsHeight: Integer): TRect;

implementation

uses
  IniFiles, UtilsUnit;

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

procedure TScreenForm.ShowControlPanelButton;
begin
  if Assigned(btnControlPanel.OnClick) and (LastMousePos <> Mouse.CursorPos) then
  begin
    btnControlPanel.Show;
    btnExit.Show;
    btnAbout.Show;
    LastMousePos := Mouse.CursorPos;
    tmrHideControl.Enabled := False;
    tmrHideControl.Enabled := True;
  end;
end;

procedure TScreenForm.EraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := Integer(True);
end;

procedure TScreenForm.NCLButtonDblClk(var Message: TWMNCLButtonDblClk);
begin
  DblClick;
end;

procedure TScreenForm.SetFullScreen(const Value: Boolean);
begin
  FFullScreen := Value;
  SettingsChanged := True;
  UpdateFullScreen;
end;

procedure TScreenForm.SetImage(const Value: TGraphic);
begin
  FImage := Value;
end;

procedure TScreenForm.SetOnControlButtonClick(const Value: TNotifyEvent);
begin
  btnControlPanel.OnClick := Value;
end;

procedure TScreenForm.SetStretchImages(const Value: Boolean);
begin
  FStretchImages := Value;
end;

procedure TScreenForm.tmrHideControlTimer(Sender: TObject);
begin
  btnControlPanel.Hide;
  btnExit.Hide;
  btnAbout.Hide;
  tmrHideControl.Enabled := False;
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

procedure TScreenForm.WMMove(var Msg: TWMMove);
begin
  inherited;
  SettingsChanged := True;
end;

procedure TScreenForm.WMNCMouseMove(var Msg: TWMNCMouseMove);
begin
  inherited;
  ShowControlPanelButton;
end;

procedure TScreenForm.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if SettingsChanged then
    SaveSettings;
end;

procedure TScreenForm.btnAboutClick(Sender: TObject);
resourcestring
  rs_AboutText =
    'Версия %s'#13#10 +
    'Автор: Илья Ненашев (http://innenashev.narod.ru)'#13#10 +
    'по заказу МультиСтудии (http://multistudia.ru)'#13#10 +
    'в лице Евгения Генриховича Кабакова'#13#10 +
    ''#13#10 +
    'Исходный код программы доступен для просмотра и доработок'#13#10 +
    'по адресу https://github.com/Nashev/MultiPult'#13#10 +
    ''#13#10 +
    '(А Вы знаете, что Ctrl+C в подобных окошках работает?)';
begin
  InfoMsg(
    Application.Title + #13#10 +
    Format(rs_AboutText, [VersionNameString])
  );
end;

procedure TScreenForm.btnExitClick(Sender: TObject);
begin
  Application.Terminate;
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
  SettingsChanged := True;
end;

procedure TScreenForm.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TScreenForm.imgClick(Sender: TObject);
begin
  Click;
end;

procedure TScreenForm.imgDblClick(Sender: TObject);
begin
  DblClick;
end;

procedure TScreenForm.imgMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowControlPanelButton;
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

procedure TScreenForm.SaveSettings;
var
  IniFile: TIniFile;
begin
  if not SettingsLoaded then
    Exit;

  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    if (WindowState = wsNormal) and not FullScreen then // развёрнутый размер не интересен для сохранения
    begin
      IniFile.WriteInteger('LastUsed', 'ScreenWindowLeft', Left);
      IniFile.WriteInteger('LastUsed', 'ScreenWindowTop', Top);
      IniFile.WriteInteger('LastUsed', 'ScreenWindowWidth', Width);
      IniFile.WriteInteger('LastUsed', 'ScreenWindowHeight', Height);
    end;
    IniFile.WriteBool('LastUsed', 'ScreenWindowMaximized', FullScreen);
  finally
    IniFile.Free;
  end;
  SettingsChanged := False;
end;

procedure TScreenForm.LoadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Left := IniFile.ReadInteger('LastUsed', 'ScreenWindowLeft', Left);
    Top := IniFile.ReadInteger('LastUsed', 'ScreenWindowTop', Top);
    Width := IniFile.ReadInteger('LastUsed', 'ScreenWindowWidth', Width);
    Height := IniFile.ReadInteger('LastUsed', 'ScreenWindowHeight', Height);
    FullScreen := IniFile.ReadBool('LastUsed', 'ScreenWindowMaximized', False);
  finally
    IniFile.Free;
  end;
  Application.ProcessMessages;
  SettingsLoaded := True;
  SettingsChanged := False;
end;

end.

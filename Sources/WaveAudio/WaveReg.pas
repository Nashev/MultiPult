{------------------------------------------------------------------------------}
{                                                                              }
{  WaveReg - Component and property editor registration                        }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit WaveReg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WaveTimer, WaveUtils, WaveStorage,
  WaveIO, WavePlayers, WaveRecorders, WaveRedirector, WaveMixer,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF};

type

  TWavePropertyDialog = class(TForm)
    InfoBox: TGroupBox;
    LengthLabel: TLabel;
    DataSizeLabel: TLabel;
    AudioFormatLabel: TLabel;
    Length: TLabel;
    DataSize: TLabel;
    AudioFormat: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    btnPlay: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Panel: TPanel;
    BitRateLabel: TLabel;
    BitRate: TLabel;
    btnStop: TButton;
    ContentSizeLabel: TLabel;
    ContentSize: TLabel;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    Player: TAudioPlayer;
    FileName: String;
    Modified: Boolean;
    procedure UpdateDetails;
    procedure PlayStart(Sender: TObject);
    procedure PlayStop(Sender: TObject);
  public
    class function Execute(const ACaption: String; AWave: TWaveStreamAdapter;
      var AFileName: String): Boolean;
  end;

  TWavePropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetValue: String; override;
    procedure SetValue (const Value: String); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TMixerNamePropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TMixerDestinationNamePropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  {$IFDEF COMPILER6_UP}
  TWaveAudioSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
  {$ENDIF}

procedure Register;

implementation

{$R *.DFM}

procedure Register;
begin
  RegisterComponents('Wave Audio', [TMultimediaTimer]);
  RegisterComponents('Wave Audio', [TAudioMixer]);
  RegisterComponents('Wave Audio', [TWaveStorage]);
  RegisterComponents('Wave Audio', [TWaveCollection]);
  RegisterComponents('Wave Audio', [TAudioPlayer]);
  RegisterComponents('Wave Audio', [TAudioRecorder]);
  RegisterComponents('Wave Audio', [TStockAudioPlayer]);
  RegisterComponents('Wave Audio', [TStockAudioRecorder]);
  RegisterComponents('Wave Audio', [TLiveAudioPlayer]);
  RegisterComponents('Wave Audio', [TLiveAudioRecorder]);
  RegisterComponents('Wave Audio', [TAudioRedirector]);
  RegisterPropertyEditor(TypeInfo(TWaveStreamAdapter), nil, '', TWavePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TAudioMixer, 'MixerName', TMixerNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TAudioMixer, 'DestinationName', TMixerDestinationNamePropertyEditor);
  {$IFDEF COMPILER6_UP}
  RegisterSelectionEditor(TCustomWaveStorage, TWaveAudioSelectionEditor);
  RegisterSelectionEditor(TWaveAudioIO, TWaveAudioSelectionEditor);
  RegisterSelectionEditor(TAudioRedirector, TWaveAudioSelectionEditor);
  {$ENDIF}
end;

{ TWaveAudioSelectionEditor }

{$IFDEF COMPILER6_UP}
procedure TWaveAudioSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('mmSystem');
  Proc('WaveUtils');
  Proc('WaveStorage');
end;
{$ENDIF}

{ TWavePropertyDialog }

class function TWavePropertyDialog.Execute(const ACaption: String;
  AWave: TWaveStreamAdapter; var AFileName: String): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      Player := TAudioPlayer.Create(Panel.Owner);
      Player.OnActivate := PlayStart;
      Player.OnDeactivate := PlayStop;
      Caption := ACaption;
      Player.Wave := AWave;
      FileName := AFileName;
      UpdateDetails;
      if (ShowModal = mrOK) and Modified then
      begin
        AWave.Assign(Player.Wave);
        AFileName := FileName;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TWavePropertyDialog.UpdateDetails;
begin
  if Player.Wave.Valid then
  begin
    Length.Caption := FormatFloat('#,##0.00 sec.', Player.Wave.Length / 1000);
    BitRate.Caption := FormatFloat('#,##0 kbps', Player.Wave.BitRate);
    DataSize.Caption := FormatFloat('#,##0 bytes', Player.Wave.DataSize);
    AudioFormat.Caption := Player.Wave.AudioFormat;
  end
  else
  begin
    Length.Caption := '';
    BitRate.Caption := '';
    DataSize.Caption := '';
    AudioFormat.Caption := '';
  end;
  ContentSize.Caption := FormatFloat('#,##0 bytes', Player.Wave.Stream.Size);
  btnClear.Enabled := not Player.Wave.Empty;
  btnSave.Enabled := not Player.Wave.Empty;
  btnPlay.Enabled := Player.Wave.Valid;
end;

procedure TWavePropertyDialog.PlayStart(Sender: TObject);
begin
  btnStop.Visible := True;
  if ActiveControl = btnPlay then
    ActiveControl := btnStop;
  btnPlay.Visible := False;
end;

procedure TWavePropertyDialog.PlayStop(Sender: TObject);
begin
  btnPlay.Visible := True;
  if ActiveControl = btnStop then
    ActiveControl := btnPlay;
  btnStop.Visible := False;
end;

procedure TWavePropertyDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Player.Active := False;
end;

procedure TWavePropertyDialog.btnClearClick(Sender: TObject);
begin
  if not Player.Wave.Empty then
  begin
    Player.Active := False;
    Player.Wave.Clear;
    Modified := True;
    UpdateDetails;
  end;
end;

procedure TWavePropertyDialog.btnLoadClick(Sender: TObject);
begin
  OpenDialog.FileName := FileName;
  if OpenDialog.Execute then
  begin
    Player.Active := False;
    Player.Wave.LoadFromFile(OpenDialog.FileName);
    FileName := OpenDialog.FileName;
    Modified := True;
    UpdateDetails;
  end;
end;

procedure TWavePropertyDialog.btnSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := FileName;
  if SaveDialog.Execute then
    Player.Wave.SaveToFile(SaveDialog.FileName);
end;

procedure TWavePropertyDialog.btnPlayClick(Sender: TObject);
begin
  Player.Active := True;
end;

procedure TWavePropertyDialog.btnStopClick(Sender: TObject);
begin
  Player.Active := False;
end;

{ TWavePropertyEditor }

function TWavePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TWavePropertyEditor.GetValue: String;
begin
  if not TWaveStreamAdapter(GetOrdValue).Empty then
    Result := '(TWave)'
  else
    Result := '(None)';
end;

procedure TWavePropertyEditor.SetValue(const Value: String);
begin
  if Value = '' then
    SetOrdValue(0);
end;

procedure TWavePropertyEditor.Edit;
var
  Component: TPersistent;
  PropertyPath: String;
  FileName: String;
  Wave: TWaveStreamAdapter;
begin
  Component := GetComponent(0);
  PropertyPath := Component.GetNamePath + '.' + GetName;
  if Component is TWaveItem then
    FileName := TWaveItem(Component).Name
  else
    FileName := '';
  Wave := TWaveStreamAdapter(GetOrdValue);
  if TWavePropertyDialog.Execute(PropertyPath, Wave, FileName) then
  begin
    Modified;
    if (Component is TWaveItem) and (TWaveItem(Component).Name = '') then
      TWaveItem(Component).Name := ChangeFileExt(ExtractFileName(FileName), '');
  end;
end;

{ TMixerNamePropertyEditor }

function TMixerNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TMixerNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  Names: TStringList;
  Index: Integer;
begin
  Names := TStringList.Create;
  try
    TAudioMixer(GetComponent(0)).FetchMixerNames(Names);
    for Index := 0 to Names.Count - 1 do
      Proc(Names[Index]);
  finally
    Names.Free;
  end;
end;

function TMixerNamePropertyEditor.GetValue: String;
begin
  Result := GetStrValue;
end;

procedure TMixerNamePropertyEditor.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

{ TMixerDestinationNamePropertyEditor }

function TMixerDestinationNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TMixerDestinationNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  Names: TStringList;
  Index: Integer;
begin
  Names := TStringList.Create;
  try
    TAudioMixer(GetComponent(0)).FetchDestinationNames(Names);
    for Index := 0 to Names.Count - 1 do
      Proc(Names[Index]);
  finally
    Names.Free;
  end;
end;

function TMixerDestinationNamePropertyEditor.GetValue: String;
begin
  Result := GetStrValue;
end;

procedure TMixerDestinationNamePropertyEditor.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

end.

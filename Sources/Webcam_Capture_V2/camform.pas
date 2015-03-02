unit camform;

(******************************************************************************

 Exemplo de Aplicativo com Webcam
 Autor: Fot�grafo Fernando VR
 Site: www.FernandoVR.com.br

 Sobre mim:
  Sou fot�grafo profissional, programador web PHP, e aprendiz no Delphi.
  Ainda estou iniciando meus estudos em Delphi por isso n�o adianta me adicionar
  para fazer perguntas dif�ceis que n�o saberei responder.

 Sobre o Aplicativo:
  Criei esse programa fu�ando centenas de c�digos fontes com webcam at� encontrar
  um modo que n�o fosse necess�rio a instala��o de nenhum componente e nem
  utiliza��o de DLL�s.
  Este programa funciona com classes de Microsoft DirectX 9.0.
  Desenvolvi no DelphiXE 2, mas analisando o c�digo fonte pode ser facilmente
  adaptado em vers�es anteriores.

******************************************************************************)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles, JPEG,
  VFrames, VSample, Direct3D9, DirectDraw, DirectShow9, DirectSound, DXTypes,
  Vcl.ComCtrls;

type
  WebcamTimer = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    img1: TImage;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    lbl1: TLabel;
    edt1: TEdit;
    lbl2: TLabel;
    trckbr1: TTrackBar;
    edt2: TEdit;
    btnligar: TButton;
    btndesligar: TButton;
    lbl4: TLabel;
    lbl_camstatus: TLabel;
    cbb1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure trckbr1Change(Sender: TObject);
    procedure edt2Change(Sender: TObject);
    procedure btnligarClick(Sender: TObject);
    procedure btndesligarClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
    fActivated  : boolean;
    fVideoImage : TVideoImage;
    fVideoBitmap:  TBitmap;
    OfflineImage : TJPEGImage;
    procedure OnNewVideoFrame(Sender : TObject; Width, Height: integer; DataPtr: pointer);
    procedure OnNewVideoCanvas(Sender : TObject; Width, Height: integer; DataPtr: pointer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PathExec,Deviceselect : string;
  IFile: TIniFile;
  JpgQuality, TimerExec : Integer;
  RS: TResourceStream;

implementation

{$R *.dfm}

{ WebcamTimer }

// Thread para Salvar Imagens no PC
procedure WebcamTimer.Execute;
var
  MyJPEG : TJPEGImage;
  JPEGFName, formattedDateTime: string;
  I :Integer;
begin
  For I:= 1 to StrToInt(Form1.edt1.Text) do
    Begin
      Form1.fVideoImage.OnNewVideoFrame := Form1.OnNewVideoFrame; // Pega o frame atual da webcam em bitmap
      formattedDateTime := FormatDateTime('yyyy_m_d_hh_nn_ss_zzz',(Now)); // Gerador de nomes para o JPG com data e hor�rio.
      MyJPEG := TJPEGImage.Create; // Cria uma imagem JPG
      JPEGFName:= PathExec+formattedDateTime+'.jpg';  // Cria todo caminho onde a imagem JPG ser� salva
      MyJPEG.CompressionQuality := strtoint(Form1.edt2.Text); // Seta a qualidade do JPG escolhido na configura��o
      try
        MyJPEG.Assign(Form1.fVideoBitmap); // Atribui o bitimap frame no JPG
        MyJPEG.SaveToFile(JPEGFName); // Salva o JPG no diret�rio
      finally
        MyJPEG.Free; // Libera o JPG da mem�ria.
      end;
      Sleep(1000);
    End;
end;

{ FORM }

// Pega frame de v�deo
procedure TForm1.OnNewVideoFrame(Sender : TObject; Width, Height: integer; DataPtr: pointer);
begin
  fVideoImage.GetBitmap(fVideoBitmap);   // Pega apenas o frame atual do v�deo.
end;

// Passa para o TImage todos os frames em quanto a cam estiver ligada.
procedure TForm1.OnNewVideoCanvas(Sender : TObject; Width, Height: integer; DataPtr: pointer);
begin
  fVideoImage.GetBitmap(fVideoBitmap);  // Pega o frame atual, N�o sei pq mas precisa desta linha para funcionar.
  img1.Picture.Bitmap.Canvas.Draw(0,0,fVideoBitmap); // Envia frame a frame de v�deo para o componente TImage, tb n�o entendi direito o pq de s� funcionar com o canvas, mas deu certo.
end;

// Executa ao iniciar o programa.
procedure TForm1.FormCreate(Sender: TObject);
var
  DevicesListed, i : Integer;
  DeviceList : TStringList;
begin
  RS:= TResourceStream.Create(hInstance, 'OfflineIMG',RT_RCDATA); // Carrega Resource da imagem padr�o quando a camera estiver desligada.
  OfflineImage := TJPEGImage.Create; // Cria Imagem JPG
  OfflineImage.LoadFromStream(RS); // Atribui a imagem do resorce carregado a uma imagem JPG.
  img1.Picture.Assign(OfflineImage); // Exibe a imagem offline padr�o no component Timage

  PathExec := ExtractFilePath(Application.ExeName); // extrai o diret�rio atual do aplicativo
  IFile := TIniFile.Create(PathExec+'config.ini'); // Seta um aquivo INI para gravar as configura��es

  fVideoBitmap := TBitmap.create; // Cria um bitmap para os frames de v�deo.
  fVideoImage := TVideoImage.Create;  // Cria uma imagem de v�deo.
  fActivated := false; // Inicia avisando que a webcam est� desligada;

////////// Configura��o da Qualidade da imagem JPG
        // Busca a qualidade gravada no arquivo INI, default: 100
        JpgQuality := IFile.ReadInteger('Config','JPGQuality',100);
        // Exibe resultado no canpo de texto
        edt2.Text := IntToStr(JpgQuality);
        // Exibe resultado no trackbar
        trckbr1.Position := JpgQuality;

////////// Configura��o de quantas imagens ser� gravadas no diret�rio
      // Busca configura��o salva no arquivo INI, default: 20 (imagens)
       TimerExec := IFile.ReadInteger('Config','TimerExec',20);
       // Exibe resultado no canpo de texto
       edt1.Text := IntToStr(TimerExec);

////////// Configura��o da webcam.
      // Lista todos os dispositivos num listbox.
      DeviceList := TStringList.Create;
      fVideoImage.GetListOfDevices(DeviceList);
      cbb1.Items := DeviceList;
      DeviceList.Free;

      // Busca configura��o salva no arquivo INI, default: nenhum dispositivo selecionado.
      Deviceselect := IFile.ReadString('WEBCAM','Device','');
      // Se caso encontrar algum item na configura��o ele busca no listbox e seleciona se o dispositivo existir na listagem.
      cbb1.ItemIndex := cbb1.Items.IndexOf(Deviceselect);
end;

// Executa ao mudar o trackbar
procedure TForm1.trckbr1Change(Sender: TObject);
begin
   edt2.Text := IntToStr(trckbr1.Position);
end;

// Executa ao mudar o valor da qualidade da imagem na caixa de texto
procedure TForm1.edt2Change(Sender: TObject);
begin
   trckbr1.Position := strtoint(edt2.Text);
end;

// Executa ao clicar no bot�o de ligar Webcam
procedure TForm1.btnligarClick(Sender: TObject);
var
camdevice: string;
begin
  // Identifica qual dispositivo est� selecionado
  camdevice := Trim(cbb1.Items.Strings[cbb1.ItemIndex]);
  try
    // Inicia a webcam
    fVideoImage.VideoStart(camdevice);
    // Pega o primeiro frame que provavelmente estar� vazio.
    fVideoImage.OnNewVideoFrame := OnNewVideoFrame;
    // Seta a webcam como ativada
    fActivated := true;
    // Exibe Status como Ligada
    lbl_camstatus.Caption := 'Ligada';
    lbl_camstatus.Font.Color := clGreen;
  except
    // Seta a webcam como desativada
    fActivated := false;
    // Exibe Status como Desligado
    lbl_camstatus.Caption := 'Desligada';
    lbl_camstatus.Font.Color := clRed;
  end;
end;

// Executa ao clicar no bot�o de desligar Webcam
procedure TForm1.btndesligarClick(Sender: TObject);
begin
  // Desliga a Webcam
  fVideoImage.VideoStop;
  // Seta a webcam como desativada
  fActivated := false;
  // Exibe Status como Desligado
  lbl_camstatus.Caption := 'Desligada';
  lbl_camstatus.Font.Color := clRed;
  // Exibe a imagem offline padr�o no component Timage
  img1.Picture.Assign(OfflineImage);
end;

// Executa ao clicar no bot�o de "pegar imagem"
procedure TForm1.btn1Click(Sender: TObject);
begin
  if fActivated then
    begin
      // Pega o frame atual da webcam em bitmap
      fVideoImage.OnNewVideoFrame := OnNewVideoFrame;
      // Envia o bitmap do frame atual para o component Timage
      img1.Picture.Bitmap.Assign(fVideoBitmap);
    end
  else
    ShowMessage('A Webcam precisa estar ligada!');
end;

// Executa ao clicar no bot�o de "Video Cam"
procedure TForm1.btn2Click(Sender: TObject);
begin
  if fActivated then
    begin
      // Inicia exibi��oo de video da webcam em bitmap
      fVideoImage.OnNewVideoFrame := OnNewVideoCanvas;
      // Envia o o v�deo para o component Timage
      img1.Picture.Bitmap.Assign(fVideoBitmap);
    end
  else
    ShowMessage('A Webcam precisa estar ligada!');
end;

// Executa ao clicar no bot�o de "Salvar na Pasta"
procedure TForm1.btn3Click(Sender: TObject);
var
  _WebcamTimer: WebcamTimer;
begin
  if fActivated then
    begin
      _WebcamTimer := WebcamTimer.Create(False);
      _WebcamTimer.FreeOnTerminate := True;
    end
  else
  ShowMessage('A Webcam precisa estar ligada!');
end;

// Executar ao fechar o programa
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Desliga a webcam caso esteja ligada.
  fVideoImage.VideoStop;

////////// SALVA TODAS AS CONFIGURA��ES NO ARQUIVO INI
  // SALVA QUALIDADE DO JPG
  IFile.WriteInteger('Config','JPGQuality',strtoint(edt2.Text));
  // SALVA QUANTIDADE DE IMAGENS POR SEGUNDOS A SER SALVA NO DIRETORIO
  IFile.WriteInteger('Config','TimerExec',strtoint(edt1.Text));
  // SALVA DISPOSITIVO DA WEBCAM SELECIONADO
  IFile.WriteString('WEBCAM','Device',cbb1.Items.Strings[cbb1.ItemIndex]);

  // Libera o Arquivo INI da Mem�ria
  IFile.Free;

  // Libera o Bitmap e imagem de v�deo da mem�ria.
  fVideoBitmap.Free;
  fVideoImage.Free;
end;

end.

unit AVIReading;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VFW, ExtCtrls, Ole2; 

type
  TAVIReader = class
  private
    FrameIndex:  INTEGER;
    pavi:PAVIFile;
    pavis:PAVIStream;
    pavisound:PAVIStream;
    ob:PGetFrame;
    pbmi:PBitmapInfoHeader;
    Punter:PByte;
    han,i:integer;
    microsperframe:integer;

    info  : PAVISTREAMINFOA;
    hexcode : string;
    EMsg : string;
    lasterr : integer;
  public
    FileName: String;
    Buffer: Pointer;
    Start, Ending:integer;

    constructor Create(Buffer: Pointer);
    destructor Destroy;  override;

    function Open(AFileName: String): integer;
    procedure GetFrame(Frame: integer);
    procedure Close;
  end;

implementation

{ TAVIReader }

procedure TAVIReader.Close;
begin
  AVIStreamGetFrameClose(ob);
  AVIStreamRelease(pavis);
end;

constructor TAVIReader.Create(Buffer: Pointer);
begin
  inherited Create;

  Self.Buffer := Buffer;
end;

destructor TAVIReader.Destroy;
begin

  inherited;
end;

procedure TAVIReader.GetFrame(Frame: integer);
begin
  pbmi:=AVIStreamGetFrame(ob,Frame);
  Punter:=Pointer(Integer(pbmi)+pbmi^.biSize);

//  with Bitmap do
    begin
//      Width := pbmi^.biWidth;
//      Height := pbmi^.biHeight;
//      PixelFormat := pf24Bit;
      
      Move(Punter^, Buffer^, pbmi^.biWidth*pbmi^.biHeight*3);
    end;
end;

function TAVIReader.Open(AFileName: String): integer;
begin
  FileName := AFileName;
  result := AVIFileOpen(pavi, Pchar(Filename), OF_READ, nil);
  AVIFILEGetStream(pavi,pavis,streamtypeVIDEO,0); 
  Start:=AVIStreamStart(pavis);
  Ending:=AVIStreamENd(pavis);
  ob:=AVIStreamGetFrameOpen(pavis,nil);
  AVIStreamBeginStreaming(pavis,Start,Ending,1000);
//  DrawDIBStart(han,microsperframe);
end;

end.

unit AVIFile32; // AVIFIL32WrapperJER

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

 {AVIFIL32.DLL - Delphi Wrapper
 This code is supplied as is with no guarantees
 It has been used successfully for a number of projects
 This code is freeware - no charge!

 Documentation will be available soon for which a small charge will be made
 Watch the Soluble Web Site for news
 www.soluble-fish.demon.co.uk

 Code prepared by

 Jimmy Robinson
 Steve Mayall
 Hakim Hasani
 Thanks for your interest

 See MM.HLP
 }
interface

uses
{$IFNDEF FPC}
  WinTypes;
{$ELSE}
  Types;
{$ENDIF}


const
  {All-Purpose Key Frame Constant}
  AVIIF_KEYFRAME                 = $00000010;
  {FourCC Constants}
  STREAMTYPEVIDEO                = $73646976; {v i d s}
  STREAMTYPEAUDIO                = $73647561; {a u d s}
  STREAMTYPEMIDI                 = $7364696D; {m i d s}
  STREAMTYPETEXT                 = $73747874; {t x t s}
  COMPTYPEDIB                    = $20424944; {D I B _}
  {AVIStreamInfo Constants}
  AVISTREAMINFO_DISABLED         = $00000001;
  AVISTREAMINFO_FORMATCHANGES    = $00010000;
  {AVIFileInfo Consants}
  AVIFILEINFO_HASINDEX           = $00000010;
  AVIFILEINFO_MUSTUSEINDEX       = $00000020;
  AVIFILEINFO_ISINTERLEAVED      = $00000100;
  AVIFILEINFO_WASCAPTUREFILE     = $00010000;
  AVIFILEINFO_COPYRIGHTED        = $00020000;
  AVIFILECAPS_CANREAD            = $00000001;
  AVIFILECAPS_CANWRITE           = $00000002;
  AVIFILECAPS_ALLKEYFRAMES       = $00000010;
  AVIFILECAPS_NOCOMPRESSION      = $00000020;
  {AVICompressOptions Constants}
  AVICOMPRESSF_INTERLEAVE        = $00000001;
  AVICOMPRESSF_DATARATE          = $00000002;
  AVICOMPRESSF_KEYFRAMES         = $00000004;
  AVICOMPRESSF_VALID             = $00000008;
  {AVIStreamFindSample Constants}
  FIND_DIR                       = $0000000F;
  FIND_NEXT                      = $00000001;
  FIND_PREV                      = $00000004;
  FIND_TYPE                      = $000000F0;
  FIND_KEY                       = $00000010;
  FIND_ANY                       = $00000020;
  FIND_FORMAT                    = $00000040;
  FIND_RET                       = $0000F000;
  FIND_POS                       = $00000000;
  FIND_LENGTH                    = $00001000;
  FIND_OFFSET                    = $00002000;
  FIND_SIZE                      = $00003000;
  FIND_INDEX                     = $00004000;
  {AVISaveOptions Constants}
  ICMF_CHOOSE_KEYFRAME           = $0001;
  ICMF_CHOOSE_DATARATE           = $0002;
  ICMF_CHOOSE_PREVIEW            = $0004;
  ICMF_CHOOSE_ALLCOMPRESSORS     = $0008;
  {AVI Error Code Constants}
  AVIERR_OK                      = $00000000;
  AVIERR_UNSUPPORTED             = $80044065;
  AVIERR_BADFORMAT               = $80044066;
  AVIERR_MEMORY                  = $80044067;
  AVIERR_INTERNAL                = $80044068;
  AVIERR_BADFLAGS                = $80044069;
  AVIERR_BADPARAM                = $8004406A;
  AVIERR_BADSIZE                 = $8004406B;
  AVIERR_BADHANDLE               = $8004406C;
  AVIERR_FILEREAD                = $8004406D;
  AVIERR_FILEWRITE               = $8004406E;
  AVIERR_FILEOPEN                = $8004406F;
  AVIERR_COMPRESSOR              = $80044070;
  AVIERR_NOCOMPRESSOR            = $80044071;
  AVIERR_READONLY                = $80044072;
  AVIERR_NODATA                  = $80044073;
// @@ SJM  AVIERR_BUFFERTOSMALL           = $80044074;
  AVIERR_BUFFERTOOSMALL          = $80044074;
  AVIERR_CANTCOMPRESS            = $80044075;
// @@ SJM AVIERR_USERABORT               = $800440B6;
  AVIERR_USERABORT               = $800440C6;
// @@ SJM  AVIERR_ERROR                   = $800440B7;
  AVIERR_ERROR                   = $800440C7;

type
  FourCC                 = LongInt;
  DWord                  = LongInt;
  PBitmapInfoHeader      = ^TBitmapInfoHeader;
  LPVoid                 = Pointer;
  PAVIFile               = Pointer;
  PAVIStream             = Pointer;
  PGetFrame              = Pointer;
  MyBuffer =  array[0..100] of byte;

  {TGUID = record
    Data1:                 LongInt;
    Data2:                 Word;
    Data3:                 Word;
    Data4:                 array[0..7] of Byte;
  end; {Globally Unique ID}

  TClsID                 = TGUID; {Class ID}
  TIID                   = TGUID; {Interface ID}
  PGUID                  = ^TGUID;
  PClsID                 = ^TGUID;
  PIID                   = ^TGUID;

  TAVIStreamInfo = record
    fccType:               FourCC;
    fccHandler:            FourCC;
    dwFlags:               DWord;
    dwCaps:                DWord;
    wPriority:             Word;
    wLanguage:             Word;
    dwScale:               DWord;
    dwRate:                DWord;
    dwStart:               DWord;
    dwLength:              DWord;
    dwInitialFrames:       DWord;
    dwSuggestedBufferSize: DWord;
    dwQuality:             DWord;
    dwSampleSize:          DWord;
    rectFrame:             TRect;
    dwEditCount:           DWord;
    dwFormatChangeCount:   DWord;
    szName:                array[0..63] of Char;
  end;

  TAVIFileInfo = record
// SJM    dwMaxBytesPerSecond:   DWord;
    dwMaxBytesPerSec:      DWord;
    dwFlags:               DWord;
    dwCaps:                DWord;
    dwStreams:             DWord;
    dwSuggestedBufferSize: DWord;
    dwWidth:               DWord;
    dwHeight:              DWord;
    dwScale:               DWord;
    dwRate:                DWord;
    dwLength:              DWord;
    dwEditCount:           DWord;
    szFileType:            array[0..63] of Char;
  end;
  //  BYTE	1
  //  WORD	2
  //  DWORD	4
  //  QWORD	8
  //  TBYTE	10

{*************JIMMY*******************}

{*************************************}


  TAVICompressOptions = record
    fccType:               FourCC;
    fccHandler:            FourCC;
    dwKeyFrameEvery:       DWord;
    dwQuality:             DWord;
    dwBytesPerSecond:      DWord;
    dwFlags:               DWord;
    lpFormat:              LPVoid;
    cbFormat:              DWord;
    lpParms:               LPVoid;
    cbParms:               DWord;
    dwInterleaveEvery:     DWord;
  end;



TCompressArray = array[0..9] of ^TAVICompressOptions;
PAVIStreamInfo      = ^TAVIStreamInfo;
PAVIFileInfo        = ^TAVIFileInfo;
PAVICompressOptions = ^TAVICompressOptions;




procedure AVIFileInit;external 'AVIFil32.dll';

procedure AVIFileExit;external 'AVIFil32.dll';
function  AVIFileAddRef(pFile: PAVIFile): LongInt stdcall; external 'AVIFil32.dll';
function  AVIFileRelease(pFile: PAVIFile): LongInt stdcall; external 'AVIFil32.dll';
function  AVIFileOpen(var pFile: PAVIFile; szFile: PChar;
                          uMode: Word; pclsidHandler: PClsID): LongInt stdcall;
                      external 'AVIFil32.dll' name 'AVIFileOpenW';
function  AVIFileInfo(pFile: PAVIFile; var afiInfo: TAVIFileInfo;
                      lSize: LongInt): LongInt stdcall; external 'AVIFil32.dll';
function  AVIFileGetStream(pFile: PAVIFile; var pStream: PAVIStream;
                           fccType: FourCC; lParam: LongInt): LongInt stdcall;
                           external 'AVIFil32.dll';
function  AVIFileCreateStream(    pFile: PAVIFile; var pStream: PAVIStream;
                              var asiInfo: TAVIStreamInfo): LongInt stdcall;
                              external 'AVIFil32.dll';
function  AVIFileWriteData(pFile: PAVIFile; ckID: DWord; lpData: LPVoid;
                           cbData: LongInt): LongInt stdcall; external 'AVIFil32.dll';
function  AVIFileReadData(    pFile: PAVIFile; ckID: DWord; lpData: LPVoid;
                          var cbData: LongInt): LongInt stdcall; external 'AVIFil32.dll';
function  AVIFileEndRecord(pFile: PAVIFile): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamAddRef(pStream: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamRelease(pStream: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamInfo(pStream: PAVIStream; var asiInfo: TAVIStreamInfo;
                        lSize: LongInt): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamFindSample(pStream: PAVIStream; lPos: LongInt;
                              lFlags: LongInt): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamReadFormat(pStream: PAVIStream; lPos: LongInt;
                              lpFormat: LPVoid; var cbFormat: LongInt):
                              LongInt stdcall;
                              external 'AVIFil32.dll';
function  AVIStreamSetFormat(pStream: PAVIStream; lPos: LongInt;
                             lpFormat: LPVoid; cbFormat: LongInt): LongInt stdcall;
                             external 'AVIFil32.dll';
function  AVIStreamReadData(pStream: PAVIStream; fccType: FourCC;
                            lpData: LPVoid; var cbData: LongInt): LongInt stdcall;
                            external 'AVIFil32.dll';
function  AVIStreamWriteData(pStream: PAVIStream; fccType: FourCC;
                             lpData: LPVoid; cbData: LongInt): LongInt stdcall;
                             external 'AVIFil32.dll';
//====== AVIStreamRead =========================================================
function  AVIStreamRead(pStream: PAVIStream; lStart, lSamples: LongInt;
                        lpBuffer: LPVoid; cbBuffer: LongInt;
                        plBytes, plSamples: PLongInt): LongInt stdcall;
                        external 'AVIFil32.dll';
//==============================================================================
function  AVIStreamWrite(pStream: PAVIStream; lStart, lSamples: LongInt;
                         lpBuffer: LPVoid; cbBuffer: LongInt; dwFlags: DWord;
                         plSampWritten, plBytesWritten: PLongInt): LongInt stdcall;
                         external 'AVIFil32.dll';
function  AVIStreamStart(pStream: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamLength(pStream: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  AVIStreamTimeToSample(pStream: PAVIStream; lTime: LongInt): LongInt stdcall;
                                external 'AVIFil32.dll';
function  AVIStreamSampleToTime(pStream: PAVIStream; lSample: LongInt):
                                LongInt stdcall;
                                external 'AVIFil32.dll';
function  AVIStreamBeginStreaming(pStream: PAVIStream;
                                  lStart, lEnd, lRate: LongInt): LongInt stdcall;
                                  external 'AVIFil32.dll' ;
function  AVIStreamEndStreaming(pStream: PAVIStream): LongInt stdcall;
                                external 'AVIFil32.dll';
function  AVIStreamGetFrameOpen(pStream: PAVIStream;
                                pbmpWanted: PBitmapInfoHeader): PGetFrame stdcall;
                                external 'AVIFil32.dll';
function  AVIStreamGetFrame(pgFrame: PGetFrame; lPos: LongInt): LPVoid stdcall;
                            external 'AVIFil32.dll';
function  AVIStreamGetFrameClose(pgFrame: PGetFrame): LongInt stdcall;
                                 external 'AVIFil32.dll';
//==============================================================================
// I have changed the  pclsidHandler: PClsID data type on 8/9/97 by Hakim
function  AVIStreamOpenFromFile(var pStream: PAVIStream; szFile: PChar;
                                    fccType: FourCC; lParam: LongInt;
                                    mode: Word;
                                var pclsidHandler: PClsID): LongInt stdcall;
                                external 'AVIFil32.dll' name 'AVIStreamOpenFromFileW';
//==============================================================================
function  AVIStreamCreate(var pStream: PAVIStream;  lParam1, lParam2: LongInt;
                              clsidHandler: TClsID): LongInt stdcall;
                              external 'AVIFil32.dll';
function  AVIMakeCompressedStream(var pCompressed: PAVIStream;
                                      pSource: PAVIStream;
                                  var acoOptions: TAVICompressOptions;
                                  var clsidHandler: TClsID): LongInt stdcall;

                                  external 'AVIFil32.dll';



function  AVISave(szFile: PChar; pclsidHandler: PClsID; pfnCallback:
                  Pointer; nStreams: Integer; pStream: PAVIStream;
                  pacoOptions: PAVICompressOptions): LongInt stdcall;
                  external 'AVIFil32.dll' name 'AVISaveW';

           


function  AVISaveV(szFile: PChar; var clsidHandler: PClsID;
                   pfnCallback: Pointer;
                   nStreams: Integer; var pStream: PAVIStream;
                   var Options: PAVICompressOptions): LongInt stdcall;
{function  AVISaveV(szFile: PChar; var clsidHandler: PClsID;
                   pfnCallback: Pointer;
                   nStreams: Integer; var pStream: PAVIStream;
                  var pacoOptions: PAVICompressOptions): LongInt stdcall;}
                   external 'AVIFil32.dll' name 'AVISaveVW';
function  AVISaveOptions(    hParent: HWnd; uiFlags: Word; nStreams: Integer;
                         var pStream: PAVIStream;
                         var pacoOptions: PAVICompressOptions): WordBool stdcall;
                         external 'AVIFil32.dll';
function  AVISaveOptionsFree(    nStreams: Integer;
                             var pacoOptions: PAVICompressOptions): LongInt stdcall;
                             external 'AVIFil32.dll';
function  AVIBuildFilter(var szFilter: PChar; cbFilter: LongInt;
                             fSaving: WordBool): LongInt stdcall; external 'AVIFil32.dll' name 'AVIBuildFilterW';
function  AVIMakeFileFromStreams(var pFile: PAVIFile; nStreams: Integer;
                                 var pStream: PAVIStream): Longint stdcall;
                                 external 'AVIFil32.dll';
function  AVIMakeStreamFromClipboard(    cfFormat: Word; hGlobal: THandle;
                                     var pStream: PAVIStream): LongInt stdcall;
                                     external 'AVIFil32.dll';
function  AVIPutFileOnClipboard(pFile: PAVIFile): LongInt stdcall; external 'AVIFil32.dll';
function  AVIGetFromClipboard(var pFile: PAVIFile): LongInt stdcall;
                              external 'AVIFil32.dll';
function  AVIClearClipboard: LongInt stdcall; external 'AVIFil32.dll';
function  CreateEditableStream(var pEditable: PAVIStream;
                                   pSource: PAVIStream): LongInt stdcall;
                               external 'AVIFil32.dll';
function  EditStreamCut(    pStream: PAVIStream;
                        var plStart, plLength: PLongInt;
                        var pResult: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  EditStreamCopy(    pStream: PAVIStream;
                         var plStart, plLength: PLongInt;
                         var pResult: PAVIStream): LongInt stdcall; external 'AVIFil32.dll';
function  EditStreamPaste(    pStream: PAVIStream;
                          var plPos, plLength: PLongInt;
                          var pResult: PAVIStream;
                              lStart, lLength: LongInt): LongInt stdcall;
                           external 'AVIFil32.dll';
function  EditStreamClone(    pStream: PAVIStream;
                          var pResult: PAVIStream): LongInt stdcall;
                          external 'AVIFil32.dll';
function  EditStreamSetName(pStream: PAVIStream; szName: PChar): LongInt stdcall;
                            external 'AVIFil32.dll' name 'EditStreamSetNameW';
function  EditStreamSetInfo(pStream: PAVIStream; var asiInfo: TAVIStreamInfo;
                            cbInfo: LongInt): LongInt stdcall; external 'AVIFil32.dll';


implementation




end.


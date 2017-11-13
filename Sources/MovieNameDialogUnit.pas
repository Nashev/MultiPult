unit MovieNameDialogUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ImgList,
  Vcl.Menus;

type
  TMovieNameDialogCloseQueryEvent = function(const ANewMovieName: string): Boolean of object;

  TMovieNameDialog = class(TForm)
    lvMovies: TListView;
    edtMovieName: TEdit;
    lblMovieName: TLabel;
    edtPhotoFolder: TEdit;
    lblPhotoFolder: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    ilIcons: TImageList;
    pmMovies: TPopupMenu;
    mmiDelete: TMenuItem;
    mmiRename: TMenuItem;
    miChoose: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lvMoviesEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure lvMoviesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvMoviesDblClick(Sender: TObject);
    procedure miChooseClick(Sender: TObject);
    procedure mmiRenameClick(Sender: TObject);
    procedure mmiDeleteClick(Sender: TObject);
    procedure pmMoviesPopup(Sender: TObject);
  private
    FCloseQueryHandler: TMovieNameDialogCloseQueryEvent;
    procedure InitMovieList;
  public
    class function Execute(const APhotoFolder, AMovieName, ACaption: string; ACloseQueryHandler: TMovieNameDialogCloseQueryEvent): Boolean;
  end;

var
  MovieNameDialog: TMovieNameDialog;

implementation

uses
  UtilsUnit, Winapi.ShellAPI;

{$R *.dfm}

{ TMovieNameDialog }

class function TMovieNameDialog.Execute(const APhotoFolder, AMovieName, ACaption: string; ACloseQueryHandler: TMovieNameDialogCloseQueryEvent): Boolean;
begin
  Result := True;
  with Self.Create(nil) do
    try
      edtPhotoFolder.Text := APhotoFolder;
      InitMovieList;
      edtMovieName.Text := AMovieName;
      FCloseQueryHandler := ACloseQueryHandler;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMovieNameDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOk) and Assigned(FCloseQueryHandler) then
    CanClose := FCloseQueryHandler(edtMovieName.Text);
    if not CanClose then
      ModalResult := mrNone;
end;

procedure TMovieNameDialog.InitMovieList;
var
  Rec: TSearchRec;
begin
  if {$IFDEF FPC}FindFirstUTF8{$ELSE}FindFirst{$ENDIF}(edtPhotoFolder.Text + '*.mp', faAnyFile - faDirectory, Rec) = 0 then
    begin
      repeat
        with lvMovies.Items.Add do
          begin
            Caption := ChangeFileExt(Rec.Name, '');
            SubItems.Add(FormatFileSize(Rec.Size));
            ImageIndex := 0;
          end;
      until {$IFDEF FPC}FindNextUTF8{$ELSE}FindNext{$ENDIF}(Rec) <> 0;
      {$IFDEF FPC}FindCloseUTF8{$ELSE}FindClose{$ENDIF}(Rec);
    end;
end;

procedure TMovieNameDialog.lvMoviesDblClick(Sender: TObject);
begin
  if Assigned(lvMovies.ItemFocused) and lvMovies.ItemFocused.Selected then
    ModalResult := mrOk;
end;

procedure TMovieNameDialog.lvMoviesEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  RenameFile(edtPhotoFolder.Text + Item.Caption + '.mp', edtPhotoFolder.Text + s + '.mp');
end;

procedure TMovieNameDialog.lvMoviesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    edtMovieName.Text := Item.Caption;
end;

procedure TMovieNameDialog.miChooseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TMovieNameDialog.mmiDeleteClick(Sender: TObject);
resourcestring
  rs_DoYouWantDeleteMovies = 'Удалить выделенные мульты (%d шт)?';
var
  Op: TSHFileOpStruct;
  s: string;
  i: Integer;
begin
  if lvMovies.SelCount = 0 then
    Exit;

  if MessageBox(
    0,
    PChar(Format(rs_DoYouWantDeleteMovies, [lvMovies.SelCount])),
    PChar(Application.Title),
    MB_ICONQUESTION or MB_OKCANCEL or MB_APPLMODAL or MB_DEFBUTTON1)
   <> IDOK
  then
    Exit;
  ZeroMemory(@Op, SizeOf(Op));
  s := '';
  for i := 0 to lvMovies.Items.Count - 1 do
    if lvMovies.Items[i].Selected then
      s := s + edtPhotoFolder.Text +  lvMovies.Items[i].Caption + '.mp'#0;
  s := s + #0;
  Op.wFunc := FO_DELETE;
  Op.fFlags := FOF_ALLOWUNDO;
  Op.pFrom := @s[1];
  CheckOSError(SHFileOperation(Op));
  if not Op.fAnyOperationsAborted then
    for i := lvMovies.Items.Count - 1 downto 0 do
      if lvMovies.Items[i].Selected then
        lvMovies.Items[i].Delete;
end;

procedure TMovieNameDialog.mmiRenameClick(Sender: TObject);
begin
  if Assigned(lvMovies.ItemFocused) then
    lvMovies.ItemFocused.EditCaption;
end;

procedure TMovieNameDialog.pmMoviesPopup(Sender: TObject);
begin
  if lvMovies.SelCount = 0 then
    Abort;
end;

end.


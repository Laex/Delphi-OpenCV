unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFrame2 = class(TFrame)
    trckbr1: TTrackBar;
    lbl1: TLabel;
    lbl2: TLabel;
    procedure trckbr1Change(Sender: TObject);
  private
    procedure UpdateState;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TFrame2.Create(AOwner: TComponent);
begin
  inherited;
  UpdateState;
end;

procedure TFrame2.trckbr1Change(Sender: TObject);
begin
  UpdateState;
end;

procedure TFrame2.UpdateState;
begin
  lbl2.Caption := trckbr1.Position.ToString;
end;

end.

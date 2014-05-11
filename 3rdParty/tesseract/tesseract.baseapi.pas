unit tesseract.baseapi;

interface

Uses
  leptonica.pix;

Type
  TTessBaseAPI = class
  public
    function Init(const datapath: pAnsiChar; const language: pAnsiChar): Integer; virtual; stdcall; abstract;
    procedure SetImage(const pix: pPix); virtual; stdcall; abstract;
    function GetUTF8Text: pAnsiChar; virtual; stdcall; abstract;
    procedure _End(); virtual; stdcall; abstract;
  end;

function CreateTessBaseAPI: TTessBaseAPI; stdcall;
procedure ReleaseTessBaseAPI(ex: TTessBaseAPI); stdcall;

implementation

uses
  tesseract.libname;

function CreateTessBaseAPI; external tesseract_classes_Dll;
procedure ReleaseTessBaseAPI; external tesseract_classes_Dll;

end.

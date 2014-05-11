program APIExample;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  leptonica.leptprotos,
  tesseract.baseapi,
  leptonica.pix,
  tesseract.libname;

Var
  outText: PAnsiChar;
  TessBaseAPI: TTessBaseAPI;
  image: pPix;

begin
  try
    TessBaseAPI := CreateTessBaseAPI;
    // // Initialize tesseract-ocr with English, without specifying tessdata path
    if TessBaseAPI.Init(nil, 'eng') <>0 then
    begin
      Writeln('Could not initialize tesseract.');
      Halt(1);
    end;

    // Open input image with leptonica library
    image := pixRead('resource\phototest.tif');
    TessBaseAPI.SetImage(image);
    // Get OCR result
    outText := TessBaseAPI.GetUTF8Text();
    Writeln('OCR output: ', outText);

    // Destroy used object and release memory
    TessBaseAPI._End;
    pixDestroy(image);
    ReleaseTessBaseAPI(TessBaseAPI);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

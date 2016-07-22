program FaceRecognizer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.core.types_c in '..\..\..\source\ocv.core.types_c.pas',
  ocv.highgui_c in '..\..\..\source\ocv.highgui_c.pas',
  ocv.imgproc.types_c in '..\..\..\source\ocv.imgproc.types_c.pas',
  ocv.imgproc_c in '..\..\..\source\ocv.imgproc_c.pas',
  ocv.lib in '..\..\..\source\ocv.lib.pas',
  uResourcePaths in '..\..\..\source\utils\uResourcePaths.pas',
  ocv.cls.contrib in '..\..\..\source\Classes\ocv.cls.contrib.pas',
  ocv.cls.core in '..\..\..\source\classes\ocv.cls.core.pas',
  ocv.cls.highgui in '..\..\..\source\classes\ocv.cls.highgui.pas',
  ocv.cls.objdetect in '..\..\..\source\classes\ocv.cls.objdetect.pas',
  ocv.cls.types in '..\..\..\source\Classes\ocv.cls.types.pas';

Var
  P         : IFaceRecognizer;
  f         : TInputArrayOfIplImage;
  l         : TInputArrayOfInteger;
  i         : pIplImage;
  lab       : Integer;
  confidence: double;

begin
  try
    P := TFaceRecognizer.createEigenFaceRecognizer;
    SetLength(f, 6);
    SetLength(l, 6);
    f[0] := cvLoadImage(cResourceFaces + 's1\1.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[0] := 0;
    f[1] := cvLoadImage(cResourceFaces + 's1\2.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[1] := 0;
    f[2] := cvLoadImage(cResourceFaces + 's1\3.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[2] := 0;
    f[3] := cvLoadImage(cResourceFaces + 's2\1.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[3] := 1;
    f[4] := cvLoadImage(cResourceFaces + 's2\2.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[4] := 1;
    f[5] := cvLoadImage(cResourceFaces + 's2\3.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    l[5] := 1;
    P.train(f, l);
    i := cvLoadImage(cResourceFaces + 's1\5.pgm', CV_LOAD_IMAGE_GRAYSCALE);
    P.predict(i,lab,confidence);
    Writeln('Label: ', lab);
    Writeln('Confidence: ',confidence:3:1);
    P := nil;

    //need free f array

    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

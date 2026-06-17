program FeatureDetector;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.core.types_c,
  ocv.highgui_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.lib,
  uResourcePaths,
  ocv.cls.contrib,
  ocv.cls.core,
  ocv.cls.highgui,
  ocv.cls.objdetect,
  ocv.cls.types,
  ocv.cls.features2d;

Var
  P: TFeatureDetector;
  D: TDescriptorExtractor;
  i: pIplImage;
  keypoints: TKeyPointArray;
  M: TMat;

begin
  try
    Writeln('Loadimage ',cResourceMedia + 'box.png');
    i := cvLoadImage(cResourceMedia + 'box.png', CV_LOAD_IMAGE_GRAYSCALE);
    Writeln('Use feature detector ',cfdGridFAST);
    P := TFeatureDetector.Create(cfdGridFAST);
    P.detect(i, keypoints, nil);
    Writeln('Found ',Length(keypoints),' keypoint');
    Writeln('Use descriptor extractor ',denSURF);
    D := TDescriptorExtractor.Create(denSURF);
    D.compute(i, keypoints, M);
    Writeln('Found ',M.total,' descriptors');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

program Weighted;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  core_c in '..\..\include\ñore\core_c.pas',
  Core.types_c in '..\..\include\ñore\Core.types_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas';

Const
  filename_src1 = 'cat2-mirror.jpg';
  filename_src2 = 'cat2.jpg';

Var
  alpha: Double = 0.5;
  beta, input: Double;
  src1, src2, dst: pIplImage;

begin
  src1 := cvLoadImage(filename_src1);
  src2 := cvLoadImage(filename_src2);
  cvNamedWindow('Linear Blend', CV_WINDOW_AUTOSIZE);
  beta := (1.0 - alpha);
  dst := cvCloneImage(src1);
  cvAddWeighted(src1, alpha, src2, beta, 0, dst);
  cvShowImage('Linear Blend', dst);
  cvwaitKey(0);
  cvReleaseImage(src1);
  cvReleaseImage(src2);
  cvReleaseImage(dst);
  cvDestroyAllWindows;
end.

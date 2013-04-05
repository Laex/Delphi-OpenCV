// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_AddWeighted;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  filename_src1 = 'Resource\cat2-mirror.jpg';
  filename_src2 = 'Resource\cat2.jpg';

Var
  alpha: Double = 0.5;
  beta, input: Double;
  src1, src2, dst: pIplImage;

begin
  try
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
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

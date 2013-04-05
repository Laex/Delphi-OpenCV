// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_AddWeighted;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
uLibName in '..\..\..\include\uLibName.pas',
highgui_c in '..\..\..\include\highgui\highgui_c.pas',
core_c in '..\..\..\include\ñore\core_c.pas',
Core.types_c in '..\..\..\include\ñore\Core.types_c.pas',
imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
legacy in '..\..\..\include\legacy\legacy.pas',
calib3d in '..\..\..\include\calib3d\calib3d.pas',
imgproc in '..\..\..\include\imgproc\imgproc.pas',
haar in '..\..\..\include\objdetect\haar.pas',
objdetect in '..\..\..\include\objdetect\objdetect.pas',
tracking in '..\..\..\include\video\tracking.pas',
Core in '..\..\..\include\ñore\core.pas'
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

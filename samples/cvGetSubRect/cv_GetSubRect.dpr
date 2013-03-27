// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_GetSubRect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\ñore\Core.types_c.pas',
  core_c in '..\..\include\ñore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\include\imgproc\imgproc_c.pas',
  LibName in '..\..\include\LibName.pas';

Const
  filename = 'Resource\opencv_logo_with_text.png';

Var
  image: pIplImage = nil;
  cut_image: pIplImage = nil;
  R:TCvRect;
begin
  try
    image := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    R:=CvRect(50,50,250,250);
    cut_image:=cvCreateImage(CvSize(0,0), image^.depth, image^.nChannels);
    cut_image := cvGetSubRect(image,cut_Image,R);
    cvNamedWindow(filename, CV_WINDOW_AUTOSIZE);
    cvNamedWindow('Cut image', CV_WINDOW_AUTOSIZE);
    cvRectangle(image,CvPoint(50,50),CvPoint(50+250,50+250),CvScalar(150));
    cvShowImage(filename, image);
    cvShowImage('Cut image', cut_image);
    cvWaitKey(0);
    cvReleaseImage(image);
    cvReleaseImage(cut_image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

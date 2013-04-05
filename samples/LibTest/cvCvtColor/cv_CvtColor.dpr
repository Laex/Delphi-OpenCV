// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_CvtColor;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  filename = 'Resource\opencv_logo_with_text.png';
  filename_gray = 'Resource\opencv_logo_with_text_gray.png';

Var
  image: pIplImage = nil;
  gray_image: pIplImage = nil;

begin
  try
    image := cvLoadImage(filename, 1);
    gray_image := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    cvCvtColor(image, gray_image, CV_RGB2GRAY);
    cvSaveImage(filename_gray, gray_image);
    cvNamedWindow(filename, CV_WINDOW_AUTOSIZE);
    cvNamedWindow('Gray image', CV_WINDOW_AUTOSIZE);
    cvShowImage(filename, image);
    cvShowImage('Gray image', gray_image);
    cvWaitKey(0);
    cvReleaseImage(image);
    cvReleaseImage(gray_image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

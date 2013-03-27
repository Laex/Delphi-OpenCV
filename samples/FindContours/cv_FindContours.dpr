// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_FindContours;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\include\сore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\include\imgproc\imgproc_c.pas',
  LibName in '..\..\include\LibName.pas';

Const
  filename = 'Resource\opencv_logo_with_text_sm.png';

Var
  image: pIplImage = nil;
  dst: pIplImage = nil;
  img_gray: pIplImage = Nil;
  contours: pCvSeq=nil;
  storage: pCvMemStorage = nil;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
    if Assigned(image) then
    begin
      cvNamedWindow('Source image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Gray image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Threshold image', CV_WINDOW_AUTOSIZE);
      cvNamedWindow('Contour image', CV_WINDOW_AUTOSIZE);
      cvShowImage('Source image', image);
      // Создаем изображение в градациях серого
      img_gray := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      dst := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
      cvCvtColor(image, img_gray, CV_BGR2GRAY);
      cvShowImage('Gray image', img_gray);
      storage := cvCreateMemStorage(0);
      cvThreshold(img_gray, dst, 128, 255, CV_THRESH_BINARY_INV);
      // cvAdaptiveThreshold(img_gray, img_gray, 255, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 21, 7);
      cvShowImage('Threshold image', dst);
      contours:=AllocMem(SizeOf(TCvSeq));
      cvFindContours(dst, storage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
      cvDrawContours(image, contours, CV_RGB(100, 200, 0), CV_RGB(200, 100, 0), 2, 2, CV_AA, cvPoint(0, 0));
      cvShowImage('Contour image', image);
      cvWaitKey(0);
      cvDestroyAllWindows;
      cvReleaseImage(image);
      cvReleaseImage(img_gray);
      cvReleaseImage(dst);
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

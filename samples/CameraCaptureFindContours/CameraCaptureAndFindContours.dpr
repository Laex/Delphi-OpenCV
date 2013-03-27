// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program CameraCaptureAndFindContours;

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

Var
  capture: PCvCapture;
  width: Double;
  height: Double;
  frame: PIplImage;
  gframe: PIplImage = nil;
  counter: Integer;
  filename: pCVChar;
  cc: Integer;
  storage: pCvMemStorage = nil;
  contours: pCvSeq = nil;
  h_next, c: pCvSeq;

begin
  try
    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY); // cvCaptureFromCAM( 0 );
    if not Assigned(capture) then
      Halt;
    // узнаем ширину и высоту кадра
    width := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH);
    height := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT);
    WriteLn(Format('[i] %.0f x %.0f', [width, height]));
    frame := Nil;
    cvNamedWindow('Capture', CV_WINDOW_AUTOSIZE);
    WriteLn('[i] press Enter for capture image and Esc for quit!');
    counter := 0;
    filename := AllocMem(512);

    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);
      // показываем
      if not Assigned(gframe) then
      begin
        gframe := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, 1);
        storage := cvCreateMemStorage(0);
        contours := AllocMem(SizeOf(TCvSeq));
      end;
      cvCvtColor(frame, gframe, CV_BGR2GRAY);
      cvThreshold(gframe, gframe, 128, 255, CV_THRESH_BINARY_INV);
      cvFindContours(gframe, storage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,
        cvPoint(0, 0));
      contours := cvApproxPoly(contours, SizeOf(TCvContour), storage, CV_POLY_APPROX_DP, 5, 1);

      cvDrawContours(frame, contours, CV_RGB(255, 0, 0), CV_RGB(0, 255, 0), 2, 2, CV_AA, cvPoint(0, 0));
      cvShowImage('capture', frame);

      cc := cvWaitKey(33);
      if (cc = 27) then
        Break
      else if (cc = 13) then
      begin
        // сохраняем кадр в файл
        filename := pCVChar(AnsiString(Format('Image %d.jpg'#0, [counter])));
        WriteLn('[i] capture - ', filename);
        cvSaveImage(filename, frame);
        Inc(counter);
      end;
    end;
    // освобождаем ресурсы
    cvDestroyWindow('capture');
    cvReleaseCapture(capture);
    cvReleaseImage(gframe);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

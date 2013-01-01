{$APPTYPE CONSOLE}
program OpenCV_2_LoadVideo;

uses
  core_c in '..\..\include\сore\core_c.pas',
  core.types_c in '..\..\include\сore\core.types_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas';

const
  // declare video filename
  VIDEO_FILE_NAME = '768x576.avi';
  // declare escape char
  ESCAPE_CHAR = 27;

var
  // declate an opencv video file capture object, users can use this object to
  // interact with the video file, get/set properties and also grab video frames
  capture: pCVCapture;
  // declare an opencv image pointer variable
  frame: pIplImage;
  // declate a key variable to get user key pressed
  key: integer;
begin
  // create capture object for specific video filename
  capture := cvCreateFileCapture(VIDEO_FILE_NAME);
  // check if capture assigned, if not then it means that opencv can not open
  // the requested file, opencv uses VFW to open the files so some file formats
  // are not fully supported (can be enhence with FFMPEG support but this is
  // beyond our scope right now
  if Assigned(capture) then
  begin
    // create display window
    cvNamedWindow('video');
    // reset user key pressed
    key := 0;
    // loop while user didn't press ESCAPE
    while key <> ESCAPE_CHAR do
    begin
      // start grabbing frames, single frame at a time, this method is a
      // combination of two methods one grab frame and the other advances to the
      // next frame
      //
      // REMARK: do not (!!!) release "frame" object since it is used internally
      // by the capture object is its memory is release when the "capture" object
      // is being destroyed
      frame := cvQueryFrame(capture);
      // check if frame is valid, when you reach end-of-file "frame" object
      // starts to be NIL (since the method advances to the next frame each time
      // is is called...)
      if Assigned(frame) then
        // show frame in window
        cvShowImage('video', frame);
      // wait for user key, delay is in msec so 1000msec = 1sec wait period
      key := cvWaitKey(1000);
    end;
    // destroy display windows
    cvDestroyAllWindows;
    // release "capture" object and its related memory
    cvReleaseCapture(capture);
  end;
end.

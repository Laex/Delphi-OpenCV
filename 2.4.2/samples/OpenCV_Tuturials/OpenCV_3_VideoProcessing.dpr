program OpenCV_3_VideoProcessing;

{$APPTYPE CONSOLE}

uses
  core_c in '..\..\include\ñore\core_c.pas',
  Core.types_c in '..\..\include\ñore\Core.types_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\include\imgproc\imgproc_c.pas';

const
  // declare video filename
  VIDEO_FILE_NAME = 'clock.avi';
  // declare escape char
  ESCAPE_CHAR = 27;

var
  // declate an opencv video file capture object, users can use this object to
  // interact with the video file, get/set properties and also grab video frames
  capture: pCVCapture;
  // declare an opencv image pointer variable
  frame: pIplImage;
  // declate gray and threshold images
  gray, threshold: pIplImage;
  // declare a key variable to get user key pressed
  key: integer;
  // declate state variable
  need_creation: boolean;

  C: TCvSize;
  P: pCvArr;

begin
  // set state variable
  need_creation := true;
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
    // create display window
    cvNamedWindow('threshold');
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
      begin
        // check if needs creation
        if need_creation then
        begin
          // create images at the same size as frame (using cvGetSize) but with
          // a different color depth, we want erode the image and that can be
          // done only in greyscale so we need to create a single 8bit channel
          // image but both the conversion and the threshold images
          // P:=pCvArr(@frame);
          C := cvGetSize(frame);
          gray := cvCreateImage(C, IPL_DEPTH_8U, 1);
          threshold := cvCreateImage(C, IPL_DEPTH_8U, 1);
          // reset state variable
          need_creation := false;
        end;
        // convert frame into greyscale, this function can convert to many
        // different color types, see third parameter consts for more details
        cvCvtColor(frame, gray, CV_RGB2GRAY);
        // threshold the image, there are many threshold methods and options
        // but here we use a simple one, binary fixed threshold, see third
        // parameter consts for more options
        cvThreshold(gray, threshold, 128, 255, CV_THRESH_BINARY);
        // for example: replace this line with the above line and see what it does
        // cvThreshold(gray,threshold,128,255,CV_THRESH_BINARY_INV);

        // show frame in window
        cvShowImage('video', frame);
        // show erode in second window
        cvShowImage('threshold', threshold);
      end; // if
      // wait for user key, delay is in msec so 1000msec = 1sec wait period
      key := cvWaitKey(1000);
    end;
    // destroy display windows
    cvDestroyAllWindows;
    // release "capture" object and its related memory
    cvReleaseCapture(capture);
  end;

end.

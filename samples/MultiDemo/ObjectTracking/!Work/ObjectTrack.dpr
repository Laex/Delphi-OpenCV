// *****************************************************************
// Delphi-OpenCV Class Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
  // Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the 'License'); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

program ObjectTrack;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.Math,
  System.SysUtils,
  System.Classes,
  core_c,
  highgui_c,
  imgproc_c,
  imgproc.types_c,
  core.types_c,
  cvUtils;

// default capture width and height
const
  FRAME_WIDTH  = 640;
  FRAME_HEIGHT = 480;
  // max number of objects to be detected in frame
  MAX_NUM_OBJECTS = 50;
  // minimum and maximum object area
  MIN_OBJECT_AREA = 20 * 20;
  MAX_OBJECT_AREA = FRAME_HEIGHT * FRAME_WIDTH / 1.5;
  // names that will appear at the top of each window
  windowName         = 'Original Image';
  windowName1        = 'HSV Image';
  windowName2        = 'Thresholded Image';
  windowName3        = 'After Morphological Operations';
  trackbarWindowName = 'Trackbars';

Var
  // initial min and max HSV filter values.
  // these will be changed using trackbars
  H_MIN : integer = 170; // 0;
  H_MAX : integer = 180;
  CH_MAX: integer = 256;
  S_MIN : integer = 160; // 0;
  S_MAX : integer = 256;
  CS_MAX: integer = 256;
  V_MIN : integer = 60; // 0;
  V_MAX : integer = 256;
  CV_MAX: integer = 256;

procedure on_trackbar(i: integer); cdecl;
begin
  // This function gets called whenever a
  // trackbar position is changed
end;

procedure createTrackbars;
begin
  // create window for trackbars
  cvNamedWindow(
    trackbarWindowName,
    0);

  /// /create memory to store trackbar name on window
  // char TrackbarName[50];
  // sprintf( TrackbarName, 'H_MIN', H_MIN);
  // sprintf( TrackbarName, 'H_MAX', H_MAX);
  // sprintf( TrackbarName, 'S_MIN', S_MIN);
  // sprintf( TrackbarName, 'S_MAX', S_MAX);
  // sprintf( TrackbarName, 'V_MIN', V_MIN);
  // sprintf( TrackbarName, 'V_MAX', V_MAX);

  // create trackbars and insert them into window
  // 3 parameters are: the address of the variable that is changing when the trackbar is moved(eg.H_LOW),
  // the max value the trackbar can move (eg. H_HIGH),
  // and the function that is called whenever the trackbar is moved(eg. on_trackbar)
  // ---->    ---->     ---->
  cvCreateTrackbar(
    'H_MIN',
    trackbarWindowName,
    @H_MIN,
    CH_MAX,
    on_trackbar);
  cvCreateTrackbar(
    'H_MAX',
    trackbarWindowName,
    @H_MAX,
    CH_MAX,
    on_trackbar);
  cvCreateTrackbar(
    'S_MIN',
    trackbarWindowName,
    @S_MIN,
    CS_MAX,
    on_trackbar);
  cvCreateTrackbar(
    'S_MAX',
    trackbarWindowName,
    @S_MAX,
    CS_MAX,
    on_trackbar);
  cvCreateTrackbar(
    'V_MIN',
    trackbarWindowName,
    @V_MIN,
    CV_MAX,
    on_trackbar);
  cvCreateTrackbar(
    'V_MAX',
    trackbarWindowName,
    @V_MAX,
    CV_MAX,
    on_trackbar);
end;

procedure drawObject(x: integer; y: integer; frame: pIplImage);
Var
  font: TCvFont;
begin
  // use some of the openCV drawing functions to draw crosshairs
  // on your tracked image!
  // added 'if' and 'else' statements to prevent
  // memory errors from writing off the screen (ie. (-25,-25) is not within the window!)
  cvCircle(
    frame,
    cvPoint(x, y),
    20,
    cvScalar(0, 255, 0),
    2);
  if (y - 25) > 0 then
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x, y - 25),
      cvScalar(0, 255, 0),
      2)
  else
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x, 0),
      cvScalar(0, 255, 0),
      2);
  if (y + 25) < FRAME_HEIGHT then
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x, y + 25),
      cvScalar(0, 255, 0),
      2)
  else
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x, FRAME_HEIGHT),
      cvScalar(0, 255, 0),
      2);
  if (x - 25) > 0 then
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x - 25, y),
      cvScalar(0, 255, 0),
      2)
  else
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(0, y),
      cvScalar(0, 255, 0),
      2);
  if (x + 25) < FRAME_WIDTH then
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(x + 25, y),
      cvScalar(0, 255, 0),
      2)
  else
    cvLine(
      frame,
      cvPoint(x, y),
      cvPoint(FRAME_WIDTH, y),
      cvScalar(0, 255, 0),
      2);

  cvInitFont(
    @font,
    CV_FONT_HERSHEY_COMPLEX,
    1.0,
    1.0,
    0,
    1,
    CV_AA);

  cvPutText(
    frame,
    c_str(intToStr(x) + ',' + intToStr(y)),
    cvPoint(x, y + 30),
    @font,
    cvScalar(0, 255, 0));
end;

procedure morphOps(thresh: pIplImage);
Var
  erodeElement, dilateElement: pIplConvKernel;
begin
  // create structuring element that will be used to "dilate" and "erode" image.
  // the element chosen here is a 3px by 3px rectangle
  erodeElement := cvCreateStructuringElementEx(
    3,
    3,
    1,
    1,
    CV_SHAPE_RECT);
  // dilate with larger element so make sure object is nicely visible
  dilateElement := cvCreateStructuringElementEx(
    8,
    8,
    3,
    3,
    CV_SHAPE_RECT);
  cvErode(
    thresh,
    thresh,
    erodeElement,
    2);
  cvDilate(
    thresh,
    thresh,
    dilateElement,
    2);
  cvReleaseStructuringElement(erodeElement);
  cvReleaseStructuringElement(dilateElement);
end;

Type
  TContours  = TArray<TArray<pCvPoint2D32f>>;
  TVec4<T>   = array [0 .. 3] of T;
  TVec4i     = TVec4<integer>;
  THierarchy = TArray<TVec4i>;

function Vec4i(const v0, v1, v2, v3: integer): TVec4i;
begin
  Result[0] := v0;
  Result[1] := v1;
  Result[2] := v2;
  Result[3] := v3;
end;

procedure cfindContours(image: pIplImage; Var _contours: TContours; Var _hierarchy: THierarchy; mode, method: integer;
  offset: TcvPoint);
Var
  storage         : pCvMemStorage;
  _cimage         : pIplImage;
  _ccontours      : pCvSeq;
  all_contours, ps: pCvSeq;
  j, i, total     : integer;
  tree_iterator   : TCvTreeNodeIterator;
  h_next,   //
    h_prev, //
    v_next, //
    v_prev: integer;
  reader  : TCvSeqReader;
  Count   : integer;
begin
  storage    := cvCreateMemStorage(0);
  _cimage    := cvCloneImage(image);
  _ccontours := nil;
  // SetLength(
  // _hierarchy,
  // 0);
  cvClearMemStorage(storage);  
  cvFindContours(
    _cimage,
    storage,
    @_ccontours,
    sizeof(TCvContour),
    mode,
    method,
    offset);
  if not Assigned(_ccontours) then
  begin
    cvReleaseMemStorage(storage);
    cvReleaseImage(_cimage);
    SetLength(
      _contours,
      0);
    Exit;
  end;
  all_contours := cvTreeToNodeSeq(
    _ccontours,
    sizeof(TCvSeq),
    storage);
  ps    := all_contours;
  total := 0;
  While Assigned(ps) do
  begin
    Inc(total);
    ps := ps^.h_next;
  end;
  SetLength(
    _contours,
    total);
  SetLength(
    _hierarchy,
    total);

  cvInitTreeNodeIterator(
    tree_iterator,
    all_contours,
    3);
  ps := cvNextTreeNode(@tree_iterator);
  j  := 0;
  while Assigned(ps) do
  begin
    // pCvContour(ps)^.color := i;
    Count := ps^.total;
    cvStartReadSeq(
      ps,
      @reader);
    if not CV_IS_SEQ_CLOSED(ps) then
      Count := Count - 1;
    SetLength(
      _contours[j],
      Count);

    for i := 0 to Count - 1 do
    begin
      CV_READ_SEQ_ELEM(
        @_contours[j][i],
        reader,
        sizeof(_contours[j][0]));
    end;
    ps := cvNextTreeNode(@tree_iterator);
    Inc(j);
  end;
  // for i := 0 to total - 1 do
  // begin
  // ps                    := cvNextTreeNode(@tree_iterator);
  // pCvContour(ps)^.color := i;
  // Count                 := ps^.total;
  // cvStartReadSeq(
  // ps,
  // @reader);
  // if not CV_IS_SEQ_CLOSED(ps) then
  // Count := Count - 1;
  // CV_READ_SEQ_ELEM(
  // pt,
  // reader);
  //
  // _contours[i] := AllocMem(ps^.total * sizeof(TCvPoint2D32f));
  // // Mat ci = _contours.getMat(i);
  // // return (flags & Mat::CONTINUOUS_FLAG) != 0;
  // // CV_Assert(ci.isContinuous());
  // if ps^.h_next <> nil then
  // cvCvtSeqToArray(
  // ps,
  // @_contours[i],
  // CV_WHOLE_SEQ);
  //
  // h_next := ifthen(
  // Assigned(ps^.h_next),
  // pCvContour(ps^.h_next)^.color,
  // -1);
  // h_prev := ifthen(
  // Assigned(ps^.h_prev),
  // pCvContour(ps^.h_prev)^.color,
  // -1);
  // v_next := ifthen(
  // Assigned(ps^.v_next),
  // pCvContour(ps^.v_next)^.color,
  // -1);
  // v_prev := ifthen(
  // Assigned(ps^.v_prev),
  // pCvContour(ps^.v_prev)^.color,
  // -1);
  // _hierarchy[i] := Vec4i(
  // h_next,
  // h_prev,
  // v_next,
  // v_prev);
  // end;
end;

Var
  // Matrix to store each frame of the webcam feed
  cameraFeed: pIplImage;

procedure findContours(const image: pIplImage; var contours: TContours; Var hierarchy: THierarchy;
  mode, method: integer);
Var
  _hierarchy  : pCvMemStorage;
  _contours   : pCvSeq;
  dst_th      : pIplImage;
  contoursCont: integer;
  l_pt0, l_pt1: TcvPoint;
  l_reader    : TCvSeqReader;
  l_iterator  : TCvTreeNodeIterator;
  i           : integer;
begin
  _hierarchy := cvCreateMemStorage(0);
  dst_th     := cvCreateImage(
    cvGetSize(image),
    IPL_DEPTH_8U,
    1);

  cvThreshold(
    image,
    dst_th,
    128,
    255,
    // CV_THRESH_BINARY_INV
    CV_THRESH_BINARY);

  cvSmooth(
    dst_th,      // function input
    dst_th,      // function output
    CV_GAUSSIAN, // use Gaussian filter (average nearby pixels, with closest pixels weighted more)
    9,           // smoothing filter window width
    9);          // smoothing filter window height

  _contours := nil;

  SetLength(
    contours,
    0);
  SetLength(
    hierarchy,
    0);

  cvClearMemStorage(_hierarchy);
  contoursCont := cvFindContours(
    dst_th,
    _hierarchy,
    @_contours,
    sizeof(TCvContour),
    mode,
    method,
    cvPoint(0, 0));

  // cvInitTreeNodeIterator(
  // l_iterator,
  // _contours,
  // 1);
  // _contours := cvNextTreeNode(@l_iterator);
  // while Assigned(_contours) do
  // begin
  // cvStartReadSeq(
  // _contours,
  // @l_reader,
  // 0);
  // // Первая точка
  // CV_READ_SEQ_ELEM(
  // @l_pt0,
  // l_reader,
  // sizeof(l_pt0));
  // // l_pts1.push_back(l_pt0);
  // for i := 0 to _contours^.total - 1 do
  // begin
  // // Последующие точки
  // CV_READ_SEQ_ELEM(
  // @l_pt1,
  // l_reader,
  // sizeof(l_pt0));
  // cvLine(
  // cameraFeed,
  // l_pt0,
  // l_pt1,
  // CV_RGB(0, 255, 0),
  // 2);
  // l_pt0 := l_pt1;
  // // l_pts1.push_back(l_pt0);
  // end;
  // _contours := cvNextTreeNode(@l_iterator);
  // end;

  if contoursCont > 0 then
    While Assigned(_contours) do
    begin
      // if CV_IS_SEQ_CLOSED(_contours) then
      cvDrawContours(
        cameraFeed,
        _contours,
        CV_RGB(52, 201, 36),
        CV_RGB(36, 201, 197),
        -1,
        CV_FILLED,
        CV_AA,
        cvPoint(0, 0)); // рисуем контур
      _contours := _contours^.h_next;
    end;

  cvReleaseMemStorage(_hierarchy);
  cvReleaseImage(dst_th);
end;

procedure trackFilteredObject(Var x, y: integer; threshold: pIplImage; var cameraFeed: pIplImage);
Var
  temp, dst: pIplImage;
  index    : integer;
  // these two vectors needed for output of findContours
  contours   : TContours;
  hierarchy  : THierarchy;
  refArea    : double;
  objectFound: Boolean;
  numObjects : integer;
  area       : double;
  // moment     : IMoments;
begin
  // find contours of filtered image using openCV findContours function
  findContours(
    threshold,
    contours,
    hierarchy,
    CV_RETR_EXTERNAL,
    CV_CHAIN_APPROX_SIMPLE);

  // use moments method to find our filtered object
  refArea     := 0;
  objectFound := false;
  //
  // // if hierarchy.block_size>0 then
  // begin
  // numObjects := Length(hierarchy);
  // // if number of objects greater than MAX_NUM_OBJECTS we have a noisy filter
  // if (numObjects < MAX_NUM_OBJECTS) then
  // begin
  // index := 0;
  // While index >= 0 do
  // begin
  // moment := Moments((cv: : Mat)contours[index]);
  // area   := moment.m00;
  // // if the area is less than 20 px by 20px then it is probably just noise
  // // if the area is the same as the 3/2 of the image size, probably just a bad filter
  // // we only want the object with the largest area so we safe a reference area each
  // // iteration and compare it to the area in the next iteration.
  // if (area > MIN_OBJECT_AREA) and (area < MAX_OBJECT_AREA) and (area > refArea) then
  // begin
  // x := moment.m10 / area;
  // y := moment.m01 / area;
  // objectFound = true;
  // end
  // else
  // objectFound = false;
  // index := hierarchy[index][0];
  // end;
  // // let user know you found an object
  // if (objectFound = = true) then
  // begin
  // putText(
  // cameraFeed,
  // 'Tracking Object',
  // cvPoint(0, 50),
  // 2,
  // 1,
  // cvScalar(0, 255, 0),
  // 2);
  // // draw object location on screen
  // drawObject(
  // x,
  // y,
  // cameraFeed);
  // end;
  //
  // end
  // else
  // putText(
  // cameraFeed,
  // 'TOO MUCH NOISE! ADJUST FILTER',
  // cvPoint(0, 50),
  // 1,
  // 2,
  // cvScalar(0, 0, 255),
  // 2);
  // end;
end;

Var
  // some boolean variables for different functionality within this program
  trackObjects: Boolean = true;
  useMorphOps : Boolean = true;
  // matrix storage for HSV image
  HSV: pIplImage = nil;
  // matrix storage for binary threshold image
  threshold: pIplImage = nil;
  // x and y values for the location of the object
  x: integer = 0;
  y: integer = 0;
  // video capture object to acquire webcam feed
  capture: pCvCapture;

begin
  try
    // create slider bars for HSV filtering
    createTrackbars();
    // open capture object at location zero (default location for webcam)
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    Assert(Assigned(capture));
    // set height and width of capture frame
    cvSetCaptureProperty(
      capture,
      CV_CAP_PROP_FRAME_WIDTH,
      FRAME_WIDTH);
    cvSetCaptureProperty(
      capture,
      CV_CAP_PROP_FRAME_HEIGHT,
      FRAME_HEIGHT);
    // start an infinite loop where webcam feed is copied to cameraFeed matrix
    // all of our operations will be performed within this loop
    while true do
    begin
      // store image to matrix
      repeat
        cameraFeed := cvQueryFrame(capture);
      until Assigned(cameraFeed);
      // convert frame from BGR to HSV colorspace
      if not Assigned(HSV) then
        HSV := cvCreateImage(
          cvGetSize(cameraFeed),
          8,
          3);
      cvCvtColor(
        cameraFeed,
        HSV,
        CV_BGR2HSV);
      // filter HSV image between values and store filtered image to
      // threshold matrix
      if not Assigned(threshold) then
        threshold := cvCreateImage(
          cvGetSize(cameraFeed),
          8,
          1);
      cvInRangeS(
        HSV,
        cvScalar(H_MIN, S_MIN, V_MIN),
        cvScalar(H_MAX, S_MAX, V_MAX),
        threshold);
      // perform morphological operations on thresholded image to eliminate noise
      // and emphasize the filtered object(s)
      if useMorphOps then
        morphOps(threshold);
      // pass in thresholded frame to our object tracking function
      // this function will return the x and y coordinates of the
      // filtered object
      if trackObjects then
        trackFilteredObject(
          x,
          y,
          threshold,
          cameraFeed);

      // show frames
      cvShowImage(
        windowName2,
        threshold);
      cvShowImage(
        windowName,
        cameraFeed);
      cvShowImage(
        windowName1,
        HSV);

      // delay 30ms so that screen can refresh.
      // image will not appear without this waitKey() command
      if cvWaitKey(30) = 27 then
        Break;
    end;

    if Assigned(capture) then
      cvReleaseCapture(capture);

    if Assigned(HSV) then
      cvReleaseImage(HSV);
    if Assigned(threshold) then
      cvReleaseImage(threshold);

  except
    on E: Exception do
      WriteLn(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.

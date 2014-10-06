// *****************************************************************
// Delphi-OpenCV Demo
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
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************
// Original: http://fivedots.coe.psu.ac.th/~ad/jg/nui055/index.html
// *******************************************************************

program HandsDetect4;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

Const
  IMG_SCALE: Integer = 2; // scaling applied to webcam image

  SMALLEST_AREA: Single = 600.0; // was 100.0f;
  // ignore smaller contour areas

  MAX_POINTS: Integer = 20; // max number of points stored in an array

  // used for simiplifying the defects list
  MIN_FINGER_DEPTH: Integer = 20;
  MAX_FINGER_ANGLE: Integer = 60; // degrees

  // angle ranges of thumb and index finger of the left hand relative to its COG
  MIN_THUMB: Integer = 120;
  MAX_THUMB: Integer = 200;

  MIN_INDEX: Integer = 60;
  MAX_INDEX: Integer = 120;

Type
  TFingerName = (LITTLE, RING, MIDDLE, INDEX, THUMB, UNKNOWN);

const
  FingerName: array [TFingerName] of AnsiString = ('LITTLE', 'RING', 'MIDDLE', 'INDEX', 'THUMB', 'UNKNOWN');

Var
  capture: PCvCapture;
  frame: pIplImage;

  // HSV ranges defining the glove colour
  hueLower, hueUpper, satLower, satUpper, briLower, briUpper: Integer;

  // JavaCV elements
  scaleImg: pIplImage; // for resizing the webcam image
  hsvImg: pIplImage; // HSV version of webcam image
  imgThreshed: pIplImage; // threshold for HSV settings
  contourStorage, approxStorage, hullStorage, defectsStorage: pCvMemStorage;

  msgFont: TcvFont;

  // hand details
  cogPt: TcvPoint; // center of gravity (COG) of contour
  contourAxisAngle: Integer;
  // contour's main axis angle relative to the horizontal (in degrees)

  // defects data for the hand contour
  tipPts, foldPts: TArray<TcvPoint>;
  depths: TArray<Single>;
  fingerTips: TArray<TcvPoint>;
  // finger identifications
  // private ArrayList<FingerName> namedFingers;
  namedFingers: TArray<TFingerName>;

procedure setHSVRanges(const fnm: String);
(*read in three lines to set the lower/upper HSV ranges for the user's glove.
  These were previously stored using the HSV Selector application
  (see NUI chapter 5 on blobs drumming).*)
begin

  // hue: 95 178
  hueLower := 0;//95;
  hueUpper := 20;//178;
  // sat: 0 255
  satLower := 30;//0;
  briUpper := 150;//255;
  // val: 0 41
  briLower := 80;//0;
  briUpper := 255;//41;
  // BufferedReader in = new BufferedReader(new FileReader(fnm));
  // String line = in.readLine();   // get hues
  // String[] toks = line.split("\\s+");
  // hueLower = Integer.parseInt(toks[1]);
  // hueUpper = Integer.parseInt(toks[2]);
  //
  // line = in.readLine();   // get saturations
  // toks = line.split("\\s+");
  // satLower = Integer.parseInt(toks[1]);
  // satUpper = Integer.parseInt(toks[2]);
  //
  // line = in.readLine();   // get brightnesses
  // toks = line.split("\\s+");
  // briLower = Integer.parseInt(toks[1]);
  // briUpper = Integer.parseInt(toks[2]);
  //
  // in.close();
  // System.out.println("Read HSV ranges from " + fnm);
  // }
  // catch (Exception e)
  // {  System.out.println("Could not read HSV ranges from " + fnm);
  // System.exit(1);
  // }
end; // end of setHSVRanges()

procedure HandDetector(const hsvFnm: String; const width, height: Integer);
begin
  scaleImg := cvCreateImage(CvSize(width div IMG_SCALE, height div IMG_SCALE), 8, 3);
  hsvImg := cvCreateImage(CvSize(width div IMG_SCALE, height div IMG_SCALE), 8, 3); // for the HSV image
  imgThreshed := cvCreateImage(CvSize(width div IMG_SCALE, height div IMG_SCALE), 8, 1); // threshold image

  // storage for contour, hull, and defect calculations by OpenCV
  contourStorage := cvCreateMemStorage(0);
  approxStorage := cvCreateMemStorage(0);
  hullStorage := cvCreateMemStorage(0);
  defectsStorage := cvCreateMemStorage(0);

  cvInitFont(@msgFont, CV_FONT_HERSHEY_SIMPLEX, 1.0, 1.0, 0, 5, CV_AA);

  // cogPt = new Point();
  SetLength(fingerTips, 0);
  // namedFingers = new ArrayList<FingerName>();
  SetLength(tipPts, MAX_POINTS); // coords of the finger tips
  SetLength(foldPts, MAX_POINTS); // coords of the skin folds between fingers
  SetLength(depths, MAX_POINTS); // distances from tips to folds
  setHSVRanges(hsvFnm);
end; // end of HandDetector()

function findBiggestContour(const imgThreshed: pIplImage): pCvSeq;
// return the largest contour in the threshold image
Var
  bigContour, contours: pCvSeq;
  maxArea: Single;
  maxBox: pCvBox2D;
  box: TCvBox2D;
  size: TCvSize2D32f;
  area: Single;
  n:Integer;
begin
  bigContour := nil;

  // generate all the contours in the threshold image as a list
  contours := AllocMem(SizeOf(TCvSeq));
  cvClearMemStorage(contourStorage);
  n:=cvFindContours(imgThreshed, contourStorage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

  // find the largest contour in the list based on bounded box size
  maxArea := SMALLEST_AREA;
  maxBox := nil;
  while Assigned(contours) do
  begin
    if (contours^.elem_size > 0) then
    begin
      box := cvMinAreaRect2(contours, contourStorage);
      if (box.size.width <> 0) and (box.size.height <> 0) then
      begin
        size := box.size;
        area := size.width * size.height;
      end;
      if (area > maxArea) then
      begin
        maxArea := area;
        bigContour := contours;
      end;
    end;
    contours := contours^.h_next;
  end;
  Result := bigContour;
end; // end of findBiggestContour()

function calculateTilt(const m11, m20, m02: double): Integer;
(*Return integer degree angle of contour's major axis relative to the horizontal,
  assuming that the positive y-axis goes down the screen.

  This code is based on maths explained in "Simple Image Analysis By Moments", by
  Johannes Kilian, March 15, 2001 (see Table 1 on p.7).
  The paper is available at:
  http://public.cranfield.ac.uk/c5354/teaching/dip/opencv/SimpleImageAnalysisbyMoments.pdf
*)
Var
  diff, theta: double;
  tilt: Integer;
begin
  diff := m20 - m02;
  if (diff = 0) then
    if (m11 = 0) then
      Exit(0)
    else if (m11 > 0) then
      Exit(45)
    else // m11 < 0
      Exit(-45);

  theta := 0.5 * ArcTan2(2 * m11, diff);
  tilt := round(RadToDeg(theta));

  if ((diff > 0)) and ((m11 = 0)) then
    Exit(0)
  else if ((diff < 0)) and ((m11 = 0)) then
    Exit(-90)
  else if ((diff > 0)) and ((m11 > 0)) then // 0 to 45 degrees
    Exit(tilt)
  else if ((diff > 0)) and ((m11 < 0)) then // -45 to 0
    Exit(180 + tilt) // change to counter-clockwise angle measure
  else if ((diff < 0)) and ((m11 > 0)) then // 45 to 90
    Exit(tilt)
  else if ((diff < 0)) and ((m11 < 0)) then // -90 to -45
    Exit(180 + tilt); // change to counter-clockwise angle measure

  WriteLn('Error in moments for tilt angle');
  Exit(0);
end; // end of calculateTilt()

procedure extractContourInfo(const bigContour: pCvSeq; const scale: Integer);
(*calculate COG and angle of the contour's main axis relative to the horizontal.
  Store them in the globals cogPt and contourAxisAngle
*)
Var
  moments: TCvMoments;
  m00, m10, m01: double;
  m11, m20, m02: double;
  yTotal: Integer;
  avgYFinger: Integer;
  i: Integer;
begin
  cvMoments(bigContour, @moments, 1); // CvSeq is a subclass of CvArr

  // center of gravity
  m00 := cvGetSpatialMoment(@moments, 0, 0);
  m10 := cvGetSpatialMoment(@moments, 1, 0);
  m01 := cvGetSpatialMoment(@moments, 0, 1);

  if (m00 <> 0) then
  begin // calculate center
    cogPt.x := round(m10 / m00) * scale;
    cogPt.y := round(m01 / m00) * scale;
  end;

  m11 := cvGetCentralMoment(@moments, 1, 1);
  m20 := cvGetCentralMoment(@moments, 2, 0);
  m02 := cvGetCentralMoment(@moments, 0, 2);
  contourAxisAngle := calculateTilt(m11, m20, m02);
  (*this angle assumes that the positive y-axis
    is down the screen*)

  // deal with hand contour pointing downwards
  (*uses fingertips information generated on the last update of
    the hand, so will be out-of-date*)
  if Length(fingerTips) > 0 then
  begin
    yTotal := 0;
    for i := 0 to High(fingerTips) do
      yTotal := yTotal + fingerTips[i].y;
    avgYFinger := yTotal div Length(fingerTips);
    if (avgYFinger > cogPt.y) then // fingers below COG
      contourAxisAngle := contourAxisAngle + 180;
  end;
  contourAxisAngle := 180 - contourAxisAngle;
  (*this makes the angle relative to a positive y-axis that
    runs up the screen*)
  // System.out.println("Contour angle: " + contourAxisAngle);
end; // end of extractContourInfo()

function angleBetween(const tip, next, prev: TcvPoint): Integer;
// calulate the angle between the tip and its neigbouring folds (in integer degrees)
begin
  Result := abs(round(RadToDeg(ArcTan2(next.x - tip.x, next.y - tip.y) - ArcTan2(prev.x - tip.x, prev.y - tip.y))));
end;

procedure reduceTips(const numPoints: Integer; const tipPts, foldPts: TArray<TcvPoint>; const depths: TArray<Single>);
(*Narrow in on 'real' finger tips by ignoring shallow defect depths, and tips
  which have too great an angle between their neighbouring fold points.

  Store the resulting finger tip coordinates in the global fingerTips list.
*)
Var
  i: Integer;
  pdx, sdx, angle: Integer;
begin
  SetLength(fingerTips, 0);

  for i := 0 to numPoints - 1 do
  begin
    if (depths[i] < MIN_FINGER_DEPTH) then // defect too shallow
      continue;

    // look at fold points on either side of a tip
    if i = 0 then
      pdx := numPoints - 1
    else
      pdx := i - 1; // predecessor of i
    if i = (numPoints - 1) then
      sdx := 0
    else
      sdx := i + 1; // successor of i
    angle := angleBetween(tipPts[i], foldPts[pdx], foldPts[sdx]);

    if (angle >= MAX_FINGER_ANGLE) then // angle between finger and folds too wide
      continue;

    // this point probably is a finger tip, so add to list
    SetLength(fingerTips, Length(fingerTips) + 1);
    fingerTips[High(fingerTips)] := tipPts[i];
  end;
  // System.out.println("No. of finger tips: " + fingerTips.size());
end; // end of reduceTips()

procedure findFingerTips(const bigContour: pCvSeq; const scale: Integer);
(*Starting with the contour, calculate its convex hull, and its
  convexity defects. Ignore defects that are unlikely to be fingers.
*)
Var
  approxContour, hullSeq, defects: pCvSeq;
  defectsTotal: Integer;
  i: Integer;
  cdf: pCvConvexityDefect;
  startPt, endPt, depthPt: TcvPoint;
begin
  approxContour := cvApproxPoly(bigContour, SizeOf(TCvContour), approxStorage, CV_POLY_APPROX_DP, 3, 1);
  // reduce number of points in the contour

  hullSeq := cvConvexHull2(approxContour, hullStorage, CV_COUNTER_CLOCKWISE, 0);
  // find the convex hull around the contour

  defects := cvConvexityDefects(approxContour, hullSeq, defectsStorage);
  // find the defect differences between the contour and hull
  defectsTotal := defects^.total;
  if (defectsTotal > MAX_POINTS) then
  begin
    WriteLn('Only processing ', MAX_POINTS, ' defect points');
    defectsTotal := MAX_POINTS;
  end;

  // copy defect information from defects sequence into arrays
  for i := 0 to defectsTotal - 1 do
  begin
    cdf := cvGetSeqElem(defects, i);
    startPt := cdf^.start^;
    tipPts[i] := cvPoint(round(startPt.x * scale), round(startPt.y * scale));
    // an array containing the coordinates of the finger tips

    endPt := cdf^._end^;
    depthPt := cdf^.depth_point^;
    foldPts[i] := cvPoint(round(depthPt.x * scale), round(depthPt.y * scale));
    // an array containing the coordinates of the skin fold between fingers
    depths[i] := cdf^.depth * scale;
    // an array containing the distances from tips to folds
  end;

  reduceTips(defectsTotal, tipPts, foldPts, depths);
end; // end of findFingerTips()

function angleToCOG(const tipPt, cogPt: TcvPoint; const contourAxisAngle: Integer): Integer;
(*calculate angle of tip relative to the COG, remembering to add the
  hand contour angle so that the hand is orientated straight up*)
Var
  yOffset, xOffset: Integer;
  theta: double;
  angleTip: Integer;
  offsetAngleTip: Integer;
begin
  yOffset := cogPt.y - tipPt.y; // make y positive up screen
  xOffset := tipPt.x - cogPt.x;
  // Point offsetPt = new Point(xOffset, yOffset);

  theta := ArcTan2(yOffset, xOffset);
  angleTip := round(RadToDeg(theta));
  offsetAngleTip := angleTip + (90 - contourAxisAngle);
  // this addition ensures that the hand is orientated straight up
  Result := offsetAngleTip;
end; // end of angleToCOG()

procedure labelThumbIndex(const fingerTips: TArray<TcvPoint>; Var nms: TArray<TFingerName>);
// attempt to label the thumb and index fingers of the hand
Var
  foundThumb, foundIndex: boolean;
  i: Integer;
  angle: Integer;
begin
  foundThumb := false;
  foundIndex := false;
  (*the thumb and index fingers will most likely be stored at the end
    of the list, since the contour hull was built in a counter-clockwise
    order by the call to cvConvexHull2() in findFingerTips(), and I am assuming
    the thumb is on the left of the hand.
    So iterate backwards through the list.
  *)
  i := High(fingerTips);
  while ((i >= 0)) do
  begin
    angle := angleToCOG(fingerTips[i], cogPt, contourAxisAngle);

    // check for thumb
    if (angle <= MAX_THUMB) and (angle > MIN_THUMB) and (not foundThumb) then
    begin
      nms[i] := THUMB;
      foundThumb := true;
    end;

    // check for index
    if (angle <= MAX_INDEX) and (angle > MIN_INDEX) and (not foundIndex) then
    begin
      nms[i] := INDEX;
      foundIndex := true;
    end;
    Dec(i);
  end;
end; // end of labelThumbIndex()

function usedName(const nms: TArray<TFingerName>; const name: TFingerName): boolean;
// does the fingers list contain name already?
Var
  i: Integer;
begin
  for i := 0 to High(nms) do
    if (nms[i] = name) then
      Exit(true);
  Result := false;
end; // end of usedName()

procedure labelPrev(Var nms: TArray<TFingerName>; i: Integer; name: TFingerName);
// move backwards through fingers list labelling unknown fingers
begin
  Dec(i);
  while (i >= 0) and (name <> UNKNOWN) do
  begin

    if (nms[i] = UNKNOWN) then
    begin // unknown finger
      name := Pred(name);
      if not usedName(nms, name) then
        nms[i] := name;
    end
    else // finger is named already
      name := nms[i];
    Dec(i);
  end;
end; // end of labelPrev()

procedure labelFwd(Var nms: TArray<TFingerName>; i: Integer; name: TFingerName);
// move forward through fingers list labelling unknown fingers
begin
  Inc(i);
  while (i < Length(nms)) and (name <> UNKNOWN) do
  begin
    if (nms[i] = UNKNOWN) then
    begin // unknown finger
      name := Succ(name);
      if (not usedName(nms, name)) then
        nms[i] := name;
    end
    else // finger is named already
      name := nms[i];
    Inc(i);
  end;
end; // end of labelFwd()

procedure labelUnknowns(Var nms: TArray<TFingerName>);
// attempt to label all the unknown fingers in the list
var
  i: Integer;
  name: TFingerName;

begin
  // find first named finger
  i := 0;
  while (i < Length(nms)) and (nms[i] = UNKNOWN) do
    Inc(i);
  if i = Length(nms) then // no named fingers found, so give up
    Exit;

  name := nms[i];
  labelPrev(nms, i, name); // fill-in backwards
  labelFwd(nms, i, name); // fill-in forwards
end; // end of labelUnknowns()

procedure nameFingers(const cogPt: TcvPoint; const contourAxisAngle: Integer; const fingerTips: TArray<TcvPoint>);
(*Use the finger tip coordinates, and the comtour's COG and axis angle to horizontal
  to label the fingers.

  Try to label the thumb and index based on their likely angle ranges
  relative to the COG. This assumes that the thumb and index finger are on the
  left side of the hand.

  Then label the other fingers based on the order of the names in the FingerName class
*)
Var
  i: Integer;
begin // reset all named fingers to unknown
  SetLength(namedFingers, 0);
  for i := 0 to High(fingerTips) do
  begin
    SetLength(namedFingers, Length(namedFingers) + 1);
    namedFingers[High(namedFingers)] := UNKNOWN;
  end;

  labelThumbIndex(fingerTips, namedFingers);

  // printFingers("named fingers", namedFingers);
  labelUnknowns(namedFingers);
  // printFingers("revised named fingers", namedFingers);
end; // end of nameFingers()

procedure update(const im: pIplImage);
(*Convert the image to HSV format. Calculate a threshold
  image using the HSV ranges for the colour being detected. Find
  the largest contour in the threshold image. Find the finger tips
  using a convex hull and defects detection, and then label the fingers
  (assuming that the thumb is on the left of the hand).
*)
Var
  bigContour: pCvSeq;

begin
  if not Assigned(scaleImg) then
    HandDetector('', im^.width, im^.height);
  // scale and convert image format to HSV
  cvResize(im, scaleImg);
  cvCvtColor(scaleImg, hsvImg, CV_BGR2HSV);
  // threshold the image using the loaded HSV settings for the user's glove
  cvInRangeS(hsvImg, cvScalar(hueLower, satLower, briLower, 0), cvScalar(hueUpper, satUpper, briUpper, 0), imgThreshed);
  cvMorphologyEx(imgThreshed, imgThreshed, nil, nil, CV_MOP_OPEN, 1);
  // do erosion followed by dilation on the image to remove specks of white & retain size
  bigContour := findBiggestContour(imgThreshed);
  if not Assigned(bigContour) then
    Exit;
  extractContourInfo(bigContour, IMG_SCALE);
  // find the COG and angle to horizontal of the contour
  findFingerTips(bigContour, IMG_SCALE);
  // detect the finger tips positions in the contour
  nameFingers(cogPt, contourAxisAngle, fingerTips);
end; // end of update()

procedure draw(const g2d: pIplImage);
// draw information about the finger tips and the hand COG
Var
  i: Integer;
  pt: TcvPoint;
  txt: AnsiString;
begin
  if Length(fingerTips) = 0 then
    Exit;
  // label the finger tips in red or green, and draw COG lines to named tips
  for i := 0 to High(fingerTips) do
  begin
    pt := fingerTips[i];
    if (namedFingers[i] = UNKNOWN) then
    begin
      cvCircle(g2d, pt, 16, CV_RGB(255, 0, 0), 3, CV_AA, 0);
      txt := IntToStr(i);
      cvPutText(g2d, PAnsiChar(@txt[1]), cvPoint(pt.x, pt.y - 10), @msgFont, cvScalar(255, 0, 0, 0));
    end
    else
    begin
      // draw yellow line to the named finger tip from COG
      cvLine(g2d, cogPt, pt, CV_RGB(255, 255, 0), 1, CV_AA, 0);
      cvCircle(g2d, pt, 16, CV_RGB(0, 255, 0), 3, CV_AA, 0);
      txt := FingerName[namedFingers[i]];
      cvPutText(g2d, PAnsiChar(@txt[1]), cvPoint(pt.x, pt.y - 10), @msgFont, cvScalar(255, 0, 0, 0));
    end;
    // draw COG
    cvCircle(g2d, cogPt, 16, CV_RGB(0, 255, 0), 3, CV_AA, 0);
  end;
end; // end of draw()

Var
  image: pIplImage;

begin
  try
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    cvNamedWindow('capture', CV_WINDOW_AUTOSIZE);
    while true do
    begin
      frame := cvQueryFrame(capture);
      image := cvCloneImage(frame);
      update(image);
      cvShowImage('capture', image);
      if cvWaitKey(33) = 27 then
        Break;
    end;
    cvReleaseCapture(capture);
    cvReleaseImage(image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

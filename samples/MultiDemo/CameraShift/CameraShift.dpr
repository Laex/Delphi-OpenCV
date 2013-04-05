{$APPTYPE CONSOLE}
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program CameraShift;

{$R *.res}

uses
  System.SysUtils,
  Math,
uLibName in '..\..\..\include\uLibName.pas',
highgui_c in '..\..\..\include\highgui\highgui_c.pas',
core_c in '..\..\..\include\сore\core_c.pas',
Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
legacy in '..\..\..\include\legacy\legacy.pas',
calib3d in '..\..\..\include\calib3d\calib3d.pas',
imgproc in '..\..\..\include\imgproc\imgproc.pas',
haar in '..\..\..\include\objdetect\haar.pas',
objdetect in '..\..\..\include\objdetect\objdetect.pas',
tracking in '..\..\..\include\video\tracking.pas',
Core in '..\..\..\include\сore\core.pas'
  ;

Var
  image: pIplImage = nil;
  hsv: pIplImage = nil;
  hue: pIplImage = nil;
  mask: pIplImage = nil;
  backproject: pIplImage = nil;
  histimg: pIplImage = nil;
  hist: PCvHistogram = nil;

  backproject_mode: integer = 0;
  select_object: integer = 0;
  track_object: integer = 0;

  origin: TCvPoint;
  selection: TCvRect;
  track_window: TCvRect;
  track_box: TCvBox2D;
  track_comp: TCvConnectedComp;
  hdims: integer = 16;
  SingleArray1D: TSingleArray1D=(0,180);
  p_SingleArray1D: pSingleArray1D=@SingleArray1D;
  SingleArray2D:TSingleArray2D;
  hranges: pSingleArray2D = @SingleArray2D;
  capture: PCvCapture;
  frame: pIplImage;
  color: TCvScalar;
  sector_data: array [0 .. 5] of array [0 .. 2] of integer = (
    (
      0,
      2,
      1
    ),
    (
      1,
      2,
      0
    ),
    (
      1,
      0,
      2
    ),
    (
      2,
      0,
      1
    ),
    (
      2,
      1,
      0
    ),
    (
      0,
      1,
      2
    )
  );

  tbVminPosition: integer = 10;
  tbVmaxPosition: integer = 256;
  tbSMinPosition: integer = 30;

function hsv2rgb(hue: Single): TCvScalar;
var
  rgb: array [0 .. 2] of longint;
  p, sector: longint;
  // sector_data : array[0..5] of array[0..2] of longint;
begin
  hue := hue * 0.033333333333333333333333333333333;
  sector := cvFloor(hue);
  p := cvRound(255 * (hue - sector));
  if (sector and 1) <> 0 then
    p := p xor 255
  else
    p := p xor 0;

  rgb[sector_data[sector][0]] := 255;
  rgb[sector_data[sector][1]] := 0;
  rgb[sector_data[sector][2]] := p;

  result := cvScalar(rgb[2], rgb[1], rgb[0], 0);
end;

procedure main_cycle();
var
  i, bin_w: integer;
  _vmin, _vmax: integer;
  max_val: Single;
  val: integer;
  cs: TCvSize;
begin
  frame := cvQueryFrame(capture);

  if not(assigned(frame)) then
    exit;

  if not(assigned(image)) then
  begin
    // * allocate all the buffers */
    cs.width := frame.width;
    cs.height := frame.height;
    image := cvCreateImage(cs, 8, 3);
    image.origin := frame.origin;
    hsv := cvCreateImage(cs, 8, 3);
    hue := cvCreateImage(cs, 8, 1);
    mask := cvCreateImage(cs, 8, 1);
    backproject := cvCreateImage(cs, 8, 1);
    hist := cvCreateHist(1, @hdims, CV_HIST_ARRAY, hranges, 1);
    histimg := cvCreateImage(cvSize(320, 200), 8, 3);
    cvZero(histimg);
  end;

  cvCopy(frame, image);
  cvCvtColor(image, hsv, CV_BGR2HSV);

  if (track_object <> 0) then
  begin
    _vmin := tbVminPosition;
    _vmax := tbVmaxPosition;

    cvInRangeS(hsv, cvScalar(0, tbSMinPosition, MIN(_vmin, _vmax), 0), cvScalar(180, 256, MAX(_vmin, _vmax), 0), mask);
    cvSplit(hsv, hue, nil, nil, nil);

    if (track_object < 0) then
    begin
      max_val := 0.0;
      cvSetImageROI(hue, selection);
      cvSetImageROI(mask, selection);
      cvCalcHist(@hue, hist, 0, mask);
      cvGetMinMaxHistValue(hist, nil, @max_val);
      if (max_val <> 0) then
        cvConvertScale(hist^.bins, hist^.bins, (255.0 / max_val), 0)
      else
        cvConvertScale(hist^.bins, hist^.bins, 0.0, 0);
      // -------cvConvertScale( hist^.bins, hist^.bins, max_val ? 255. / max_val : 0., 0 );
      cvResetImageROI(hue);
      cvResetImageROI(mask);
      track_window := selection;

      track_object := 1;

      cvZero(histimg);
      bin_w := round(histimg^.width / hdims);
      for i := 0 to hdims - 1 do
      begin
        val := cvRound(cvGetReal1D(hist^.bins, i) * histimg^.height / 255);
        color := hsv2rgb(i * 180.0 / hdims);
        cvRectangle(histimg, cvPoint(i * bin_w, histimg^.height), cvPoint((i + 1) * bin_w, histimg^.height - val),
          color, -1, 8, 0);
      end;
    end;

    cvCalcBackProject(hue, backproject, hist);
    cvAnd(backproject, mask, backproject, nil);

    // Writeln(Format('Track window: x:%d y:%d w:%d h:%d', [track_window.x, track_window.y, track_window.width,track_window.height]));
    cvCamShift(backproject, track_window, cvTermCriteria((CV_TERMCRIT_EPS or CV_TERMCRIT_ITER), 10, 1), @track_comp,
      @track_box);
    track_window := track_comp.rect;

    if (backproject_mode <> 0) then
      cvCvtColor(backproject, image, CV_GRAY2BGR);
    if (image.origin <> IPL_ORIGIN_TL) then
      track_box.angle := -track_box.angle;
    { draw an ellipse around the tracked object }
    cvEllipseBox(image, track_box, CV_RGB(255, 0, 0), 3, CV_AA, 0);
  end;

  { draw a rectangle on the area selected with mouse }
  if (select_object > 0) and (selection.width > 0) and (selection.height > 0) then
  begin
    cvSetImageROI(image, selection);
    cvXorS(image, cvScalarAll(255), image, nil);
    cvResetImageROI(image);
  end;

  { visualize the camera image in the window }
  cvShowImage('Capture', image);
  cvShowImage('Histogram', histimg);
end;

// обработчик событий от мышки
procedure myMouseCallback(event: integer; x: integer; y: integer; flags: integer; param: Pointer); cdecl;
begin
  if event = CV_EVENT_LBUTTONDOWN then
  begin
    origin := cvPoint(x, y);
    selection := cvRect(x, y, 0, 0);
    select_object := 1;
    track_object := 0;
  end
  else if (event = CV_EVENT_MOUSEMOVE) and assigned(image) and (select_object <> 0) then
  begin
    selection.x := MIN(x, origin.x);
    selection.y := MIN(y, origin.y);
    selection.width := ABS(x - origin.x);
    selection.height := ABS(y - origin.y);
    selection.x := MAX(selection.x, 0);
    selection.y := MAX(selection.y, 0);
    selection.width := MIN(selection.width, image.width);
    selection.height := MIN(selection.height, image.height);
  end
  else if event = CV_EVENT_LBUTTONUP then
  begin
    select_object := 0;
    if (selection.width > 0) and (selection.height > 0) then
      track_object := -1
    else
      track_object := 0;
  end;
end;

Var
  c: integer;

begin
  // try
  SingleArray2D[0]:=p_SingleArray1D;
  // окно для отображения
  cvNamedWindow('Capture', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('Histogram', CV_WINDOW_AUTOSIZE);
  cvCreateTrackbar('VMinPos', 'Capture', @tbVminPosition, 256, nil);
  cvCreateTrackbar('VMaxPos', 'Capture', @tbVmaxPosition, 256, nil);
  cvCreateTrackbar('SMinPos', 'Capture', @tbSMinPosition, 256, nil);
  cvSetMouseCallback('Capture', myMouseCallback, image);
  capture := cvCreateCameraCapture(CV_CAP_ANY);
  if not assigned(capture) then
    Halt;

  While True do
  begin
    main_cycle;
    c := cvWaitKey(33);
    if c = 27 then
      Break
    else if c = Ord('b') then
      backproject_mode := backproject_mode xor 1
    else if c = Ord('c') then
    begin
      track_object := 0;
      cvZero(histimg);
    end;
  end;

  cvReleaseCapture(capture);
  cvDestroyAllWindows;
  // except
  // on E: Exception do
  // Writeln(E.ClassName, ': ', E.Message);
  // end;

end.

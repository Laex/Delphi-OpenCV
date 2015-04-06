// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Mikhail Grigorev
// email: sleuthhound@gmail.com
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

program HandsDetect;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

const
  NUM_FINGERS = 5;
  NUM_DEFECTS = 8;

type
  pCtx = ^TCtx;

  TCtx = record
    capture: pCvCapture; // Capture handle
    image: pIplImage; // Input image
    thr_image: pIplImage; // After filtering and thresholding
    temp_image1: pIplImage; // Temporary image (1 channel)
    temp_image3: pIplImage; // Temporary image (3 channels)
    contour: pCvSeq; // Контур руки
    hull: pCvSeq; // Каркас контура выпуклости
    hand_center: TCvPoint;
    fingers: pCvPointArray; // Detected fingers positions
    defects: pCvPointArray; // Convexity defects depth points
    hull_st: pCvMemStorage;
    contour_st: pCvMemStorage;
    temp_st: pCvMemStorage;
    defects_st: pCvMemStorage; // Хранилище для результата вычисления последовательности дефектов
    kernel: pIplConvKernel; // Kernel for morph operations
    NUM_FINGERS: Integer;
    hand_radius: Integer;
    NUM_DEFECTS: Integer;
  end;

var
  key: Integer;
  MyCtx: pCtx;

procedure InitCapture(const Ctx: pCtx);
var
  width: Double;
  height: Double;
begin
  Ctx.capture := cvCreateCameraCapture(CV_CAP_ANY);
  if not Assigned(Ctx.capture) then
  begin
    WriteLn('[i] Error initializing capture.');
    Halt;
  end;
  // Узнаем ширину и высоту кадра
  width := cvGetCaptureProperty(Ctx.capture, CV_CAP_PROP_FRAME_WIDTH);
  height := cvGetCaptureProperty(Ctx.capture, CV_CAP_PROP_FRAME_HEIGHT);
  // WriteLn(Format('[i] %.0f x %.0f', [width, height]));
  Ctx.image := cvQueryFrame(Ctx.capture);
  WriteLn('[i] Start capture.');
end;

procedure InitWindows;
begin
  cvNamedWindow('output', CV_WINDOW_AUTOSIZE);
  cvNamedWindow('thresholded', CV_WINDOW_AUTOSIZE);
  cvMoveWindow('output', 50, 50);
  cvMoveWindow('thresholded', 700, 50);
end;

procedure InitCtx(const Ctx: pCtx);
begin
  // Создаём одноканальные картинки
  Ctx.thr_image := cvCreateImage(cvGetSize(Ctx.image), IPL_DEPTH_8U, 1);
  Ctx.temp_image1 := cvCreateImage(cvGetSize(Ctx.image), IPL_DEPTH_8U, 1);
  // Создаём 3-х канальную картинку
  Ctx.temp_image3 := cvCreateImage(cvGetSize(Ctx.image), IPL_DEPTH_8U, 3);
  Ctx.kernel := cvCreateStructuringElementEx(9, 9, 4, 4, CV_SHAPE_RECT, nil);
  // Создаем хранилища
  Ctx.contour_st := cvCreateMemStorage(0);
  Ctx.hull_st := cvCreateMemStorage(0);
  Ctx.temp_st := cvCreateMemStorage(0);
  Ctx.fingers := AllocMem((NUM_FINGERS + 1) * sizeof(TCvPoint));
  Ctx.defects := AllocMem(NUM_DEFECTS * sizeof(TCvPoint));
end;

procedure FilterAndThreshold(const Ctx: pCtx);
begin
  // Soften image
  cvSmooth(Ctx.image, Ctx.temp_image3, CV_GAUSSIAN, 11, 11, 0, 0);
  // Remove some impulsive noise
  cvSmooth(Ctx.temp_image3, Ctx.temp_image3, CV_MEDIAN, 11, 11, 0, 0);
  cvCvtColor(Ctx.temp_image3, Ctx.temp_image3, CV_BGR2HSV);
  { * Apply threshold on HSV values
    * Threshold values should be customized according to environment
    * }
  cvInRangeS(Ctx.temp_image3, cvScalar(0, 0, 160, 0), cvScalar(255, 400, 300, 255), Ctx.thr_image);
  // Apply morphological opening
  cvMorphologyEx(Ctx.thr_image, Ctx.thr_image, nil, Ctx.kernel, CV_MOP_OPEN, 1);
  cvSmooth(Ctx.thr_image, Ctx.thr_image, CV_GAUSSIAN, 3, 3, 0, 0);
end;

// Поиск контуров
procedure FindContour(const Ctx: pCtx);
var
  area, max_area: Double;
  contours, tmp, contour: pCvSeq;
  cont: pIplImage;
begin
  area := 0.0;
  max_area := 0.0;
  contours := nil;
  tmp := nil;
  contour := nil;

  // cvFindContours modifies input image, so make a copy
  cvCopy(Ctx.thr_image, Ctx.temp_image1, nil);
  // Находим контуры
  cvClearMemStorage(Ctx.temp_st);
  cvFindContours(Ctx.temp_image1, Ctx.temp_st, @contours, sizeof(TCvContour), CV_RETR_EXTERNAL,
    CV_CHAIN_APPROX_NONE { CV_CHAIN_APPROX_SIMPLE } , cvPoint(0, 0));

  // Заведём картинку для контура
  cont := cvCreateImage(cvGetSize(Ctx.temp_image1), IPL_DEPTH_8U, 3);
  // Select contour having greatest area
  tmp := contours;
  while (tmp <> nil) do
  begin
    area := abs(cvContourArea(tmp, CV_WHOLE_SEQ, 0));
    if area > max_area then
    begin
      // WriteLn(Format('[i] area = %.1f', [area]));
      max_area := area;
      contour := tmp;
    end;
    // Рисуем контур
    cvDrawContours(cont, tmp, CV_RGB(255, 216, 0), CV_RGB(0, 0, 250), 0, 1, 8, cvPoint(0, 0));
    tmp := tmp.h_next;
  end;
  // Approximate contour with poly-line
  if contour <> nil then
  begin
    contour := cvApproxPoly(contour, sizeof(TCvContour), Ctx.contour_st, CV_POLY_APPROX_DP, 2, 1);
    Ctx.contour := contour;
  end;
  // Покажем контур
  cvDrawContours(cont, contour, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), 0, 2, 8, cvPoint(0, 0));
  cvNamedWindow('ContT', CV_WINDOW_AUTOSIZE);
  cvShowImage('ContT', cont);
end;

procedure FindConvexHull(const Ctx: pCtx);
var
  defects: pCvSeq;
  defect_array: pCvConvexityDefect;
  i: Integer;
  d, x, y: Integer;
  dist: Double;
begin
  x := 0;
  y := 0;
  dist := 0;

  Ctx.hull := nil;

  if not Assigned(Ctx.contour) then
    Exit;

  // Вычисляем каркас контура выпуклости
  Ctx.hull := cvConvexHull2(Ctx.contour, Ctx.hull_st, CV_CLOCKWISE, 0);
  if Assigned(Ctx.hull) then
  begin
    // Вычисляем последовательность дефектов
    defects := cvConvexityDefects(Ctx.contour, Ctx.hull, Ctx.defects_st);

    if Assigned(defects) and (defects.total <> 0) then
    begin
      defect_array := AllocMem(defects.total * sizeof(TCvConvexityDefect));
      cvCvtSeqToArray(defects, defect_array, CV_WHOLE_SEQ);

      // Average depth points to get hand center
      // Вычисление глубины средней точки, чтобы получить центр руки
      i := 0;
      while (i < defects.total) and (i < NUM_DEFECTS) do
      begin
        x := x + defect_array[i].depth_point.x;
        y := y + defect_array[i].depth_point.y;
        Ctx.defects[i] := cvPoint(defect_array[i].depth_point.x, defect_array[i].depth_point.y);
        Inc(i);
        // WriteLn(Format('[i] x = %d, y = %d', [Ctx.defects[i].x, Ctx.defects[i].y]));
        // WriteLn(Format('[i] defects.total: %d', [defects.total]));
      end;

      x := x div defects.total;
      y := y div defects.total;

      Ctx.NUM_DEFECTS := defects.total;
      Ctx.hand_center := cvPoint(x, y);
      // WriteLn(Format('[i] defects.total: %d', [defects.total]));
      // WriteLn(Format('[i] hand_center: x = %d, y = %d', [Ctx.hand_center.x, Ctx.hand_center.y]));

      // Compute hand radius as mean of distances of defects' depth point to hand center
      // Вычисляем радиус руки
      for i := 0 to defects.total - 1 do
      begin
        d := (x - defect_array[i].depth_point.x) * (x - defect_array[i].depth_point.x) +
          (y - defect_array[i].depth_point.y) * (y - defect_array[i].depth_point.y);
        dist := dist + sqrt(d);
      end;

      Ctx.hand_radius := Trunc(dist) div defects.total;
      // WriteLn(Format('[i] hand_radius: %d', [Ctx.hand_radius]));

      FreeMem(defect_array);
    end;
  end;
end;

procedure FindFingers(const Ctx: pCtx);
var
  n, i, cx, cy, dist, dist1, dist2: Integer;
  // points: pCvPoint;
  points: pCvPointArray;
  max_point: TCvPoint;
  finger_distance: array [0 .. NUM_FINGERS + 1] of Integer;
begin
  dist1 := 0;
  dist2 := 0;
  Ctx.NUM_FINGERS := 0;

  if (not Assigned(Ctx.contour)) or (not Assigned(Ctx.hull)) then
    Exit;

  n := Ctx.contour.total;
  points := AllocMem(n * sizeof(TCvPoint));

  cvCvtSeqToArray(Ctx.contour, points, CV_WHOLE_SEQ);

  { * Fingers are detected as points where the distance to the center
    * is a local maximum
    * }
  for i := 0 to n - 1 do
  begin
    cx := Ctx.hand_center.x;
    cy := Ctx.hand_center.y;

    dist := (cx - points[i].x) * (cx - points[i].x) + (cy - points[i].y) * (cy - points[i].y);

    if (dist < dist1) and (dist1 > dist2) and (max_point.x <> 0) and (max_point.y < cvGetSize(Ctx.image).height - 10)
    then
    begin
      finger_distance[Ctx.NUM_FINGERS] := dist;
      Inc(Ctx.NUM_FINGERS);
      Ctx.fingers[Ctx.NUM_FINGERS] := max_point;
      if Ctx.NUM_FINGERS >= NUM_FINGERS + 1 then
        Break;
    end;

    dist2 := dist1;
    dist1 := dist;
    max_point := points[i];
  end;

  FreeMem(points);
end;

procedure Display(const Ctx: pCtx);
var
  i: Integer;
begin
  if Ctx.NUM_FINGERS = NUM_FINGERS then
  begin
    // ifdef SHOW_HAND_CONTOUR
    // cvDrawContours(ctx.image, ctx.contour, CV_RGB(0,0,255), CV_RGB(0,255,0), 0, 1, CV_AA, cvPoint(0,0));
    // #endif
    cvCircle(Ctx.image, Ctx.hand_center, 5, CV_RGB(255, 0, 255), 1, CV_AA, 0);
    cvCircle(Ctx.image, Ctx.hand_center, Ctx.hand_radius, CV_RGB(255, 0, 0), 1, CV_AA, 0);
    for i := 0 to Ctx.NUM_FINGERS - 1 do
    begin
      cvCircle(Ctx.image, Ctx.fingers[i], 10, CV_RGB(0, 255, 0), 3, CV_AA, 0);
      cvLine(Ctx.image, Ctx.hand_center, Ctx.fingers[i], CV_RGB(255, 255, 0), 1, CV_AA, 0);
    end;
    for i := 0 to Ctx.NUM_DEFECTS - 1 do
      cvCircle(Ctx.image, Ctx.defects[i], 2, CV_RGB(200, 200, 200), 2, CV_AA, 0);
  end;
  cvShowImage('output', Ctx.image);
  cvShowImage('thresholded', Ctx.thr_image);
end;

procedure ClearRes(const Ctx: pCtx);
begin
  // Освобождаем ресурсы
  cvReleaseCapture(Ctx.capture);
  // cvReleaseImage(Ctx.image);
  cvReleaseImage(Ctx.thr_image);
  cvReleaseImage(Ctx.temp_image1);
  cvReleaseImage(Ctx.temp_image3);
  cvReleaseMemStorage(Ctx.hull_st);
  cvReleaseMemStorage(Ctx.contour_st);
  cvReleaseMemStorage(Ctx.defects_st);
  cvReleaseMemStorage(Ctx.temp_st);
  cvReleaseStructuringElement(Ctx.kernel);
  FreeMem(Ctx.fingers);
  FreeMem(Ctx.defects);
  cvDestroyAllWindows();
end;

begin
  try
    MyCtx := AllocMem(sizeof(TCtx));
    InitCapture(MyCtx);
    InitWindows;
    InitCtx(MyCtx);
    while true do
    begin
      MyCtx.image := cvQueryFrame(MyCtx.capture);
      // Фильтруем
      FilterAndThreshold(MyCtx);
      // Определение контуров
      FindContour(MyCtx);
      FindConvexHull(MyCtx);
      FindFingers(MyCtx);
      Display(MyCtx);
      key := cvWaitKey(33);
      if (key = 27) then
        Break;
    end;
    ClearRes(MyCtx);
    FreeMem(MyCtx);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

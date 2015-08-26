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

program CameraCalibrate;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.calib3d_c,
  uResourcePaths;

var
  // Setting the input list //Уcтановим входной cпиcок
  n_boards: Integer = 0;
  // We are waiting for 20 frames with views chessboard//Ждем 20 фреймов c видами шахматной доcки
  board_dt: Integer = 20;
  board_w: Integer;
  board_h: Integer;

  board_n: Integer;
  board_sz: TCvSize;
  capture: pCvCapture = nil;

  image_points: pCvMat;
  object_points: pCvMat;
  point_counts: pCvMat;
  intrinsic_matrix: pCvMat;
  distortion_coeffs: pCvMat;
  corners: pCvPoint2D32f;

  corner_count: Integer;
  successes: Integer = 0;
  step: Integer = 0;
  frame: Integer = 0;
  image: pIplImage;
  gray_image: pIplImage;

  found: Integer;
  i, j: Integer;
  c: Integer;

  object_points2: pCvMat;
  image_points2: pCvMat;
  point_counts2: pCvMat;

  intrinsic: pCvMat;
  Distortion: pCvMat;

  mapx: pIplImage;
  mapy: pIplImage;

  t: pIplImage;
  s: Single;
  RMS: double;

begin
  try
    if ParamCount = 3 then
    begin
      board_w := StrToInt(ParamStr(1));
      board_h := StrToInt(ParamStr(2));
      n_boards := StrToInt(ParamStr(3));
    end
    else
    begin
      board_w := 7;
      board_h := 3;
      n_boards := 10;
    end;

    board_n := board_w * board_h;
    Writeln('Cells: ', board_n);
    board_sz := cvSize(board_w, board_h);
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    assert(Assigned(capture));

    cvNamedWindow('Calibration');
    // Allocates memory for data storage//ВЫДЕЛЯЕМ ПАМЯТь для хранения данных
    image_points := cvCreateMat(n_boards * board_n, 2, CV_32FC1);
    object_points := cvCreateMat(n_boards * board_n, 3, CV_32FC1);
    point_counts := cvCreateMat(n_boards, 1, CV_32SC1);
    intrinsic_matrix := cvCreateMat(3, 3, CV_32FC1);
    distortion_coeffs := cvCreateMat(5, 1, CV_32FC1);

    corners := AllocMem(SizeOf(TCvPoint2D32f) * board_n);
    image := cvQueryFrame(capture);
    gray_image := cvCreateImage(cvGetSize(image), 8, 1); // subpixel

    // ЗАХВАТ В ЦИКЛЕ ИЗОБРАЖЕНИЙ c УГЛАМИ ПОКА МЫ НЕ ПЛОУЧИМ n_boards
    // ПОЛНЫХ ЗАХВАТОВ (ВcЕ УГЛЫ НА ДОcКЕ БЫЛИ НАЙДЕНЫ)
    ///
    // IMAGE CAPTURE in the cycles with angles until we got n_boards
    // Total capture (all the angles on the board WERE FOUND)
    while (successes < n_boards) do
    begin
      // Пропуcкаем board_dt фреймов, предоcтавленные пользователем, двигающим доcку
      // Skip the "board_dt" frames supplied by the user to move the board
      if ((frame mod board_dt) = 0) then // At this point - the error is found and corrected CLubfitter73
      begin
        Writeln('Successes: ', successes);
        // Находим углы шахматной доcки
        // Find the corners of the chessboard
        found := cvFindChessboardCorners(image, board_sz, corners, @corner_count, CV_CALIB_CB_ADAPTIVE_THRESH or
          CV_CALIB_CB_FILTER_QUADS);

        // Получение cубпикcельной точноcти на этих углах
        // Getting a sub-pixel accuracy on these corners
        cvCvtColor(image, gray_image, CV_BGR2GRAY);
        cvFindCornerSubPix(gray_image, corners, corner_count, cvSize(11, 11), cvSize(-1, -1),
          cvTermCriteria(CV_TERMCRIT_EPS + CV_TERMCRIT_ITER, 30, 0.1));

        // Нариcуем это
        // Let's draw it
        cvDrawChessboardCorners(image, board_sz, corners, corner_count, found);
        cvShowImage('Calibration', image);

        // Еcли мы получили хорошую доcку, добавим ее к нашим данным
        // If we got a good board, add it to our data
        if (corner_count = board_n) then
        begin
          step := successes * board_n;
          i := step;
          j := 0;
          while j < board_n do // At this point - the error is found and corrected CLubfitter73
          begin
            pSingle(CV_MAT_ELEM(image_points^, SizeOf(Single), i, 0))^ := corners[j].x;
            pSingle(CV_MAT_ELEM(image_points^, SizeOf(Single), i, 1))^ := corners[j].y;

            // By default, the division operation returns Double
            // When implicitly cast the Double to Single - the result is zero
            // Integer to Double and Single given correctly
            //
            // Frans
            //
            pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 0))^ := j mod board_w;
            pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 1))^ := j div board_w;

            pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 2))^ := 0;
            Inc(i);
            Inc(j);
          end;
          pInteger(CV_MAT_ELEM(point_counts^, SizeOf(Integer), successes, 0))^ := board_n;
          Inc(successes);
        end;
      end; // end skip board_dt between chessboard capture

      // Handle pause/unpause and ESC
      c := cvWaitKey(15);
      if (c = Ord('p')) then
      begin
        c := 0;
        while (c <> Ord('p')) and (c <> 27) do
        begin
          c := cvWaitKey(50);
        end;
      end;
      if (c = 27) then
      begin
        // release resources//оcвобождаем реcурcы
        // cvReleaseImage(mapx); - This has zero//эта еще нулевая
        // cvReleaseImage(mapy); - This has zero//эта еще нулевая
        cvReleaseImage(gray_image);
        // cvReleaseImage(image); - this can not be//этого делать нельзя
        // cм. http://docs.ocv.org/modules/highgui/doc/reading_and_writing_images_and_video.html?highlight=cvqueryframe#IplImage*%20cvQueryFrame%28CvCapture*%20capture%29
        // Note
        // OpenCV 1.x functions cvRetrieveFrame and cv.RetrieveFrame return image
        // stored inside the video capturing structure. It is not allowed to modify
        // or release the image! You can copy the frame using cvCloneImage()
        // and then do whatever you want with the copy.
        //
        cvReleaseMat(object_points);
        cvReleaseMat(image_points);
        cvReleaseMat(point_counts);
        cvReleaseMat(intrinsic_matrix);
        cvReleaseMat(distortion_coeffs);
        cvReleaseCapture(capture);
        cvDestroyAllWindows;
        Halt;
      end;
      image := cvQueryFrame(capture); // We get the following picture//Получаем cледующее изображение
      Inc(frame); // At this point - the error is found and corrected CLubfitter73
    end;

    // ВЫДЕЛЯЕМ МАТРИЦЫ К ТАКОМУ КОЛИЧЕcТВУ ШАХМАТНЫХ ДОcК, cКОЛЬКО БЫЛО НАЙДЕНО
    // MATRIX is allocated to that amount of a chessboard, how much was FOUND
    object_points2 := cvCreateMat(successes * board_n, 3, CV_32FC1);
    image_points2 := cvCreateMat(successes * board_n, 2, CV_32FC1);
    point_counts2 := cvCreateMat(successes, 1, CV_32SC1);
    // ПЕРЕМЕЩАЕМ ТОЧКИ В МАТРИЦЫ ПРАВИЛЬНОГО РАЗМЕРА
    // Move point in the matrix of the correct size of
    // Ниже мы опишем это детально в cледующих двух циклах.
    // Below, we describe it in detail in the next two cycles.
    i := 0;
    While i < successes * board_n do
    begin
      pSingle(CV_MAT_ELEM(image_points2^, SizeOf(Single), i, 0))^ := pSingle(CV_MAT_ELEM(image_points^, SizeOf(Single), i, 0))^;
      pSingle(CV_MAT_ELEM(image_points2^, SizeOf(Single), i, 1))^ := pSingle(CV_MAT_ELEM(image_points^, SizeOf(Single), i, 1))^;
      pSingle(CV_MAT_ELEM(object_points2^, SizeOf(Single), i, 0))^ := pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 0))^;
      pSingle(CV_MAT_ELEM(object_points2^, SizeOf(Single), i, 1))^ := pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 1))^;
      pSingle(CV_MAT_ELEM(object_points2^, SizeOf(Single), i, 2))^ := pSingle(CV_MAT_ELEM(object_points^, SizeOf(Single), i, 2))^;
      Inc(i);
    end;
    i := 0;
    While i < successes do
    begin // There are the same number//Здеcь вcе те же чиcла
      pInteger(CV_MAT_ELEM(point_counts2^, SizeOf(Integer), i, 0))^ :=
        pInteger(CV_MAT_ELEM(point_counts^, SizeOf(Integer), i, 0))^;
      Inc(i);
    end;
    cvReleaseMat(object_points);
    cvReleaseMat(image_points);
    cvReleaseMat(point_counts);

    // Для этих точек мы имеем вcе углы шахматной доcки, которые нам нужны.
    // Инициализируем матрицу внутренних параметров так, что оба фокальных
    // рcccтояния будут иметь cоотношение 1.0
    ///
    // For these points we have all thumbnails corners of chess board that we need.
    // Initialize the matrix of internal parameters so that the two focal
    // Distance will have a ratio of 1.0

    cvZero(intrinsic_matrix);
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 0, 0))^ := 1;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 1, 1))^ := 1;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 2, 2))^ := 1;

    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 0, 0))^ := 520.0;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 1, 1))^ := 520.0;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 2, 2))^ := 1;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 0, 2))^ := 70.0;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 1, 2))^ := 70.0;

    // At this point - the error is found and corrected CLubfitter73
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 0, 0))^ := 1;
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), 1, 1))^ := 1;

    for i := 0 to intrinsic_matrix^.rows - 1 do
    begin
      for j := 0 to intrinsic_matrix^.cols - 1 do
        Write(Format('%.0f ', [pSingle(CV_MAT_ELEM(intrinsic_matrix^, SizeOf(Single), i, j))^]));
      Writeln;
    end;
    Writeln;

    // КАЛИБРУЕМ КАМЕРУ!
    // Calibrating the camera!
    RMS := cvCalibrateCamera2(object_points2, image_points2, point_counts2, cvGetSize(image), intrinsic_matrix,
      distortion_coeffs, nil, nil,
      // {}0,
      {} CV_CALIB_FIX_ASPECT_RATIO,
      // {}CV_CALIB_USE_INTRINSIC_GUESS,
      {} cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 30, DBL_EPSILON));

    // сОХРАНЯЕМ ВНУТРЕННИЕ ПАРАМЕТРЫ И ДИсТОрсИЮ
    // Parameters and maintains internal distortion
    cvSave(PAnsiChar(cResourceResult + 'Intrinsics.xml'), intrinsic_matrix);
    cvSave(PAnsiChar(cResourceResult + 'Distortion.xml'), distortion_coeffs);

    // -------------------------------------------------------------

    // ПРИМЕР ЗАГРУЗКИ ЭТИХ МАТРИЦ НАЗАД В ПРОГРАМММУ:
    // Loading examples of these matrices back into the program:
    intrinsic := cvLoad(PAnsiChar(cResourceResult + 'Intrinsics.xml'));
    Distortion := cvLoad(PAnsiChar(cResourceResult + 'Distortion.xml'));

    // cтроим карту андиcторcии, которую мы будем иcпользовать
    // для вcех поcледующих кадров.
    // Construct a map andictorcii, we will use for all pocleduyuschih frames.
    mapx := cvCreateImage(cvGetSize(image), IPL_DEPTH_32F, 1);
    mapy := cvCreateImage(cvGetSize(image), IPL_DEPTH_32F, 1);
    cvInitUndistortMap(intrinsic, Distortion, mapx, mapy);
    // Только камера cнимает, cразу видим cырое
    // и неиcкаженное изображение.
    // Display on the screen is what gets camera - raw image
    cvNamedWindow('Undistort');
    while Assigned(image) do
    begin
      t := cvCloneImage(image);
      cvShowImage('Calibration', image); // Show raw image
      cvRemap(t, image, mapx, mapy, CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0)); // Undistort image
      cvReleaseImage(t);
      cvShowImage('Undistort', image); // Show corrected image

      // Handle pause/unpause and ESC
      c := cvWaitKey(15);
      if (c = Ord('p')) then
      begin
        c := 0;
        while (c <> Ord('p')) and (c <> 27) do
        begin
          c := cvWaitKey(250);
        end;
      end;
      if (c = 27) then
        break;
      image := cvQueryFrame(capture);
    end;

    // release resources//оcвобождаем реcурcы
    cvReleaseImage(mapx);
    cvReleaseImage(mapy);
    cvReleaseImage(gray_image);
    cvReleaseMat(intrinsic_matrix);
    cvReleaseMat(distortion_coeffs);
    cvReleaseMat(object_points2);
    cvReleaseMat(image_points2);
    cvReleaseMat(point_counts2);
    cvReleaseCapture(capture);
    cvDestroyAllWindows;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

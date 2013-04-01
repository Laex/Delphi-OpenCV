// Чтение ширины и высоты шахматной доски,
// чтение и коллекционирование запрошенного количества видов
// и калибровка камеры

// calib.cpp
// Инициализация входных данных:
// calib board_w board_h number_of_views
//
// Нажатие ‘p’ - установка/выключение паузы, ESC - выход

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program CameraCalibrate;

{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\сore\core_c.pas',
  Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  legacy in '..\..\..\include\legacy\legacy.pas',
  calib3d in '..\..\..\include\calib3d\calib3d.pas',
  types_c in '..\..\..\include\сore\types_c.pas';

Var
  n_boards: Integer = 0; // Установим входной список
  board_dt: Integer = 20; // Ждем 20 фреймов с видами шахматной доски
  board_w: Integer;
  board_h: Integer;

  board_n: Integer;
  board_sz: TCvSize;
  capture: pCvCapture;

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
    // ВЫДЕЛЯЕМ ХРАНИЛИЩЕ ПАМЯТИ
    image_points := cvCreateMat(n_boards * board_n, 2, CV_32FC1);
    object_points := cvCreateMat(n_boards * board_n, 3, CV_32FC1);
    point_counts := cvCreateMat(n_boards, 1, CV_32SC1);
    intrinsic_matrix := cvCreateMat(3, 3, CV_32FC1);
    distortion_coeffs := cvCreateMat(5, 1, CV_32FC1);

    corners := AllocMem(SizeOf(TCvPoint2D32f) * board_n);
    image := cvQueryFrame(capture);
    gray_image := cvCreateImage(cvGetSize(image), 8, 1); // subpixel

    // ЗАХВАТ В ЦИКЛЕ ИЗОБРАЖЕНИЙ С УГЛАМИ ПОКА МЫ НЕ ПЛОУЧИМ n_boards
    // ПОЛНЫХ ЗАХВАТОВ (ВСЕ УГЛЫ НА ДОСКЕ БЫЛИ НАЙДЕНЫ)
    //
    while (successes < n_boards) do
    begin
      // Пропускаем board_dt фреймов, предоставленные пользователем, двигающим доску
      if ((frame div board_dt) = 0) then
      begin
        Writeln('Successes: ', successes);
        // Находим углы шахматной доски:
        found := cvFindChessboardCorners(image, board_sz, corners, @corner_count, CV_CALIB_CB_ADAPTIVE_THRESH or
          CV_CALIB_CB_FILTER_QUADS);

        // Получение субпиксельной точности на этих углах
        cvCvtColor(image, gray_image, CV_BGR2GRAY);
        cvFindCornerSubPix(gray_image, corners, corner_count, cvSize(11, 11), cvSize(-1, -1),
          cvTermCriteria(CV_TERMCRIT_EPS + CV_TERMCRIT_ITER, 30, 0.1));

        // Нарисуем это
        cvDrawChessboardCorners(image, board_sz, corners, corner_count, found);
        cvShowImage('Calibration', image);

        // Если мы получили хорошую доску, добавим ее к нашим данным
        if (corner_count = board_n) then
        begin
          step := successes * board_n;
          i := step;
          j := 0;
          while i < board_n do
          begin
            pSingle(CV_MAT_ELEM(image_points^, CV_32FC1, i, 0))^ := corners[j].x;
            pSingle(CV_MAT_ELEM(image_points^, CV_32FC1, i, 1))^ := corners[j].y;
            pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 0))^ := j / board_w;
            pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 1))^ := j div board_w;
            pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 2))^ := 0;
            Inc(i);
            Inc(j);
          end;
          pInteger(CV_MAT_ELEM(point_counts^, CV_32SC1, successes, 0))^ := board_n;
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
        Halt;
      image := cvQueryFrame(capture); // Получаем следующее изображение
    end; // КОНЕЦ КОЛЛЕКЦИОНИРОВАНИЕ ЦИКЛОМ WHILE.

    // ВЫДЕЛЯЕМ МАТРИЦЫ К ТАКОМУ КОЛИЧЕСТВУ ШАХМАТНЫХ ДОСК, СКОЛЬКО БЫЛО НАЙДЕНО
    object_points2 := cvCreateMat(successes * board_n, 3, CV_32FC1);
    image_points2 := cvCreateMat(successes * board_n, 2, CV_32FC1);
    point_counts2 := cvCreateMat(successes, 1, CV_32SC1);
    // ПЕРЕМЕЩАЕМ ТОЧКИ В МАТРИЦЫ ПРАВИЛЬНОГО РАЗМЕРА
    // Ниже мы опишем это детально в следующих двух циклах.
    // Мы можем вместо этого написать:
    // image_points->rows := object_points->rows := \
    // successes*board_n; point_counts->rows := successes;
    i := 0;
    While i < successes * board_n do
    begin
      pSingle(CV_MAT_ELEM(image_points2^, CV_32FC1, i, 0))^ := pSingle(CV_MAT_ELEM(image_points^, CV_32FC1, i, 0))^;
      pSingle(CV_MAT_ELEM(image_points2^, CV_32FC1, i, 1))^ := pSingle(CV_MAT_ELEM(image_points^, CV_32FC1, i, 1))^;
      pSingle(CV_MAT_ELEM(object_points2^, CV_32FC1, i, 0))^ := pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 0))^;
      pSingle(CV_MAT_ELEM(object_points2^, CV_32FC1, i, 1))^ := pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 1))^;
      pSingle(CV_MAT_ELEM(object_points2^, CV_32FC1, i, 2))^ := pSingle(CV_MAT_ELEM(object_points^, CV_32FC1, i, 2))^;
      Inc(i);
    end;
    i := 0;
    While i < successes do
    begin // Здесь все те же числа
      pInteger(CV_MAT_ELEM(point_counts2^, CV_32SC1, i, 0))^ := pInteger(CV_MAT_ELEM(point_counts^, CV_32SC1, i, 0))^;
      Inc(i);
    end;
    cvReleaseMat(object_points);
    cvReleaseMat(image_points);
    cvReleaseMat(point_counts);

    // Для этих точек мы имеем все углы шахматной доски, которые нам нужны.
    // Инициализируем матрицу внутренних параметров так, что оба фокальных
    // расстояния будут иметь соотношение 1.0

    cvZero(intrinsic_matrix);
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 0, 0))^ := 1;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 1, 1))^ := 1;
    // pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 2, 2))^ := 1;

    pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 0, 0))^ := 520.0;
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 1, 1))^ := 520.0;
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 2, 2))^ := 1;
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 0, 2))^ := 70.0;
    pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, 1, 2))^ := 70.0;

    for i := 0 to intrinsic_matrix^.rows - 1 do
    begin
      for j := 0 to intrinsic_matrix^.cols - 1 do
        Write(Format('%.0f ', [pSingle(CV_MAT_ELEM(intrinsic_matrix^, CV_32FC1, i, j))^]));
      Writeln;
    end;
    Writeln;

    // КАЛИБРУЕМ КАМЕРУ!
    cvCalibrateCamera2(object_points2, image_points2, point_counts2, cvGetSize(image), intrinsic_matrix,
      distortion_coeffs, nil, nil,
      // {}0,
      // {}CV_CALIB_FIX_ASPECT_RATIO,
      CV_CALIB_USE_INTRINSIC_GUESS,
      { } cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 30, DBL_EPSILON));

    // СОХРАНЯЕМ ВНУТРЕННИЕ ПАРАМЕТРЫ И ДИСТОРСИЮ
    cvSave('result\Intrinsics.xml', intrinsic_matrix);
    cvSave('result\Distortion.xml', distortion_coeffs);

    // -------------------------------------------------------------

    // ПРИМЕР ЗАГРУЗКИ ЭТИХ МАТРИЦ НАЗАД В ПРОГРАМММУ:
    intrinsic := cvLoad('result\Intrinsics.xml');
    Distortion := cvLoad('result\Distortion.xml');

    // Строим карту андисторсии, которую мы будем использовать
    // для всех последующих кадров.
    //
    mapx := cvCreateImage(cvGetSize(image), IPL_DEPTH_32F, 1);
    mapy := cvCreateImage(cvGetSize(image), IPL_DEPTH_32F, 1);
    cvInitUndistortMap(intrinsic, Distortion, mapx, mapy);
    // Только камера снимает, сразу видим сырое
    // и неискаженное изображение.
    //
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

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  ******************************************************************* *)

program CarNumDetect;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  core in '..\..\..\include\core\core.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  core_c in '..\..\..\include\core\core_c.pas',
  Math, IniFiles;

const
  filename_ini = 'carnumdetect.ini';
  WindowNameOriginal = 'Original Car in Gray Scale';
  WindowNameThreshold = 'Threshold Car Image';
  WindowNameCarsNumber = 'Cars Number Detect';

type
  pCtx = ^TCtx;
  ArrCvRect = Array of TCvRect;
  ArrCvBox2D = Array of TCvBox2D;
  ArrDouble =  Array of Double;
  TCtx = packed record
    MyCapture: pCvCapture;       // Capture handle
    MyWriter: pCvVideoWriter;    // Writer handle
    MyInputImage: pIplImage;     // Input image
    MyOrigColorImage: pIplImage; // Original Color Image
    MyThresholdImage: pIplImage; // Threshold Image
    MyStorage: pCvMemStorage;    // Memory storage
    MyCvRect: ArrCvRect;
    MyCvBox2D: ArrCvBox2D;
    MyArea: ArrDouble;
  end;

var
  MyCtx: pCtx;
  Key, SourceCapture, VideoCamNum, RotateImageAngle: Integer;
  BinarizationMethod, WriteVideoCaptureToFile, AdaptiveThresholdBlockSize: Integer;
  Threshold, ThresholdMaxValue: Integer;
  ImageFileCapture, VideoFileCapture, WriteVideoCaptureFileDir: AnsiString;
  WriteVideoFileName: AnsiString;
  MinRatio, MaxRatio, MinFillArea, VideoCamWidth, VideoCamHeight: Double;
  MinArea, MaxArea, MinFullArea, MaxFullArea: Integer;

procedure FindBox(const Ctx: pCtx; min_ratio, max_ratio: double; min_area, max_area, minfullarea, maxfullarea: integer; minfillarea: double);
var
  c, contours: pCvContour;
  box: TCvBox2D;
  rect: TCvRect;
  ratio, contourarea: double;
  area: single;
begin
  contours := nil;
  c := nil;
  contours := AllocMem(SizeOf(TCvContour));
  c := AllocMem(SizeOf(TCvContour));
  cvFindContours(Ctx.MyThresholdImage, Ctx.MyStorage, @contours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_NONE, cvPoint(0,0));
  if contours <> nil then
  begin
    c := contours;
    while (c <> nil) do
    begin
      { cvMinAreaRect2 — возвращает минимально возможный прямоугольник,
        которым можно обвести контур
          points — последовательность или массив точек
          storage — опционально — временное хранилище памяти
        Отличие функции cvMinAreaRect2 от cvBoundingRect в типе возвращаемой структуры.
        cvMinAreaRect2 возвращает CvBox2D, которая описывает прямоугольник, который
        может быть повёрнут относительно системы координат изображения на угол angle.
      }
      box := cvMinAreaRect2(c);
      area := box.size.width * box.size.height;
      // Если площадь больше нуля
      if area > 0 then
      begin
        // Ширина должна быть больше чем высоты
        if box.size.width > box.size.height then
        begin
          // Фильт по площади
          if (area > min_area) and (area < max_area) then
          begin
            {if box.size.width > box.size.height then
            begin
              WriteLn(Format('[d] Rect size: height=%s, width=%s', [FloatToStr(box.size.height), FloatToStr(box.size.width)]));
              WriteLn(Format('[d] Ratio: %s', [FloatToStr(IfThen(box.size.width < box.size.height, box.size.width/box.size.height, box.size.height/box.size.width))]));
              WriteLn(Format('[d] Full Contour Area: %s', [FloatToStr(abs(cvContourArea(c, CV_WHOLE_SEQ)))]));
              WriteLn(Format('[d] Contour Area: %s', [FloatToStr(abs(cvContourArea(c, CV_WHOLE_SEQ))/(area))]));
              WriteLn('------------------------- ');
              cvRectangle(Ctx.MyOrigColorImage, cvPoint(Round(box.center.x-box.size.width/2),Round(box.center.y-box.size.height/2)), cvPoint(Round(box.center.x+box.size.width/2),Round(box.center.y+box.size.height/2)), CV_RGB(255,255,0), 2);
            end;}
            ratio := IfThen(box.size.width < box.size.height, box.size.width/box.size.height, box.size.height/box.size.width);
            // Фильт по соотношению сторон
            if (ratio > min_ratio) and (ratio < max_ratio) then
            begin
              { cvContourArea — возвращает площадь контура
                  contour — контур (последовательность или массив вершин)
                  slice — начальная и конечные точки контура (по-умолчанию весь контур)
              }
              // Фильт по площади контура
              contourarea := cvContourArea(c, CV_WHOLE_SEQ);
              if (abs(contourarea) > minfullarea) and (abs(contourarea) < maxfullarea) then
              begin
                // Фильт по соотношению площади контура к площади прямоугольника
                if abs(contourarea)/(area) > minfillarea then
                begin
                  WriteLn(Format('[i] Rect size: height=%s, width=%s', [FloatToStr(box.size.height), FloatToStr(box.size.width)]));
                  WriteLn(Format('[i] Ratio: %s', [FloatToStr(ratio)]));
                  WriteLn(Format('[i] Full Contour Area: %s', [FloatToStr(abs(contourarea))]));
                  WriteLn(Format('[i] Contour Area: %s', [FloatToStr(abs(contourarea)/(area))]));
                  WriteLn('------------------------- ');
                  //SetLength(Ctx.MyCvBox2D, length(Ctx.MyCvBox2D)+1);
                  //Ctx.MyCvBox2D[length(Ctx.MyCvBox2D)+1] := box;
                  //SetLength(Ctx.MyArea, length(Ctx.MyArea)+1);
                  //Ctx.MyArea[length(Ctx.MyArea)+1] := cvContourArea(c, CV_WHOLE_SEQ);
                  //cvRectangle(Ctx.MyOrigColorImage, cvPoint(Round(box.center.x-box.size.width/2),Round(box.center.y-box.size.height/2)), cvPoint(Round(box.center.x+box.size.width/2),Round(box.center.y+box.size.height/2)), CV_RGB(255,255,0), 2);
                  { cvBoundingRect — возвращает прямоугольник, которым можно обвести контур
                    points — набор 2D-точек — последовательность или вектор (CvMat) точек
                    update — флаг обновления:
                    0 (CvContour) — прямоугольник не рассчитывается, а берётся из поля rect заголовка контура
                    1 (CvContour) — прямоугольник рассчитывается и записывается в поле rect заголовка контура
                    0 (CvSeq или CvMat) — прямоугольник рассчитывается и возвращается
                    1 (CvSeq или CvMat) — ! ошибка выполнения!
                    Функция возвращает прямоугольник, у которого стороны строго вертикальны и
                    горизонтальны (параллельны сторонам(системе координат) изображения).
                  }
                  rect := cvBoundingRect(c);
                  //SetLength(Ctx.MyCvRect, Length(Ctx.MyCvRect)+1);
                  //Ctx.MyCvRect[Length(Ctx.MyCvRect)+1] := rect;
                  cvRectangle(Ctx.MyOrigColorImage, cvPoint(rect.x, rect.y), cvPoint(rect.x+rect.width, rect.y+rect.height), CV_RGB(255,255,0), 2);
                end;
              end;
            end;
          end;
        end;
      end;
      c := c.h_next;
    end;
    c := nil;
    FreeMem(c, SizeOf(TCvContour));
  end;
  contours := nil;
  FreeMem(contours, SizeOf(TCvContour));
  cvClearMemStorage(Ctx.MyStorage);
end;

// Функция поворота изображения на заданный угол
procedure RotateImage(var image: pIplImage; angle: double = 90);
var
  rot_mat: pCvMat;
  scale: double;
  temp: pIplImage;
  center: TcvPoint2D32f;
begin
  // Матрица трансформации
  rot_mat := cvCreateMat(2, 3, CV_32FC1);
  // Вращение относительно центра изображения
  center.x := image.width div 2;
  center.y := image.height div 2;
  scale := 1;
  cv2DRotationMatrix(center, angle, scale, rot_mat);
  // Создаем изображение
  temp := nil;
  temp := cvCreateImage(cvSize(image.width, image.height), image.depth, image.nChannels);
  // Выполняем вращение
  cvWarpAffine(image, temp, rot_mat, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
  // Копируем изображение
  cvCopy(temp, image);
  // Освобождаем ресурсы
  cvReleaseImage(temp);
  cvReleaseMat(rot_mat);
end;

procedure InitImageCapture(const Ctx: pCtx);
begin
  if FileExists(ImageFileCapture) then
  begin
    Ctx.MyInputImage := cvLoadImage(pAnsiChar(ImageFileCapture), CV_LOAD_IMAGE_GRAYSCALE);
    Ctx.MyOrigColorImage := cvLoadImage(pAnsiChar(ImageFileCapture));
    Ctx.MyThresholdImage := cvCreateImage(cvSize(Ctx.MyInputImage^.width, Ctx.MyInputImage^.height), IPL_DEPTH_8U, 1);
    WriteLn('[i] Start image capture ' + ImageFileCapture);
  end
  else
  begin
    WriteLn('[i] Error reading the image file ' + ImageFileCapture);
    Halt;
  end;
end;

procedure InitVideoCapture(const Ctx: pCtx);
var
  width: Double;
  height: Double;
  fps: Double;
begin
  if SourceCapture = 0 then // Захват видео с камеры
    Ctx.MyCapture := cvCreateCameraCapture(VideoCamNum)
  else if SourceCapture = 2 then // Захват видео из файла
  begin
    if FileExists(VideoFileCapture) then
    begin
      Ctx.MyCapture := cvCreateFileCapture(pAnsiChar(VideoFileCapture));
      WriteVideoCaptureToFile := 0;
    end
    else
    begin
      WriteLn('[i] Error reading the video file ' + VideoFileCapture);
      Halt;
    end;
  end;
  if not Assigned(Ctx.MyCapture) then
  begin
    WriteLn('[i] Error initializing video capture.');
    Halt;
  end;
  // Устанавливаем параметры захвата видео
  if (VideoCamWidth <> 0.0) or (VideoCamHeight <> 0.0) then
  begin
    cvSetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_WIDTH, VideoCamWidth);
    cvSetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_HEIGHT, VideoCamHeight);
    cvSetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FPS, 25.0);
  end;
  // Получаем чиcло кадров
  fps := cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FPS);
  // Узнаем ширину и высоту кадра
  width := cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_WIDTH);
  height := cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_HEIGHT);
  // Создаем картинку
  Ctx.MyOrigColorImage := cvCreateImage(cvSize(Round(width), Round(height)), IPL_DEPTH_8U, 1);
  Ctx.MyOrigColorImage := cvQueryFrame(Ctx.MyCapture);
  WriteLn('[i] Start video capture.');
  WriteLn(Format('[i] VideoCapture size: %.0f x %.0f', [width, height]));
  WriteLn(Format('[i] VideoCapture FPS: %.0f', [fps]));
  // Создаем изображения
  Ctx.MyInputImage := cvCreateImage(cvSize(Ctx.MyOrigColorImage^.width, Ctx.MyOrigColorImage^.height), IPL_DEPTH_8U, 1);
  Ctx.MyThresholdImage := cvCreateImage(cvSize(Ctx.MyOrigColorImage^.width, Ctx.MyOrigColorImage^.height), IPL_DEPTH_8U, 1);
end;

procedure InitVideoWriter(const Ctx: pCtx);
var
  size: TCvSize;
  fps: Double;
begin
  // Размер картинки
  size := cvSize(Trunc(cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_WIDTH)), Trunc(cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_HEIGHT)));
  fps := 10;//cvGetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FPS);
  WriteLn(Format('[i] VideoWriter size: %d x %d', [size.width, size.height]));
  WriteLn(Format('[i] VideoWriter FPS: %.0f', [fps]));
  WriteVideoFileName := IncludeTrailingPathDelimiter(WriteVideoCaptureFileDir) + FormatDateTime('ddmmyyyyhhmmss', Now)+'.avi';
  WriteLn(Format('[i] VideoWriter filename: %s', [WriteVideoFileName]));
  Ctx.MyWriter := cvCreateVideoWriter(pAnsiChar(WriteVideoFileName), CV_FOURCC('X', 'V', 'I', 'D'), fps, size, 1);
end;

procedure ImageFilterAndThreshold(const Ctx: pCtx);
begin
  // Выделение монохромного изображения (бинаризация) с использованием адаптивного подхода.
  // max_value = 255 - Максимальное значение используемое только с CV_THRESH_BINARY и CV_THRESH_BINARY_INV
  // adaptive_method = CV_ADAPTIVE_THRESH_MEAN_C - Метод CV_ADAPTIVE_THRESH_MEAN_C или CV_ADAPTIVE_THRESH_GAUSSIAN_C
  // threshold_type = CV_THRESH_BINARY или CV_THRESH_BINARY_INV
  // block_size = 15 - Размер окрестностей пикселя, используемых для вычисления порогового значения.
  // param1 = 0 - Параметр, зависимый от метода. Для CV_ADAPTIVE_THRESH_MEAN_C и CV_ADAPTIVE_THRESH_GAUSSIAN_C это константа, вычитаемая из среднего значения.
  if BinarizationMethod = 0 then
    cvAdaptiveThreshold(Ctx.MyInputImage, Ctx.MyThresholdImage, ThresholdMaxValue, CV_ADAPTIVE_THRESH_MEAN_C, CV_THRESH_BINARY, AdaptiveThresholdBlockSize, 0)
  else
    // Выделение монохромного изображения (бинаризация) с использованием порогового метода.
    cvThreshold(Ctx.MyInputImage, Ctx.MyThresholdImage, Threshold, ThresholdMaxValue, CV_THRESH_BINARY_INV);
end;

procedure VideoFilterAndThreshold(const Ctx: pCtx);
begin
  cvCvtColor(Ctx.MyOrigColorImage, Ctx.MyInputImage, CV_BGR2GRAY);
  // Выделение монохромного изображения (бинаризация) с использованием адаптивного подхода.
  // max_value = 255 - Максимальное значение используемое только с CV_THRESH_BINARY и CV_THRESH_BINARY_INV
  // adaptive_method = CV_ADAPTIVE_THRESH_MEAN_C - Метод CV_ADAPTIVE_THRESH_MEAN_C или CV_ADAPTIVE_THRESH_GAUSSIAN_C
  // threshold_type = CV_THRESH_BINARY или CV_THRESH_BINARY_INV
  // block_size = 15 - Размер окрестностей пикселя, используемых для вычисления порогового значения.
  // param1 = 0 - Параметр, зависимый от метода. Для CV_ADAPTIVE_THRESH_MEAN_C и CV_ADAPTIVE_THRESH_GAUSSIAN_C это константа, вычитаемая из среднего значения.
  if BinarizationMethod = 0 then
    cvAdaptiveThreshold(Ctx.MyInputImage, Ctx.MyThresholdImage, ThresholdMaxValue, CV_ADAPTIVE_THRESH_MEAN_C, CV_THRESH_BINARY, AdaptiveThresholdBlockSize, 0)
  else
    // Выделение монохромного изображения (бинаризация) с использованием порогового метода.
    cvThreshold(Ctx.MyInputImage, Ctx.MyThresholdImage, Threshold, ThresholdMaxValue, CV_THRESH_BINARY_INV);
end;

procedure InitCtx(const Ctx: pCtx);
begin
  // Создаем хранилища
  Ctx.MyStorage := cvCreateMemStorage(0);
end;

procedure ClearRes(const Ctx: pCtx);
begin
  // Освобождаем ресурсы
  if SourceCapture = 0 then
  begin
    if Assigned(Ctx.MyCapture) then
      cvReleaseCapture(Ctx.MyCapture);
    if Assigned(Ctx.MyWriter) then
      cvReleaseVideoWriter(Ctx.MyWriter);
  end;
  cvReleaseImage(Ctx.MyInputImage);
  cvReleaseImage(Ctx.MyThresholdImage);
  cvReleaseMemStorage(Ctx.MyStorage);
  cvDestroyAllWindows();
end;

procedure InitWindows(const Ctx: pCtx);
begin
  if SourceCapture = 1 then
    cvNamedWindow(WindowNameOriginal, CV_WINDOW_AUTOSIZE);
  cvNamedWindow(WindowNameThreshold, CV_WINDOW_AUTOSIZE);
  cvNamedWindow(WindowNameCarsNumber, CV_WINDOW_AUTOSIZE);
  // Размеcтим окна по рабочему cтолу
  if (Ctx.MyInputImage.width < 1920 / 4) and (Ctx.MyInputImage.height < 1080 / 2) then
  begin
    if SourceCapture = 1 then
    begin
      cvMoveWindow(WindowNameOriginal, 0, 0);
      cvMoveWindow(WindowNameThreshold, Ctx.MyInputImage.width + 10, 0);
      cvMoveWindow(WindowNameCarsNumber, 0, Ctx.MyInputImage.height + 35);
    end
    else
    begin
      cvMoveWindow(WindowNameThreshold, 0, 0);
      cvMoveWindow(WindowNameCarsNumber, Ctx.MyInputImage.width + 10, 0);
    end;
  end;
end;

procedure ShowWindow(const Ctx: pCtx);
begin
  // Поворачиваем изображение
  if RotateImageAngle > 0 then
  begin
    RotateImage(MyCtx.MyOrigColorImage, RotateImageAngle);
    RotateImage(MyCtx.MyThresholdImage, RotateImageAngle);
    if SourceCapture = 1 then
      RotateImage(MyCtx.MyInputImage, RotateImageAngle);
  end;
  // Показываем исходное изображение
  if SourceCapture = 1 then
    cvShowImage(WindowNameOriginal, MyCtx.MyInputImage);
  // Показываем отфильтрованное изображение
  cvShowImage(WindowNameThreshold, MyCtx.MyThresholdImage);
  // Показываем что получилоcь
  cvShowImage(WindowNameCarsNumber, MyCtx.MyOrigColorImage);
end;

// Загружаем настройки
procedure InitSettings(inipath: WideString);
var
  fullininame: WideString;
  INI: TIniFile;
begin
  // Проверяем наличие каталога
  if not DirectoryExists(inipath) then
  begin
    try
      CreateDir(inipath);
    except
      on e :
        Exception do
          WriteLn('Exception in procedure InitSettings: Unable to create directory ' + inipath);
    end;
  end;
  fullininame := inipath + filename_ini;
  if FileExists(fullininame) then
  begin
    INI := TIniFile.Create(fullininame);
    try
      // 0 - VideoCam, 1 - ImageFile, 2 - VideoFile
      SourceCapture := INI.ReadInteger('Main', 'SourceCapture', 0);
      VideoCamNum :=INI.ReadInteger('Main', 'VideoCamNum', 0);
      VideoCamWidth := INI.ReadFloat('Main', 'VideoCamWidth', 0.0);
      VideoCamHeight := INI.ReadFloat('Main', 'VideoCamWidth', 0.0);
      ImageFileCapture := INI.ReadString('Main', 'ImageFileCapture', 'Resource\CarNom1.jpg');
      VideoFileCapture := INI.ReadString('Main', 'VideoFileCapture', 'Resource\CarNom.avi');
      RotateImageAngle := INI.ReadInteger('Main', 'RotateImageAngle', 0);
      WriteVideoCaptureToFile := INI.ReadInteger('Main', 'WriteVideoCaptureToFile', 0);
      WriteVideoCaptureFileDir := INI.ReadString('Main', 'WriteVideoCaptureFileDir', ExtractFilePath(ParamStr(0)));
      if WriteVideoCaptureFileDir = '' then
        WriteVideoCaptureFileDir := ExtractFilePath(ParamStr(0));
      // 0 - cvAdaptiveThreshold, 1 - cvThreshold
      BinarizationMethod := INI.ReadInteger('Main', 'BinarizationMethod', 0);
      AdaptiveThresholdBlockSize := INI.ReadInteger('Main', 'AdaptiveThresholdBlockSize', 15);
      Threshold := INI.ReadInteger('Main', 'Threshold', 128);
      ThresholdMaxValue := INI.ReadInteger('Main', 'ThresholdMaxValue', 255);
      MinRatio := INI.ReadFloat('Main', 'MinRatio', 0.15);
      MaxRatio := INI.ReadFloat('Main', 'MaxRatio', 0.5);
      MinArea := INI.ReadInteger('Main', 'MinArea', 500);
      MaxArea := INI.ReadInteger('Main', 'MaxArea', 25000);
      MinFullArea := INI.ReadInteger('Main', 'MinFullArea', 800);
      MaxFullArea := INI.ReadInteger('Main', 'MaxFullArea', 7000);
      MinFillArea := INI.ReadFloat('Main', 'MinFillArea', 0.7);
    finally
      INI.Free;
    end;
  end
  else
  begin
    INI := TIniFile.Create(fullininame);
    try
      SourceCapture := 0;
      VideoCamNum := 0;
      VideoCamWidth := 0.0;
      VideoCamHeight := 0.0;
      ImageFileCapture := 'Resource\CarNom1.jpg';
      VideoFileCapture := 'Resource\CarNom.avi';
      WriteVideoCaptureFileDir := ExtractFilePath(ParamStr(0));
      RotateImageAngle := 0;
      WriteVideoCaptureToFile := 0;
      BinarizationMethod := 0;
      AdaptiveThresholdBlockSize := 15;
      Threshold := 128;
      ThresholdMaxValue := 255;
      MinRatio := 0.15;
      MaxRatio := 0.5;
      MinArea := 500;
      MaxArea := 25000;
      MinFullArea := 800;
      MaxFullArea := 7000;
      MinFillArea := 0.7;
      // Сохраняем настройки
      INI.WriteInteger('Main', 'SourceCapture', SourceCapture);
      INI.WriteInteger('Main', 'VideoCamNum', VideoCamNum);
      INI.WriteFloat('Main', 'VideoCamWidth', VideoCamWidth);
      INI.WriteFloat('Main', 'VideoCamWidth', VideoCamHeight);
      INI.WriteString('Main', 'ImageFileCapture', ImageFileCapture);
      INI.WriteString('Main', 'VideoFileCapture', VideoFileCapture);
      INI.WriteInteger('Main', 'RotateImageAngle', RotateImageAngle);
      INI.WriteInteger('Main', 'WriteVideoCaptureToFile', WriteVideoCaptureToFile);
      INI.WriteString('Main', 'WriteVideoCaptureFileDir', WriteVideoCaptureFileDir);
      INI.WriteInteger('Main', 'BinarizationMethod', BinarizationMethod);
      INI.WriteInteger('Main', 'AdaptiveThresholdBlockSize', AdaptiveThresholdBlockSize);
      INI.WriteInteger('Main', 'Threshold', Threshold);
      INI.WriteInteger('Main', 'ThresholdMaxValue', ThresholdMaxValue);
      INI.WriteFloat('Main', 'MinRatio', MinRatio);
      INI.WriteFloat('Main', 'MaxRatio', MaxRatio);
      INI.WriteInteger('Main', 'MinArea', MinArea);
      INI.WriteInteger('Main', 'MaxArea', MaxArea);
      INI.WriteInteger('Main', 'MinFullArea', MinFullArea);
      INI.WriteInteger('Main', 'MaxFullArea', MaxFullArea);
      INI.WriteFloat('Main', 'MinFillArea', MinFillArea);
    finally
      INI.Free;
    end;
  end;
end;

begin
  try
    MyCtx := AllocMem(sizeof(TCtx));
    InitSettings(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
    InitCtx(MyCtx);
    // Захват из файла картинки
    if SourceCapture = 1 then
    begin
      InitImageCapture(MyCtx);
      InitWindows(MyCtx);
      // Фильтруем
      ImageFilterAndThreshold(MyCtx);
      // Поиск номера
      // 0.15 и 0.5 - мин. и макс. соотношение сторон
      // 500 и 25000 - мин. и макс. площадь
      // 800 и 7000 - мин. и макс.
      // 0.7 - мин. соотношение площади контура к площади прямоугольника
      FindBox(MyCtx, MinRatio, MaxRatio, MinArea, MaxArea, MinFullArea, MaxFullArea, MinFillArea);
      // Показываем результат
      ShowWindow(MyCtx);
      // Ждём нажатия клавиши
      cvWaitKey(0);
    end
    else // Захват видео с камеры или из файла
    begin
      InitVideoCapture(MyCtx);
      InitWindows(MyCtx);
      if WriteVideoCaptureToFile = 1 then
        InitVideoWriter(MyCtx);
      while true do
      begin
        MyCtx.MyOrigColorImage := cvQueryFrame(MyCtx.MyCapture);
        if MyCtx.MyOrigColorImage = nil then
          Break;
        // Фильтруем
        VideoFilterAndThreshold(MyCtx);
        // Поиск номера
        // 0.15 и 0.5 - мин. и макс. соотношение сторон
        // 500 и 25000 - мин. и макс. площадь
        // 800 и 7000 - мин. и макс. площадь контура
        // 0.7 - мин. соотношение площади контура к площади прямоугольника
        FindBox(MyCtx, MinRatio, MaxRatio, MinArea, MaxArea, MinFullArea, MaxFullArea, MinFillArea);
        // Показываем результат
        ShowWindow(MyCtx);
        // Сохраняем в файл
        if Assigned(MyCtx.MyWriter) then
          cvWriteFrame(MyCtx.MyWriter, MyCtx.MyOrigColorImage);
        key := cvWaitKey(33);
        if (key = 27) then
          Break;
      end;
    end;
    // Очистка ресурсов
    ClearRes(MyCtx);
    FreeMem(MyCtx, SizeOf(TCtx));
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.

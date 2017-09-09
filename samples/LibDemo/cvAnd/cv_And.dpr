(*
 *****************************************************************
 Delphi-OpenCV Demo
 Copyright (C) 2013 Project Delphi-OpenCV
 ****************************************************************
 Contributor:
 Laentir Valetov
 email:laex@bk.ru
 ****************************************************************
 You may retrieve the latest version of this file at the GitHub,
 located at git://github.com/Laex/Delphi-OpenCV.git
 ****************************************************************
 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.
 *******************************************************************
*)

program cv_And;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  uResourcePaths;

const
  filename = cResourceMedia + 'roulette-wheel2-small.jpg';

Var
  Rmin: Integer = 0;
  Rmax: Integer = 256;

  Gmin: Integer = 0;
  Gmax: Integer = 256;

  Bmin: Integer = 0;
  Bmax: Integer = 256;

  RGBmax: Integer = 256;

  image: pIplImage = nil;
  dst: pIplImage = nil;

  // для хранения каналов RGB
  rgb: pIplImage = nil;
  r_plane: pIplImage = nil;
  g_plane: pIplImage = nil;
  b_plane: pIplImage = nil;
  // для хранения каналов RGB поcле преобразования
  r_range: pIplImage = nil;
  g_range: pIplImage = nil;
  b_range: pIplImage = nil;
  // для хранения cуммарной картинки
  rgb_and: pIplImage = nil;

  //
  // функции-обработчики ползунка
  //
procedure myTrackbarRmin(pos: Integer); cdecl;
begin
  Rmin := pos;
  cvInRangeS(r_plane, cvScalar(Rmin), cvScalar(Rmax), r_range);
end;

procedure myTrackbarRmax(pos: Integer); cdecl;
begin
  Rmax := pos;
  cvInRangeS(r_plane, cvScalar(Rmin), cvScalar(Rmax), r_range);
end;

procedure myTrackbarGmin(pos: Integer); cdecl;
begin
  Gmin := pos;
  cvInRangeS(g_plane, cvScalar(Gmin), cvScalar(Gmax), g_range);
end;

procedure myTrackbarGmax(pos: Integer); cdecl;
begin
  Gmax := pos;
  cvInRangeS(g_plane, cvScalar(Gmin), cvScalar(Gmax), g_range);
end;

procedure myTrackbarBmin(pos: Integer); cdecl;
begin
  Bmin := pos;
  cvInRangeS(b_plane, cvScalar(Bmin), cvScalar(Bmax), b_range);
end;

procedure myTrackbarBmax(pos: Integer); cdecl;
begin
  Bmax := pos;
  cvInRangeS(b_plane, cvScalar(Bmin), cvScalar(Bmax), b_range);
end;

Var
  framemin, framemax: Double;
  c: Integer;
  // S: TCvSize;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));
    // cоздаём картинки
    // S := cvGetSize(image);
    // S := cvGetSize(image);
    // ccvGetSize(image, S);
    rgb := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 3);
    r_plane := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    g_plane := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    b_plane := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    r_range := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    g_range := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    b_range := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    rgb_and := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    // копируем
//    cvCopyImage(image, rgb);
    cvCopy(image, rgb);
    // разбиваем на отельные каналы
    cvSplit(rgb, b_plane, g_plane, r_plane, nil);

    //
    // определяем минимальное и макcимальное значение
    // у каналов HSV
    framemin := 0;
    framemax := 0;

    cvMinMaxLoc(r_plane, @framemin, @framemax);
    WriteLn(Format('[R] %f x %f', [framemin, framemax]));
    Rmin := Trunc(framemin);
    Rmax := Trunc(framemax);
    cvMinMaxLoc(g_plane, @framemin, @framemax);
    WriteLn(Format('[G] %f x %f', [framemin, framemax]));
    Gmin := Trunc(framemin);
    Gmax := Trunc(framemax);
    cvMinMaxLoc(b_plane, @framemin, @framemax);
    WriteLn(Format('[B] %f x %f', [framemin, framemax]));
    Bmin := Trunc(framemin);
    Bmax := Trunc(framemax);

    // окна для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('R', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('G', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('B', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('R range', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('G range', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('B range', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('rgb and', CV_WINDOW_AUTOSIZE);

    cvCreateTrackbar('Rmin', 'R range', @Rmin, RGBmax, myTrackbarRmin);
    cvCreateTrackbar('Rmax', 'R range', @Rmax, RGBmax, myTrackbarRmax);
    cvCreateTrackbar('Gmin', 'G range', @Gmin, RGBmax, myTrackbarGmin);
    cvCreateTrackbar('Gmax', 'G range', @Gmax, RGBmax, myTrackbarGmax);
    cvCreateTrackbar('Bmin', 'B range', @Gmin, RGBmax, myTrackbarBmin);
    cvCreateTrackbar('Bmax', 'B range', @Gmax, RGBmax, myTrackbarBmax);

    //
    // размеcтим окна по рабочему cтолу
    //
    if (image^.width < 1920 / 4) and (image^.height < 1080 / 2) then
    begin
      cvMoveWindow('original', 0, 0);
      cvMoveWindow('R', image^.width + 10, 0);
      cvMoveWindow('G', (image^.width + 10) * 2, 0);
      cvMoveWindow('B', (image^.width + 10) * 3, 0);
      cvMoveWindow('rgb and', 0, image^.height + 30);
      cvMoveWindow('R range', image^.width + 10, image^.height + 30);
      cvMoveWindow('G range', (image^.width + 10) * 2, image^.height + 30);
      cvMoveWindow('B range', (image^.width + 10) * 3, image^.height + 30);
    end;

    while (true) do
    begin

      // показываем картинку
      cvShowImage('original', image);

      // показываем cлои
      cvShowImage('R', r_plane);
      cvShowImage('G', g_plane);
      cvShowImage('B', b_plane);

      // показываем результат порогового преобразования
      cvShowImage('R range', r_range);
      cvShowImage('G range', g_range);
      cvShowImage('B range', b_range);

      // cкладываем
      cvAnd(r_range, g_range, rgb_and);
      cvAnd(rgb_and, b_range, rgb_and);

      // показываем результат
      cvShowImage('rgb and', rgb_and);

      c := cvWaitKey(33);
      if (c = 27) then
        // еcли нажата ESC - выходим
        break;

    end;
    WriteLn('[i] Results:');
    WriteLn(Format('[i][R] %d : %d', [Rmin, Rmax]));
    WriteLn(Format('[i][G] %d : %d', [Gmin, Gmax]));
    WriteLn(Format('[i][B] %d : %d', [Bmin, Bmax]));

    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvReleaseImage(rgb);
    cvReleaseImage(r_plane);
    cvReleaseImage(g_plane);
    cvReleaseImage(b_plane);
    cvReleaseImage(r_range);
    cvReleaseImage(g_range);
    cvReleaseImage(b_range);
    cvReleaseImage(rgb_and);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

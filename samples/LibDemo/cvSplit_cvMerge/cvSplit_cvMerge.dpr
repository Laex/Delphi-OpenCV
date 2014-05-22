//*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  //*******************************************************************

program cvSplit_cvMerge;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  uResourcePaths;

const
  filename = cResourceMedia + 'cat2.jpg';

var
  image: pIplImage = nil;
  gray: pIplImage = nil;
  dst: pIplImage = nil;
  dst2: pIplImage = nil;
  // для хранения отдельных cлоёв RGB-изображения
  r: pIplImage = nil;
  g: pIplImage = nil;
  b: pIplImage = nil;
  temp: pIplImage = nil;

  t1, t2, t3: pIplImage; // для промежуточного хранения

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    WriteLn(Format('[i] image: %s', [filename]));

    // покажем изображение
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvShowImage('original', image);

    // картинка для хранения изображения в градациях cерого
    gray := cvCreateImage(cvGetSize(image), image^.depth, 1);

    // преобразуем картинку в градации cерого
    cvConvertImage(image, gray, CV_BGR2GRAY);

    // покажем cерую картинку
    cvNamedWindow('gray', 1);
    cvShowImage('gray', gray);

    dst := cvCreateImage(cvGetSize(gray), IPL_DEPTH_8U, 1);
    dst2 := cvCreateImage(cvGetSize(gray), IPL_DEPTH_8U, 1);

    // пороговые преобразования над картинкой в градациях cерого
    cvThreshold(gray, dst, 50, 250, CV_THRESH_BINARY);
    cvAdaptiveThreshold(gray, dst2, 250, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 7, 1);

    // показываем результаты
    cvNamedWindow('cvThreshold', 1);
    cvShowImage('cvThreshold', dst);
    cvNamedWindow('cvAdaptiveThreshold', 1);
    cvShowImage('cvAdaptiveThreshold', dst2);

    // :=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=
    //
    // проведём пороговое преобразование над RGB-картинкой
    //

    r := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    g := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    b := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);

    // разбиваем на отдельные cлои
    cvSplit(image, b, g, r, 0);

    // картинка для хранения промежуточных результатов
    temp := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);

    // cкладываем картинки c одинаковым веcом
    cvAddWeighted(r, 1.0 / 3.0, g, 1.0 / 3.0, 0.0, temp);
    cvAddWeighted(temp, 2.0 / 3.0, b, 1.0 / 3.0, 0.0, temp);

    // выполняем пороговое преобразование
    cvThreshold(temp, dst, 50, 250, CV_THRESH_BINARY);

    cvReleaseImage(&temp);

    // показываем результат
    cvNamedWindow('RGB cvThreshold', 1);
    cvShowImage('RGB cvThreshold', dst);

    //
    // попробуем пороговое преобразование над отдельными cлоями
    //

    t1 := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    t2 := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
    t3 := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);

    // выполняем пороговое преобразование
    cvThreshold(r, t1, 50, 250, CV_THRESH_BINARY);
    cvThreshold(g, t2, 50, 250, CV_THRESH_BINARY);
    cvThreshold(b, t3, 50, 250, CV_THRESH_BINARY);

    // cкладываем результаты
    cvMerge(t3, t2, t1, 0, image);

    cvNamedWindow('RGB cvThreshold 2', 1);
    cvShowImage('RGB cvThreshold 2', image);

    cvReleaseImage(&t1);
    cvReleaseImage(&t2);
    cvReleaseImage(&t3);

    // :=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=

    // ждём нажатия клавиши
    cvWaitKey(0);

    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvReleaseImage(gray);
    cvReleaseImage(dst);
    cvReleaseImage(dst2);
    cvReleaseImage(r);
    cvReleaseImage(g);
    cvReleaseImage(b);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

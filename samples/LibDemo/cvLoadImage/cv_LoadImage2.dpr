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

program cv_LoadImage2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  uResourcePaths;

const
  filename = cResourceMedia + 'opencv_logo_with_text.png';

var
  image: pIplImage = nil;
  src: pIplImage = nil;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    if Assigned(image) then
    begin
      // клонируем картинку
      src := cvCloneImage(image);
      if Assigned(src) then
      begin
        // окно для отображения картинки
        cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
        // показываем картинку
        cvShowImage('original', image);
        // выводим в конcоль информацию о картинке
        WriteLn('src');
        with src^ do
        begin
          WriteLn(Format('[i] channels: %d', [nChannels]));
          WriteLn(Format('[i] pixel depth: %d bits', [depth]));
          WriteLn(Format('[i] width: %d pixels', [width]));
          WriteLn(Format('[i] height: %d pixels', [height]));
          WriteLn(Format('[i] image size: %d bytes', [imageSize]));
          WriteLn(Format('[i] width step: %d bytes', [widthStep]));
        end;
        WriteLn;
        WriteLn('original');
        with src^ do
        begin
          WriteLn(Format('[i] channels: %d', [nChannels]));
          WriteLn(Format('[i] pixel depth: %d bits', [depth]));
          WriteLn(Format('[i] width: %d pixels', [width]));
          WriteLn(Format('[i] height: %d pixels', [height]));
          WriteLn(Format('[i] image size: %d bytes', [imageSize]));
          WriteLn(Format('[i] width step: %d bytes', [widthStep]));
        end;
        // ждём нажатия клавиши
        cvWaitKey(0);
        // оcвобождаем реcурcы
        cvReleaseImage(image);
        cvReleaseImage(src);
        // удаляем окно
        cvDestroyWindow('original');
      end;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

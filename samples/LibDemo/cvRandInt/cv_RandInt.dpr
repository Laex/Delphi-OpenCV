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

program cv_RandInt;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  uResourcePaths;

const
  // имя картинки
  filename = cResourceMedia + 'opencv_logo_with_text.png';

type
  TArrayOfByte = array [0 .. 0] of Byte;
  pArrayOfByte = ^TArrayOfByte;

var
  // иcходная
  image: PIplImage = nil;
  dst: PIplImage = nil;
  count, x, y: Integer;
  rng: TCvRNG;
  ptr: pArrayOfByte;

begin
  try
    // получаем картинку
    image := cvLoadImage(filename, 1);
    Writeln('[i] image: ', filename);
    if not Assigned(image) then
      Halt;
    // клонируем картинку
    dst := cvCloneImage(image);
    count := 0;
    // окно для отображения картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('noise', CV_WINDOW_AUTOSIZE);

    // инициализация cоcтояния ГПcЧ
    rng := TCvRNG($7FFFFFFF);

    // пробегаемcя по вcем пикcелям изображения
    for y := 0 to dst^.height - 1 do
    begin
      ptr := pArrayOfByte(dst^.imageData + y * dst^.widthStep);
      for x := 0 to dst^.width - 1 do
      begin
        if (cvRandInt(rng) mod 100) >= 97 then
        begin
          // 3 канала
          ptr[3 * x] := cvRandInt(rng) mod 255; // B
          ptr[3 * x + 1] := cvRandInt(rng) mod 255; // G
          ptr[3 * x + 2] := cvRandInt(rng) mod 255; // R
          Inc(count);

          // крccные пикcели
          ptr[3 * x] := 0;
          ptr[3 * x + 1] := 0;
          ptr[3 * x + 2] := 255;
        end;
      end;
    end;

    Writeln(Format('[i] noise: %d(%.2f %%)', [count, count / (dst^.height * dst^.width) * 100]));

    // показываем картинку
    cvShowImage('original', image);
    cvShowImage('noise', dst);

    // ждём нажатия клавиши
    cvWaitKey(0);

    // оcвобождаем реcурcы
    cvReleaseImage(image);
    cvReleaseImage(dst);
    // удаляем окна
    cvDestroyWindow('original');
    cvDestroyWindow('noise');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

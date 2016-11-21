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

{$I OpenCV.inc}

unit ocv.editor;

interface

Uses
  ocv.core.types_c;

procedure ImageToHSVPlane(const InputImage: pIplImage; var h_plane, s_plane, v_plane: pIplImage);
procedure HSVPlaneToImage(const h_plane, s_plane, v_plane: pIplImage; Var OutputImage: pIplImage);

// --------------------------------------- Насыщенность ------------------------------------------------------
{
  Для изменения насыщенности изображение преобразуется в систему цветности
  HSV и разбивается на слои. К значениям слоя «Sature» прибавляется шаг. Слои объединяются.
}
procedure Sature(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
// ------------------------------- Экспозиция ----------------------------------
{
  Изображение преобразуется в HSV и разбивается на слои.
  Для слоя «Value» выполняем преобразование с помощью гистограммы,
  заданной функцией i + sin(i * 0.01255) * step * 10.
}
procedure Expo(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
// ---------------------------- Оттенок -------------------------------------
{
  Параметр оттенка характеризует наличие в изображении зеленого и пурпурного цвета.
  В системе цветности RGB можно управлять зеленым слоем, но при этом нужно не забывать
  компенсировать падение яркости другим двумя слоями. Для преобразования красного и
  синего слоев используется положительная гамма-функция экспозиции, для зеленого – отрицательная.
}
procedure Hue(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);

// ------------------------------------ Цветовая температура ------------------------------
{
  Ингредиенты: те же, что и в оттенке, но гистограммы для красного и зеленого положительные, а для синего слоя двойная отрицательная.
  Цветовая температура характеризует наличие в изображении желтого и синего цветов. Значит будем «крутить» синий.
}

procedure Temperature(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);

// ------------------------------------ Свет и тени ------------------------------
{
  Параметр «свет» характеризует яркость светлых областей изображения,
  а параметр «тени» — яркость темных областей. Преобразовывать будем канал яркостей.
}
procedure White(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
procedure Shadow(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
// -------------------------------- Контраст ----------------------------------
{
  Контраст определяется в разности яркостей.
  Т.е. для увеличения контраста нам нужно раздвинуть диапазон
  яркостей от центра к краям. Преобразование выполняется для всех слоев.
}
procedure Contrast(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
// -------------------------------- Резкость ----------------------------------
{
  Резкость (четкость) определяется выделением отдельных элементов, их контуров.
  Величина, обратная резкости – размытость.
  В opencv для размытия изображения используем функцию blur,
  принимающую в качестве параметров исходное изображение, выходное изображение,
  и размер матрицы размытия. От размера матрицы размытия и зависит сила размытия.
  Этот размер должен быть четным, чтобы не указывать вручную центр матрицы.
  Четкость в opencv проще всего повысить с помощью матрицы свертки, используя
  специальную для этого матрицу. Функция «filter2D», которая принимает исходное
  изображение, результирующее изображение, количество бит на значение матрицы
  свертки, матрицу свертки, выполняет непосредственно преобразование.
  Итак, как будет выглядеть метод повышения/понижения четкости.
}

procedure Clarity(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);

implementation

Uses
  System.Math,
  ocv.core_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

// ---------------------------------------------------------------------------------------------
procedure ImageToHSVPlane(const InputImage: pIplImage; var h_plane, s_plane, v_plane: pIplImage);
Var
  hsv: pIplImage;
begin
  hsv := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 3);
  h_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  s_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  v_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  try
    cvCvtColor(InputImage, hsv, CV_BGR2HSV_FULL);
    cvCvtPixToPlane(hsv, h_plane, s_plane, v_plane, nil);
  finally
    cvReleaseImage(hsv);
  end;
end;

procedure HSVPlaneToImage(const h_plane, s_plane, v_plane: pIplImage; Var OutputImage: pIplImage);
Var
  hsv: pIplImage;
begin
  hsv := cvCreateImage(cvGetSize(h_plane), IPL_DEPTH_8U, 3);
  OutputImage := cvCreateImage(cvGetSize(h_plane), IPL_DEPTH_8U, 3);
  try
    cvCvtPlaneToPix(h_plane, s_plane, v_plane, nil, hsv);
    cvCvtColor(hsv, OutputImage, CV_HSV2BGR_FULL);
  finally
    cvReleaseImage(hsv);
  end;
end;

function AddDoubleToByte(const bt: byte; const d: double): byte;
begin
  result := bt;
  if (result + d) > 255 then
    result := 255
  else if (result + d) < 0 then
    result := 0
  else
    result := result + Trunc(d);
end;

function GetGammaExpo(const step: Integer): pIplImage;
Var
  i: Integer;
begin
  result := cvCreateImage(cvSize(256, 1), IPL_DEPTH_8U, 1);
  for i := 0 to 255 do
    result^.imageData[i] := AddDoubleToByte(i, sin(i * 0.01255) * step * 10);
end;

// --------------------------------------- Насыщенность ------------------------------------------------------
{
  Для изменения насыщенности изображение преобразуется в систему цветности
  HSV и разбивается на слои. К значениям слоя «Sature» прибавляется шаг. Слои объединяются.
}
procedure Sature(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane: pIplImage;
  s_plane: pIplImage;
  v_plane: pIplImage;
  i, j: Integer;
begin
  ImageToHSVPlane(InputImage, h_plane, s_plane, v_plane);
  try
    for i := 0 to s_plane^.width * s_plane^.height - 1 do
    begin
      j := s_plane^.imageData[i] + step * 5;
      if j < 256 then
        s_plane^.imageData[i] := j
      else
        s_plane^.imageData[i] := 255;
    end;
    HSVPlaneToImage(h_plane, s_plane, v_plane, OutputImage);
  finally
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
  end;
end;

// ------------------------------- Экспозиция ----------------------------------
{
  Изображение преобразуется в HSV и разбивается на слои.
  Для слоя «Value» выполняем преобразование с помощью гистограммы,
  заданной функцией i + sin(i * 0.01255) * step * 10.
}
procedure Expo(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane: pIplImage;
  s_plane: pIplImage;
  v_plane: pIplImage;
  _v_plane: pIplImage;
  lut: pIplImage;
begin
  ImageToHSVPlane(InputImage, h_plane, s_plane, v_plane);
  _v_plane := cvClone(v_plane);
  lut := GetGammaExpo(step);
  try
    cvLUT(v_plane, _v_plane, lut);
    HSVPlaneToImage(h_plane, s_plane, _v_plane, OutputImage);
  finally
    cvReleaseImage(lut);
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
    cvReleaseImage(_v_plane);
  end;
end;

// ---------------------------- Оттенок -------------------------------------
{
  Параметр оттенка характеризует наличие в изображении зеленого и пурпурного цвета.
  В системе цветности RGB можно управлять зеленым слоем, но при этом нужно не забывать
  компенсировать падение яркости другим двумя слоями. Для преобразования красного и
  синего слоев используется положительная гамма-функция экспозиции, для зеленого – отрицательная.
}
procedure Hue(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane, s_plane, v_plane: pIplImage;
  _h_plane, _s_plane, _v_plane: pIplImage;
  lut0, lut1, lut2: pIplImage;
begin
  h_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  s_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  v_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  _h_plane := cvClone(h_plane);
  _s_plane := cvClone(s_plane);
  _v_plane := cvClone(v_plane);
  try
    cvCvtPixToPlane(InputImage, h_plane, s_plane, v_plane, nil);
    lut0 := GetGammaExpo(step);
    lut1 := GetGammaExpo(-step);
    lut2 := GetGammaExpo(step);
    cvLUT(h_plane, _h_plane, lut0);
    cvLUT(s_plane, _s_plane, lut1);
    cvLUT(v_plane, _v_plane, lut2);
    OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
    cvCvtPlaneToPix(_h_plane, _s_plane, _v_plane, nil, OutputImage);
  finally
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
    cvReleaseImage(_h_plane);
    cvReleaseImage(_s_plane);
    cvReleaseImage(_v_plane);
    cvReleaseImage(lut0);
    cvReleaseImage(lut1);
    cvReleaseImage(lut2);
  end;
end;

// ------------------------------------ Цветовая температура ------------------------------
{
  Ингредиенты: те же, что и в оттенке, но гистограммы для красного и зеленого положительные, а для синего слоя двойная отрицательная.
  Цветовая температура характеризует наличие в изображении желтого и синего цветов. Значит будем «крутить» синий.
}

procedure Temperature(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane, s_plane, v_plane: pIplImage;
  _h_plane, _s_plane, _v_plane: pIplImage;
  lut0, lut1, lut2: pIplImage;
begin
  h_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  s_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  v_plane := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  _h_plane := cvClone(h_plane);
  _s_plane := cvClone(s_plane);
  _v_plane := cvClone(v_plane);
  try
    cvCvtPixToPlane(InputImage, h_plane, s_plane, v_plane, nil);
    lut0 := GetGammaExpo(-step * 2);
    lut1 := GetGammaExpo(step);
    lut2 := GetGammaExpo(step);
    cvLUT(h_plane, _h_plane, lut0);
    cvLUT(s_plane, _s_plane, lut1);
    cvLUT(v_plane, _v_plane, lut2);
    OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
    cvCvtPlaneToPix(_h_plane, _s_plane, _v_plane, nil, OutputImage);
  finally
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
    cvReleaseImage(_h_plane);
    cvReleaseImage(_s_plane);
    cvReleaseImage(_v_plane);
    cvReleaseImage(lut0);
    cvReleaseImage(lut1);
    cvReleaseImage(lut2);
  end;
end;

// ------------------------------------ Свет и тени ------------------------------
{
  Параметр «свет» характеризует яркость светлых областей изображения,
  а параметр «тени» — яркость темных областей. Преобразовывать будем канал яркостей.
}
function GetGammaLightShadow(const step: Integer; const reverse: Boolean): pIplImage;
const
  M_E = 2.71828182845905;
Var
  i: Integer;
  d: double;
begin
  result := cvCreateImage(cvSize(256, 1), IPL_DEPTH_8U, 1);
  if reverse then
    d := 256
  else
    d := 0;
  for i := 0 to 255 do
    result^.imageData[i] := AddDoubleToByte(i, power(0.36811145 * M_E, -power(abs(d - i), 1.7)) * 0.2 * step * abs(d - i));
end;

procedure White(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane: pIplImage;
  s_plane: pIplImage;
  v_plane: pIplImage;
  lut: pIplImage;
  _v_plane: pIplImage;
begin
  ImageToHSVPlane(InputImage, h_plane, s_plane, v_plane);
  _v_plane := cvClone(v_plane);
  try
    lut := GetGammaLightShadow(step, true);
    cvLUT(v_plane, _v_plane, lut);
    OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
    HSVPlaneToImage(h_plane, s_plane, _v_plane, OutputImage);
  finally
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
    cvReleaseImage(_v_plane);
    cvReleaseImage(lut);
  end;
end;

procedure Shadow(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  h_plane: pIplImage;
  s_plane: pIplImage;
  v_plane: pIplImage;
  lut: pIplImage;
  _v_plane: pIplImage;
begin
  ImageToHSVPlane(InputImage, h_plane, s_plane, v_plane);
  _v_plane := cvClone(v_plane);
  try
    lut := GetGammaLightShadow(step, false);
    cvLUT(v_plane, _v_plane, lut);
    OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
    HSVPlaneToImage(h_plane, s_plane, _v_plane, OutputImage);
  finally
    cvReleaseImage(h_plane);
    cvReleaseImage(s_plane);
    cvReleaseImage(v_plane);
    cvReleaseImage(_v_plane);
    cvReleaseImage(lut);
  end;
end;

// -------------------------------- Контраст ----------------------------------
{
  Контраст определяется в разности яркостей.
  Т.е. для увеличения контраста нам нужно раздвинуть диапазон
  яркостей от центра к краям. Преобразование выполняется для всех слоев.
}
procedure Contrast(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
Var
  rgb_r: pIplImage;
  rgb_g: pIplImage;
  rgb_b: pIplImage;
  _rgb_r: pIplImage;
  _rgb_g: pIplImage;
  _rgb_b: pIplImage;
  contrastLevel, d: double;
  lut: pIplImage;
  p: PByte;
  i: Integer;
begin
  rgb_r := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  rgb_g := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  rgb_b := cvCreateImage(cvGetSize(InputImage), IPL_DEPTH_8U, 1);
  cvCvtPixToPlane(InputImage, rgb_r, rgb_g, rgb_b, nil);
  _rgb_r := cvClone(rgb_r);
  _rgb_g := cvClone(rgb_g);
  _rgb_b := cvClone(rgb_b);
  lut := cvCreateImage(cvSize(256, 1), IPL_DEPTH_8U, 1);
  try
    contrastLevel := (100 + step) / 100;
    p := lut^.imageData;
    for i := 0 to 255 do
    begin
      d := ((i / 255 - 0.5) * contrastLevel + 0.5) * 255;
      if (d > 255) then
        d := 255
      else if (d < 0) then
        d := 0;
      p[i] := Trunc(d);
    end;
    cvLUT(rgb_r, _rgb_r, lut);
    cvLUT(rgb_g, _rgb_g, lut);
    cvLUT(rgb_b, _rgb_b, lut);
    OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
    cvCvtPlaneToPix(_rgb_r, _rgb_g, _rgb_b, nil, OutputImage);
  finally
    cvReleaseImage(rgb_r);
    cvReleaseImage(rgb_g);
    cvReleaseImage(rgb_b);
    cvReleaseImage(_rgb_r);
    cvReleaseImage(_rgb_g);
    cvReleaseImage(_rgb_b);
    cvReleaseImage(lut);
  end;
end;
// -------------------------------- Резкость ----------------------------------
{
  Резкость (четкость) определяется выделением отдельных элементов, их контуров.
  Величина, обратная резкости – размытость.
  В opencv для размытия изображения используем функцию blur,
  принимающую в качестве параметров исходное изображение, выходное изображение,
  и размер матрицы размытия. От размера матрицы размытия и зависит сила размытия.
  Этот размер должен быть четным, чтобы не указывать вручную центр матрицы.
  Четкость в opencv проще всего повысить с помощью матрицы свертки, используя
  специальную для этого матрицу. Функция «filter2D», которая принимает исходное
  изображение, результирующее изображение, количество бит на значение матрицы
  свертки, матрицу свертки, выполняет непосредственно преобразование.
  Итак, как будет выглядеть метод повышения/понижения четкости.
}

procedure Clarity(const step: Integer; const InputImage: pIplImage; Var OutputImage: pIplImage);
var
//  matr: array [0 .. 9] of single;
  kernel_matrix: pCvMat;
begin
  OutputImage := cvCreateImage(cvGetSize(InputImage), InputImage^.depth, InputImage^.nChannels);
  if (step < 0) then
    cvSmooth(InputImage, OutputImage, cv_blur, -step * 2 + 1, step * 2 + 1, 0, 0)
  else
  begin
    kernel_matrix := cvCreateMat(3, 3, CV_32FC1);
    PSingle(kernel_matrix^.Data)[0] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[1] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[2] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[3] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[4] := 1.3 + 0.4 * step;
    PSingle(kernel_matrix^.Data)[5] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[6] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[7] := -0.0375 - 0.05 * step;
    PSingle(kernel_matrix^.Data)[8] := -0.0375 - 0.05 * step;
    cvFilter2D(InputImage, OutputImage, kernel_matrix);
  end;
end;

end.

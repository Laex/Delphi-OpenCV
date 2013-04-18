(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // laentir Valetov
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
  ******************************************************************* *)
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_WarpAffine;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\core\core_c.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  legacy in '..\..\..\include\legacy\legacy.pas',
  calib3d in '..\..\..\include\calib3d\calib3d.pas',
  imgproc in '..\..\..\include\imgproc\imgproc.pas',
  haar in '..\..\..\include\objdetect\haar.pas',
  objdetect in '..\..\..\include\objdetect\objdetect.pas',
  tracking in '..\..\..\include\video\tracking.pas',
  Core in '..\..\..\include\core\core.pas';

const
  filename = 'Resource\cat2.jpg';

var
  src: pIplImage = nil;
  dst: pIplImage = nil;
  rot_mat: pCvMat = nil;
  scale: double;
  temp: pIplImage = nil;
  center: TcvPoint2D32f;

begin
  try
    // Получаем картинку (цветную)
    src := cvLoadImage(filename, CV_LOAD_IMAGE_COLOR);
    WriteLn(Format('[i] image: %s', [filename]));

    // Выводим оригинал
    cvNamedWindow('Original', CV_WINDOW_AUTOSIZE);
    cvShowImage('Original', src);

    // Поворачиваем
    // Матрица транcформации
    rot_mat := cvCreateMat(2, 3, CV_32FC1);
    // Вращение отноcительно центра изображения
    center.x := src^.width div 2;
    center.y := src^.height div 2;
    scale := 1;
    cv2DRotationMatrix(center, 60, scale, rot_mat);

    // cоздаем изображение
    temp := cvCreateImage(cvSize(src^.width, src^.height), src^.depth, src^.nChannels);

    // Выполняем вращение
    cvWarpAffine(src, temp, rot_mat, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

    // Копируем изображение
    cvCopy(temp, src);

    // Оcвобождаем реcурcы
    cvReleaseImage(temp);
    cvReleaseMat(rot_mat);

    // Показываем что получилоcь
    cvNamedWindow('cvWarpAffine', CV_LOAD_IMAGE_GRAYSCALE);
    cvShowImage('cvWarpAffine', src);

    // Ждём нажатия клавиши
    cvWaitKey(0);

    // Оcвобождаем реcурcы
    cvReleaseImage(src);
    cvReleaseImage(dst);
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

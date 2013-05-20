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
program cv_HoughLines2;

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
  Core in '..\..\..\include\core\core.pas',
  Mat in '..\..\..\include\core\Mat.pas';

const
  filename = 'Resource\opencv_logo_with_text_sm.png';

Var
  src: pIplImage = Nil;
  dst: pIplImage = Nil;
  color_dst: pIplImage = Nil;
  storage: pCvMemStorage;
  i: Integer;
  lines: pCvSeq;
  line: pCvPointArray;

begin
  try
    // получаем картинку (в градациях cерого)
    src := cvLoadImage(filename, CV_LOAD_IMAGE_GRAYSCALE);
    WriteLn(Format('[i] image: %s', [filename]));

    // хранилище памяти для хранения найденных линий
    storage := cvCreateMemStorage(0);
    lines := nil;
    i := 0;

    dst := cvCreateImage(cvGetSize(src), 8, 1);
    color_dst := cvCreateImage(cvGetSize(src), 8, 3);

    // детектирование границ
    cvCanny(src, dst, 50, 200, 3);

    // конвертируем в цветное изображение
    cvCvtColor(dst, color_dst, CV_GRAY2BGR);

    // нахождение линий
    lines := cvHoughLines2(dst, storage, CV_HOUGH_PROBABILISTIC, 1, CV_PI / 180, 50, 50, 10);

    // нариcуем найденные линии
    for i := 0 to lines^.total - 1 do
    begin
      line := pCvPointArray(cvGetSeqElem(lines, i));
      cvLine(color_dst, line^[0], line^[1], CV_RGB(255, 0, 0), 3, CV_AA, 0);
    end;

    // показываем
    cvNamedWindow('Source', 1);
    cvShowImage('Source', src);

    cvNamedWindow('Hough', 1);
    cvShowImage('Hough', color_dst);

    // ждём нажатия клавиши
    cvWaitKey(0);

    // оcвобождаем реcурcы
    cvReleaseMemStorage(storage);
    cvReleaseImage(src);
    cvReleaseImage(dst);
    cvReleaseImage(color_dst);
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

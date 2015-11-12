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

program cv_MatchShapes2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.utils,
  uResourcePaths;

// сравнение объектов по моментам их контуров
procedure testMatch(original, templ: pIplImage);
Var
  src, dst: pIplImage;
  binI: pIplImage;
  binT: pIplImage;
  rgb: pIplImage;
  rgbT: pIplImage;

  storageI: pCvMemStorage;
  storageT: pCvMemStorage;
  contoursI: pCvSeq;
  contoursT: pCvSeq;
  contoursCont: Integer;
  font: TCvFont;
  counter: Integer;
  buf: string;
  seq0: pCvSeq;
  seqT: pCvSeq;
  perimT: double;
  perim: double;
  seqM: pCvSeq;
  matchM: double;
  match0: double;

  point: TCvPoint2D32f;
  rad: Float;

begin
  assert(original <> nil);
  assert(templ <> nil);

  WriteLn('[i] test cvMatchShapes()\n');

  src := nil;
  dst := nil;

  src := cvCloneImage(original);

  binI := cvCreateImage(cvGetSize(src), 8, 1);
  binT := cvCreateImage(cvGetSize(templ), 8, 1);

  // заведём цветные картинки
  rgb := cvCreateImage(cvGetSize(original), 8, 3);
  cvConvertImage(src, rgb, CV_GRAY2BGR);
  rgbT := cvCreateImage(cvGetSize(templ), 8, 3);
  cvConvertImage(templ, rgbT, CV_GRAY2BGR);

  // получаем границы изображения и шаблона
  cvCanny(src, binI, 50, 200);
  cvCanny(templ, binT, 50, 200);

  // показываем
  cvNamedWindow('cannyI', 1);
  cvShowImage('cannyI', binI);

  cvNamedWindow('cannyT', 1);
  cvShowImage('cannyT', binT);

  // для хранения контуров
  storageI := cvCreateMemStorage(0);
  storageT := cvCreateMemStorage(0);
  contoursI := nil;
  contoursT := nil;

  // находим контуры изображения
  contoursCont := cvFindContours(binI, storageI, @contoursI, sizeof(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

  // для отметки контуров
  cvInitFont(@font, CV_FONT_HERSHEY_PLAIN, 1.0, 1.0);
  counter := 0;

  // нарисуем контуры изображения
  if (contoursI <> nil) then
  begin
    seq0 := contoursI;
    While seq0 <> nil do
    begin
      // рисуем контур
      cvDrawContours(rgb, seq0, CV_RGB(255, 216, 0), CV_RGB(0, 0, 250), 0, 1, 8, cvPoint(0, 0));
      // выводим его номер
      cvMinEnclosingCircle(seq0, @point, @rad); // получим окружность содержащую контур
      Inc(counter);
      cvPutText(rgb, c_str(IntToStr(counter)), cvPointFrom32f(point), @font, CV_RGB(0, 255, 0));
      seq0 := seq0^.h_next;
    end;
  end;
  // показываем
  cvNamedWindow('cont', 1);
  cvShowImage('cont', rgb);

  cvConvertImage(src, rgb, CV_GRAY2BGR);

  // находим контуры шаблона
  cvFindContours(binT, storageT, @contoursT, sizeof(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

  seqT := nil;
  perimT := 0;

  if (contoursT <> nil) then
  begin
    // находим самый длинный контур
    seq0 := contoursT;
    while seq0 <> nil do
    begin
      perim := cvContourPerimeter(seq0);
      if (perim > perimT) then
      begin
        perimT := perim;
        seqT := seq0;
      end;
      // рисуем
      cvDrawContours(rgbT, seq0, CV_RGB(255, 216, 0), CV_RGB(0, 0, 250), 0, 1, 8, cvPoint(0, 0)); // рисуем контур
      seq0 := seq0^.h_next;
    end;
  end;
  // покажем контур шаблона
  cvDrawContours(rgbT, seqT, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), 0, 2, 8, cvPoint(0, 0)); // рисуем контур
  cvNamedWindow('contT', 1);
  cvShowImage('contT', rgbT);

  seqM := nil;
  matchM := 1000;
  // обходим контуры изображения
  counter := 0;
  if (contoursI <> nil) then
  begin
    // поиск лучшего совпадения контуров по их моментам
    seq0 := contoursI;
    while seq0 <> nil do
    begin
      match0 := cvMatchShapes(seq0, seqT, CV_CONTOURS_MATCH_I3);
      if (match0 < matchM) then
      begin
        matchM := match0;
        seqM := seq0;
      end;
      Inc(counter);
      WriteLn(Format('[i] %d match: %.2f', [counter, match0]));
      seq0 := seq0^.h_next;
    end;
  end;
  // рисуем найденный контур
  cvDrawContours(rgb, seqM, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), 0, 2, 8, cvPoint(0, 0)); // рисуем контур

  cvNamedWindow('find', 1);
  cvShowImage('find', rgb);

  // ждём нажатия клавиши
  cvWaitKey(0);

  // освобождаем ресурсы
  cvReleaseMemStorage(storageI);
  cvReleaseMemStorage(storageT);
  cvReleaseImage(src);
  cvReleaseImage(dst);
  cvReleaseImage(rgb);
  cvReleaseImage(rgbT);
  cvReleaseImage(binI);
  cvReleaseImage(binT);

  // удаляем окна
  cvDestroyAllWindows();
end;

const
  const_original = cResourceMedia + 'matchshapes2.jpg';
  const_template = cResourceMedia + 'matchshapes_template.jpg';

Var
  original: pIplImage = nil;
  templ: pIplImage = nil;
  filename, filename2: String;

begin
  try

    // имя картинки задаётся первым параметром

    filename := ifthen(ParamCount > 0, ParamStr(1), const_original);
    // получаем картинку
    original := cvLoadImage(c_str(filename), 0);

    WriteLn('[i] image: ', filename);
    assert(Assigned(original));

    // имя шаблона задаётся вторым параметром
    filename2 := ifthen(ParamCount > 1, ParamStr(2), const_template);
    // получаем картинку
    templ := cvLoadImage(c_str(filename2), 0);

    WriteLn('[i] template: ', filename2);
    assert(Assigned(templ));

    // покажем изображения
    cvNamedWindow('original', 1);
    cvShowImage('original', original);
    cvNamedWindow('template', 1);
    cvShowImage('template', templ);

    // сравнение
    testMatch(original, templ);

    // освобождаем ресурсы
    cvReleaseImage(original);
    cvReleaseImage(templ);
    // удаляем окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

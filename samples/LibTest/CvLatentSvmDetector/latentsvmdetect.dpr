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

program latentsvmdetect;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\core\core_c.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  Core in '..\..\..\include\core\core.pas',
  objdetect_c in '..\..\..\include\objdetect\objdetect_c.pas',
  cvUtils in '..\..\..\include\cvUtils.pas',
  Mat in '..\..\..\include\core\Mat.pas';

procedure help;
begin
  Writeln('This program demonstrated the use of the latentSVM detector.');
  Writeln('It reads in a trained object model and then uses that to detect the object in an image');
  Writeln('Call:');
  Writeln('latentsvmdetect [<image_filename> <model_filename> [<threads_number>]]');
  Writeln('  The defaults for image_filename and model_filename are cat.jpg and cat.xml respectively');
  Writeln('  Press any key to quit.');
end;

var
  tbbNumThreads: Integer = -1;
  model_filename: AnsiString = 'resource\cat.xml';
  image_filename: AnsiString = 'resource\cat.jpg';

procedure detect_and_draw_objects(image: pIplImage; detector: pCvLatentSvmDetector; numThreads: Integer = -1);
Var
  storage: pCvMemStorage;
  detections: pCvSeq;
  i: Integer;
  start, finish: int64;
  detection: TCvObjectDetection;
  score: Single;
  bounding_box: TCvRect;
begin
  storage := cvCreateMemStorage(0);
  detections := nil;
  i := 0;
  start := 0;
  finish := 0;
  start := cvGetTickCount();
  detections := cvLatentSvmDetectObjects(image, detector, storage, 0.5, numThreads);
  finish := cvGetTickCount();
  Writeln(Format('detection time = %.3f', [(finish - start) / (cvGetTickFrequency() * 1000000.0)]));
  for i := 0 to detections^.total - 1 do
  begin
    detection := pCvObjectDetection(cvGetSeqElem(detections, i))^;
    score := detection.score;
    bounding_box := detection.rect;
    cvRectangle(image, cvPoint(bounding_box.x, bounding_box.y), cvPoint(bounding_box.x + bounding_box.width,
      bounding_box.y + bounding_box.height), CV_RGB(cvRound(255.0 * score), 0, 0), 3);
  end;
  cvReleaseMemStorage(storage);
end;

Var
  detector: pCvLatentSvmDetector;
  image: pIplImage;

begin
  try
    // Внимание!
    // Есть ошибка. Неправильная работа с памятью.
    // Attention!
    // There is an error. Improper handling of memory.

    help();
    if ParamCount > 1 then
    begin
      image_filename := ParamStr(1);
      model_filename := ParamStr(2);
      if ParamCount > 2 then
        tbbNumThreads := StrToInt(ParamStr(3));
    end;
    image := cvLoadImage(image_filename.AsPAnsiChar);
    if not Boolean(image) then
    begin
      Writeln('Unable to load the image');
      Writeln('Pass it as the first parameter: latentsvmdetect <path to cat.jpg> <path to cat.xml>');
      Halt;
    end;
    detector := cvLoadLatentSvmDetector(model_filename.AsPAnsiChar);
    if not Boolean(detector) then
    begin
      Writeln('Unable to load the model');
      Writeln('Pass it as the second parameter: latentsvmdetect <path to cat.jpg> <path to cat.xml>');
      cvReleaseImage(image);
      Halt;
    end;
    detect_and_draw_objects(image, detector, tbbNumThreads);
    cvNamedWindow('test', 0);
    cvShowImage('test', image);
    cvWaitKey(0);
    cvReleaseLatentSvmDetector(detector);
    cvReleaseImage(image);
    cvDestroyAllWindows;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

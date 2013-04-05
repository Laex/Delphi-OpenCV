// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_MatchShapes;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
uLibName in '..\..\..\include\uLibName.pas',
highgui_c in '..\..\..\include\highgui\highgui_c.pas',
core_c in '..\..\..\include\сore\core_c.pas',
Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
legacy in '..\..\..\include\legacy\legacy.pas',
calib3d in '..\..\..\include\calib3d\calib3d.pas',
imgproc in '..\..\..\include\imgproc\imgproc.pas',
haar in '..\..\..\include\objdetect\haar.pas',
objdetect in '..\..\..\include\objdetect\objdetect.pas',
tracking in '..\..\..\include\video\tracking.pas',
Core in '..\..\..\include\сore\core.pas'
  ;

const
  const_original = 'resource\matchshapes2.jpg';
  const_template = 'resource\matchshapes_template.jpg';

var
  original: pIplImage = nil;
  template: pIplImage = nil;
  original_filename, template_filename: AnsiString;

  src: pIplImage = nil;
  dst: pIplImage = nil;
  binI: pIplImage = nil;
  binT: pIplImage = nil;
  rgb: pIplImage = nil;
  rgbT: pIplImage = nil;
  storage: pCvMemStorage = nil;
  contoursI: pCvSeq = nil;
  contoursT: pCvSeq = nil;
  seq0: pCvSeq = nil;
  seqT: pCvSeq = nil;
  seqM: pCvSeq = nil;
  contoursCont: integer;
  font: TCvFont;
  counter: integer;
  perim, match0: double;
  perimT: double = 0;
  matchM: double = 1000;

begin
  try
    // Первый параметр указывает исходную картинку, второй - шаблон поиска
    if ParamCount = 2 then
    begin
      original_filename := ParamStr(1);
      template_filename := ParamStr(2);
    end
    else
    begin
      original_filename := const_original;
      template_filename := const_template;
    end;

    // Получаем картинку
    original := cvLoadImage(pCVChar(original_filename), CV_LOAD_IMAGE_COLOR);
    WriteLn(Format('[i] original image: %s', [original_filename]));

    // Получаем картинку шаблона
    template := cvLoadImage(pCVChar(template_filename), CV_LOAD_IMAGE_COLOR);
    WriteLn(Format('[i] template image: %s', [template_filename]));

    // Покажем наши изображения
    cvNamedWindow('Original', CV_WINDOW_AUTOSIZE);
    cvShowImage('Original', original);
    cvNamedWindow('Template', CV_WINDOW_AUTOSIZE);
    cvShowImage('Template', template);

    // Cравнение
    WriteLn('[i] Run test cvMatchShapes()');

    // Клонируем картинку
    src := cvCloneImage(original);

    // Создаём одноканальные картинки
    binI := cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
    binT := cvCreateImage(cvGetSize(template), IPL_DEPTH_8U, 1);

    // Заведём цветные картинки
    rgb := cvCreateImage(cvGetSize(original), IPL_DEPTH_8U, 3);
    cvConvertImage(src, rgb, CV_GRAY2BGR);
    rgbT := cvCreateImage(cvGetSize(template), IPL_DEPTH_8U, 3);
    cvConvertImage(template, rgbT, CV_GRAY2BGR);

    // Получаем границы изображения и шаблона
    cvCanny(src, binI, 50, 200);
    cvCanny(template, binT, 50, 200);

    // Показываем
    // cvNamedWindow('CannyI', CV_WINDOW_AUTOSIZE);
    // cvShowImage('CannyI', binI);
    // cvNamedWindow('CannyT', CV_WINDOW_AUTOSIZE);
    // cvShowImage('CannyT', binT);

    // Создаем хранилище
    storage := nil;
    storage := cvCreateMemStorage(0);

    // Находим контуры
    contoursCont := cvFindContours(binI, storage, @contoursI, sizeof(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,
      cvPoint(0, 0));

    // Для отметки контуров
    cvInitFont(@font, CV_FONT_HERSHEY_PLAIN, 1.0, 1.0);

    // Нарисуем контуры изображения
    if contoursI <> nil then
    begin
      // Рисуем контур
      seq0 := contoursI;
      while (seq0 <> nil) do
      begin
        cvDrawContours(rgb, seq0, CV_RGB(255, 216, 0), CV_RGB(0, 0, 250), 0, 1, 8, cvPoint(0, 0));
        seq0 := seq0.h_next;
      end;
    end;
    // Показываем
    // cvNamedWindow('Cont', CV_WINDOW_AUTOSIZE);
    // cvShowImage('Cont', rgb );

    cvConvertImage(src, rgb, CV_GRAY2BGR);

    // Находим контуры шаблона
    cvFindContours(binT, storage, @contoursT, sizeof(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

    if contoursT <> nil then
    begin
      // Находим самый длинный контур
      seq0 := contoursT;
      while (seq0 <> nil) do
      begin
        perim := cvContourPerimeter(seq0);
        if perim > perimT then
        begin
          perimT := perim;
          seqT := seq0;
        end;
        // Рисуем
        cvDrawContours(rgbT, seq0, CV_RGB(255, 216, 0), CV_RGB(0, 0, 250), 0, 1, 8, cvPoint(0, 0));
        seq0 := seq0.h_next;
      end;
    end;
    // Покажем контур шаблона
    cvDrawContours(rgbT, seqT, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), 0, 2, 8, cvPoint(0, 0));
    // cvNamedWindow('ContT', CV_WINDOW_AUTOSIZE);
    // cvShowImage('ContT', rgbT);

    // Обходим контуры изображения
    counter := 0;
    if contoursI <> nil then
    begin
      // Поиск лучшего совпадения контуров по их моментам
      seq0 := contoursI;
      while (seq0 <> nil) do
      begin
        match0 := cvMatchShapes(seq0, seqT, CV_CONTOURS_MATCH_I3);
        if match0 < matchM then
        begin
          matchM := match0;
          seqM := seq0;
        end;
        Inc(counter);
        WriteLn(Format('[i] %d match: %.2f', [counter, match0]));
        seq0 := seq0.h_next;
      end;
    end;
    // Рисуем найденный контур
    cvDrawContours(rgb, seqM, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), 0, 2, 8, cvPoint(0, 0));
    cvNamedWindow('Find', CV_WINDOW_AUTOSIZE);
    cvShowImage('Find', rgb);

    // Ждём нажатия клавиши
    cvWaitKey(0);

    // Освобождаем ресурсы
    cvReleaseMemStorage(storage);
    cvReleaseImage(src);
    cvReleaseImage(dst);
    cvReleaseImage(rgb);
    cvReleaseImage(rgbT);
    cvReleaseImage(binI);
    cvReleaseImage(binT);

    // Удаляем окна
    cvDestroyAllWindows();

    // Освобождаем ресурсы
    cvReleaseImage(original);
    cvReleaseImage(template);
    // Удаляем все окна
    cvDestroyAllWindows();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

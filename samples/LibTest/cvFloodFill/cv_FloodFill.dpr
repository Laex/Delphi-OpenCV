// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_FloodFill;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

// заливка области картинки цветом
procedure fill(src: pIplImage; seed: TCvPoint; color: TCvScalar); // = CV_RGB(255, 0, 0)
Var
  comp: TCvConnectedComp;
begin
  cvFloodFill(src, seed, color, cvScalarAll(10), // минимальная разность
    cvScalarAll(10), // максимальная разность
    @comp, CV_FLOODFILL_FIXED_RANGE + 8, 0);
  // покажем площадь заливки
  WriteLn(Format('[filled area]%.2f', [comp.area]));
end;

// обработчик событий от мышки
procedure myMouseCallback(event: Integer; x: Integer; y: Integer; flags: Integer; param: Pointer); cdecl;
Var
  img: pIplImage;
begin
  img := pIplImage(param);
  case event of
    CV_EVENT_MOUSEMOVE:
      ;
    CV_EVENT_LBUTTONDOWN:
      begin
        WriteLn(Format('%dx%d', [x, y]));
        // вызываем нашу функцию-обёртку вокруг cvFloodFill()
        fill(img, CvPoint(x, y), CV_RGB(255, 0, 0));
      end;
    CV_EVENT_LBUTTONUP:
      ;
  end;
end;

Const
  filename = 'Resource\cat2.jpg';

Var
  src: pIplImage = nil;
  dst: pIplImage = nil;
  c: Integer;

begin
  try
    // получаем картинку
    src := cvLoadImage(filename);
    WriteLn(Format('[i] image: %s', [filename]));
    // покажем изображение
    cvNamedWindow('original', 1);
    // вешаем обработчик мышки
    cvSetMouseCallback('original', myMouseCallback, src);

    while true do
    begin
      // показываем картинку
      cvShowImage('original', src);
      c := cvWaitKey(33);
      if (c = 27) then // если нажата ESC - выходим
        break;
    end;
    // освобождаем ресурсы
    cvReleaseImage(src);
    cvReleaseImage(dst);
    // удаляем окна
    cvDestroyAllWindows;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

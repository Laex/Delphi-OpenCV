{$APPTYPE CONSOLE}
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program HelloWorld;

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

Var
  // задаЄм высоту и ширину картинки
  height: Integer = 620;
  width: Integer = 440;
  pt: TCvPoint;
  hw: pIplImage;
  font: TCvFont;

begin
  try
    // задаЄм точку дл€ вывода текста
    pt := CvPoint(height div 4, width div 2);
    // —оздаЄи 8-битную, 3-канальную картинку
    hw := cvCreateImage(CvSize(height, width), 8, 3);
    // заливаем картинку чЄрным цветом
    CvSet(pCvArr(hw), cvScalar(0, 0, 0));
    // инициализаци€ шрифта
    cvInitFont(@font, CV_FONT_HERSHEY_COMPLEX, 1.0, 1.0, 0, 1, CV_AA);
    // использу€ шрифт выводим на картинку текст
    cvPutText(hw, 'OpenCV Step By Step', pt, @font, CV_RGB(150, 0, 150));
    // создаЄм окошко
    cvNamedWindow('Hello World', 0);
    // показываем картинку в созданном окне
    cvShowImage('Hello World', hw);
    // ждЄм нажати€ клавиши
    cvWaitKey(0);
    // освобождаем ресурсы
    cvReleaseImage(hw);
    cvDestroyWindow('Hello World');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

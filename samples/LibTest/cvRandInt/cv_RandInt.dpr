// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_RandInt;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  // им€ картинки
  filename = 'Resource\opencv_logo_with_text.png';

Type
  TArrayOfByte = array [0 .. 0] of Byte;
  pArrayOfByte = ^TArrayOfByte;

Var
  // исходна€
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
    // окно дл€ отображени€ картинки
    cvNamedWindow('original', CV_WINDOW_AUTOSIZE);
    cvNamedWindow('noise', CV_WINDOW_AUTOSIZE);

    // инициализаци€ состо€ни€ √ѕ—„
    rng := TCvRNG($7FFFFFFF);

    // пробегаемс€ по всем пиксел€м изображени€
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

          // красные пиксели
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

    // ждЄм нажати€ клавиши
    cvWaitKey(0);

    // освобождаем ресурсы
    cvReleaseImage(image);
    cvReleaseImage(dst);
    // удал€ем окна
    cvDestroyWindow('original');
    cvDestroyWindow('noise');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

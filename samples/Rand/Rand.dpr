program Rand;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Core.types_c in '..\..\include\сore\Core.types_c.pas',
  core_c in '..\..\include\сore\core_c.pas',
  highgui_c in '..\..\include\highgui\highgui_c.pas';

Const
  // имя картинки
  filename = 'opencv_logo_with_text.png';

Type
  TArrayOfByte = array [0 .. 0] of Byte;
  pArrayOfByte = ^TArrayOfByte;

Var
  // исходная
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

    // инициализация состояния ГПСЧ
    rng := TCvRNG($7FFFFFFF);

    // пробегаемся по всем пикселям изображения
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

    // ждём нажатия клавиши
    cvWaitKey(0);

    // освобождаем ресурсы
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

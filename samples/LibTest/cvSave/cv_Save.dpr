{$APPTYPE CONSOLE}
{$POINTERMATH ON}
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_Save;

{$R *.res}

uses
  System.SysUtils,
{$I ..\..\uses_include.inc}
  ;

Const
  kernet_filename = 'result\kernel.xml';

Var
  kernel: array [0 .. 8] of Single;
  kernel_matrix: TCvMat;
  i: Integer;
  j: Integer;
  matrix: pCvMat;
  ptr: pSingle;
  fs: pCvFileStorage;
  frame_count: Integer;
  s: pCvSeq;
  frame_width: Integer;
  frame_height: Integer;
  color_cvt_matrix: pCvMat;

begin
  try
    // массив, содержащий данные матрицы
    kernel[0] := 1;
    kernel[1] := 0;
    kernel[2] := 0;
    kernel[3] := 0;
    kernel[4] := 2;
    kernel[5] := 0;
    kernel[6] := 0;
    kernel[7] := 0;
    kernel[8] := 3;
    // создаём матрицу
    kernel_matrix := cvMat(3, 3, CV_32FC1, @kernel);
    // сохраняем матрицу в XML-файл
    cvSave(kernet_filename, @kernel_matrix);
    // а теперь загрузим данные из XML-файла
    matrix := pCvMat(cvLoad(kernet_filename));
    // покажем содержимое матрицы
    // 1 вариант: с использованием макроса CV_MAT_ELEM
    for i := 0 to matrix^.rows - 1 do
    begin
      for j := 0 to matrix^.cols - 1 do
        Write(Format('%.0f ', [pSingle(CV_MAT_ELEM(matrix^, CV_32FC1, i, j))^]));
      Writeln;
    end;
    Writeln;

    // 2 вариант: с использованием cvGet2D()
    // cvGetReal2D() для CV_64FC1
    for i := 0 to matrix^.rows - 1 do
    begin
      for j := 0 to matrix^.cols - 1 do
        Write(Format('%.0f ', [cvGet2D(matrix, i, j).val[0]]));
      Writeln;
    end;
    Writeln('-----');

    // 3 вариант: прямой доступ к элементам
    for i := 0 to matrix^.rows - 1 do
    begin
      ptr := pSingle((Integer(matrix^.data) + i * matrix^.step));
      for j := 0 to matrix^.cols - 1 do
        Write(Format('%.0f ', [ptr[j]]));
      Writeln;
    end;
    Writeln('-----');

    // освобождаем ресурсы
    cvReleaseMat(matrix);

    // Чтение XML
    Writeln('Example 3_19 Reading in cfg.xml');

    // открываем файл для чтения
    fs := cvOpenFileStorage('resource\cfg.xml', 0, CV_STORAGE_READ);
    // считываем значения
    frame_count := cvReadIntByName(fs, 0, 'frame_count', 5 { значение по-умолчанию } );
    s := cvGetFileNodeByName(fs, 0, 'frame_size')^.seq;
    frame_width := cvReadInt(pCvFileNode(cvGetSeqElem(s, 0)));
    frame_height := cvReadInt(pCvFileNode(cvGetSeqElem(s, 1)));
    color_cvt_matrix := pCvMat(cvRead(fs, 0));

    // показываем
    WriteLn(Format('frame_count=%d, frame_width=%d, frame_height=%d',[frame_count,frame_width,frame_height]));

    cvReleaseFileStorage(fs);
    Writeln('Press <Enter>');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

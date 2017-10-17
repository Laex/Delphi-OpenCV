(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://ocv.org
  Online docs: http://docs.ocv.org
  Q&A forum:   http://answers.ocv.org
  Dev zone:    http://code.ocv.org
  **************************************************************************************************
  Original: https://sites.google.com/site/komputernoezrenie/end
  **************************************************************************************************
*)
{$POINTERMATH ON}
unit uMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    btn1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btn1Click(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.legacy,
  ocv.utils;

procedure TMainForm.btn1Click(Sender: TObject);
const
  N_Samples = 7; // Количество изображений

Type
  TArrayOfpIplImage = pIplImage;
  pArrayOfpIplImage = ^TArrayOfpIplImage;
  TArrayOfFloat = array [0 .. 0] of Float;
  pArrayOfFloat = ^TArrayOfFloat;
  ppArrayOfFloat = ^pArrayOfFloat;

Var
  img_load: pIplImage;
  img_load_ch1: array [0 .. N_Samples - 1] of pIplImage;

  test_img: pIplImage;

  mean_img: pIplImage; // Усредненное лицо (плавающая точка)
  result_img: pIplImage; // Усредненное лицо
  size: TCvSize;
  i, j: Integer;
  facesfilename: AnsiString;
  Tc: TCvTermCriteria;
  nEigens: Integer;

  eig_img: Array Of pIplImage;
  EigenVals: pCvMat;
  min_val, max_val: double;
  coeffs: Array Of array of Float;
  projectedTestFace: Array Of Float;
  leastDistSq: double;
  iTrain, iNearest: Integer;
  distSq: double;
  d_i: Float;
  k: Integer;

begin
  mean_img := nil; // Усредненное лицо (плавающая точка)
  result_img := nil; // Усредненное лицо
  // ****************************************
  // Загрузка лиц (обучающая выборка)
  // ****************************************
  for i := 0 to N_Samples - 1 do
  begin
    facesfilename := '../../resource/faces/s' + IntToStr(i + 1) + '/1.pgm';
    if not FileExists(facesfilename) then
    begin
      ShowMessage('File ' + facesfilename + ' not found');
      Halt;
    end;
    img_load := cvLoadImage(c_str(facesfilename));
    size := cvSize(img_load.width, img_load.height);
    img_load_ch1[i] := cvCreateImage(size, IPL_DEPTH_8U, 1);
    cvSplit(img_load, img_load_ch1[i], nil, nil, nil);
    // Вывод загруженных лиц на форму
    ipDraw(10 + (size.width + 10) * i, 50, img_load, Handle);
  end;

  // Здесь размещаются по экрану Лейблы :)
  Label1.Left := (10 + (size.width + 10) * N_Samples - Label1.width) div 2;
  Label1.Visible := true;

  Label2.Top := (50 + (size.height + 10) - 5);
  Label2.Left := (10 + (size.width + 10) + (size.width + 10) * (N_Samples - 1) - Label2.width) div 2;

  Label2.Visible := true;

  Label3.Top := (50 + (size.height + 20) * 3 - 15);
  Label3.Left := (10 + (size.width + 10) + (size.width + 10) * (N_Samples - 1) - Label3.width) div 2;

  Label3.Visible := true;

  Label4.Top := (50 + (size.height + 20) * 2 - 15);
  Label4.Left := (10 + (size.width + 10) + (size.width + 10) * (N_Samples - 1) - Label4.width) div 2;

  Label4.Visible := true;

  //

  // ****************************************
  // Загрузка проверяемого лица
  // ****************************************
  i := Random(7) + 1; // Номер индивида
  j := Random(9) + 2; //Номер лица
  // Загрузим его второе изображение (для тренировки использовалось первое)
  facesfilename := '../../resource/faces/s' + IntToStr(i) + '/'+j.ToString+'.pgm';
  img_load := cvLoadImage(c_str(facesfilename));
  test_img := cvCreateImage(size, IPL_DEPTH_8U, 1);
  cvSplit(img_load, test_img, nil, nil, nil);
  ipDraw(10 + ((size.width + 10) * (N_Samples - 1)) div 2, 50 + (size.height + 20) * 2, img_load, Handle);
  // ****************************************
  // Выделение памяти под изображения
  // ****************************************
  // Усредненное лицо
  if not Assigned(mean_img) then
  begin
    mean_img := cvCreateImage(size, IPL_DEPTH_32F, 1);
  end;
  // Серое изображение для вывода результата
  if not Assigned(result_img) then
  begin
    result_img := cvCreateImage(size, IPL_DEPTH_8U, 1);
  end;
  // -----------------------------------
  // Задаем критерии окончания процесса
  // -----------------------------------
  Tc.cType := CV_TERMCRIT_NUMBER or CV_TERMCRIT_EPS;

  // Тут можно поварьировать (задал от балды)
  Tc.max_iter := 100;
  Tc.epsilon := 0.001;
  // -----------------------------------------------------
  // Количество векторов базиса = кол-во изображений - 1
  // -----------------------------------------------------
  nEigens := N_Samples - 1;

  // Это базис
  SetLength(eig_img, nEigens);
  EigenVals := cvCreateMat(1, nEigens, IPL_DEPTH_32F);
  // Выделяем память под базис
  for i := 0 to N_Samples - 1 do
  begin
    eig_img[i] := cvCreateImage(size, IPL_DEPTH_32F, 1);
  end;
  // Вычисляем базис
  cvCalcEigenObjects(N_Samples, @img_load_ch1[0], eig_img, CV_EIGOBJ_NO_CALLBACK, 0, 0, @Tc, mean_img, pFloat(EigenVals.data));
  // ****************************************
  // Вывод промежуточного результата (Базис лиц)
  // ****************************************
  for j := 0 to nEigens - 1 do
  begin
    cvMinMaxLoc(eig_img[j], @min_val, @max_val);
    if (min_val <> max_val) then
    begin
      cvConvertScale(eig_img[j], result_img, 255.0 / (max_val - min_val), -min_val);
      ipDraw(10 + (size.width + 10) * j + (size.width + 10) div 2, 50 + (size.height + 20), result_img, Handle);
    end;
  end;
  // -----------------------------------------------------------
  // Разложение тестируемого изображения по полученному базису
  // -----------------------------------------------------------
  SetLength(coeffs, N_Samples);
  for i := 0 to N_Samples - 1 do
  begin
    SetLength(coeffs[i], nEigens);
    cvEigenDecomposite(img_load_ch1[i], nEigens, @eig_img[0], CV_EIGOBJ_NO_CALLBACK, 0, mean_img, @coeffs[i][0]);
  end;
  // -----------------------------------------------------------
  // Задаем тестовое изображение
  // -----------------------------------------------------------
  SetLength(projectedTestFace, nEigens);
  cvEigenDecomposite(test_img, nEigens, eig_img, CV_EIGOBJ_NO_CALLBACK, 0, mean_img, @projectedTestFace[0]);
  // -----------------------------------------------------------
  leastDistSq := DBL_MAX;
  iTrain := 0;
  iNearest := 0;

  for iTrain := 0 to N_Samples - 1 do
  begin
    distSq := 0;
    for i := 0 to nEigens - 1 do
    begin
      d_i := projectedTestFace[i] - coeffs[iTrain][i];
      distSq := distSq + d_i * d_i / pFloat(EigenVals.data)[i];
    end;
    if distSq < leastDistSq then
    begin
      leastDistSq := distSq;
      iNearest := iTrain;
    end;

  end;
  // ****************************************
  // Результат ткстирования (Вывод распознанного индивида)
  // ****************************************
  // APIDrawIpl(10+(10+size.width),50+(size.height+20)*2,img_load_ch1[iNearest],Form1.Handle);

  for i := 0 to N_Samples - 1 do
  begin
    // Вывод загруженных лиц на форму
    if (i <> iNearest) then
    begin
      cvLine(img_load_ch1[i], cvPoint(0, 0), cvPoint(size.width, size.height), CV_RGB(255, 0, 0), 3, 8);
      cvLine(img_load_ch1[i], cvPoint(size.width, 0), cvPoint(0, size.height), CV_RGB(255, 0, 0), 3, 8);
    end;
    ipDraw(10 + (size.width + 10) * i, 50 + (size.height + 20) * 3, img_load_ch1[i], Handle);
  end;

  // ****************************************
  // Освобождаем память
  // ****************************************
  for k := 0 to N_Samples - 1 do
  begin
    cvReleaseImage(eig_img[k]);
    cvReleaseImage(img_load_ch1[k]);
  end;

  // Аккуратно освобождаем массив
  // for i := 0 to N_Samples - 1 do
  // begin
  // FreeMem(coeffs[i]);
  // end;
  // delete coeffs;

  cvReleaseImage(img_load);
  cvReleaseImage(mean_img);
  cvReleaseImage(test_img);
  cvReleaseImage(result_img);
end;

end.

// *****************************************************************
// Delphi-OpenCV Class Demo
// Copyright (C) 2013 Project Delphi-OpenCV
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
// *******************************************************************
// Original:
// http://docs.opencv.org/modules/ml/doc/k_nearest_neighbors.html#cvknearest-is-regression
// *******************************************************************

program cls2DPointClassification;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Core.types_c,
  core_c,
  highgui_c,
  ml;

Const
  K10: Integer = 10;

Var
  i, j, K, accuracy: Integer;
  response: float;
  train_sample_count: Integer = 100;
  rng_state: TCvRNG;
  trainData: pCvMat;
  trainClasses: pCvMat;
  img: pIplImage;
  _sample: array [0 .. 1] of float;
  sample: TCvMat;
  trainData1, //
    trainData2, //
    trainClasses1, //
    trainClasses2: TCvMat;
  knn: TCvKNearest;
  nearests: pCvMat;
  t: TCvScalar;
  pt: TCvPoint;

begin
  try
    rng_state := CvRNG(-1);
    trainData := cvCreateMat(train_sample_count, 2, CV_32FC1);
    trainClasses := cvCreateMat(train_sample_count, 1, CV_32FC1);
    img := cvCreateImage(cvSize(500, 500), 8, 3);
    sample := CvMat(1, 2, CV_32FC1, @_sample);
    cvZero(img);
    // form the training samples
    cvGetRows(trainData, @trainData1, 0, train_sample_count div 2);
    cvRandArr(@rng_state, @trainData1, CV_RAND_NORMAL, cvScalar(200, 200), cvScalar(50, 50));

    cvGetRows(trainData, @trainData2, train_sample_count div 2, train_sample_count);
    cvRandArr(@rng_state, @trainData2, CV_RAND_NORMAL, cvScalar(300, 300), cvScalar(50, 50));

    cvGetRows(trainClasses, @trainClasses1, 0, train_sample_count div 2);
    cvSet(@trainClasses1, cvScalar(1));

    cvGetRows(trainClasses, @trainClasses2, train_sample_count div 2, train_sample_count);
    cvSet(@trainClasses2, cvScalar(2));

    // learn classifier
    knn := CreateCvKNearest(trainData, trainClasses, nil, false, K10);
    nearests := cvCreateMat(1, K10, CV_32FC1);

    for i := 0 to img^.height - 1 do
    begin
      for j := 0 to img^.width - 1 do
      begin
        pFloat(sample.data)[0] := j;
        pFloat(sample.data)[1] := i;
        // estimate the response and get the neighbors' labels
        response := knn.find_nearest(@sample, K10, nil, nil, nearests, nil);

        // compute the number of neighbors representing the majority
        accuracy := 0;
        for K := 0 to K10 - 1 do
        begin
          if (pFloat(nearests^.data)[K] = response) then
            Inc(accuracy);
        end;
        // highlight the pixel depending on the accuracy (or confidence)
        if response = 1 then
        begin
          if accuracy > 5 then
            t := CV_RGB(180, 0, 0)
          else
            CV_RGB(180, 120, 0);
        end
        else
        begin
          if accuracy > 5 then
            t := CV_RGB(0, 180, 0)
          else
            CV_RGB(120, 120, 0);
        end;
        cvSet2D(img, i, j, t);
      end;
    end;

    ReleaseCvKNearest(knn);

    // display the original training samples
    for i := 0 to (train_sample_count div 2) - 1 do
    begin
      pt.x := cvRound(pFloat(trainData1.data)[i * 2]);
      pt.y := cvRound(pFloat(trainData1.data)[i * 2 + 1]);
      cvCircle(img, pt, 2, CV_RGB(255, 0, 0), CV_FILLED);
      pt.x := cvRound(pFloat(trainData2.data)[i * 2]);
      pt.y := cvRound(pFloat(trainData2.data)[i * 2 + 1]);
      cvCircle(img, pt, 2, CV_RGB(0, 255, 0), CV_FILLED);
    end;

    cvNamedWindow('classifier result', 1);
    cvShowImage('classifier result', img);
    cvWaitKey(0);

    cvReleaseMat(trainClasses);
    cvReleaseMat(trainData);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

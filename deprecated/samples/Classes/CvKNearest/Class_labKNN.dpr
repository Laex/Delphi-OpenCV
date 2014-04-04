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
// http://public.cranfield.ac.uk/c5354/teaching/ml/examples/c/knn_ex/
// *******************************************************************

program Class_labKNN;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Core.types_c,
  core_c,
  ml;

{DEFINE Test}

Const
  CarTrain_FileName = 'resource\car.train';
{$IFDEF Test}
  CarTest_FileName          = 'resource\car.test';
  NUMBER_OF_TESTING_SAMPLES = 345;
{$ELSE}
  CarTest_FileName          = 'resource\car.data';
  NUMBER_OF_TESTING_SAMPLES = 1728;
{$ENDIF}
  ATTRIBUTES_PER_SAMPLE                                 = 6; // not the last as this is the class
  NUMBER_OF_CLASSES                                     = 4; // classes 0->3
  Classes: array [0 .. NUMBER_OF_CLASSES - 1] of String = ('unacc', 'acc', 'good', 'vgood');
  NUMBER_OF_TRAINING_SAMPLES                            = 1383;

function hash(str: AnsiString): Single;
Var
  i, R: Integer;
begin
  R      := 5381;
  for i  := 1 to Length(str) do
    R    := (R shl 5) + R + ord(str[i]);
  Result := R;
end;

function read_data_from_csv(const filename: String; Var data, _classes: TCvMat): Boolean;
var
  i, line, attribute: Integer;
  S                 : TStringList;
  Sp                : TStringList;
begin
  if not FileExists(filename) then
  begin
    WriteLn(
      'ERROR: cannot read file ',
      filename);
    Exit(False); // all not OK
  end;
  S  := TStringList.Create;
  Sp := TStringList.Create;
  try
    S.LoadFromFile(filename);
    // for each sample in the file
    for line := 0 to S.Count - 1 do
    begin
      Sp.CommaText := S[line];
      // for each attribute on the line in the file
      for attribute := 0 to Sp.Count - 1 do
        if attribute = 6 then
        // last attribute is the class
        begin
          // find the class number and record this
          for i := 0 to NUMBER_OF_CLASSES - 1 do
            if SameText(Classes[i], Sp[attribute]) then
              PSingle(CV_MAT_ELEM(_classes, CV_32FC1, line, 0))^ := i;
        end
        else
        // for all other attributes just read in the string value
        // and use a hash function to convert to to a float
        // (N.B. openCV uses a floating point decision tree implementation!)
        begin
          PSingle(CV_MAT_ELEM(data, CV_32FC1, line, attribute))^ := hash(AnsiString(Sp[attribute]));
        end;
    end;
  finally
    S.Free;
    Sp.Free;
  end;
  Result := True;
end;

Var
  k, i: Integer;
  training_data, //
    training_classifications: pCvMat;
  testing_data, //
    testing_classifications: pCvMat;

  var_type   : pCvMat;
  resultNode : Float; // node returned from a prediction
  knn        : ICvKNearest;
  tsample    : Integer;
  test_sample: TCvMat;

  correct_class  : Integer;
  wrong_class    : Integer;
  false_positives: array [0 .. NUMBER_OF_CLASSES - 1] of Integer;

begin
  try
    // lets just check the version first
    // WriteLn(Format('OpenCV version %s (%d.%d.%d)', [CV_VERSION, CV_MAJOR_VERSION, CV_MINOR_VERSION,CV_SUBMINOR_VERSION]);
    k := 10;
    // define training data storage matrices (one for attribute examples, one
    // for classifications)
    training_data := cvCreateMat(
      NUMBER_OF_TRAINING_SAMPLES,
      ATTRIBUTES_PER_SAMPLE,
      CV_32FC1);
    training_classifications := cvCreateMat(
      NUMBER_OF_TRAINING_SAMPLES,
      1,
      CV_32FC1);
    // define testing data storage matrices
    testing_data := cvCreateMat(
      NUMBER_OF_TESTING_SAMPLES,
      ATTRIBUTES_PER_SAMPLE,
      CV_32FC1);
    testing_classifications := cvCreateMat(
      NUMBER_OF_TESTING_SAMPLES,
      1,
      CV_32FC1);

    // define all the attributes as categorical (i.e. categories)
    // alternatives are CV_VAR_CATEGORICAL or CV_VAR_ORDERED(=CV_VAR_NUMERICAL)
    // that can be assigned on a per attribute basis
    // this is a classification problem (i.e. predict a discrete number of class
    // outputs) so also the last (+1) output var_type element to CV_VAR_CATEGORICAL

    var_type := cvCreateMat(
      ATTRIBUTES_PER_SAMPLE + 1,
      1,
      CV_8U);
    cvSet(
      var_type,
      cvScalarAll(CV_VAR_CATEGORICAL)); // all inputs are categorical
    try
      // load training and testing data sets
      if read_data_from_csv(CarTrain_FileName, training_data^, training_classifications^) and
        read_data_from_csv(CarTest_FileName, testing_data^, testing_classifications^) then
      begin

        // train K-Nearest Neighbour classifier (using training data)

        WriteLn(
          'Using training database: ',
          CarTrain_FileName);
        knn := CreateCvKNearest;

        knn.train(
          training_data,
          training_classifications,
          nil,
          False,
          k);

        // perform classifier testing and report results

        correct_class := 0;
        wrong_class   := 0;
        FillChar(
          false_positives,
          SizeOf(false_positives),
          0);
        WriteLn(
          'Using testing database: ',
          CarTest_FileName);

        for tsample := 0 to NUMBER_OF_TESTING_SAMPLES - 1 do
        begin
          // extract a row from the testing matrix
          cvGetRow(
            testing_data,
            @test_sample,
            tsample);
          // run decision tree prediction

          resultNode := knn.find_nearest(
            @test_sample,
            k,
            nil,
            nil,
            nil);

          WriteLn(Format('Testing Sample %d -> class result %s', [tsample, Classes[Trunc(resultNode)]]));

          // if the prediction and the (true) testing classification are the same
          // (N.B. openCV uses a floating point decision tree implementation!)

          if abs(resultNode - PSingle(CV_MAT_ELEM(testing_classifications^, CV_32FC1, tsample, 0))^) >= FLT_EPSILON then
          begin
            // if they differ more than floating point error => wrong class
            Inc(wrong_class);
            Inc(false_positives[Trunc(resultNode)]);
          end
          else
          begin
            // otherwise correct
            Inc(correct_class);
          end;
        end;

        WriteLn(Format('Results on the testing database: %s'#13#10#9'Correct classification: %d (%4.2f%%)'#13#10#9 +
          'Wrong classifications: %d (%4.2f%%)', [CarTest_FileName, correct_class,
          correct_class * 100 / NUMBER_OF_TESTING_SAMPLES, wrong_class,
          wrong_class * 100 / NUMBER_OF_TESTING_SAMPLES]));

        for i := 0 to NUMBER_OF_CLASSES - 1 do
        begin
          WriteLn(Format(#9'Class %s false postives 	%d (%4.2f%%)', [Classes[i], false_positives[i],
            false_positives[i] * 100 / NUMBER_OF_TESTING_SAMPLES]));
        end;
      end;
    finally
      // free all memory
      cvReleaseMat(training_data);
      cvReleaseMat(training_classifications);
      cvReleaseMat(testing_data);
      cvReleaseMat(testing_classifications);
      cvReleaseMat(var_type);

      WriteLn('Press Enter to Exit');
      Readln;
    end;
  except
    on E: Exception do
      WriteLn(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.

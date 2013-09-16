// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
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
// Original: http://pebibyte.wordpress.com/2011/01/21/simple-face-recognition-using-opencv/
// *******************************************************************

program FaceRecog;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.Character,
  System.SysUtils,
  System.Classes,
  highgui_c,
  core_c,
  Core.types_c,
  imgproc_c,
  imgproc.types_c,
  legacy,
  cvUtils,
  objdetect;

Var
  /// / Global variables
  nTrainFaces          : Integer = 0;        // number of training images
  nEigens              : Integer = 0;        // number of eigenvalues
  faceImgArr           : array of pIplImage; // array of face images
  personNumTruthMat    : pCvMat    = nil;    // array of person numbers
  pAvgTrainImg         : pIplImage = nil;    // the average image
  eigenVectArr         : array of pIplImage; // eigenvectors
  eigenValMat          : pCvMat = nil;       // eigenvalues
  projectedTrainFaceMat: pCvMat = nil;       // projected training faces

function loadFaceImgArray(const Filename: String): Integer;
Var
  S: TStringList;
  i: Integer;
begin
  Result := 0;
  S      := TStringList.Create;
  try
    if FileExists(Filename) then
    begin
      S.LoadFromFile(Filename);
      Result := S.Count;
      SetLength(
        faceImgArr,
        Result);
      personNumTruthMat := cvCreateMat(
        1,
        Result,
        CV_32SC1);

      for i := 0 to Result - 1 do
      begin
        PInteger(personNumTruthMat^.data)[i] := i;
        faceImgArr[i]                        := cvLoadImage(
          c_str(S[i]),
          CV_LOAD_IMAGE_GRAYSCALE);
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure doPCA();
Var
  i          : Integer;
  calcLimit  : TCvTermCriteria;
  faceImgSize: TCvSize;
begin
  // set the number of eigenvalues to use
  nEigens := nTrainFaces - 1;
  // allocate the eigenvector images
  faceImgSize.width  := faceImgArr[0]^.width;
  faceImgSize.height := faceImgArr[0]^.height;
  SetLength(
    eigenVectArr,
    nEigens);
  for i             := 0 to nEigens - 1 do
    eigenVectArr[i] := cvCreateImage(
      faceImgSize,
      IPL_DEPTH_32F,
      1);

  // allocate the eigenvalue array
  eigenValMat := cvCreateMat(
    1,
    nEigens,
    CV_32FC1);

  // allocate the averaged image
  pAvgTrainImg := cvCreateImage(
    faceImgSize,
    IPL_DEPTH_32F,
    1);

  // set the PCA termination criterion
  calcLimit := cvTermCriteria(
    CV_TERMCRIT_ITER,
    nEigens,
    1);

  // compute average image, eigenvalues, and eigenvectors
  cvCalcEigenObjects(
    nTrainFaces,
    faceImgArr,
    eigenVectArr,
    CV_EIGOBJ_NO_CALLBACK,
    0,
    0,
    @calcLimit,
    pAvgTrainImg,
    pFloat(eigenValMat^.data));
end;

procedure storeTrainingData();
Var
  fileStorage: pCvFileStorage;
  i          : Integer;
  varname    : AnsiString;
begin
  // create a file-storage interface
  fileStorage := cvOpenFileStorage(
    'result\facedata.xml',
    0,
    CV_STORAGE_WRITE);

  // store all the data
  cvWriteInt(
    fileStorage,
    'nEigens',
    nEigens);
  cvWriteInt(
    fileStorage,
    'nTrainFaces',
    nTrainFaces);
  cvWrite(
    fileStorage,
    'trainPersonNumMat',
    personNumTruthMat,
    cvAttrList(0, 0));
  cvWrite(
    fileStorage,
    'eigenValMat',
    eigenValMat,
    cvAttrList(0, 0));
  cvWrite(
    fileStorage,
    'projectedTrainFaceMat',
    projectedTrainFaceMat,
    cvAttrList(0, 0));
  cvWrite(
    fileStorage,
    'avgTrainImg',
    pAvgTrainImg,
    cvAttrList(0, 0));
  for i := 0 to nEigens - 1 do
  begin
    varname := 'eigenVect_' + IntToStr(i);
    cvWrite(
      fileStorage,
      c_str(varname),
      eigenVectArr[i],
      cvAttrList(0, 0));
  end;
  // release the file-storage interface
  cvReleaseFileStorage(fileStorage);
end;

procedure learn;
var
  i: Integer;
begin
  // load training data
  nTrainFaces := loadFaceImgArray(ParamStr(2));
  if (nTrainFaces < 2) then
  begin
    WriteLn('Need 2 or more training faces');
    WriteLn(
      'Input file contains only ',
      nTrainFaces);
    Exit;
  end;

  // do PCA on the training faces
  doPCA;

  // project the training images onto the PCA subspace
  projectedTrainFaceMat := cvCreateMat(
    nTrainFaces,
    nEigens,
    CV_32FC1);
  for i := 0 to nTrainFaces - 1 do
  begin
    cvEigenDecomposite(
      faceImgArr[i],
      nEigens,
      eigenVectArr,
      0,
      0,
      pAvgTrainImg,
      pFloat(projectedTrainFaceMat^.data) + i * nEigens);
  end;
  // store the recognition data as an xml file
  storeTrainingData();
end;

function loadTrainingData(Var pTrainPersonNumMat: pCvMat): Boolean;
Var
  fileStorage: pCvFileStorage;
  i          : Integer;
  varname    : String;
begin
  // create a file-storage interface
  fileStorage := cvOpenFileStorage(
    'result\facedata.xml',
    0,
    CV_STORAGE_READ);
  if not Assigned(fileStorage) then
  begin
    WriteLn('Can''t open facedata.xml');
    Exit(False);
  end;

  nEigens := cvReadIntByName(
    fileStorage,
    0,
    'nEigens',
    0);
  nTrainFaces := cvReadIntByName(
    fileStorage,
    0,
    'nTrainFaces',
    0);
  pTrainPersonNumMat    := pCvMat(cvReadByName(fileStorage, 0, 'trainPersonNumMat', 0));
  eigenValMat           := pCvMat(cvReadByName(fileStorage, 0, 'eigenValMat', 0));
  projectedTrainFaceMat := pCvMat(cvReadByName(fileStorage, 0, 'projectedTrainFaceMat', 0));
  pAvgTrainImg          := pIplImage(cvReadByName(fileStorage, 0, 'avgTrainImg', 0));
  SetLength(
    eigenVectArr,
    nEigens);
  for i := 0 to nEigens - 1 do
  begin
    varname         := 'eigenVect_' + IntToStr(i);
    eigenVectArr[i] := pIplImage(cvReadByName(fileStorage, 0, c_str(varname), 0));
  end;

  // release the file-storage interface
  cvReleaseFileStorage(fileStorage);

  Result := True;
end;

function findNearestNeighbor(projectedTestFace: array of Float): Integer;
Var
  leastDistSq        : double;
  i, iTrain, iNearest: Integer;
  distSq             : double;
  d_i                : Float;
begin
  leastDistSq := DBL_MAX;
  iNearest    := 0;
  for iTrain  := 0 to nTrainFaces - 1 do
  begin
    distSq := 0;
    for i  := 0 to nEigens - 1 do
    begin
      d_i    := projectedTestFace[i] - pFloat(projectedTrainFaceMat^.data)[iTrain * nEigens + i];
      distSq := distSq + d_i * d_i / pFloat(eigenValMat^.data)[i];
    end;
    if (distSq < leastDistSq) then
    begin
      leastDistSq := distSq;
      iNearest    := iTrain;
    end;
  end;

  WriteLn(leastDistSq:8:2);
  Result := iNearest;
end;

procedure recognize();
Var
  i, nTestFaces           : Integer; // the number of test images
  trainPersonNumMat       : pCvMat;  // the person numbers during training
  projectedTestFace       : array of Float;
  iNearest, nearest, truth: Integer;
begin
  nTestFaces        := 0;   // the number of test images
  trainPersonNumMat := nil; // the person numbers during training
  projectedTestFace := nil;

  // load test images and ground truth for person number
  nTestFaces := loadFaceImgArray(ParamStr(2));
  WriteLn(
    nTestFaces,
    ' test faces loaded');

  // load the saved training data
  if not loadTrainingData(trainPersonNumMat) then
    Exit;

  // project the test images onto the PCA subspace
  SetLength(
    projectedTestFace,
    nEigens);
  for i := 0 to nTestFaces - 1 do
  begin
    // project the test image onto the PCA subspace
    cvEigenDecomposite(
      faceImgArr[i],
      nEigens,
      @eigenVectArr[0],
      CV_EIGOBJ_NO_CALLBACK,
      nil,
      pAvgTrainImg,
      @projectedTestFace[0]);

    iNearest := findNearestNeighbor(projectedTestFace);
    truth    := PInteger(personNumTruthMat^.data)[i];
    nearest  := PInteger(trainPersonNumMat^.data)[iNearest];

    WriteLn(Format('nearest = %d, Truth = %d', [nearest, truth]));
  end;
end;

procedure printUsage();
begin
  WriteLn('Usage: eigenface <command>');
  WriteLn('  Valid commands are');
  WriteLn('    train train.txt');
  WriteLn('    test test.txt');
end;

begin
  try
    // validate that an input was specified
    if ParamCount <> 2 then
      printUsage()
    else if SameText(ParamStr(1), 'train') then
      learn
    else if SameText(ParamStr(1), 'test') then
      recognize
    else
      WriteLn(
        'Unknown command: ',
        ParamStr(1));
  except
    on E: Exception do
      WriteLn(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.

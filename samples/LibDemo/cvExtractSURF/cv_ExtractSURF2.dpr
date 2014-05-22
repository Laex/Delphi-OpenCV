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
// ***************************************************************

program cv_ExtractSURF;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.compat,
  ocv.nonfree,
  uResourcePaths;

function compareSURFDescriptors(const d1: pSingle; const d2: pSingle; best: Double; length: Integer): Double;
var
  total_cost: Double;
  i: Integer;
  t0, t1, t2, t3: Double;
begin
  total_cost := 0;
  assert(length mod 4 = 0);
  i := 0;
  while i < length do
  begin
    t0 := d1[i] - d2[i];
    t1 := d1[i + 1] - d2[i + 1];
    t2 := d1[i + 2] - d2[i + 2];
    t3 := d1[i + 3] - d2[i + 3];
    total_cost := total_cost + t0 * t0 + t1 * t1 + t2 * t2 + t3 * t3;
    if (total_cost > best) then
      break;
    i := i + 4;
  end;
  Result := total_cost;
end;

function naiveNearestNeighbor(const vec: pSingle; laplacian: Integer; const model_keypoints: pCvSeq;
  const model_descriptors: pCvSeq): Integer;
Var
  length: Integer;
  i, neighbor: Integer;
  d, dist1, dist2: Double;
  reader, kreader: TCvSeqReader;
  kp: pCvSURFPoint;
  mvec: pSingle;
begin
  length := model_descriptors.elem_size div sizeof(single);
  neighbor := -1;
  dist1 := 1E6;
  dist2 := 1E6;
  cvStartReadSeq(model_keypoints, @kreader, 0);
  cvStartReadSeq(model_descriptors, @reader, 0);

  for i := 0 to model_descriptors.total - 1 do
  begin
    kp := pCvSURFPoint(kreader.ptr);
    mvec := pSingle(reader.ptr);
    CV_NEXT_SEQ_ELEM(kreader.seq.elem_size, kreader);
    CV_NEXT_SEQ_ELEM(reader.seq.elem_size, reader);
    if (laplacian <> kp.laplacian) then
      continue;
    d := compareSURFDescriptors(vec, mvec, dist2, length);
    if (d < dist1) then
    begin
      dist2 := dist1;
      dist1 := d;
      neighbor := i;
    end
    else if (d < dist2) then
      dist2 := d;
  end;
  if (dist1 < 0.6 * dist2) then
    Exit(neighbor);
  Exit(-1);
end;

Type
  TCvPointArr = Array [0 .. 3] of TCvPoint;
  TIntegerDynArray = Array of Integer;
  pIntegerDynArray = ^TIntegerDynArray;

Procedure findPairs(objectKeypoints, objectDescriptors, imageKeypoints, imageDescriptors: pCvSeq; Var ptpairs: TIntegerDynArray);
Var
  i, nearest_neighbor, n: Integer;
  reader, kreader: TCvSeqReader;
  kp: pCvSURFPoint;
  descriptor: pSingle;
Begin
  cvStartReadSeq(objectKeypoints, @kreader);
  cvStartReadSeq(objectDescriptors, @reader);
  n := 0;
  SetLength(ptpairs, 0);
  initialize(ptpairs, 0);
  for i := 0 to objectDescriptors.total - 1 do
  begin
    kp := pCvSURFPoint(kreader.ptr);
    descriptor := pSingle(reader.ptr);
    CV_NEXT_SEQ_ELEM(kreader.seq.elem_size, kreader);
    CV_NEXT_SEQ_ELEM(reader.seq.elem_size, reader);
    nearest_neighbor := naiveNearestNeighbor(descriptor, kp.laplacian, imageKeypoints, imageDescriptors);
    if nearest_neighbor >= 0 Then
    begin
      n := n + 1;
      SetLength(ptpairs, n + 1);
      ptpairs[n] := nearest_neighbor;
    End;
  End;
End;

const
  object_filename = cResourceMedia + 'box.png';
  scene_filename = cResourceMedia + 'box_in_scene.png';

Var
  image, ObjImg, objectcolor, correspond: pIplImage;
  storage: pCvMemStorage;
  objectKeypoints, objectDescriptors, imageKeypoints, imageDescriptors: pCvSeq;
  params: TCvSURFParams;
  tt: Double;
  src_corners: TCvPointArr;
  i: Integer;
  ptpairs: TIntegerDynArray;
  p1, p2: pCvSURFPoint;

begin
  try
    initModule_nonfree;
    ObjImg := cvLoadImage(object_filename, CV_LOAD_IMAGE_GRAYSCALE);
    image := cvLoadImage(scene_filename, CV_LOAD_IMAGE_GRAYSCALE);
    storage := cvCreateMemStorage(0);

    objectcolor := cvCreateImage(cvSize(ObjImg.Width, ObjImg.Height), 8, 3);
    cvCvtColor(ObjImg, objectcolor, CV_GRAY2BGR);
    params := cvSURFParams(500, 1);
    tt := cvGetTickCount;
    cvExtractSURF(ObjImg, 0, @objectKeypoints, @objectDescriptors, storage, params);
    cvExtractSURF(image, 0, @imageKeypoints, @imageDescriptors, storage, params);
    tt := cvGetTickCount - tt;
    src_corners[0] := cvPoint(0, 0);
    src_corners[1] := cvPoint(ObjImg.Width, 0);
    src_corners[2] := cvPoint(ObjImg.Width, ObjImg.Height);
    src_corners[3] := cvPoint(0, ObjImg.Height);
    correspond := cvCreateImage(cvSize(image.Width, ObjImg.Height + image.Height), 8, 1);
    cvSetImageROI(correspond, cvRect(0, 0, ObjImg.Width, ObjImg.Height));
    cvCopy(ObjImg, correspond);
    cvSetImageROI(correspond, cvRect(0, ObjImg.Height, correspond.Width, correspond.Height));
    cvCopy(image, correspond);
    cvResetImageROI(correspond);

    findPairs(objectKeypoints, objectDescriptors, imageKeypoints, imageDescriptors, ptpairs);
    i := 0;
    While i < length(ptpairs) do
    begin
      p1 := pCvSURFPoint(cvGetSeqElem(objectKeypoints, ptpairs[i]));
      p2 := pCvSURFPoint(cvGetSeqElem(imageKeypoints, ptpairs[i]));
      cvLine(correspond, cvPointFrom32f(p1.pt), cvPoint(cvRound(p2.pt.x), cvRound(p2.pt.y + ObjImg.Height)), cvscalarAll(255));
      i := i + 1;
    End;

    cvShowImage('A', correspond);
    While True Do
      if cvWaitKey(50) = 27 Then
        break;

    cvReleaseImage(objectcolor);
    cvReleaseMemStorage(storage);
    cvReleaseImage(ObjImg);
    cvReleaseImage(image);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

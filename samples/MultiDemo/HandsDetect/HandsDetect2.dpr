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
// Original: http://anikettatipamula.blogspot.ro/2012/02/hand-gesture-using-opencv.html
// *******************************************************************

program HandsDetect2;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

procedure detect(const img_8uc1: pIplImage; const img_8uc3: pIplImage);
Var
  storage: pCvMemStorage;
  first_contour: pCvSeq;
  maxitem: pCvSeq;
  area, areamax: Double;
  maxn: Integer;
  Nc: Integer;
  n: Integer;
  c: pCvSeq;
  storage3: pCvMemStorage;
  pt0: TCvPoint;
  storage1, storage2: pCvMemStorage;
  ptseq: pCvSeq;
  hull, defects: pCvSeq;
  i, j: Integer;
  p: pCvPoint;
  hullcount: Integer;
  defectArray: pCvConvexityDefect;
  nomdef: Integer;
  font: TCvFont;
  txt: AnsiString;
begin
  // 8uc1 is BW image with hand as white And 8uc3 is the original image
  storage := cvCreateMemStorage(0);
  first_contour := AllocMem(SizeOf(TCvSeq));
  maxitem := nil;
  area := 0;
  areamax := 0;
  maxn := 0;

  // function to find the white objects in the image and return the object boundaries
  cvClearMemStorage(storage);
  Nc := cvFindContours(img_8uc1, storage, @first_contour, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,
    cvPoint(0, 0));
  // Try all four values and see what happens

  n := 0;
  WriteLn('Total Contours Detected: ', Nc);
  // Here we find the contour with maximum area

  if (Nc > 0) then
  begin
    c := first_contour;
    While c <> nil do
    begin
      // cvCvtColor( img_8uc1, img_8uc3, CV_GRAY2BGR );
      area := Abs(cvContourArea(c, CV_WHOLE_SEQ));
      if (area > areamax) then
      begin
        areamax := area;
        maxitem := c;
        maxn := n;
        c := c^.h_next;
      end;
      Inc(n);
    end;

    storage3 := cvCreateMemStorage(0);
    if (areamax > 5000) then // check for area greater than certain value and find convex hull
    begin
      maxitem := cvApproxPoly(maxitem, SizeOf(TCvContour), storage3, CV_POLY_APPROX_DP, 10, 1);
      storage1 := cvCreateMemStorage(0);
      storage2 := cvCreateMemStorage(0);
      ptseq := cvCreateSeq(CV_SEQ_KIND_GENERIC or CV_32SC2, SizeOf(TCvContour), SizeOf(TCvPoint), storage1);
      for i := 0 to maxitem^.total - 1 do
      begin
        p := CV_GET_SEQ_ELEM(SizeOf(TCvPoint), maxitem, i);
        pt0.x := p^.x;
        pt0.y := p^.y;
        cvSeqPush(ptseq, @pt0);
      end;
      hull := cvConvexHull2(ptseq, nil, CV_CLOCKWISE, 0);
      hullcount := hull^.total;
      defects := cvConvexityDefects(ptseq, hull, storage2);
      WriteLn('cvConvexityDefects total ', defects^.total);

      // int m_nomdef:=0;
      // This cycle marks all defects of convexity of current contours.
      While Assigned(defects) do
      begin
        nomdef := defects^.total; // defect amount
        // outlet_float( m_nomdef, nomdef );
        WriteLn('defect no ', nomdef);
        if (nomdef = 0) then
        begin
          defects := defects^.h_next;
          continue;
        end;
        // Alloc memory for defect set.
        // fprintf(stderr,'malloc');
        defectArray := AllocMem(SizeOf(TCvConvexityDefect) * nomdef);
        // Get defect set.
        // fprintf(stderr,'cvCvtSeqToArray');
        cvCvtSeqToArray(defects, defectArray, CV_WHOLE_SEQ);
        // Draw marks for all defects.
        for i := 0 to nomdef - 1 do
        begin
          WriteLn('Defect depth for defect ', i:4, defectArray[i].depth:10:3);
          cvLine(img_8uc3, defectArray[i].start^, defectArray[i].depth_point^, CV_RGB(255, 255, 0), 1, CV_AA, 0);
          cvCircle(img_8uc3, defectArray[i].depth_point^, 5, CV_RGB(0, 0, 164), 2, 8, 0);
          cvCircle(img_8uc3, defectArray[i].start^, 5, CV_RGB(0, 0, 164), 2, 8, 0);
          cvLine(img_8uc3, defectArray[i].depth_point^, defectArray[i]._end^, CV_RGB(255, 255, 0), 1, CV_AA, 0);
        end;
        txt := '0' + IntToStr(nomdef - 1);
        cvInitFont(@font, CV_FONT_HERSHEY_SIMPLEX, 1.0, 1.0, 0, 5, CV_AA);
        cvPutText(img_8uc3, PAnsiChar(@txt[1]), cvPoint(50, 50), @font, cvScalar(0, 0, 255, 0));
        Inc(j);
        // Free memory.
        freemem(defectArray);
        defects := defects^.h_next;
      end;

      cvReleaseMemStorage(storage);
      cvReleaseMemStorage(storage1);
      cvReleaseMemStorage(storage2);
      cvReleaseMemStorage(storage3);
    end;
  end;
end;

Var
  capture: PCvCapture;
  frame: pIplImage;
  image: pIplImage = nil;
  gray_image: pIplImage = nil;

begin
  try
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    cvNamedWindow('capture', CV_WINDOW_AUTOSIZE);
    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);

      image := cvCloneImage(frame);
      gray_image := cvCreateImage(cvGetSize(image), IPL_DEPTH_8U, 1);
      cvCvtColor(image, gray_image, CV_RGB2GRAY);
      detect(gray_image, image);
      cvShowImage('capture', image);
      cvReleaseImage(image);
      cvReleaseImage(gray_image);
      if cvWaitKey(33) = 27 then
        Break;
    end;
    cvReleaseCapture(capture);
    cvDestroyWindow('capture');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

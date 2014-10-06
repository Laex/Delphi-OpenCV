//*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
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
  //*******************************************************************

program FrameRecon;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.calib3d_c,
  uResourcePaths;

var
  image: pIplImage = nil;
  imageDiv: pIplImage;
  outImg: pIplImage = nil;
  extractedFrame: pIplImage;
  // gray: pIplImage;
  pgray, pcontor: pIplImage;
  intrinsic: PCvMat;
  distortion: PCvMat;
  obj, pro: PCvMat;
  rot, tra: PCvMat;
  mapMatrix: PCvMat;
  origQuad, quad: array [0 .. 3] of TCvPoint2D32f;
  capture: PCvCapture;
  frame: pIplImage;
  selcam: longint = 0;
  cont: integer;

  // Canny/Threshold parameters
  cbCannyChecked: Boolean = false;
  sePar1Value: Double = 100;
  sePar2Value: Double = 50;
  cbPar3Value: integer = 3;
  // approx poly param.
  cbDisplayContChecked: Boolean = false;
  sePolyPar1Value: Double = 7;
  sePolyPar2Value: integer = 0;
  // max perim ratio difference %
  sePerimRatioDiffValue: Double = 20;

procedure drawPerspective(image: pIplImage);
var
  i: integer;
  pt1, pt2: TCvPoint;
begin

  cvZero(pro);

  cvProjectPoints2(obj, rot, tra, intrinsic, distortion, pro);

  cvCircle(image, cvPoint(round(cvGet(pro, 0, 0)), round(cvGet(pro, 0, 1))), 10, cv_rgb(255, 0, 0));
  for i := 0 to 3 do
  begin
    pt1.x := round(cvGet(pro, i, 0));
    pt1.y := round(cvGet(pro, i, 1));
    if i < 3 then
    begin
      pt2.x := round(cvGet(pro, i + 1, 0));
      pt2.y := round(cvGet(pro, i + 1, 1));
    end
    else
    begin
      pt2.x := round(cvGet(pro, 0, 0));
      pt2.y := round(cvGet(pro, 0, 1));
    end;
    cvLine(image, pt1, pt2, cv_rgb(0, 255, 0), 1, CV_AA, 0);
  end;
end;

procedure extractContours(const origImg, image: pIplImage; const storage: pCvMemStorage; var contList: PCvSeq);
var
  nc: integer;
  contours: PCvSeq;

begin
  // *********** find contours in Canny result image
  // find contours and store them all as a list
  contours := nil;
  if (origImg.NChannels > 1) then
  begin
    pgray := cvCreateImage(cvSize(image.width, image.height), IPL_DEPTH_8U, 1);
    cvCvtColor(origImg, pgray, CV_BGR2GRAY)
  end
  else
    pgray := cvCloneImage(origImg);
  pcontor := cvCreateImage(cvSize(image.width, image.height), IPL_DEPTH_8U, 1);
  if cbCannyChecked then
    cvCanny(pgray, pcontor, sePar1Value, sePar2Value, cbPar3Value)
  else
    cvThreshold(pgray, pcontor, sePar1Value, 255, CV_THRESH_BINARY_INV);
  if (image.NChannels > 1) and (cbDisplayContChecked) then
    cvCvtColor(pcontor, image, CV_GRAY2BGR);
  // warning! findcontours destroy the input image, so display it before the call
  cvClearMemStorage(storage);  
  nc := cvFindContours(pcontor, storage, @contours, sizeof(TCvContour), CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE,
    cvPoint(0, 0));

  if nc = 0 then
    contList := nil
  else
    contList := contours;

  cvReleaseImage(pgray);
  cvReleaseImage(pcontor);
end;

procedure drawFrame(const origImg, image: pIplImage);
var
  nc, i: integer;
  contList, contours: PCvSeq;
  seqResult, hole, hole2: PCvSeq;
  storage: pCvMemStorage;
  a, p: Double;
  bRect: TCvRect;

  nholes: integer;
  bol1, bol2, bol3: Boolean;
  m1, q1: single;
  m2, q2: single;
  vert1, vert2: Boolean;
  mi1, qi1: single;
  mi2, qi2: single;
  vertH1, vertH2: Boolean;

  p1, p2: PCvPoint;
  p1Ext: TCvPoint;
  frame_found: Boolean;

  moments: PCvMoments;
  compactness: Double;
begin
  storage := cvCreateMemStorage(0);
  extractContours(origImg, image, storage, contList);

  // test each contour
  contours := contList;
  while (contours <> nil) do
  begin
    a := cvContourArea(contours, CV_WHOLE_SEQ);
    if (abs(a) > 500) and (contours.total > 3) then
    begin
      seqResult := cvApproxPoly(contours, sizeof(TCvContour), storage, CV_POLY_APPROX_DP, sePolyPar1Value,
        sePolyPar2Value);
      if seqResult.total = 4 then
      begin
        Writeln('ContourArea ', a:2:2);
        Writeln('ApproxPoly total ', seqResult.total);
        frame_found := false;
        // calcola coefficienti delle rette diagonali del contorno esterno
        p1 := PCvPoint(cvGetSeqElem(seqResult, 0));
        p1Ext.x := p1.x;
        p1Ext.y := p1.y;
        p2 := PCvPoint(cvGetSeqElem(seqResult, 2));
        if (p1.x <> p2.x) then
        begin
          vert1 := false;
          m1 := (p2.y - p1.y) / (p2.x - p1.x);
          q1 := (p2.x * p1.y - p1.x * p2.y) / (p2.x - p1.x);
        end
        else
        begin
          vert1 := true;
        end;
        p1 := PCvPoint(cvGetSeqElem(seqResult, 1));
        p2 := PCvPoint(cvGetSeqElem(seqResult, 3));
        if (p1.x <> p2.x) then
        begin
          vert2 := false;
          m2 := (p2.y - p1.y) / (p2.x - p1.x);
          q2 := (p2.x * p1.y - p1.x * p2.y) / (p2.x - p1.x);
        end
        else
        begin
          vert2 := true;
        end;

        // internal contours with CV_RETR_TREE
        hole := contours.v_next;
        nholes := 0;
        // 4) scorre i contorni interni (TREE)
        while (hole <> nil) do
        begin
          inc(nholes);
          if nholes > 1 then
            break;
          if hole.total <= 3 then
            Continue;
          // 4.1) approssima ogni contorno con pilogono
          hole2 := cvApproxPoly(hole, sizeof(TCvContour), storage, CV_POLY_APPROX_DP, sePolyPar1Value, sePolyPar2Value);
          // 4.2) verifica se poligono ha 4 vertici
          if hole2.total <> 4 then
            Continue;
          p1 := PCvPoint(cvGetSeqElem(hole2, 0));

          // 4.3) calcola coefficienti delle rette diagonali
          p2 := PCvPoint(cvGetSeqElem(hole2, 2));
          if (p1.x <> p2.x) then
          begin
            vertH1 := false;
            mi1 := (p2.y - p1.y) / (p2.x - p1.x);
            qi1 := (p2.x * p1.y - p1.x * p2.y) / (p2.x - p1.x);
          end
          else
          begin
            vertH1 := true;
          end;
          p1 := PCvPoint(cvGetSeqElem(hole2, 1));
          p2 := PCvPoint(cvGetSeqElem(hole2, 3));
          if (p1.x <> p2.x) then
          begin
            vertH2 := false;
            mi2 := (p2.y - p1.y) / (p2.x - p1.x);
            qi2 := (p2.x * p1.y - p1.x * p2.y) / (p2.x - p1.x);
          end
          else
          begin
            vertH2 := true;
          end;

          // 4.4) se i coefficienti delle diagonali del poligono interno
          // sono uguali a quelli del poligono esterno, a meno di errore dato,
          // la ricerca e' positiva, uscire dalla funzione
          if (vert1 = vertH1) and (vert2 = vertH2) and (abs(m1 - mi1) / m1 <= (sePerimRatioDiffValue / 100)) and
            (abs(q1 - qi1) / q1 <= (sePerimRatioDiffValue / 100)) and
            (abs(m2 - mi2) / m2 <= (sePerimRatioDiffValue / 100)) and
            (abs(q2 - qi2) / q2 <= (sePerimRatioDiffValue / 100)) then
          begin
            frame_found := true;
            break;
          end;

          hole := hole.h_next;

        end; // while hole
        if (frame_found) then
        begin
          Writeln('frame_found');

          cvZero(rot);
          cvZero(tra);
          // copy projection data to CvMat array
          for i := 0 to 3 do
          begin
            p1 := PCvPoint(cvGetSeqElem(seqResult, i));
            cvSet(pro, i, 0, p1.x);
            cvSet(pro, i, 1, p1.y);
            quad[i].x := p1.x;
            quad[i].y := p1.y;
          end;

          cvFindExtrinsicCameraParams2(obj, pro, intrinsic, distortion, rot, tra);
          Writeln('Z dist. ' + FloatToStr(trunc(cvGet(tra, 0, 2))));

          cvGetPerspectiveTransform(@origQuad, @quad, mapMatrix);
          bRect := pcvcontour(seqResult).rect;

          cvZero(extractedFrame);
          cvWarpPerspective(origImg, extractedFrame, mapMatrix,
            (CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS + CV_WARP_INVERSE_MAP), cvScalar(0, 0, 0, 0));

          cvDrawContours(image, PCvSeq(seqResult), cv_rgb(255, 0, 0), cv_rgb(0, 255, 0), 0, 4, CV_AA, cvPoint(0, 0));

          break;
        end;
      end; // if seqresult.total=4
    end; // if abs
    // take the next contour
    contours := contours.h_next;
  end;
  cvRelease(contList);
  cvReleaseMemStorage(storage);
end;

procedure main_cycle(frame: pIplImage);
var
  cs: TCvSize;
begin
  if not(assigned(image)) then
  begin
    // * allocate all the buffers
    cs.width := frame.width;
    cs.height := frame.height;
    image := cvCreateImage(cs, 8, 3);
    image.Origin := frame.Origin;
    imageDiv := cvCreateImage(cvSize(cs.width div 2, cs.height div 2), 8, 3);
    imageDiv.Origin := frame.Origin;
    outImg := cvCreateImage(cs, 8, 3);
    outImg.Origin := frame.Origin;
  end;

  cvCopy(frame, image);
  cvSmooth(image, image, CV_GAUSSIAN, 3, 3);

  cvCopy(image, outImg);

  drawFrame(image, outImg);
  if cont = 20 then
  begin
    cont := 1;
    cvShowImage('Paint window', extractedFrame);
  end
  else
    inc(cont);

  drawPerspective(outImg);
  cvCopy(outImg, frame);
end;

procedure Help;
begin
  Writeln('c - CannyChecked');
  Writeln('d - DisplayContChecked');
end;

begin
  try
    ///
    // Calibration: Resource\Calibration_Chess.png
    // Test: Resource\frame.png
    ///

    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not assigned(capture) then
      Halt;
    cvNamedWindow('Paint window', 1);
    cvNamedWindow('Original', 1);

    intrinsic := cvLoad(cResourceMedia + 'Intrinsics.xml');
    // multiply by 2 because the matrix is computed on 320x240 image
    cvConvertScale(intrinsic, intrinsic, 2.0, 0);
    distortion := cvLoad(cResourceMedia + 'Distortion.xml');
    obj := cvCreateMat(4, 3, CV_32FC1);
    pro := cvCreateMat(4, 2, CV_32FC1);
    rot := cvCreateMat(1, 3, CV_32FC1);
    tra := cvCreateMat(1, 3, CV_32FC1);
    mapMatrix := cvCreateMat(3, 3, CV_32FC1);
    cvSet(obj, 0, 0, -100);
    cvSet(obj, 0, 1, -100);
    cvSet(obj, 0, 2, 0.0);
    origQuad[0].x := 0;
    origQuad[0].y := 0;

    cvSet(obj, 1, 0, 100.0);
    cvSet(obj, 1, 1, -100);
    cvSet(obj, 1, 2, 0.0);
    origQuad[1].x := 300;
    origQuad[1].y := 0;

    cvSet(obj, 2, 0, 100.0);
    cvSet(obj, 2, 1, 100.0);
    cvSet(obj, 2, 2, 0.0);
    origQuad[2].x := 300;
    origQuad[2].y := 300;

    cvSet(obj, 3, 0, -100);
    cvSet(obj, 3, 1, 100.0);
    cvSet(obj, 3, 2, 0.0);
    origQuad[3].x := 0;
    origQuad[3].y := 300;

    cvSet(rot, 0, 0, 0.0);
    cvSet(rot, 0, 1, 0.0);
    cvSet(rot, 0, 2, 0.0);

    cvSet(tra, 0, 0, 0.0);
    cvSet(tra, 0, 1, 0.0);
    // the Z coord on translation vector is set to focal length
    cvSet(tra, 0, 2, cvGet(intrinsic, 0, 0));

    // appo image for warp perspective
    extractedFrame := cvCreateImage(cvSize(800, 600), 8, 3);

    Help;

    while true do
    begin
      // получаем кадр
      frame := cvQueryFrame(capture);
      if not assigned(frame) then
        break;
      cvShowImage('Original', frame);
      main_cycle(frame);
      case cvWaitKey(10) of
        27:
          break;
        Ord('c'):
          cbCannyChecked := not cbCannyChecked;
        Ord('d'):
          cbDisplayContChecked := not cbDisplayContChecked;
      end;
    end;

    cvReleaseCapture(capture);
    cvReleaseImage(image);
    cvReleaseImage(outImg);
    cvReleaseImage(extractedFrame);
    cvDestroyAllWindows;
  except
    on e: Exception do
      Writeln(e.ClassName, ': ', e.Message);
  end;

end.

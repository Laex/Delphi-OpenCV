(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)

// Example 10-2. Kalman filter sample code
//
// Use Kalman Filter to model particle in circular trajectory.
(* *************** License:**************************
  Oct. 3, 2008
  Right to use this code in any way you want without warrenty, support or any guarentee of it working.

  BOOK: It would be nice if you cited it:
  Learning OpenCV: Computer Vision with the OpenCV Library
  by Gary Bradski and Adrian Kaehler
  Published by O'Reilly Media, October 3, 2008

  AVAILABLE AT:
  http://www.amazon.com/Learning-OpenCV-Computer-Vision-Library/dp/0596516134
  Or: http://oreilly.com/catalog/9780596516130/
  ISBN-10: 0596516134 or: ISBN-13: 978-0596516130

  OTHER OPENCV SITES:
  * The source code is on sourceforge at:
  http://sourceforge.net/projects/opencvlibrary/
  * The OpenCV wiki page (As of Oct 1, 2008 this is down for changing over servers, but should come back):
  http://opencvlibrary.sourceforge.net/
  * An active user group is at:
  http://tech.groups.yahoo.com/group/OpenCV/
  * The minutes of weekly OpenCV development meetings are at:
  http://pr.willowgarage.com/wiki/OpenCV
  ************************************************** *)

program ch10_ex10_2;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.utils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.tracking_c,
  ocv.compat,
  uResourcePaths;

function phi2xy(const img: PIplImage; const mat: PCvMat): TcvPoint;
begin
  Result := cvPoint(cvRound(img^.width / 2 + img^.width / 3 * cos(pFloat(mat^.data)[0])),
    cvRound(img^.height / 2 - img^.width / 3 * sin(pFloat(mat^.data)[0])))
end;

Var

  CVX_RED: TCvScalar;
  CVX_GREEN: TCvScalar;
  CVX_BLUE: TCvScalar;

  CVX_CYAN: TCvScalar;
  CVX_MAGENTA: TCvScalar;
  CVX_YELLOW: TCvScalar;

  CVX_WHITE: TCvScalar;
  CVX_BLACK: TCvScalar;
  CVX_GRAY50: TCvScalar;

  rng: TCvRandState;
  img: PIplImage;
  kalman: PCvKalman;
  x_k: PCvMat;
  w_k: PCvMat;
  z_k: PCvMat;
  F: array [0 .. 3] of Single;
  y_k: PCvMat;

begin
  try
    // Init CVX
    CVX_RED := CV_RGB($FF, $00, $00);
    CVX_GREEN := CV_RGB($00, $FF, $00);
    CVX_BLUE := CV_RGB($00, $00, $FF);

    CVX_CYAN := CV_RGB($00, $FF, $FF);
    CVX_MAGENTA := CV_RGB($FF, $00, $FF);
    CVX_YELLOW := CV_RGB($FF, $FF, $00);

    CVX_WHITE := CV_RGB($FF, $FF, $FF);
    CVX_BLACK := CV_RGB($00, $00, $00);
    CVX_GRAY50 := CV_RGB($88, $88, $88);

    // Initialize, create Kalman Filter object, window, random number
    // generator etc.
    //
    cvNamedWindow('Kalman', 1);
    cvRandInit(@rng, 0, 1, -1, CV_RAND_UNI);

    img := cvCreateImage(cvSize(500, 500), 8, 3);
    kalman := cvCreateKalman(2, 1, 0);
    // state is (phi, delta_phi) - angle and angular velocity
    // Initialize with random guess.
    //
    x_k := cvCreateMat(2, 1, CV_32FC1);
    cvRandSetRange(@rng, 0, 0.1, 0);
    rng.disttype := CV_RAND_NORMAL;
    cvRand(@rng, x_k);

    // process noise
    //
    w_k := cvCreateMat(2, 1, CV_32FC1);

    // measurements, only one parameter for angle
    //
    z_k := cvCreateMat(1, 1, CV_32FC1);
    cvZero(z_k);

    // Transition matrix 'F' describes relationship between
    // model parameters at step k and at step k+1 (this is
    // the 'dynamics' in our model.
    //
    F[0] := 1;
    F[1] := 1;
    F[2] := 0;
    F[3] := 1;
    Move(F, kalman^.transition_matrix^.data.ptr^, sizeof(F));
    // Initialize other Kalman filter parameters.
    //
    cvSetIdentity(kalman^.measurement_matrix, cvRealScalar(1));
    cvSetIdentity(kalman^.process_noise_cov, cvRealScalar(1E-5));
    cvSetIdentity(kalman^.measurement_noise_cov, cvRealScalar(1E-1));
    cvSetIdentity(kalman^.error_cov_post, cvRealScalar(1));

    // choose random initial state
    //
    cvRand(@rng, kalman^.state_post);

    while True do
    begin
      // predict point position
      y_k := cvKalmanPredict(kalman^, 0);

      // generate measurement (z_k)
      //
      cvRandSetRange(@rng, 0, sqrt(pFloat(kalman^.measurement_noise_cov^.data)[0]), 0);
      cvRand(@rng, z_k);
      cvMatMulAdd(kalman^.measurement_matrix, x_k, z_k, z_k);
      // plot points (eg convert to planar co-ordinates and draw)
      //
      cvZero(img);
      cvCircle(img, phi2xy(img, z_k), 4, CVX_YELLOW); // observed state
      cvCircle(img, phi2xy(img, y_k), 4, CVX_WHITE, 2); // 'predicted' state
      cvCircle(img, phi2xy(img, x_k), 4, CVX_RED); // real state
      cvShowImage('Kalman', img);
      // adjust Kalman filter state
      //
      cvKalmanCorrect(kalman^, z_k);

      // Apply the transition matrix 'F' (eg, step time forward)
      // and also apply the 'process' noise w_k.
      //
      cvRandSetRange(@rng, 0, sqrt(pFloat(kalman^.process_noise_cov^.data)[0]), 0);
      cvRand(@rng, w_k);
      cvMatMulAdd(kalman^.transition_matrix, x_k, w_k, x_k);

      // exit if user hits 'Esc'
      if (cvWaitKey(100) = 27) then
        break;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

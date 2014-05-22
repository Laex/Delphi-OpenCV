//*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // Frans van Daalen (CLubfitter73)
  // email: clubfitter73@gmail.com
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

program Posit;
{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.calib3d_c,
  ocv.compat,
  uResourcePaths;

Var
  P: Integer;
  PositObject: PCvPOSITObject;
  ModelPoints: pCvPoint3D32f;
  SrcImagePoints: pCvPoint2D32f;
  Cube_Size: Single = 10;
  focal_length: double = 1000;
  flEpsilon: double;
  criteria: TCvTermCriteria;
  rotation_matrix: TCvMatr32f;
  translation_vector: TCvVect32f;
  ProjectedPoints: pCvPoint2D32f;
  Point3D: TcvPoint3D32f;
  Point2D: TcvPoint2D32f;
  img: pIplImage;
  centreX, centreY: Integer;

begin
  try
    // Create the model points
    // the first must be (0,0,0)
    ModelPoints := AllocMem(SizeOf(TcvPoint3D32f) * 4);
    ModelPoints[0].x := 0.0;
    ModelPoints[0].y := 0.0;
    ModelPoints[0].z := 0.0;
    //
    ModelPoints[1].x := 0.0;
    ModelPoints[1].y := 0.0;
    ModelPoints[1].z := Cube_Size;
    //
    ModelPoints[2].x := Cube_Size;
    ModelPoints[2].y := 0.0;
    ModelPoints[2].z := 0.0;
    //
    ModelPoints[3].x := 0.0;
    ModelPoints[3].y := Cube_Size;
    ModelPoints[3].z := 0.0;
    // Create the image points
    SrcImagePoints := AllocMem(SizeOf(TcvPoint2D32f) * 4);
    SrcImagePoints[0].x := -48;
    SrcImagePoints[0].y := -224;
    //
    SrcImagePoints[1].x := -287;
    SrcImagePoints[1].y := -174;
    //
    SrcImagePoints[2].x := 132;
    SrcImagePoints[2].y := -153;
    //
    SrcImagePoints[3].x := -52;
    SrcImagePoints[3].y := 149;
    // Create The POSIT object with the model points
    PositObject := cvCreatePOSITObject(ModelPoints, 4);
    // Estimate the pose
    rotation_matrix := AllocMem(SizeOf(PSingle) * 9);
    translation_vector := AllocMem(SizeOf(PSingle) * 3);
    criteria := cvTermCriteria(CV_TERMCRIT_EPS or CV_TERMCRIT_ITER, 100, flEpsilon);
    cvPOSIT(PositObject, SrcImagePoints, focal_length, criteria, rotation_matrix, translation_vector);
    writeln('-.- SOURCE MODEL POINTS -.-');
    for P := 0 to 3 do
      writeln(format('%.0f, %.0f, %.0f', [ModelPoints[P].x, ModelPoints[P].y, ModelPoints[P].z]));
    writeln(#13#10 + '-.- SOURCE IMAGE POINTS -.-');
    for P := 0 to 3 do
      writeln(format('%.0f, %.0f', [SrcImagePoints[P].x, SrcImagePoints[P].y]));
    writeln(#13#10 + '-.- ESTIMATED ROTATION -.-');
    for P := 0 to 2 do
      writeln(format('%.8f | %.8f | %.8f', [rotation_matrix[P * 3], rotation_matrix[P * 3 + 1],
        rotation_matrix[P * 3 + 2]]));
    writeln(#13#10 + '-.- ESTIMATED TRANSLATION -.-');
    writeln(format('%.8f | %.8f | %.8f', [translation_vector[0], translation_vector[1], translation_vector[2]]));
    // Project the model points with the estimated pose
    ProjectedPoints := AllocMem(SizeOf(TcvPoint2D32f) * 4);
    for P := 0 to 3 do
    Begin
      Point3D.x := rotation_matrix[0] * ModelPoints[P].x + rotation_matrix[1] * ModelPoints[P].y + rotation_matrix[2] *
        ModelPoints[P].z + translation_vector[0];
      Point3D.y := rotation_matrix[3] * ModelPoints[P].x + rotation_matrix[4] * ModelPoints[P].y + rotation_matrix[5] *
        ModelPoints[P].z + translation_vector[1];
      Point3D.z := rotation_matrix[6] * ModelPoints[P].x + rotation_matrix[7] * ModelPoints[P].y + rotation_matrix[8] *
        ModelPoints[P].z + translation_vector[2];
      Point2D := cvPoint2D32f(0.0, 0.0);
      if Not IsZero(Point3D.z) then
      Begin
        Point2D.x := focal_length * Point3D.x / Point3D.z;
        Point2D.y := focal_length * Point3D.y / Point3D.z;
      End;
      ProjectedPoints[P] := cvPoint2D32f(Point2D.x, Point2D.y);
    End;
    img := cvLoadImage(cResourceMedia + 'cv_posit.jpg');
    if not Assigned(img) then
    Begin
      writeln('Image not loaded!');
      halt;
    End;
    centreX := round(img.width * 0.5);
    centreY := round(img.height * 0.5);
    // Draw the source image points;
    for P := 0 to 3 do
      cvCircle(img, cvPoint(centreX + round(SrcImagePoints[P].x), centreY - round(SrcImagePoints[P].y)), 8,
        CV_RGB(255, 0, 0));
    // Draw the axes
    cvLine(img, cvPoint(centreX + round(ProjectedPoints[0].x), centreY - round(ProjectedPoints[0].y)),
      cvPoint(centreX + round(ProjectedPoints[1].x), centreY - round(ProjectedPoints[1].y)), CV_RGB(0, 0, 255), 2);
    cvLine(img, cvPoint(centreX + round(ProjectedPoints[0].x), centreY - round(ProjectedPoints[0].y)),
      cvPoint(centreX + round(ProjectedPoints[2].x), centreY - round(ProjectedPoints[2].y)), CV_RGB(255, 0, 0), 2);
    cvLine(img, cvPoint(centreX + round(ProjectedPoints[0].x), centreY - round(ProjectedPoints[0].y)),
      cvPoint(centreX + round(ProjectedPoints[3].x), centreY - round(ProjectedPoints[3].y)), CV_RGB(0, 255, 0), 2);
    // Draw the projected model points
    writeln(#13#10 + '-.- ESTIMATED IMAGE POINTS -.-');
    for P := 0 to 3 do
    begin
      cvCircle(img, cvPoint(centreX + round(ProjectedPoints[P].x), centreY - round(ProjectedPoints[P].y)), 3,
        CV_RGB(255, 255, 255), -1);
      writeln(format('%.3f, %.3f', [ProjectedPoints[P].x, ProjectedPoints[P].y]));
    end;
    cvNamedWindow('Posit');
    cvShowImage('Posit', img);
    cvWaitKey();
    cvReleasePOSITObject(PositObject);
    cvReleaseImage(img);
    cvDestroyAllWindows;
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;

end.

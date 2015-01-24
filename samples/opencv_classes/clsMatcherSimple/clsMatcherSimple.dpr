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
  Original: opencv/samples/cpp/matcher_simple.cpp
  *******************************************************************
*)
program clsMatcherSimple;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.Core.types_c,
  ocv.core_c,
  ocv.highgui,
  ocv.highgui_c,
  ocv.features2d,
  ocv.nonfree,
  ocv.mat,
  uResourcePaths;

procedure Help;
begin
  WriteLn('This program demonstrates using features2d detector, '#13#10'descriptor extractor and simple matcher'#13#10 +
    'Using the SURF desriptor:'#13#10#13#10'Usage:'#13#10#9'matcher_simple <image1> <image2>');
end;

{ .$DEFINE USE_SURF }
{ .$DEFINE USE_SIFT }
{ .$DEFINE USE_ORB }
{$DEFINE USE_BRISK}

Var
{$IFDEF USE_SURF}
  detector: TSurfFeatureDetector;
  extractor: TSurfDescriptorExtractor;
{$ENDIF}
{$IFDEF USE_SIFT}
  detector: TSiftFeatureDetector;
  extractor: TSiftDescriptorExtractor;
{$ENDIF}
{$IFDEF USE_ORB}
  detector: TORBFeatureDetector;
  extractor: TORBDescriptorExtractor;
{$ENDIF}
{$IFDEF USE_BRISK}
  detector: TBRISKFeatureDetector;
  extractor: TBRISKDescriptorExtractor;
{$ENDIF}
  img1, img2: TccvMat;
  keypoints1, keypoints2: TCVectorKeyPoint;
  descriptors1: TccvMat;
  descriptors2: TccvMat;
  matches: TCVectorDMatch;
  img_matches: TccvMat;
  matcher: TBFMatcher;

begin
  try
    if ParamCount <> 2 then
    begin
      Help;
      img1 := imread(cResourceMedia + 'baboon200.jpg', CV_LOAD_IMAGE_GRAYSCALE);
      img2 := imread(cResourceMedia + 'baboon200_rotated.jpg', CV_LOAD_IMAGE_GRAYSCALE);
    end
    else
    begin
      img1 := imread(ParamStr(1), CV_LOAD_IMAGE_GRAYSCALE);
      img2 := imread(ParamStr(2), CV_LOAD_IMAGE_GRAYSCALE);
    end;

    if img1.empty or img2.empty then
    begin
      WriteLn('Can''t read one of the images');
      Halt(1);
    end;

{$IFDEF USE_SURF}
    detector := TSurfFeatureDetector.Create(400);
    extractor := TSurfFeatureDetector.Create;
{$ENDIF}
{$IFDEF USE_SIFT}
    detector := TSiftFeatureDetector.Create;
    extractor := TSiftDescriptorExtractor.Create;
{$ENDIF}
{$IFDEF USE_ORB}
    detector := TORBFeatureDetector.Create;
    extractor := TORBDescriptorExtractor.Create;
{$ENDIF}
{$IFDEF USE_BRISK}
    detector := TBRISKFeatureDetector.Create;
    extractor := TBRISKDescriptorExtractor.Create;
{$ENDIF}
    // detecting keypoints
    keypoints1 := TCVectorKeyPoint.Create;
    keypoints2 := TCVectorKeyPoint.Create;
    detector.detect(img1, keypoints1);
    detector.detect(img2, keypoints2);

    // computing descriptors
    extractor.compute(img1, keypoints1, descriptors1);
    extractor.compute(img2, keypoints2, descriptors2);

    // matching descriptors
    matcher := TBFMatcher.Create(NORM_L2);
    matches := TCVectorDMatch.Create;
    matcher.match(descriptors1, descriptors2, matches);

    // drawing the results
    namedWindow('matches', 1);
    DrawMatches(img1, keypoints1, img2, keypoints2, matches, img_matches);
    imshow('matches', img_matches);
    waitKey(0);

    img1.Free;
    img2.Free;
    detector.Free;
    keypoints1.Free;
    keypoints2.Free;
    extractor.Free;
    descriptors1.Free;
    descriptors2.Free;
    matcher.Free;
    matches.Free;
    img_matches.Free;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

(*///////////////////////////////////////////////////////////////////////////////////////
//
//  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
//
//  By downloading, copying, installing or using the software you agree to this license.
//  If you do not agree to this license, do not download, install,
//  copy or use the software.
//
//
//                        Intel License Agreement
//                For Open Source Computer Vision Library
//
// Copyright (C) 2000, Intel Corporation, all rights reserved.
// Third party copyrights are property of their respective owners.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
//   * Redistribution's of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//
//   * Redistribution's in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//   * The name of Intel Corporation may not be used to endorse or promote products
//     derived from this software without specific prior written permission.
//
// This software is provided by the copyright holders and contributors "as is" and
// any express or implied warranties, including, but not limited to, the implied
// warranties of merchantability and fitness for a particular purpose are disclaimed.
// In no event shall the Intel Corporation or contributors be liable for any direct,
// indirect, incidental, special, exemplary, or consequential damages
// (including, but not limited to, procurement of substitute goods or services;
// loss of use, data, or profits; or business interruption) however caused
// and on any theory of liability, whether in contract, strict liability,
// or tort (including negligence or otherwise) arising in any way out of
// the use of this software, even if advised of the possibility of such damage.
//
//*)

{$IFDEF DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$ELSE}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$ENDIF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
Unit haar;

interface

Uses core_c, Core.types_c;

Const
  CV_HAAR_FEATURE_MAX = 3;

Type

  TSumType = integer;
  pSumType = ^TSumType;

  Tsqsumtype = Double;
  psqsumtype = ^TSqsumtype;

  TCvHidHaarFeatureRect = packed record
    p0, p1, p2, p3: psumtype;
  end;

  TCvHidHaarFeature = array [0 .. CV_HAAR_FEATURE_MAX - 1] of TCvHidHaarFeatureRect;

  pCvHidHaarTreeNode = ^TCvHidHaarTreeNode;

  TCvHidHaarTreeNode = packed record
    feature: TCvHidHaarFeature;
    threshold: Single;
    left: integer;
    right: integer;
  end;

  pCvHidHaarClassifier = ^TCvHidHaarClassifier;

  TCvHidHaarClassifier = packed record
    count: integer;
    // CvHaarFeature* orig_feature;
    node: pCvHidHaarTreeNode;
    alpha: pSingle;
  end;

  pCvHidHaarStageClassifier = ^TCvHidHaarStageClassifier;

  TCvHidHaarStageClassifier = packed record
    count: integer;
    threshold: Single;
    classifier: pCvHidHaarClassifier;
    two_rects: integer;

    next: pCvHidHaarStageClassifier;
    child: pCvHidHaarStageClassifier;
    parent: pCvHidHaarStageClassifier;
  end;

  TCvHidHaarClassifierCascade = packed record
    count: integer;
    isStumpBased: integer;
    has_tilted_features: integer;
    is_tree: integer;
    inv_window_area: Real;
    sum, sqsum, tilted: TCvMat;
    stage_classifier: pCvHidHaarStageClassifier;
    pq0, pq1, pq2, pq3: psqsumtype;
    p0, p1, p2, p3: psumtype;
    ipp_stages: pPointer;
  end;

implementation

end.

// --------------------------------- OpenCV license.txt ---------------------------
// IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
//
// By downloading, copying, installing or using the software you agree to this license.
// If you do not agree to this license, do not download, install,
// copy or use the software.
//
//
// License Agreement
// For Open Source Computer Vision Library
//
// Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
// Copyright (C) 2009, Willow Garage Inc., all rights reserved.
// Third party copyrights are property of their respective owners.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// * Redistribution's of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// * Redistribution's in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// * The name of the copyright holders may not be used to endorse or promote products
// derived from this software without specific prior written permission.
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

// **************************************************************************************************
// Project Delphi-OpenCV
// **************************************************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// Mikhail Grigorev
// email:sleuthound@gmail.com
// **************************************************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// **************************************************************************************************
// License:
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU Lesser General Public License (the  "LGPL License"), in which case the
// provisions of the LGPL License are applicable instead of those above.
// If you wish to allow use of your version of this file only under the terms
// of the LGPL License and not to allow others to use your version of this file
// under the MPL, indicate your decision by deleting  the provisions above and
// replace  them with the notice and other provisions required by the LGPL
// License.  If you do not delete the provisions above, a recipient may use
// your version of this file under either the MPL or the LGPL License.
//
// For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
// **************************************************************************************************
// Warning: Using Delphi XE3 syntax!
// **************************************************************************************************
// The Initial Developer of the Original Code:
// OpenCV: open source computer vision library
// Homepage:    http://ocv.org
// Online docs: http://docs.ocv.org
// Q&A forum:   http://answers.ocv.org
// Dev zone:    http://code.ocv.org
// **************************************************************************************************
// Original file:
// opencv\modules\objdetect\src\haar.cpp
// *************************************************************************************************

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
unit ocv.haar;

interface

uses ocv.core.types_c;

Const
  CV_HAAR_FEATURE_MAX = 3;

Type

  TSumType = integer;
  pSumType = ^TSumType;

  Tsqsumtype = Double;
  psqsumtype = ^Tsqsumtype;

  TCvHidHaarFeatureRect = record
    p0, p1, p2, p3: pSumType;
  end;

  TCvHidHaarFeature = array [0 .. CV_HAAR_FEATURE_MAX - 1] of TCvHidHaarFeatureRect;

  pCvHidHaarTreeNode = ^TCvHidHaarTreeNode;

  TCvHidHaarTreeNode = record
    feature: TCvHidHaarFeature;
    threshold: Single;
    left: integer;
    right: integer;
  end;

  pCvHidHaarClassifier = ^TCvHidHaarClassifier;

  TCvHidHaarClassifier = record
    count: integer;
    // CvHaarFeature* orig_feature;
    node: pCvHidHaarTreeNode;
    alpha: pSingle;
  end;

  pCvHidHaarStageClassifier = ^TCvHidHaarStageClassifier;

  TCvHidHaarStageClassifier = record
    count: integer;
    threshold: Single;
    classifier: pCvHidHaarClassifier;
    two_rects: integer;

    next: pCvHidHaarStageClassifier;
    child: pCvHidHaarStageClassifier;
    parent: pCvHidHaarStageClassifier;
  end;

  TCvHidHaarClassifierCascade = record
    count: integer;
    isStumpBased: integer;
    has_tilted_features: integer;
    is_tree: integer;
    inv_window_area: Real;
    sum, sqsum, tilted: TCvMat;
    stage_classifier: pCvHidHaarStageClassifier;
    pq0, pq1, pq2, pq3: psqsumtype;
    p0, p1, p2, p3: pSumType;
    ipp_stages: pPointer;
  end;

implementation

end.

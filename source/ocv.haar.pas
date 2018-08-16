(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  Mikhail Grigorev
  email:sleuthhound@gmail.com
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  Warning: Using Delphi XE3 syntax!
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://ocv.org
  Online docs: http://docs.ocv.org
  Q&A forum:   http://answers.ocv.org
  Dev zone:    http://code.ocv.org
  **************************************************************************************************
  Original file:
  opencv\modules\objdetect\src\haar.cpp
  *************************************************************************************************
*)

{$I OpenCV.inc}

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

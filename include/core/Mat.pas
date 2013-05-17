// --------------------------------- OpenCV license.txt ---------------------------
(* //    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //    By downloading, copying, installing or using the software you agree to this license.
  //    If you do not agree to this license, do not download, install,
  //    copy or use the software.
  //
  //
  //                             License Agreement
  //                  For Open Source Computer Vision Library
  //
  //   Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  //   Copyright (C) 2009, Willow Garage Inc., all rights reserved.
  //   Third party copyrights are property of their respective owners.
  //
  //   Redistribution and use in source and binary forms, with or without modification,
  //   are permitted provided that the following conditions are met:
  //
  //     * Redistribution's of source code must retain the above copyright notice,
  //       this list of conditions and the following disclaimer.
  //
  //     * Redistribution's in binary form must reproduce the above copyright notice,
  //       this list of conditions and the following disclaimer in the documentation
  //       and/or other materials provided with the distribution.
  //
  //     * The name of the copyright holders may not be used to endorse or promote products
  //       derived from this software without specific prior written permission.
  //
  //   This software is provided by the copyright holders and contributors "as is" and
  //   any express or implied warranties, including, but not limited to, the implied
  //   warranties of merchantability and fitness for a particular purpose are disclaimed.
  //   In no event shall the Intel Corporation or contributors be liable for any direct,
  //   indirect, incidental, special, exemplary, or consequential damages
  //   (including, but not limited to, procurement of substitute goods or services;
  //   loss of use, data, or profits; or business interruption) however caused
  //   and on any theory of liability, whether in contract, strict liability,
  //   or tort (including negligence or otherwise) arising in any way out of
  //   the use of this software, even if advised of the possibility of such damage. *)

(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  laentir Valetov
  //  email:laex@bk.ru
  //  **************************************************************************************************
  //  You may retrieve the latest version of this file at the GitHub,
  //  located at git://github.com/Laex/Delphi-OpenCV.git
  //  **************************************************************************************************
  //  License:
  //  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  //  you may not use this file except in compliance with the License. You may obtain a copy of the
  //  License at http://www.mozilla.org/MPL/
  //
  //  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  //  ANY KIND, either express or implied. See the License for the specific language governing rights
  //  and limitations under the License.
  //
  //  Alternatively, the contents of this file may be used under the terms of the
  //  GNU Lesser General Public License (the  "LGPL License"), in which case the
  //  provisions of the LGPL License are applicable instead of those above.
  //  If you wish to allow use of your version of this file only under the terms
  //  of the LGPL License and not to allow others to use your version of this file
  //  under the MPL, indicate your decision by deleting  the provisions above and
  //  replace  them with the notice and other provisions required by the LGPL
  //  License.  If you do not delete the provisions above, a recipient may use
  //  your version of this file under either the MPL or the LGPL License.
  //
  //  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  //  **************************************************************************************************
  //  Warning: Using Delphi XE3 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  **************************************************************************************************
  //  Original file:
  //  opencv\modules\core\include\opencv2\core\mat.hpp
  //  ************************************************************************************************* *)

unit Mat;

interface

Uses WinApi.Windows, Core.types_c;

const
  MAGIC_VAL = $42FF0000;
  AUTO_STEP = 0;
  CONTINUOUS_FLAG = CV_MAT_CONT_FLAG;
  SUBMATRIX_FLAG = CV_SUBMAT_FLAG;

  MAGIC_MASK = $FFFF0000;
  TYPE_MASK = $00000FFF;
  DEPTH_MASK = 7;

Type

  // Attention!
  // The sequence of function declarations interface must match the
  // sequence of function declarations in the project "opencv_classes" (C++)

  IMat = interface
    ['{9C458D5C-F577-4A2D-89A0-FC426B80CC56}']
    // ! returns element size in bytes,
    // similar to CV_ELEM_SIZE(cvmat->type)
    function elemSize(): size_t; stdcall;
    // ! returns the size of element channel in bytes.
    function elemSize1(): size_t; stdcall;
    // ! returns element type, similar to CV_MAT_TYPE(cvmat->type)
    function _type: Integer; stdcall;
    // ! returns element type, similar to CV_MAT_DEPTH(cvmat->type)
    function depth: Integer; stdcall;
    // ! returns element type, similar to CV_MAT_CN(cvmat->type)
    function channels: Integer; stdcall;
    // ! returns step/elemSize1()
    function step1(i: Integer=0): size_t; stdcall;
    // ! returns true if matrix data is NULL
    function empty: bool; stdcall;
    // ! returns the total number of matrix elements
    function total: size_t; stdcall;
    // * ! includes several bit - fields: - the magic signature - continuity flag - depth - number of channels * /
    function flags: Integer; stdcall;
    // ! the matrix dimensionality, >= 2
    function dims: Integer; stdcall;
    // ! the number of rows and columns or (-1, -1) when the matrix has more than 2 dimensions
    function rows: Integer; stdcall;
    function cols: Integer; stdcall;
    // ! pointer to the data
    function data: pByte; stdcall;
    // ! pointer to the reference counter;
    // when matrix points to user-allocated data, the pointer is NULL
    function refcount: pInteger; stdcall;

    // -----------------------------------
    function getMat(): Pointer; stdcall;
    procedure setMat(mat:Pointer); stdcall;
  end;

  // ! default constructor
function CreateMat: IMat; overload; safecall;
// ! constructs 2D matrix of the specified size and type
// (_type is CV_8UC1, CV_64FC3, CV_32SC(12) etc.)
function CreateMat(rows, cols, _type: Integer): IMat; overload; safecall;

implementation

Uses uLibName;

function CreateMat: IMat; external OpenCV_Classes_DLL index 1;
function CreateMat(rows, cols, _type: Integer): IMat; external OpenCV_Classes_DLL index 2;

end.

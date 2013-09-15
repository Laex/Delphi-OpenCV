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
  //  opencv\modules\core\include\opencv2\core\types.hpp
  //  ************************************************************************************************* *)

unit core.types;

interface

Uses Windows, core.types_c, Mat;

Type
  /// ///////////////////////////// Point_ ////////////////////////////////
  (* !
    //  template 2D point class.

    //  The class defines a point in 2D space. Data type of the point coordinates is specified
    //  as a template parameter. There are a few shorter aliases available for user convenience.
    //  See cv::Point, cv::Point2i, cv::Point2f and cv::Point2d.
  *)
  IPoint = interface
    ['{CA53E78F-F6BF-46E2-B034-7BDD72FFD952}']
    function get_x(): Integer; stdcall;
    function get_y(): Integer; stdcall;
    procedure set_x(x: Integer); stdcall;
    procedure set_y(y: Integer); stdcall;
    // ---------------------------------
    function getPoint(): Pointer;
    // ---------------------------------
    property x: Integer read get_x write set_x;
    property y: Integer read get_y write set_y;
  end;

  IScalar = interface
    ['{0887A2A4-3A75-4739-A6E8-F9CB502B8864}']
    function isReal(): BOOL; stdcall;
    // ---------------------------------
    function getScalar(): Pointer; stdcall;
  end;

  ISize2i = interface
    ['{14B0A243-5E3D-4ECD-9F1E-8AD47CEF0CE0}']
    // ---------------------------------
    function getSize(): Pointer; stdcall;
  end;

  ISize = ISize2i;

  IString = interface
    ['{50C8309F-69B6-4E8C-A2B5-F2530007CCA1}']
    function getString(): Pointer; stdcall;
  end;

function Point: IPoint; overload; safecall;
function Point(x, y: Integer): IPoint; overload; safecall;
function Scalar: IScalar; overload; safecall;
function Scalar(v0, v1: Integer; v2: Integer = 0; v3: Integer = 0): IScalar; overload; safecall;
function Scalar(v0: Integer): IScalar; overload; safecall;
function CreateSize: ISize; overload; safecall;
function CreateSize(width, height: Integer): ISize; overload; safecall;

function CString(const s: pCVChar): Pointer; safecall;

Type
TIplImageRecordHelper = record helper for TIplImage
  function InitFromMat(const Mat: IMat): TIplImage;
end;

implementation

Uses uLibName, core_c;

function Point: IPoint; external OpenCV_Classes_DLL index 200;
function Point(x, y: Integer): IPoint; external OpenCV_Classes_DLL index 201;
function Scalar: IScalar; external OpenCV_Classes_DLL index 202;
function Scalar(v0, v1: Integer; v2: Integer; v3: Integer): IScalar; external OpenCV_Classes_DLL index 203;
function Scalar(v0: Integer): IScalar; external OpenCV_Classes_DLL index 204;
function CreateSize: ISize; external OpenCV_Classes_DLL index 205;
function CreateSize(width, height: Integer): ISize; external OpenCV_Classes_DLL index 206;

function CString; external OpenCV_Classes_DLL index 300;

function TIplImageRecordHelper.InitFromMat(const Mat: IMat): TIplImage;
begin
  Assert(Mat.dims <= 2);
  cvInitImageHeader(
    @Self,
    CvSize(Mat.cols, Mat.rows),
    cvIplDepth(Mat.flags),
    Mat.channels);
  cvSetData(
    @Self,
    Mat.data,
    Mat.step1);
end;

end.

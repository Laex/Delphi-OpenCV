// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// Mikhail Grigorev
// email:sleuthound@gmail.com
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

unit opencv.mat;

interface

uses
  WinApi.Windows,
  opencv.core.types_c,
  opencv.highgui_c;

Type
  TocvMat = class
  public
    // similar to CV_ELEM_SIZE(cvmat->type)
    function elemSize(): size_t; virtual; stdcall; abstract;
    // ! returns the size of element channel in bytes.
    function elemSize1(): size_t; virtual; stdcall; abstract;
    // ! returns element type, similar to CV_MAT_TYPE(cvmat->type)
    function _type: Integer; virtual; stdcall; abstract;
    // ! returns element type, similar to CV_MAT_DEPTH(cvmat->type)
    function depth: Integer; virtual; stdcall; abstract;
    // ! returns element type, similar to CV_MAT_CN(cvmat->type)
    function channels: Integer; virtual; stdcall; abstract;
    // ! returns step/elemSize1()
    function step1(i: Integer = 0): size_t; virtual; stdcall; abstract;
    // ! returns true if matrix data is NULL
    function empty: bool; virtual; stdcall; abstract;
    // ! returns the total number of matrix elements
    function total: size_t; virtual; stdcall; abstract;
    // * ! includes several bit - fields: - the magic signature - continuity flag - depth - number of channels * /
    function flags: Integer; virtual; stdcall; abstract;
    // ! the matrix dimensionality, >= 2
    function dims: Integer; virtual; stdcall; abstract;
    // ! the number of rows and columns or (-1, -1) when the matrix has more than 2 dimensions
    function rows: Integer; virtual; stdcall; abstract;
    function cols: Integer; virtual; stdcall; abstract;
    // ! pointer to the data
    function data: pByte; virtual; stdcall; abstract;
    procedure copyTo(Var m: TocvMat); virtual; stdcall; abstract;
    // ------------------------------------------------
  end;

function CreateMat: TocvMat; stdcall; overload;
function CreateMat(rows, cols, _type: Integer): TocvMat; stdcall; overload;
procedure ReleaseMat(ex: TocvMat); stdcall;

implementation

uses opencv.ulibname;

function CreateMat: TocvMat; stdcall; external opencv_classes_lib name 'CreateMat'; overload;
function CreateMat(rows, cols, _type: Integer): TocvMat; stdcall; external opencv_classes_lib name 'CreateMatRCT'; overload;
procedure ReleaseMat; external opencv_classes_lib;

end.

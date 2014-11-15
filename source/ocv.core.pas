(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  Mikhail Grigorev
  email:sleuthound@gmail.com
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
*)

//
{$I OpenCV.inc}
//
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
unit ocv.core;

interface

uses
  ocv.mat, ocv.core.types_c, Winapi.Windows;

Type

  TccvSize_<T> = class
    // ! various constructors
    // Size_();
    // Size_(_Tp _width, _Tp _height);
    // Size_(const Size_ & sz);
    // Size_(const CvSize & sz);
    // Size_(const CvSize2D32f & sz);
    // Size_(const Point_<_Tp> & pt);
    // Size_ & operator = (const Size_ & sz);
    // ! the area (width*height)
    // _Tp area() const;

    // ! conversion of another data type.
    // template < typename _Tp2 > operator Size_<_Tp2>()  const;

    // ! conversion to the old-style OpenCV types
    // operator CvSize()const;
    // operator CvSize2D32f()const;

    // _Tp width, height; // the width and the height
  end;

  TccvSize2i = TccvSize_<integer>;
  TccvSize2d = TccvSize_<double>;

  TccvSize = class(TccvSize2i)
  public
    class function Create(width, height: integer): TccvSize; overload;
    procedure Free; reintroduce;
  end;

function Size(const width, height: integer): TccvSize; overload;
function Size(const S: TccvSize; const width, height: integer): TccvSize; overload;

Type
  TccvVec3<T> = class

  end;

  TccvVec3d = TccvVec3<double>;
  TccvVec3b = TccvVec3<byte>;

  TccvPoint<T> = class
  public
    function getX(): T; virtual; stdcall; abstract;
    procedure setX(x: T); virtual; stdcall; abstract;
    function getY(): T; virtual; stdcall; abstract;
    procedure setY(y: T); virtual; stdcall; abstract;

    property x: T read getX write setX;
    property y: T read getY write setY;
  end;

  TccvPoint2i = TccvPoint<integer>;

  TccvPoint = class(TccvPoint2i)
  public
    class function Create: TccvPoint; overload;
    class function Create(_x, _y: integer): TccvPoint; overload;
    procedure Free; reintroduce;
  end;

function Point(const x: integer = 0; const y: integer = 0): TccvPoint; overload;
function Point(const P: TccvPoint; const x: integer = 0; const y: integer = 0): TccvPoint; overload;

Type
  TccvVectorOfPoint<T> = class
  public
    function Size(): integer; virtual; stdcall; abstract;
  end;

  TccvVectorOfPoint2i = TccvVectorOfPoint<TccvPoint2i>;
  TccvVectorOfPoint   = TccvVectorOfPoint2i;

  TccvVectorOfVectorOfPoint<T> = class
  public
    function Size(): integer; virtual; stdcall; abstract;
    procedure push_back(Val: TccvVectorOfPoint2i); virtual; stdcall; abstract;
  end;

  TccvVectorOfVectorOfPoint2i = TccvVectorOfVectorOfPoint<TccvVectorOfPoint2i>;

  TccvRotatedRect = class

  end;

  TccvScalar_<Tp> = class
  public
    // ! returns a scalar with all elements set to v0
    procedure all(v0: Tp); virtual; stdcall; abstract;
    // ! conversion to the old-style CvScalar
    function CvScalar: TCvScalar; virtual; stdcall; abstract;
    // ! conversion to another data type
    // template<typename T2> operator Scalar_<T2>() const;
    // ! per-element product
    // Scalar_<_Tp> mul(const Scalar_<_Tp>& t, double scale=1 ) const;
    function mul(T: TccvScalar_<Tp>; scale: double = 1): TccvScalar_<Tp>; virtual; stdcall; abstract;
    // returns (v0, -v1, -v2, -v3)
    function conj: TccvScalar_<Tp>; virtual; stdcall; abstract;
    // returns true iff v1 == v2 == v3 == 0
    function isReal: bool; virtual; stdcall; abstract;
    //
    function GetVec(const Index: integer): Tp; virtual; stdcall; abstract;
    procedure SetVec(const Index: integer; const Value: Tp); virtual; stdcall; abstract;
    property Vec[const Index: integer]: Tp Read GetVec write SetVec; default;
  end;

  TccvScalar = class(TccvScalar_<double>)
  public
    class function Create: TccvScalar; overload;
    class function Create(v0, v1: double; v2: double = 0; v3: double = 0): TccvScalar; overload;
    class function Create(const S: TCvScalar): TccvScalar; overload;
    class function Create(v0: double): TccvScalar; overload;
    procedure Free; reintroduce;
  end;

function Scalar(const v0: double = 0; const v1: double = 0; const v2: double = 0; const v3: double = 0)
  : TccvScalar; overload;
function Scalar(const S: TccvScalar; const v0: double = 0; const v1: double = 0; const v2: double = 0;
  const v3: double = 0): TccvScalar; overload;

(* ! draws the circle outline or a solid circle in the image

  CV_EXPORTS_W void circle(CV_IN_OUT Mat& img, Point center, int radius,
  const Scalar& color, int thickness=1,int lineType=8, int shift=0);
*)
procedure circle(img: TccvMat; center: TccvPoint; radius: integer; const color: TccvScalar; thickness: integer = 1;
  lineType: integer = 8; shift: integer = 0); stdcall;

// ! draws the line segment (pt1, pt2) in the image
// CV_EXPORTS_W void line(CV_IN_OUT Mat& img, Point pt1, Point pt2, const Scalar& color,
// int thickness=1, int lineType=8, int shift=0);
procedure line(img: TccvMat; pt1, pt2: TccvPoint; color: TccvScalar; thickness: integer = 1; lineType: integer = 8;
  shift: integer = 0); stdcall;

// ! renders text string in the image
// CV_EXPORTS_W void putText( Mat& img, const string& text, Point org,
// int fontFace, double fontScale, Scalar color,
// int thickness=1, int lineType=8,
// bool bottomLeftOrigin=false );
procedure putText(img: TccvMat; text: String; org: TccvPoint; fontFace: integer; fontScale: double; color: TccvScalar;
  thickness: integer = 1; lineType: integer = 8; bottomLeftOrigin: bool = false); stdcall;

/// ////////////////////////////////////////////////////////////////////////
// function CreateVec3d(): TccvVec3d; stdcall; overload;
// function CreateVec3d(v0, v1, v2: Double): TccvVec3d; stdcall; overload;
// function Vec3d(v0, v1, v2: Double): TccvVec3d; stdcall;
// procedure ReleaseVec3d(ex: TccvVec3d); stdcall;
//
// function CreateVec3b(): TccvVec3b; stdcall; overload;
// function CreateVec3b(v0, v1, v2: byte): TccvVec3b; stdcall; overload;
// function Vec3b(v0, v1, v2: byte): TccvVec3b; stdcall;
// procedure ReleaseVec3b(ex: TccvVec3b); stdcall;
//
// procedure ReleasePoint2i(ex: TccvPoint2i); stdcall;
// procedure ReleaseVectorOfPoint2i(ex: TccvVectorOfPoint2i); stdcall;
// procedure ReleaseVectorOfVectorOfPoint2i(ex: TccvVectorOfVectorOfPoint2i); stdcall;
//
// procedure MSER(m: TccvMat; var ex: TccvVectorOfVectorOfPoint2i); stdcall;

implementation

uses ocv.lib;

procedure circle; stdcall; external opencv_classes_lib;
procedure line; stdcall; external opencv_classes_lib;
procedure putText; stdcall; external opencv_classes_lib;

/// ///////////////////////////////////////////////////////////
function CreateVec3d(): TccvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d'; overload;
function CreateVec3d(v0, v1, v2: double): TccvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d3'; overload;
function Vec3d(v0, v1, v2: double): TccvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d3'; overload;
procedure ReleaseVec3d(ex: TccvVec3d); stdcall; external opencv_classes_lib;

function CreateVec3b(): TccvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b'; overload;
function CreateVec3b(v0, v1, v2: byte): TccvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b3'; overload;
function Vec3b(v0, v1, v2: byte): TccvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b3'; overload;
procedure ReleaseVec3b(ex: TccvVec3b); stdcall; external opencv_classes_lib;

procedure ReleasePoint2i(ex: TccvPoint2i); stdcall; external opencv_classes_lib;
procedure ReleaseVectorOfPoint2i(ex: TccvVectorOfPoint2i); stdcall; external opencv_classes_lib;
procedure ReleaseVectorOfVectorOfPoint2i(ex: TccvVectorOfVectorOfPoint2i); stdcall; external opencv_classes_lib;

procedure MSER(m: TccvMat; var ex: TccvVectorOfVectorOfPoint2i); stdcall; external opencv_classes_lib;

function Scalar(const v0: double = 0; const v1: double = 0; const v2: double = 0; const v3: double = 0)
  : TccvScalar; overload;
begin
  Result := TccvScalar.Create(v0, v1, v2, v3);
end;

function Scalar(const S: TccvScalar; const v0: double = 0; const v1: double = 0; const v2: double = 0;
  const v3: double = 0): TccvScalar; overload;
begin
  S[0] := v0;
  S[1] := v1;
  S[2] := v2;
  S[3] := v3;
  Result := S;
end;

function Point(const x: integer = 0; const y: integer = 0): TccvPoint;
begin
  Result := TccvPoint.Create(x, y);
end;

function Point(const P: TccvPoint; const x: integer = 0; const y: integer = 0): TccvPoint;
begin
  P.x := x;
  P.y := y;
  Result := P;
end;

{ TccvScalar }

class function TccvScalar.Create(v0, v1, v2, v3: double): TccvScalar;
begin

end;

class function TccvScalar.Create: TccvScalar;
begin

end;

class function TccvScalar.Create(v0: double): TccvScalar;
begin

end;

class function TccvScalar.Create(const S: TCvScalar): TccvScalar;
begin

end;

procedure TccvScalar.Free;
begin

end;

{ TccvSize }

class function TccvSize.Create(width, height: integer): TccvSize;
begin

end;

procedure TccvSize.Free;
begin

end;

function Size(const width, height: integer): TccvSize; overload;
begin

end;

function Size(const S: TccvSize; const width, height: integer): TccvSize; overload;
begin

end;

{ TccvPoint }

class function TccvPoint.Create: TccvPoint;
begin

end;

class function TccvPoint.Create(_x, _y: integer): TccvPoint;
begin

end;

procedure TccvPoint.Free;
begin

end;

end.

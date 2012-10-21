(* ************************************************************************

  ** Converted with C to Pascal Converter 2.0
  ** Release: 2.1.4.2012
  ** Email: al_gun@ncable.net.au
  ** Updates: http://cc.codegear.com/Author/302259
  ** Blogs: http://delphiprogrammingwithalgun.blogspot.com/
  ** Copyright (c) 2005, 2012 Ural Gunaydin (a.k.a. Al Gun)

  ************************************************************************* *)

unit core_c;

{$ifdef DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$else}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$endif}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

(*
  ** 'C2PTypes.pas' declares external windows data types for the conversion purposes.
  ** It's created by the CtoPas converter and saved under
  ** "\Program Files\Common Files\AlGun Shared\CToPas 2.0\P_Files" folder.
  ** Consult the Windows and Delphi help files for more information about defined data types
*)

uses
  // {C2PTypes,}
  Windows,
  // Messages, SysUtils, Classes,
  Core.types_c;

(* M///////////////////////////////////////////////////////////////////////////////////////
  //
  //  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //  By downloading, copying, installing or using the software you agree to this license.
  //  If you do not agree to this license, do not download, install,
  //  copy or use the software.
  //
  //
  //                           License Agreement
  //                For Open Source Computer Vision Library
  //
  // Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  // Copyright (C) 2009, Willow Garage Inc., all rights reserved.
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
  //   * The name of the copyright holders may not be used to endorse or promote products
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
  //M*/


  {$ifndef __OPENCV_CORE_C_H__}
  {$define __OPENCV_CORE_C_H__}

  {$HPPEMIT '#include 'opencv2/core/types_c.h''}

  {$ifdef __cplusplus}
  //extern "C" {
  {$endif}

  (****************************************************************************************\
  *          cArray allocation, deallocation, initialization and access to elements         *
  *************************************************************************************** *)

(* <malloc> wrapper.
  If there is no enough memory, the cFunction
  (as well as other OpenCV functions that call cvAlloc)
  raises an error. *)
procedure cvAlloc(size: size_t); cdecl;
(* <free> wrapper.
  Here and further all the memory releasing functions
  (that all call cvFree) take Double cPointer in order to
  to clear cPointer to the data after releasing it.
  Passing cPointer to 0 cPointer is Ok: nothing happens in this
*)
procedure cvFree_(ptr: Pointer); cdecl;
{$DEFINE cvFree(ptr(cvFree_}	(* (ptr)), *(ptr)=0)

  (* Allocates and initializes IplImage header *)
function cvCreateImageHeader(size: CvSize; depth: Integer; channels: Integer): IplImage; cdecl;

(* Inializes IplImage header *)
// function cvInitImageHeader(IplImage * image, CvSize size, Integer depth, CVAPI(IplImage)channels,
// Integer origin CV_DEFAULT(v1: 4)): Integer): IplImage;

(* Creates IPL image (header and data) *)
function cvCreateImage(size: CvSize; depth, channels: Integer): pIplImage; cdecl;

(* Releases (i.e. deallocates) IPL image header *)
procedure cvReleaseImageHeader(image: array of IplImage); cdecl;

(* Releases IPL image header and data *)
procedure cvReleaseImage(var image: pIplImage); cdecl;

(* Creates a copy of IPL image (widthStep may differ) *)
function cvCloneImage(image: pIplImage): IplImage; cdecl;

(* Sets a Channel Of Interest (only a few functions support COI) -
  use cvCopy to extract the selected channel and/or put it back *)
procedure cvSetImageCOI(var image: IplImage; coi: Integer); cdecl;

(* Retrieves image Channel Of Interest *)
function cvGetImageCOI(image: pIplImage): Integer; cdecl;

(* Sets image ROI (region of interest) (COI is not changed) *)
procedure cvSetImageROI(var image: IplImage; rect: CvRect); cdecl;

(* Resets image ROI and COI *)
procedure cvResetImageROI(var image: IplImage); cdecl;

(* Retrieves image ROI *)
function cvGetImageROI(image: pIplImage): CvRect; cdecl;

(* Allocates and initalizes CvMat header *)
function cvCreateMatHeader(rows: Integer; cols: Integer; cType: Integer): CvMat; cdecl;

//
// const
// CV_AUTOSTEP = $7FFFFFFF;
// {$EXTERNALSYM CV_AUTOSTEP}
// (* Initializes CvMat header *)
// CVAPI(CvMat)cvInitMatHeader(CvMat * mat, Integer rows, Integer cols,
// function cType,
// function
// function CV_DEFAULT(v1: CV_AUTOSTEP)): Integer;
//
// (* Allocates and initializes CvMat header and allocates data *)
// CVAPI(CvMat)cvCreateMat(Integer rows, Integer cols, Integer cType): Pointer;
//
// (* Releases CvMat header and deallocates matrix data
// (reference counting is used for data) *)
// procedure cvReleaseMat(v1: var Decrements CvMat data reference counter and
// deallocates the data if it reaches 0 * )CV_INLINE CV_INLINE procedure cvDecRefData(CvArr * arr)
// (CV_IS_MAT(arr))begin CvMat * mat := (CvMat)arr;
// mat^.data.ptr := 0;
// if (mat^.refcount <> 0 and - - * mat^.refcount = 0) then
// cvFree(and mat^.refcount);
// mat^.refcount := 0;
// end;
// else if (CV_IS_MATND(arr)) then
// begin
// CvMatND * mat := (CvMatND)arr;
// mat^.data.ptr := 0;
// if (mat^.refcount <> 0 and - - * mat^.refcount = 0) then
// cvFree(and mat^.refcount);
// mat^.refcount := 0;
// end;
// end;
//
// (* Increments CvMat data reference counter *)
// CV_INLINE
// function cvIncRefData(var mat := (CvMat)arr;
// if (mat^.refcount <> 0) then refcount := + + * mat^.refcount; end;
// else if (CV_IS_MATND(arr)) then begin CvMatND * mat := (CvMatND)arr;
// if (mat^.refcount <> 0) then refcount := + + * mat^.refcount; end; result := refcount; end;
// (* Creates an exact copy of the input matrix (except: CV_IS_MAT( arr )) then     begin         CvMat;
// be: may;
// var )CVAPI(CvMat) cvCloneMat(  CvMat* mat: step value)): Integer;
//
//
// (* Makes a new matrix from <rect> subrectangle of input array.
// No data is copied *)
// CVAPI(CvMat)cvGetSubRect(CvArr * arr, CvMat * submat, CvRect rect);
//
// const cvGetSubArr = cvGetSubRect;
// {$EXTERNALSYM cvGetSubArr}
// (* Selects row span of the input array: arr(start_row:delta_row:end_row,:)
// (end_row is not included into the span). *)
// CVAPI(CvMat)cvGetRows(CvArr * arr, CvMat * submat, Integer start_row, Integer end_row,
// function delta_row CV_DEFAULT(v1: 1)): Integer;
//
// CV_INLINE CvMat * cvGetRow(CvArr * arr, CvMat * submat, Integer row)
//
// begin result := cvGetRows(arr, submat, row, row + 1, 1); end;
//
// (* Selects column span of the input array: arr(:,start_col:end_col)
// (end_col is not included into the span) *)
// CVAPI(CvMat)cvGetCols(CvArr * arr, CvMat * submat, Integer start_col, Integer end_col);
//
// CV_INLINE CvMat * cvGetCol(CvArr * arr, CvMat * submat, Integer col)
//
// begin result := cvGetCols(arr, submat, col, col + 1); end;
//
// (* Select a diagonal of the input array.
// (diag = 0 means the main diagonal, >0 means a diagonal above the main one,
// <0 - below the main one).
// The diagonal will be represented as a column (nx1 matrix). *)
// CVAPI(CvMat)cvGetDiag(CvArr * arr, CvMat * submat,
// function diag CV_DEFAULT(v1: 0)): Integer;
//
// (* low-level scalar <-> raw data conversion functions *)
// procedure cvScalarToRawData(0)): Integer;
//
// procedure cvRawDataToScalar(Pointer data: v1:; cType: Integer; var scalar: CvScalar);
//
// (* Allocates and initializes CvMatND header *)
// CVAPI(CvMatND)cvCreateMatNDHeader(Integer dims, Integer * sizes, Integer cType);
//
// (* Allocates and initializes CvMatND header and allocates data *)
// CVAPI(CvMatND)cvCreateMatND(Integer dims, Integer * sizes, Integer cType);
//
// (* Initializes preallocated CvMatND header *)
// CVAPI(CvMatND)cvInitMatNDHeader(CvMatND * mat, Integer dims, Integer * sizes,
// function cType,
// function
// function CV_DEFAULT(v1: 0)): Integer;
//
// (* Releases CvMatND *)
// CV_INLINE CV_INLINE
// procedure cvReleaseMatND(var)mat): Pointer; end; (* Creates a copy of CvMatND (except: (CvMat;
// be: may;
// var )CVAPI(CvMatND) cvCloneMatND(  CvMatND* mat ): data;(* Allocates and initializes CvSparseMat header and allocates data *)
// CVAPI(CvSparseMat)cvCreateSparseMat(Integer dims: steps); var sizes: Integer; cType: Integer);
//
// (* Releases CvSparseMat *)
// procedure cvReleaseSparseMat(mat: array of CvSparseMat);
//
// (* Creates a copy of CvSparseMat (except, may be, zero items) *)
// CVAPI(CvSparseMat)cvCloneSparseMat(CvSparseMat * mat);
//
// (* Initializes sparse array iterator
// (returns the first node or 0 if the cArray is empty) *) then CVAPI(CvSparseNode)
// cvInitSparseMatIterator(CvSparseMat * mat, CvSparseMatIterator * mat_iterator);
//
/// / returns next sparse array node (or NULL if there is no more nodes)
// CV_INLINE CvSparseNode * cvGetNextSparseNode(CvSparseMatIterator * mat_iterator)
// begin if (mat_iterator^.node^.next) then result := mat_iterator^.node = mat_iterator^.node^.next;
// else begin Integer idx; for (idx := + + mat_iterator^.curidx; idx < mat_iterator^.mat^.hashsize;
// idx + +)begin * node = (CvSparseNode)mat_iterator^.mat^.hashtable: array [0 .. idx -
// 1] of CvSparseNode; if (node) then begin mat_iterator^.curidx := idx;
// result := mat_iterator^.node = node; end; end; result := 0; end; end;
//
// (* *************** matrix iterator: used for n-ary operations on dense arrays ******** *)
//
// const CV_MAX_ARR = 10;
// {$EXTERNALSYM CV_MAX_ARR}
// type = record count: Integer; (* number of arrays *)
// dims: Integer; (* number of dimensions to iterate *)
// size: CvSize; (* maximal common linear size: { width = size, height = 1 } *)
// ptr: array [0 .. CV_MAX_ARR - 1] of ^uchar; (* pointers to the array slices *)
// stack: array [0 .. CV_MAX_DIM - 1] of Integer; (* for internal use *)
// hdr: array [0 .. CV_MAX_ARR - 1] of ^CvMatND; (* pointers to the headers of the
// end;
// CvNArrayIterator;
//
// const CV_NO_DEPTH_CHECK = 1;
// {$EXTERNALSYM CV_NO_DEPTH_CHECK}
// const CV_NO_CN_CHECK = 2;
// {$EXTERNALSYM CV_NO_CN_CHECK}
// const CV_NO_SIZE_CHECK = 4;
// {$EXTERNALSYM CV_NO_SIZE_CHECK}
//
// (* initializes iterator that traverses through several arrays simulteneously
// (the cFunction together with cvNextArraySlice is used for
// N-ari element-wise operations) *)
// CVAPI(Integer)cvInitNArrayIterator(Integer count, CvArr * * arrs, CvArr * mask, CvMatND * stubs,
// CvNArrayIterator * array_iterator,
// function flags CV_DEFAULT(v1: 0)): Integer;
//
// (* returns zero value if iteration is finished, non-zero (slice length) otherwise *)
// CVAPI(Integer)cvNextNArraySlice(CvNArrayIterator * array_iterator);
//
// (* Returns type of array elements:
// const CV_64FC4 = Args: array of const;
// {$EXTERNALSYM CV_64FC4}
// CVAPI(Integer) cvGetElemType(  CvArr* arr );
//
// (* Retrieves number of an array dimensions and
// optionally sizes of the dimensions *)
// CVAPI(Integer)cvGetDims(CvArr * arr, Integer * sizes CV_DEFAULT(0));
//
// (* Retrieves size of a particular array dimension.
// For 2d arrays cvGetDimSize(arr,0) returns number of rows (image height)
// and cvGetDimSize(arr,1) returns number of columns (image width) *)
// CVAPI(Integer)cvGetDimSize(CvArr * arr, Integer index);
//
// (* ptr = &arr(idx0,idx1,...). All indexes are zero-based,
// the major dimensions go first (e.g. (y,x) for 2D, (z,y,x) for 3D *)
// CVAPI(uchar)cvPtr1D(CvArr * arr, Integer idx0, Integer * cType CV_DEFAULT(0));
// CVAPI(uchar)cvPtr2D(CvArr * arr, Integer idx0, Integer idx1, Integer * cType CV_DEFAULT(0));
// CVAPI(uchar)cvPtr3D(CvArr * arr, Integer idx0, Integer idx1, Integer idx2,
// function cType CV_DEFAULT(v1: 0)): Integer;
//
// (* For CvMat or IplImage number of indices should be 2
// (row index (y) goes first, column index (x) goes next).
// For CvMatND or CvSparseMat number of infices should match number of <dims> and
// indices order should match the cArray dimension order. *)
// CVAPI(uchar)cvPtrND(CvArr * arr, Integer * idx, Integer * cType CV_DEFAULT(0),
// function create_node CV_DEFAULT(v1: 0)): Cardinal;
//
// (* value = arr(idx0,idx1,...) *)
// CVAPI(CvScalar)cvGet1D(CvArr * arr, Integer idx0): Integer; CVAPI(CvScalar)cvGet2D(CvArr * arr,
// Integer idx0, Integer idx1); CVAPI(CvScalar)cvGet3D(CvArr * arr, Integer idx0, Integer idx1,
// Integer idx2); CVAPI(CvScalar)cvGetND(CvArr * arr, Integer * idx);
//
// (* for 1-channel arrays *)
// CVAPI(Double)cvGetReal1D(CvArr * arr, Integer idx0); CVAPI(Double)cvGetReal2D(CvArr * arr,
// Integer idx0, Integer idx1); CVAPI(Double)cvGetReal3D(CvArr * arr, Integer idx0, Integer idx1,
// Integer idx2); CVAPI(Double)cvGetRealND(CvArr * arr, Integer * idx);
//
// (* arr(idx0,idx1,...) = value *)
// procedure cvSet1D(var arr: CvArr; idx0: Integer; value: CvScalar);
// procedure cvSet2D(var arr: CvArr; idx0: Integer; idx1: Integer; value: CvScalar);
// procedure cvSet3D(var arr: CvArr; idx0: Integer; idx1: Integer; idx2: Integer; value: CvScalar);
// procedure cvSetND(var arr: CvArr; var idx: Integer; value: CvScalar);
//
// (* for 1-channel arrays *)
// procedure cvSetReal1D(var arr: CvArr; idx0: Integer; value: Double);
// procedure cvSetReal2D(var arr: CvArr; idx0: Integer; idx1: Integer; value: Double);
// procedure cvSetReal3D(var arr: CvArr; idx0: Integer; idx1: Integer; idx2: Integer; value: Double);
// procedure cvSetRealND(var arr: CvArr; var idx: Integer; value: Double);
//
// (* clears element of ND dense array,
// in  of sparse arrays it deletes the specified node *)
// procedure cvClearND(var arr: CvArr; var idx: Integer);
//
// (* Converts CvArr (IplImage or CvMat,...) to CvMat.
// If the last parameter is non-zero, cFunction can
// convert multi(>2)-dimensional cArray to CvMat as LongInt as
// the last array's dimension is continous. The resultant
// matrix will be have appropriate (a huge) number of rows *)
// CVAPI(CvMat)cvGetMat(CvArr * arr, CvMat * header,
// function coi CV_DEFAULT(v1: 0)): Integer;
//
// (* Converts CvArr (IplImage or CvMat) to IplImage *)
// function cvGetImage(CvArr * arr, IplImage * image_header);
//
// (* Changes a shape of multi-dimensional array.
// new_cn = 0 means that number of channels remains unchanged.
// new_dims = 0 means that number and sizes of dimensions remain the same
// (unless they need to be changed to set the new number of channels)
// if new_dims = 1, there is no need to specify new dimension sizes
// The resultant configuration should be achievable w/o data copying.
// If the resultant cArray is sparse, CvSparseMat header should be passed
// to the cFunction else if the cResult is 1 or 2 dimensional,
// CvMat header should be passed to the cFunction
// else CvMatND header should be passed *)
// CVAPI(CvArr)cvReshapeMatND(CvArr * arr, Integer sizeof_header, CvArr * header, Integer new_cn,
// Integer new_dims, Integer * new_sizes);
//
/// / >> Following declaration is a macro definition!
// const cvReshapeND(arr, header, new_cn, new_dims, new_sizes)cvReshapeMatND((arr), SizeOf((header)),
// (header), (new_cn), (new_dims), (new_sizes));
//
// CVAPI(CvMat)cvReshape(CvArr * arr, CvMat * header,
// function new_cn, Integer new_rows CV_DEFAULT(v1: 0)): Integer;
//
// (* Repeats source 2d array several times in both horizontal and
// vertical direction to fill destination cArray *)
// procedure cvRepeat(var src: CvArr; var dst: CvArr);
//
// (* Allocates array data *)
// procedure cvCreateData(var arr: CvArr);
//
// (* Releases array data *)
// procedure cvReleaseData(var arr: CvArr);
//
// (* Attaches user data to the array header. The step is reffered to
// the pre-last dimension. That is, all the planes of the cArray
// must be joint (w/o gaps) *)
// procedure cvSetData(var arr: CvArr; data: Pointer; step: Integer);
//
// (* Retrieves raw data of CvMat, IplImage or CvMatND.
// In the latter  the cFunction raises an error if
// the cArray can not be represented as a matrix *)
// procedure cvGetRawData(var Returns width and height of array in elements * )

// CVAPI(CvSize) cvGetSize( const CvArr* arr );
//function cvGetSize(const arr: CvArr): CvSize; cdecl;
//todo -cmedium -oLaex: Использовать процедуру OpenCV (need use OpenCV)
function cvGetSize(const image: pIplImage): CvSize;

//
// (* Copies source array to destination array *)
// procedure cvCopy(var Sets all or " masked " elements of input array to the same value * )
// procedure cvSet(CvArr * arr: v1: 0)): CvArr; (; value: CvScalar;
// var Clears all the array elements(Sets them to 0) * )
// procedure cvSetZero(CvArr * arr: unction mask CV_DEFAULT(v1: 0)): CvArr; ();
// const cvZero = cvSetZero;
// {$EXTERNALSYM cvZero}
// (* Splits a multi-channel array into the set of single-channel arrays or
// particular : array[0..color-1] of extracts plane *)
// procedure cvSplit(var src: CvArr; var dst0: CvArr; var dst1: CvArr; var dst2: CvArr;
// var dst3: CvArr);
//
// (* Merges a set of single-channel arrays into the single multi-channel array
// inserts one particular : array[0..color-1] of or plane to the cArray *)
// procedure cvMerge(var src0: CvArr; var src1: CvArr; var = src3: onst CvArr; var } CvArr * dst:
// {$EXTERNALSYM CvArr);
//
// (* Copies several channels from input arrays to
// certain channels of output arrays *)
// procedure  cvMixChannels(
// src: array of CvArr;
// src_count: Integer;
// dst: array of CvArr;
// dst_count: Integer;
// var from_to: nteger;
// pair_count: Integer);
//
// (* Performs linear transformation on every source array element:
// dst(x,y,c) = scale*src(x,y,c)+shift.
// Arbitrary combination of input and output cArray depths are allowed
// (number of channels must be the same), thus the cFunction can be used
// for cType conversion *)
// procedure  cvConvertScale(
// 0)): Double;const cvCvtScale = cvConvertScale;{$EXTERNALSYM cvCvtScale}const cvScale =
// cvConvertScale; {$EXTERNALSYM cvScale}// >> Following declaration is a macro definition!const cvConvert( src: v1:;
// )cvConvertScale((src): dst; v3: (dst); :; : );
//
// (* Performs linear transformation on every source array element,
// stores absolute value of the cResult:
// dst(x,y,c) = abs(scale*src(x,y,c)+shift).
// destination cArray must have 8u cType.
// In other cases one may use cvConvertScale + cvAbsDiffS *)
// procedure cvConvertScaleAbs(var checks termination criteria validity and
// Sets eps to default_eps(if it is not set) then : v1: 0)): Double;
// const cvCvtScaleAbs = cvConvertScaleAbs;
// {$EXTERNALSYM cvCvtScaleAbs}(; var)CVAPI(CvTermCriteria)cvCheckTermCriteria(CvTermCriteria criteria
// : max_iter to default_max_iters(if it is not set) then; default_eps: Double;
// default_max_iters: Integer);
//
// (* ***************************************************************************************\
// *                   Arithmetic, logic and comparison operations                          *
// *************************************************************************************** *)
//
// (* dst(mask) = src1(mask) + src2(mask) *)
// procedure cvAdd(var dst(mask) = src(mask) + value * )
// procedure cvAddS(CvArr * src: v1: 0)): CvArr; (; value: CvScalar; var dst: CvArr;
// var dst(mask) = src1(mask) - src2(mask) * )
// procedure cvSub(CvArr * src1: unction mask CV_DEFAULT(v1: 0)): CvArr; (; var src2: CvArr;
// var dst: CvArr; var dst(mask) = src(mask) - value = src(mask) + (-value) * )CV_INLINE CV_INLINE
// procedure cvSubS(CvArr * src: unction mask CV_DEFAULT(v1: 0)): CvArr; (; value: CvScalar;
// var dst: CvArr; CV_DEFAULT(0))begin cvAddS(src: k; -value.val: array [0 .. -1] of CvScalar(;
// -value.val: array [0 .. 0] of v12; -value.val: array [0 .. 1] of v13;
// -value.val: array [0 .. 2] of v14); v15: dst; v16: mask); end;
//
// (* dst(mask) = value - src(mask) *)
// procedure cvSubRS(var dst(idx) = src1(idx) * src2(idx) * scale(scaled element -
// wise multiplication of 2 arrays) * )
// procedure cvMul(CvArr * src1: v1: 0)): CvArr; (; var src2: CvArr; dst:
// function; var element - wise division / inversion with scaling: dst(idx) = src1(idx) * scale /
// src2(idx) or dst(idx) = scale / src2(idx) if src1 = 0 * ) then
// procedure cvDiv(CvArr * src1:
// function scale CV_DEFAULT(v1: 1)): Double; (; var src2: CvArr; var dst: CvArr;
// var dst = src1 * scale + src2 * )
// procedure cvScaleAdd(CvArr * src1:
// function scale CV_DEFAULT(v1: 1)): Double; (; scale: CvScalar; var src2: vArr; var dst): CvArr;
/// / >> Following declaration is a macro definition!const cvAXPY( A: CvArr;
// v11: real_scalar; :;)cvScaleAdd(A: C; v14: cvRealScalar(real_scalar); :; : );
//
// (* dst = src1 * alpha + src2 * beta + gamma *)
// procedure cvAddWeighted(var src1: CvArr; alpha: Double; var src2: vArr; beta: Double; gamma: Double;
// var dst: CvArr);
//
// (* result = sum_i(src1(i) * src2(i)) (results for all channels are accumulated together) *)
// CVAPI(Double)cvDotProduct(CvArr * src1, CvArr * src2);
//
// (* dst(idx) = src1(idx) & src2(idx) *)
// procedure cvAnd(var dst(idx) = src(idx)& value * )
// procedure cvAndS(CvArr * src: v1: 0)): CvArr; (; value: CvScalar; dst: unction;
// var mask CV_DEFAULT(v1: 0)): CvArr; (* dst(idx) = src1(idx) | src2(idx) *)
// procedure cvOr(CvArr * src1: CvArr; var src2: CvArr; dst: unction;
// var mask CV_DEFAULT(v1: 0)): CvArr; (* dst(idx) = src(idx) | value *)
// procedure cvOrS(CvArr * src: CvArr; value: CvScalar; dst: unction;
// var mask CV_DEFAULT(v1: 0)): CvArr; (* dst(idx) = src1(idx) ^ src2(idx) *)
// procedure cvXor(CvArr * src1: CvArr; var src2: CvArr; dst: unction;
// var mask CV_DEFAULT(v1: 0)): CvArr; (* dst(idx) = src(idx) ^ value *)
// procedure cvXorS(CvArr * src: CvArr; value: CvScalar; dst: unction;
// var mask CV_DEFAULT(v1: 0)): CvArr; (* dst(idx) = ~src(idx) *)
// procedure cvNot(CvArr * src: CvArr; var dst: CvArr);
//
// (* dst(idx) = lower(idx) <= src(idx) < upper(idx) *)
// procedure cvInRange(var src: CvArr; var lower: CvArr; var upper: vArr; var dst: CvArr);
//
// (* dst(idx) = lower <= src(idx) < upper *)
// procedure cvInRangeS(var src: CvArr; lower: CvScalar; upper: CvScalar; var dst: CvArr);
//
// const CV_CMP_EQ = 0;
// {$EXTERNALSYM CV_CMP_EQ}
// const CV_CMP_GT = 1;
// {$EXTERNALSYM CV_CMP_GT}
// const CV_CMP_GE = 2;
// {$EXTERNALSYM CV_CMP_GE}
// const CV_CMP_LT = 3;
// {$EXTERNALSYM CV_CMP_LT}
// const CV_CMP_LE = 4;
// {$EXTERNALSYM CV_CMP_LE}
// const CV_CMP_NE = 5;
// {$EXTERNALSYM CV_CMP_NE}
// (* The comparison operation support single-channel arrays only.
// Destination image should be 8uC1 or 8sC1 *)
//
// (* dst(idx) = src1(idx) _cmp_op_ src2(idx) *)
// procedure cvCmp(var src1: CvArr; var src2: CvArr; var dst: CvArr; cmp_op: Integer);
//
// (* dst(idx) = src1(idx) _cmp_op_ value *)
// procedure cvCmpS(var src: CvArr; value: Double; var dst: CvArr; cmp_op: Integer);
//
// (* dst(idx) = min(src1(idx),src2(idx)) *)
// procedure cvMin(var src1: CvArr; var src2: CvArr; var dst: CvArr);
//
// (* dst(idx) = max(src1(idx),src2(idx)) *)
// procedure cvMax(var src1: CvArr; var src2: CvArr; var dst: CvArr);
//
// (* dst(idx) = min(src(idx),value) *)
// procedure cvMinS(var src: CvArr; value: Double; var dst: CvArr);
//
// (* dst(idx) = max(src(idx),value) *)
// procedure cvMaxS(var src: CvArr; value: Double; var dst: CvArr);
//
// (* dst(x,y,c) = abs(src1(x,y,c) - src2(x,y,c)) *)
// procedure cvAbsDiff(var src1: CvArr; var src2: CvArr; var dst: CvArr);
//
// (* dst(x,y,c) = abs(src(x,y,c) - value(c)) *)
// procedure cvAbsDiffS(var src: CvArr; var dst: CvArr; value: CvScalar);
/// / >> Following declaration is a macro definition!
// const cvAbs(src, dst)cvAbsDiffS((src), (dst), cvScalarAll(0));
//
// (* ***************************************************************************************\
// *                                Math operations                                         *
// *************************************************************************************** *)
//
// (* Does cartesian->polar coordinates conversion.
// Either of output cComponents (magnitude or angle) is optional *)
// procedure cvCartToPolar(var Does polar - > cartesian coordinates conversion.Either of output
// cComponents(magnitude or angle) is optional.If magnitude is missing it is assumed to be all 1
// 's */procedure  cvPolarToCart(  CvArr* magnitude: v1: 0)): Integer;(;var angle: CvArr;var x
// : CvArr; var y: CvArr; var Does powering: dst(idx) = src(idx)^power * )
// procedure cvPow(CvArr * src:
// function angle_in_degrees CV_DEFAULT(v1: 0)): Integer; (; var dst: CvArr;
// var Does exponention: dst(idx) = exp(src(idx)).Overflow is not handled yet.Underflow is handled.
// Maximal relative error is ~ 7E-6 for single - precision input * )
// procedure cvExp(CvArr * src: Double power): CvArr; (; var dst: CvArr);
//
// (* Calculates natural logarithms: dst(idx) = log(abs(src(idx))).
// Logarithm of 0 gives large negative number(~-700)
// Maximal relative error is ~3e-7 for single-precision output
// *)
// procedure cvLog(var src: CvArr; var dst: CvArr);
//
// (* Fast arctangent calculation *)
// CVAPI(single)cvFastArctan(single y, single x);
//
// (* Fast cubic root calculation *)
// CVAPI(single)cvCbrt(single value);
//
// (* Checks array values for NaNs, Infs or simply for too large numbers
// (if CV_CHECK_RANGE is set) then . If CV_CHECK_QUIET is set,
// no runtime errors is raised (cFunction returns zero value in  of 'bad' values).
// Otherwise cvError is called *)
// const CV_CHECK_RANGE = 1;
// {$EXTERNALSYM CV_CHECK_RANGE}
// const CV_CHECK_QUIET = 2;
// {$EXTERNALSYM CV_CHECK_QUIET}
// CVAPI(Integer)cvCheckArr(CvArr * arr, Integer flags CV_DEFAULT(0),
// function min_val CV_DEFAULT(v1: 0)): Double; const cvCheckArray = cvCheckArr;
// {$EXTERNALSYM cvCheckArray}
// const CV_RAND_UNI = 0;
// {$EXTERNALSYM CV_RAND_UNI}
// const CV_RAND_NORMAL = 1;
// {$EXTERNALSYM CV_RAND_NORMAL}
// procedure cvRandArr(var rng: CvRNG; var arr: CvArr; dist_type: Integer; param1: CvScalar;
// param2: CvScalar);
//
// procedure cvRandShuffle(var src: v1: 1. )): Double; const CV_SORT_EVERY_ROW = 0;
// {$EXTERNALSYM CV_SORT_EVERY_ROW}const CV_SORT_EVERY_COLUMN = 1;
// {$EXTERNALSYM CV_SORT_EVERY_COLUMN}const CV_SORT_ASCENDING = 0;
// {$EXTERNALSYM CV_SORT_ASCENDING}const CV_SORT_DESCENDING = 16; {$EXTERNALSYM CV_SORT_DESCENDING} procedure cvSort(CvArr; var dst CV_DEFAULT(0): CvArr; var Finds real roots of A cubic equation * )CVAPI(Integer)cvSolveCubic(CvMat * coeffs:
// function idxmat CV_DEFAULT(v1: 0)): Integer; (; var roots): CvArr;
// (* Finds all real and complex roots of a polynomial equation *)
// procedure cvSolvePoly(CvMat * coeffs: CvMat; var roots2: CvMat;
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \ * Matrix operations *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * )
// (* Calculates cross product of two 3d vectors *)
// procedure cvCrossProduct(CvArr * src1: array of
// function maxiter CV_DEFAULT(v1: 100)): Integer; (; var src2: CvArr; var dst: CvArr);
//
// (* Matrix transform: dst = A*B + C, C is optional *)
/// / >> Following declaration is a macro definition!
// const cvMatMulAdd(src1, src2, src3, dst)cvGEMM((src1), (src2), 1., (src3), 1., (dst), 0);
/// / >> Following declaration is a macro definition!
// const cvMatMul(src1, src2, dst)cvMatMulAdd((src1), (src2), 0, (dst));
//
// const CV_GEMM_A_T = 1;
// {$EXTERNALSYM CV_GEMM_A_T}
// const CV_GEMM_B_T = 2;
// {$EXTERNALSYM CV_GEMM_B_T}
// const CV_GEMM_C_T = 4;
// {$EXTERNALSYM CV_GEMM_C_T}
// (* Extended matrix transform:
// dst = alpha*op(A)*op(B) + beta*op(C), where op(X) is X or X xor T *)
// procedure cvGEMM(var Transforms each element of source array and stores resultant vectors
// in destination cArray * )
// procedure cvTransform(CvArr * src: v1: 0)): Integer; const cvMatMulAddEx = cvGEMM;
// {$EXTERNALSYM cvMatMulAddEx}(; var dst: CvArr; var transmat: vMat;
// var shiftvec CV_DEFAULT(0): vMat); const cvMatMulAddS = cvTransform;
// {$EXTERNALSYM cvMatMulAddS}
// (* Does perspective transform on every element of input array *)
// procedure cvPerspectiveTransform(var src: CvArr; var dst: CvArr; var mat: vMat);
//
// (* Calculates (A-delta)*(A-delta)^T (order=0) or (A-delta)^T*(A-delta) (order=1) *)
// procedure cvMulTransposed(var Tranposes Matrix.Square matrices can be transposed in - place * )
// procedure cvTranspose(CvArr * src: v1: 1. )): Double; (; var dst): CvArr; const cvT = cvTranspose;
// {$EXTERNALSYM cvT}(* Completes the symmetric matrix from the lower (LtoR=0) or from the upper (LtoR!=0) part *)
// procedure cvCompleteSymm(CvMat * Matrix: CvArr; LtoR CV_DEFAULT(0): Integer);
//
// (* Mirror array data around horizontal (flip=0),
// vertical (flip=1) or both(flip=-1) axises:
// cvFlip(src) flips images vertically and sequences horizontally (inplace) *)
// procedure cvFlip(v1: 0); var Performs Singular value Decomposition of A Matrix * )
// procedure cvSVD(CvArr * A:
// function flip_mode CV_DEFAULT(v1: 0)): Integer; const cvMirror = cvFlip;
// {$EXTERNALSYM cvMirror}const CV_SVD_MODIFY_A = 1;
// {$EXTERNALSYM CV_SVD_MODIFY_A}const CV_SVD_U_T = 2; {$EXTERNALSYM CV_SVD_U_T}const CV_SVD_V_T = 4;
// {$EXTERNALSYM CV_SVD_V_T}(; var W: CvArr; var U CV_DEFAULT(0): CvArr;
// var Performs Singular value Back Substitution(solves A * x = B): flags must be the same as
// in cvSVD * )
// procedure cvSVBkSb(CvArr * W:
// function V CV_DEFAULT(v1: 0)): CvArr; (; var U: CvArr; var = B: onst CvArr; var } CvArr * x:
// {$EXTERNALSYM CvArr;
// flags: Integer);
//
// const CV_LU = 0;
// {$EXTERNALSYM CV_LU}
// const CV_SVD = 1;
// {$EXTERNALSYM CV_SVD}
// const CV_SVD_SYM = 2;
// {$EXTERNALSYM CV_SVD_SYM}
// const CV_CHOLESKY = 3;
// {$EXTERNALSYM CV_CHOLESKY}
// const CV_QR = 4;
// {$EXTERNALSYM CV_QR}
// const CV_NORMAL = 16;
// {$EXTERNALSYM CV_NORMAL}
// (* Inverts matrix *)
// CVAPI(Double)cvInvert(CvArr * src, CvArr * dst,
// function method CV_DEFAULT(v1: CV_LU)): Integer; const cvInv = cvInvert;
// {$EXTERNALSYM cvInv}
// (* Solves linear system (src1)*(dst) = (src2)
// (returns 0 if src1 is a singular and CV_LU method is used) *) then CVAPI(Integer)cvSolve(CvArr *
// src1, CvArr * src2, CvArr * dst,
// function method CV_DEFAULT(v1: CV_LU)): Integer;
//
// (* Calculates determinant of input matrix *)
// CVAPI(Double)cvDet(CvArr * mat);
//
// (* Calculates trace of the matrix (sum of elements on the main diagonal) *)
// CVAPI(CvScalar)cvTrace(CvArr * mat);
//
// (* Finds eigen values and vectors of a symmetric matrix *)
// procedure cvEigenVV(-1: v1: ); highindex CV_DEFAULT(-1): Integer): Integer;
//
/// // * Finds selected eigen values and vectors of a symmetric matrix */
/// / CVAPI(void)  cvSelectedEigenVV( CvArr* mat, CvArr* evects, CvArr* evals,
/// / int lowindex, int highindex ): Double;
//
// (* Makes an identity matrix (mat_ij = i == j) *)
// procedure cvSetIdentity(v1: cvRealScalar(1)));
//
// (* Fills matrix with given range of numbers *)
// CVAPI(CvArr)cvRange(CvArr * mat, Double start, Double end);
//
// (* Calculates covariation matrix for a set of vectors *)
// (* transpose([v1-avg, v2-avg,...]) * [v1-avg,v2-avg,...] *)
// const CV_COVAR_SCRAMBLED = 0;
// {$EXTERNALSYM CV_COVAR_SCRAMBLED}
// (* [v1-avg, v2-avg,...] * transpose([v1-avg,v2-avg,...]) *)
// const CV_COVAR_NORMAL = 1;
// {$EXTERNALSYM CV_COVAR_NORMAL}
// (* do not calc average (i.e. mean vector) - use the input vector instead
// (useful for calculating covariance matrix by parts) *)
// const CV_COVAR_USE_AVG = 2;
// {$EXTERNALSYM CV_COVAR_USE_AVG}
// (* scale the covariance matrix coefficients by number of the vectors *)
// const CV_COVAR_SCALE = 4;
// {$EXTERNALSYM CV_COVAR_SCALE}
// (* all the input vectors are stored in a single matrix, as its rows *)
// const CV_COVAR_ROWS = 8;
// {$EXTERNALSYM CV_COVAR_ROWS}
// (* all the input vectors are stored in a single matrix, as its columns *)
// const CV_COVAR_COLS = 16;
// {$EXTERNALSYM CV_COVAR_COLS}
// procedure cvCalcCovarMatrix(vects: array of CvArr; count: Integer; var cov_mat: CvArr;
// var avg: CvArr; flags: Integer);
//
// const CV_PCA_DATA_AS_ROW = 0;
// {$EXTERNALSYM CV_PCA_DATA_AS_ROW}
// const CV_PCA_DATA_AS_COL = 1;
// {$EXTERNALSYM CV_PCA_DATA_AS_COL}
// const CV_PCA_USE_AVG = 2;
// {$EXTERNALSYM CV_PCA_USE_AVG}
// procedure cvCalcPCA(var data: CvArr; var mean: CvArr; var eigenvals: CvArr; var eigenvects: CvArr;
// flags: Integer);
//
// procedure cvProjectPCA(var data: CvArr; var mean: CvArr; var eigenvects: vArr; var cResult: CvArr);
//
// procedure cvBackProjectPCA(var proj: CvArr; var mean: CvArr; var eigenvects: vArr;
// var cResult: CvArr);
//
// (* Calculates Mahalanobis(weighted) distance *)
// CVAPI(Double)cvMahalanobis(CvArr * vec1, CvArr * vec2, CvArr * mat);
// const cvMahalonobis = cvMahalanobis;
// {$EXTERNALSYM cvMahalonobis}
// (* ***************************************************************************************\
// *                                    cArray Statistics                                    *
// *************************************************************************************** *)
//
// (* Finds sum of array elements *)
// CVAPI(CvScalar)cvSum(CvArr * arr);
//
// (* Calculates number of non-zero pixels *)
// CVAPI(Integer)cvCountNonZero(CvArr * arr);
//
// (* Calculates mean value of array elements *)
// CVAPI(CvScalar)cvAvg(CvArr * arr, CvArr * mask CV_DEFAULT(0));
//
// (* Calculates mean and standard deviation of pixel values *)
// procedure cvAvgSdv(var Finds global minimum: v1: 0)): CvArr; (; var)
// procedure cvMinMaxLoc(CvArr * arr: maximum and their positions; var min_val: Double;
// var max_val: Double; var min_loc CV_DEFAULT(0): CvPoint; var max_loc CV_DEFAULT(0): CvPoint;
// var types of array norm * )const CV_C = 1; {$EXTERNALSYM CV_C}const CV_L1 = 2;
// {$EXTERNALSYM CV_L1}const CV_L2 = 4; {$EXTERNALSYM CV_L2}const CV_NORM_MASK = 7;
// {$EXTERNALSYM CV_NORM_MASK}const CV_RELATIVE = 8; {$EXTERNALSYM CV_RELATIVE}const CV_DIFF = 16;
// {$EXTERNALSYM CV_DIFF}const CV_MINMAX = 32;
// {$EXTERNALSYM CV_MINMAX}const CV_DIFF_C = (CV_DIFF or CV_C: unction mask CV_DEFAULT(v1: 0))
// : CvArr; ();
// {$EXTERNALSYM CV_DIFF_C}
// const CV_DIFF_L1 = (CV_DIFF or CV_L1);
// {$EXTERNALSYM CV_DIFF_L1}
// const CV_DIFF_L2 = (CV_DIFF or CV_L2);
// {$EXTERNALSYM CV_DIFF_L2}
// const CV_RELATIVE_C = (CV_RELATIVE or CV_C);
// {$EXTERNALSYM CV_RELATIVE_C}
// const CV_RELATIVE_L1 = (CV_RELATIVE or CV_L1);
// {$EXTERNALSYM CV_RELATIVE_L1}
// const CV_RELATIVE_L2 = (CV_RELATIVE or CV_L2);
// {$EXTERNALSYM CV_RELATIVE_L2}
// (* Finds norm, difference norm or relative difference norm for an array (or two arrays) *)
// CVAPI(Double)cvNorm(CvArr * arr1, CvArr * arr2 CV_DEFAULT(0),
// function norm_type CV_DEFAULT(v1: 0)): Integer;
//
// procedure cvNormalize(0.: v1: ); norm_type CV_DEFAULT(CV_L2):
// function; var mask CV_DEFAULT(0): vArr): Integer;
//
// const CV_REDUCE_SUM = 0;
// {$EXTERNALSYM CV_REDUCE_SUM}
// const CV_REDUCE_AVG = 1;
// {$EXTERNALSYM CV_REDUCE_AVG}
// const CV_REDUCE_MAX = 2;
// {$EXTERNALSYM CV_REDUCE_MAX}
// const CV_REDUCE_MIN = 3;
// {$EXTERNALSYM CV_REDUCE_MIN}
// procedure cvReduce(v1: - 1); * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * \ * Discrete Linear Transforms and Related Functions * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * * * * * * * * * * * * * * * * * * )const CV_DXT_FORWARD = 0;
// {$EXTERNALSYM CV_DXT_FORWARD}const CV_DXT_INVERSE = 1;
// {$EXTERNALSYM CV_DXT_INVERSE}const CV_DXT_SCALE = 2;
// (* divide result by size of array *){$EXTERNALSYM CV_DXT_SCALE}const CV_DXT_INV_SCALE =
// (CV_DXT_INVERSE + CV_DXT_SCALE): Double;
// {$EXTERNALSYM CV_DXT_INV_SCALE}const CV_DXT_INVERSE_SCALE = CV_DXT_INV_SCALE;
// {$EXTERNALSYM CV_DXT_INVERSE_SCALE}const CV_DXT_ROWS = 4;
// (* transform each row individually *){$EXTERNALSYM CV_DXT_ROWS}const CV_DXT_MUL_CONJ = 8;
// (* conjugate the second argument of cvMulSpectrums *){$EXTERNALSYM CV_DXT_MUL_CONJ}(* Discrete Fourier Transform:    complex^.complex: array of function op CV_DEFAULT(v1: CV_REDUCE_SUM)): Integer;(;
// (forward): cReal^.ccs;
// var )procedure  cvDFT(  CvArr* src: ccs^.cReal (inverse);
// var dst: CvArr;
// flags: Integer;
// var Multiply results of DFTs: DFT(X)*DFT(Y) or DFT(X)*conj(DFT(Y)) *)
// procedure cvMulSpectrums(CvArr * src1:
// function nonzero_rows CV_DEFAULT(v1: 0)): Integer; const cvFFT = cvDFT;
// {$EXTERNALSYM cvFFT}(; var src2: CvArr; var dst: CvArr;
// var Finds optimal DFT vector size >= size0 * )CVAPI(Integer)cvGetOptimalDFTSize(Integer size0
// : Integer flags): CvArr; ();
//
// (* Discrete Cosine Transform *)
// procedure cvDCT(var src: CvArr; var dst: CvArr; flags: Integer);
//
// (* ***************************************************************************************\
// *                              Dynamic data structures                                   *
// *************************************************************************************** *)
//
// (* Calculates length of sequence slice (with support of negative indices). *)
// CVAPI(Integer)cvSliceLength(CvSlice slice, CvSeq * seq);
//
// (* Creates new memory storage.
// block_size = 0 means that default,
// somewhat optimal size, is used (currently, it is 64K) *)
// CVAPI(CvMemStorage)cvCreateMemStorage(Integer block_size CV_DEFAULT(0));
//
// (* Creates a memory storage that will borrow memory blocks from parent storage *)
// CVAPI(CvMemStorage)cvCreateChildMemStorage(CvMemStorage * parent);
//
// (* Releases memory storage. All the children of a parent must be released before
// the parent. A child storage returns all the blocks to parent when it is released *)
// procedure cvReleaseMemStorage(storage: array of CvMemStorage);
//
// (* Clears memory storage. This is the only way(!!!) (besides cvRestoreMemStoragePos)
// to reuse memory allocated for the storage - cvClearSeq,cvClearSet Args: array of const
// do not free any memory.
// A child storage returns all the blocks to the parent when it is cleared *)
// procedure cvClearMemStorage(var storage: CvMemStorage);
//
// (* Remember a storage "free memory" position *)
// procedure cvSaveMemStoragePos(var storage: CvMemStorage; var pos: CvMemStoragePos);
//
// (* Restore a storage "free memory" position *)
// procedure cvRestoreMemStoragePos(var storage: CvMemStorage; var pos: CvMemStoragePos);
//
// (* Allocates continuous buffer of the specified size in the storage *)
// procedure cvMemStorageAlloc(var storage: CvMemStorage; size: size_t);
//
// (* Allocates string in memory storage *)
// CVAPI(CvString)cvMemStorageAllocString(CvMemStorage * storage, PCVChar ptr,
// function len CV_DEFAULT(v1: - 1)): Integer;
//
// (* Creates new empty sequence that will reside in the specified storage *)
// CVAPI(CvSeq)cvCreateSeq(Integer seq_flags, size_t header_size, size_t elem_size,
// CvMemStorage * storage);
//
// (* Changes default size (granularity) of sequence blocks.
// The default size is ~1Kbyte *)
// procedure cvSetSeqBlockSize(var seq: CvSeq; delta_elems: Integer);
//
// (* Adds new element to the end of sequence. Returns pointer to the element *)
// CVAPI(schar)cvSeqPush(CvSeq * seq, Pointer element CV_DEFAULT(0));
//
// (* Adds new element to the beginning of sequence. Returns pointer to it *)
// CVAPI(schar)cvSeqPushFront(CvSeq * seq, Pointer element CV_DEFAULT(0));
//
// (* Removes the last element from sequence and optionally saves it *)
// procedure cvSeqPop(v1: 0));
//
// (* Removes the first element from sequence and optioanally saves it *)
// procedure cvSeqPopFront(v1: 0));
//
// const CV_FRONT = 1;
// {$EXTERNALSYM CV_FRONT}
// const CV_BACK = 0;
// {$EXTERNALSYM CV_BACK}
// (* Adds several new elements to the end of sequence *)
// procedure cvSeqPushMulti(var Removes several elements from the end of sequence and
// optionally saves them * )
// procedure cvSeqPopMulti(CvSeq * seq: v1: 0)): Integer; (; var elements: void; count:
// function; var Inserts A new element in the middle of sequence.cvSeqInsert(seq
// : Integer in_front CV_DEFAULT(v1: 0)): Integer; (; :; = cvSeqPushFront(seq: lem);
// var)CVAPI(schar)cvSeqInsert(CvSeq * seq: lem); before_index: Integer;
// var Removes specified sequence element * )
// procedure cvSeqRemove(CvSeq * seq: unction element CV_DEFAULT(v1: 0)): Pointer; (; index: Integer);
//
// (* Removes all the elements from the sequence. The freed memory
// can be reused later only by the same sequence unless cvClearMemStorage
// or cvRestoreMemStoragePos is called *)
// procedure cvClearSeq(var seq: CvSeq);
//
// (* Retrieves pointer to specified sequence element.
// Negative indices are supported and mean counting from the end
// (e.g -1 means the last sequence element) *)
// CVAPI(schar)cvGetSeqElem(CvSeq * seq, Integer index);
//
// (* Calculates index of the specified sequence element.
// Returns -1 if element does not belong to the sequence *)
// CVAPI(Integer)cvSeqElemIdx(CvSeq * seq, Pointer element, CvSeqBlock * * block CV_DEFAULT(0));
//
// (* Initializes sequence writer. The new elements will be added to the end of sequence *)
// procedure cvStartAppendToSeq(var seq: CvSeq; var writer: CvSeqWriter);
//
// (* Combination of cvCreateSeq and cvStartAppendToSeq *)
// procedure cvStartWriteSeq(seq_flags: Integer; header_size: Integer; elem_size: Integer;
// var storage: CvMemStorage; var writer: CvSeqWriter);
//
// (* Closes sequence writer, updates sequence header and returns pointer
// to the resultant sequence
// (which may be useful if the sequence was created using cvStartWriteSeq)) then
// *)
// CVAPI(CvSeq)cvEndWriteSeq(CvSeqWriter * writer);
//
// (* Updates sequence header. May be useful to get access to some of previously
// written elements via cvGetSeqElem or sequence reader *)
// procedure cvFlushSeqWriter(var writer: CvSeqWriter);
//
// (* Initializes sequence reader.
// The sequence can be read in forward or backward direction *)
// procedure cvStartReadSeq(var Returns current sequence reader position(currently observed sequence
// element) * )CVAPI(Integer)cvGetSeqReaderPos(CvSeqReader * reader: v1: 0)): Integer; ();
//
// (* Changes sequence reader position. It may seek to an absolute or
// to relative to the current position *)
// procedure cvSetSeqReaderPos(var Copies sequence content to A continuous piece of memory * )
// procedure cvCvtSeqToArray(CvSeq * seq: v1: 0)): Integer; (; var elements: void;
// slice CV_DEFAULT(CV_WHOLE_SEQ): CvSlice);
//
// (* Creates sequence header for array.
// After that all the operations on sequences that do not alter the content
// can be applied to the resultant sequence *)
// CVAPI(CvSeq)cvMakeSeqHeaderForArray(Integer seq_type, Integer header_size, Integer elem_size,
// Pointer elements, Integer total, CvSeq * seq, CvSeqBlock * block);
//
// (* Extracts sequence slice (with or without copying sequence elements) *)
// CVAPI(CvSeq)cvSeqSlice(CvSeq * seq, CvSlice slice, CvMemStorage * storage CV_DEFAULT(0),
// function copy_function CV_DEFAULT(v1: 0)): Integer;
//
// CV_INLINE CvSeq * cvCloneSeq(CvSeq * seq, CvMemStorage * storage CV_DEFAULT(0))begin result :=
// cvSeqSlice(seq, CV_WHOLE_SEQ, storage, 1): data; end;
//
// (* Removes sequence slice *)
// procedure cvSeqRemoveSlice(var seq: CvSeq; slice: CvSlice);
//
// (* Inserts a sequence or array into another sequence *)
// procedure cvSeqInsertSlice(var seq: CvSeq; before_index: Integer; var from_arr: CvArr);
//
// (* a < b ? -1 : a > b ? 1 : 0 *)
// type CvCmpFunc =
// function(A: Pointer; B: Pointer; userdata: Pointer): Integer; CV_CDECL *;
//
// (* Sorts sequence in-place given element comparison function *)
// procedure cvSeqSort(v1: 0));
//
// (* Finds element in a [sorted] sequence *)
// CVAPI(schar)cvSeqSearch(CvSeq * seq, Pointer elem, CvCmpFunc func, Integer is_sorted,
// Integer * elem_idx,
// function userdata CV_DEFAULT(v1: 0)): Pointer;
//
// (* Reverses order of sequence elements in-place *)
// procedure cvSeqInvert(var seq: CvSeq);
//
// (* Splits sequence into one or more equivalence classes using the specified criteria *)
// CVAPI(Integer)cvSeqPartition(CvSeq * seq, CvMemStorage * storage, CvSeq * * labels,
// CvCmpFunc is_equal, Pointer userdata);
//
// (* *********** Internal sequence functions *********** *)
// procedure cvChangeSeqBlock(reader: Pointer; direction: Integer);
// procedure cvCreateSeqBlock(var writer: CvSeqWriter);
//
// (* Creates a new set *)
// CVAPI(cvSet)cvCreateSet(Integer set_flags, Integer header_size, Integer elem_size,
// CvMemStorage * storage);
//
// (* Adds new element to the set and returns pointer to it *)
// CVAPI(Integer)cvSetAdd(cvSet * set_header, CvSetElem * elem CV_DEFAULT(0),
// CvSetElem * * inserted_elem CV_DEFAULT(0));
//
// (* Fast variant of cvSetAdd *)
// CV_INLINE CvSetElem * cvSetNew(cvSet * set_header)begin CvSetElem * elem := set_header^.free_elems;
// if (elem) then begin set_header^.free_elems := elem^.next_free;
// elem^.flags := elem^.flags and CV_SET_ELEM_IDX_MASK; set_header^.active_count := mod +1; end;
// else cvSetAdd(set_header, 0, (CvSetElem * )&elem); result := elem; end;
//
// (* Removes set element given its pointer *)
// CV_INLINE CV_INLINE
// procedure cvSetRemoveByPtr(var & &(elem - > flags & CV_SET_ELEM_IDX_MASK) < set_header - > total * )
// : CvSetElem)elem; Assert(_elem^.flags > := 0(); _elem^.next_free := set_header^.free_elems;
// _elem^.flags := (_elem^.flags and CV_SET_ELEM_IDX_MASK) or CV_SET_ELEM_FREE_FLAG;
// set_header^.free_elems := _elem; set_header^.active_count := mod -1; end;
//
// (* Removes element from the set by its index *)
// procedure cvSetRemove(var set_header: cvSet; index: Integer);
//
// (* Returns a set element by index. If the element doesn't belong to the set,
// 0 is returned *)
// CV_INLINE CvSetElem * cvGetSetElem(cvSet * set_header, Integer idx)begin CvSetElem * elem :=
// (CvSetElem)cvGetSeqElem((CvSeq)set_header, idx);
// result := elem and CV_IS_SET_ELEM(elem)? elem: 0; end;
//
// (* Removes all the elements from the set *)
// procedure cvClearSet(var set_header: cvSet);
//
// (* Creates new graph *)
// CVAPI(CvGraph)cvCreateGraph(Integer graph_flags, Integer header_size, Integer vtx_size,
// Integer edge_size, CvMemStorage * storage);
//
// (* Adds new vertex to the graph *)
// CVAPI(Integer)cvGraphAddVtx(CvGraph * graph, CvGraphVtx * vtx CV_DEFAULT(0),
// CvGraphVtx * * inserted_vtx CV_DEFAULT(0));
//
// (* Removes vertex from the graph together with all incident edges *)
// CVAPI(Integer)cvGraphRemoveVtx(CvGraph * graph, Integer index);
// CVAPI(Integer)cvGraphRemoveVtxByPtr(CvGraph * graph, CvGraphVtx * vtx);
//
// (* Link two vertices specifed by indices or pointers if they
// are not connected or result:= cPointer to already existing edge
// connecting the vertices.
// Functions result:= 1 if a new edge was created, 0 otherwise *)
// CVAPI(Integer)cvGraphAddEdge(CvGraph * graph, Integer start_idx, Integer end_idx,
// CvGraphEdge * edge CV_DEFAULT(0), CvGraphEdge * * inserted_edge CV_DEFAULT(0));
//
// CVAPI(Integer)cvGraphAddEdgeByPtr(CvGraph * graph, CvGraphVtx * start_vtx, CvGraphVtx * end_vtx,
// CvGraphEdge * edge CV_DEFAULT(0), CvGraphEdge * * inserted_edge CV_DEFAULT(0));
//
// (* Remove edge connecting two vertices *)
// procedure cvGraphRemoveEdge(var graph: CvGraph; start_idx: Integer; end_idx: Integer);
// procedure cvGraphRemoveEdgeByPtr(var graph: CvGraph; var start_vtx: CvGraphVtx;
// var end_vtx: CvGraphVtx);
//
// (* Find edge connecting two vertices *)
// CVAPI(CvGraphEdge)cvFindGraphEdge(CvGraph * graph, Integer start_idx, Integer end_idx);
// CVAPI(CvGraphEdge)cvFindGraphEdgeByPtr(CvGraph * graph, CvGraphVtx * start_vtx,
// CvGraphVtx * end_vtx); const cvGraphFindEdge = cvFindGraphEdge;
// {$EXTERNALSYM cvGraphFindEdge}
// const cvGraphFindEdgeByPtr = cvFindGraphEdgeByPtr;
// {$EXTERNALSYM cvGraphFindEdgeByPtr}
// (* Remove all vertices and edges from the graph *)
// procedure cvClearGraph(var graph: CvGraph);
//
// (* Count number of edges incident to the vertex *)
// CVAPI(Integer)cvGraphVtxDegree(CvGraph * graph, Integer vtx_idx);
// CVAPI(Integer)cvGraphVtxDegreeByPtr(CvGraph * graph, CvGraphVtx * vtx);
//
// (* Retrieves graph vertex by given index *)
/// / >> Following declaration is a macro definition!
// const cvGetGraphVtx(graph, idx(CvGraphVtx)cvGetSetElem((cvSet(graph), (idx));
//
// (* Retrieves index of a graph vertex given its pointer *)
// // >> Following declaration is a macro definition!
// const cvGraphVtxIdx(graph, vtx((vtx)^.flags and CV_SET_ELEM_IDX_MASK);
//
// (* Retrieves index of a graph edge given its pointer *)
// // >> Following declaration is a macro definition!
// const cvGraphEdgeIdx(graph, edge((edge)^.flags and CV_SET_ELEM_IDX_MASK);
//
// // >> Following declaration is a macro definition!
// const cvGraphGetVtxCount(graph((graph)^.active_count);
// // >> Following declaration is a macro definition!
// const cvGraphGetEdgeCount(graph((graph)^.edges^.active_count);
//
// const CV_GRAPH_VERTEX = 1;
// {$EXTERNALSYM CV_GRAPH_VERTEX}
// const CV_GRAPH_TREE_EDGE = 2;
// {$EXTERNALSYM CV_GRAPH_TREE_EDGE}
// const CV_GRAPH_BACK_EDGE = 4;
// {$EXTERNALSYM CV_GRAPH_BACK_EDGE}
// const CV_GRAPH_FORWARD_EDGE = 8;
// {$EXTERNALSYM CV_GRAPH_FORWARD_EDGE}
// const CV_GRAPH_CROSS_EDGE = 16;
// {$EXTERNALSYM CV_GRAPH_CROSS_EDGE}
// const CV_GRAPH_ANY_EDGE = 30;
// {$EXTERNALSYM CV_GRAPH_ANY_EDGE}
// const CV_GRAPH_NEW_TREE = 32;
// {$EXTERNALSYM CV_GRAPH_NEW_TREE}
// const CV_GRAPH_BACKTRACKING = 64;
// {$EXTERNALSYM CV_GRAPH_BACKTRACKING}
// const CV_GRAPH_OVER = -1;
// {$EXTERNALSYM CV_GRAPH_OVER}
// const CV_GRAPH_ALL_ITEMS = -1;
// {$EXTERNALSYM CV_GRAPH_ALL_ITEMS}
// (* flags for graph vertices and edges *)
// const CV_GRAPH_ITEM_VISITED_FLAG = (1 shl 30);
// {$EXTERNALSYM CV_GRAPH_ITEM_VISITED_FLAG}
// // >> Following declaration is a macro definition!
// const CV_IS_GRAPH_VERTEX_VISITED(vtx)(((CvGraphVtx(vtx))^.flags and CV_GRAPH_ITEM_VISITED_FLAG);
// // >> Following declaration is a macro definition!
// const CV_IS_GRAPH_EDGE_VISITED(edge)(((CvGraphEdge(edge))^.flags and CV_GRAPH_ITEM_VISITED_FLAG);
// const CV_GRAPH_SEARCH_TREE_NODE_FLAG = (1 shl 29);
// {$EXTERNALSYM CV_GRAPH_SEARCH_TREE_NODE_FLAG}
// const CV_GRAPH_FORWARD_EDGE_FLAG = (1 shl 28);
// {$EXTERNALSYM CV_GRAPH_FORWARD_EDGE_FLAG}
// type
//
// = record vtx: ^CvGraphVtx; (* current graph vertex (or current edge origin) *)
// dst: ^CvGraphVtx; (* current graph edge destination vertex *)
// edge: ^CvGraphEdge; (* current edge *)
// graph: ^CvGraph; (* the graph *)
// stack: ^CvSeq; (* the graph vertex stack *)
// index: Integer; (* the lower bound of certainly visited vertices *)
// mask: Integer; (* event mask *)
// end; CvGraphScanner;
//
// (* Creates new graph scanner. *)
// CVAPI(CvGraphScanner)cvCreateGraphScanner(CvGraph * graph, CvGraphVtx * vtx CV_DEFAULT(0),
// function mask CV_DEFAULT(v1: CV_GRAPH_ALL_ITEMS)): Integer;
//
// (* Releases graph scanner. *)
// procedure cvReleaseGraphScanner(scanner: array of CvGraphScanner);
//
// (* Get next graph element *)
// CVAPI(Integer)cvNextGraphItem(CvGraphScanner * scanner);
//
// (* Creates a copy of graph *)
// CVAPI(CvGraph)cvCloneGraph(CvGraph * graph, CvMemStorage * storage);
//
// (* ***************************************************************************************\
// *                                     Drawing                                            *
// *************************************************************************************** *)
//
// (* ***************************************************************************************\
// *       Drawing functions work with images/matrices of arbitrary cType.                   *
// *       For color images the channel order is BGR : array[0..A-1] of                                      *
// *       Antialiasing is supported only for 8-bit image now.                              *
// *       All the functions include parameter color that means rgb value (that may be      *
// *       constructed with CV_RGB macro) for color images and brightness                   *
// *       for grayscale images.                                                            *
// *       If a drawn figure is partially or completely outside of the image, it is clipped.*
// *************************************************************************************** *)
//
/// / >> Following declaration is a macro definition!
// const CV_RGB(r, g, B)CvScalar((B), (g), (r), 0); const CV_FILLED = -1;
// {$EXTERNALSYM CV_FILLED}
// const CV_AA = 16;
// {$EXTERNALSYM CV_AA}
// (* Draws 4-connected, 8-connected or antialiased line segment connecting two points *)
// procedure cvLine(8: v1: ); shift CV_DEFAULT(0): Integer): Integer;
//
// (* Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2),
// if thickness<0 (e.g. thickness = CV_FILLED), the filled box is drawn *) then
// procedure cvRectangle(8: v1: ); shift CV_DEFAULT(0): Integer): Integer;
//
// (* Draws a rectangle specified by a CvRect structure *)
// procedure cvRectangleR(8: v1: ); shift CV_DEFAULT(0): Integer): Integer;
//
// (* Draws a circle with specified center and radius.
// Thickness works in the same way as with cvRectangle *)
// procedure cvCircle(8: v1: ); shift CV_DEFAULT(0): Integer): Integer;
//
// (* Draws ellipse outline, filled ellipse, elliptic arc or filled elliptic sector,
// depending on <thickness>, <start_angle> and <end_angle> parameters. The resultant figure
// is rotated by <angle>. All the angles are in degrees *)
// procedure cvEllipse(8: v1: ); shift CV_DEFAULT(0): Integer): Integer;
//
// CV_INLINE CV_INLINE
// procedure cvEllipseBox(
//
// v1: 1);
//
// var 0.5:
// function line_type CV_DEFAULT(var 0.5: 0))begin CvSize axes;
// axes.width := cvRound(box.size.width): Integer; axes.height := cvRound(box.size.height);
//
// cvEllipse(img, cvPointFrom32f(box.center), axes, box.angle, 0, 360, color, thickness, line_type,
// shift); end;
//
// (* Fills convex or monotonous polygon. *)
// procedure cvFillConvexPoly(var Fills an area bounded by one or more arbitrary polygons * )
// procedure cvFillPoly(CvArr * img: v1: 0)): Integer; (; pts: array of CvPoint; var npts: Integer;
// contours: Integer; color: CvScalar; var Draws one or more polygonal curves * )
// procedure cvPolyLine(CvArr * img:
// function line_type CV_DEFAULT(v1: 0)): Integer; (; pts: array of CvPoint; var npts: Integer;
// contours: Integer; is_closed:
// function; color: CvScalar; thickness CV_DEFAULT(v1: 8: Integer);
// shift CV_DEFAULT(0): Integer): Integer;
//
// const cvDrawRect = cvRectangle;
// {$EXTERNALSYM cvDrawRect}
// const cvDrawLine = cvLine;
// {$EXTERNALSYM cvDrawLine}
// const cvDrawCircle = cvCircle;
// {$EXTERNALSYM cvDrawCircle}
// const cvDrawEllipse = cvEllipse;
// {$EXTERNALSYM cvDrawEllipse}
// const cvDrawPolyLine = cvPolyLine;
// {$EXTERNALSYM cvDrawPolyLine}
// (* Clips the line segment connecting *pt1 and *pt2
// by the rectangular window
// (0<=x<img_size.width, 0<=y<img_size.height). *)
// CVAPI(Integer)cvClipLine(CvSize img_size, CvPoint * pt1, CvPoint * pt2);
//
// (* Initializes line iterator. Initially, line_iterator->ptr will point
// to pt1 (or pt2, see left_to_right description) location in the image.
// Returns the number of pixels on the line between the ending points. *)
// CVAPI(Integer)cvInitLineIterator(CvArr * image, CvPoint pt1, CvPoint pt2,
// CvLineIterator * line_iterator,
// function connectivity CV_DEFAULT(v1: 0)): Integer;
//
// (* Moves iterator to the next line point *)
/// / >> Following declaration is a macro definition!
// const CV_NEXT_LINE_POINT(line_iterator); begin Integer _line_iterator_mask = (line_iterator).err <
// 0 ? - 1: 0; (line_iterator).err := mod +(line_iterator).minus_delta + ((line_iterator)
// .plus_delta and _line_iterator_mask); (line_iterator).ptr := mod +(line_iterator).minus_step +
// ((line_iterator).plus_step and _line_iterator_mask); end;
//
// (* basic font types *)
// const CV_FONT_HERSHEY_SIMPLEX = 0;
// {$EXTERNALSYM CV_FONT_HERSHEY_SIMPLEX}
// const CV_FONT_HERSHEY_PLAIN = 1;
// {$EXTERNALSYM CV_FONT_HERSHEY_PLAIN}
// const CV_FONT_HERSHEY_DUPLEX = 2;
// {$EXTERNALSYM CV_FONT_HERSHEY_DUPLEX}
// const CV_FONT_HERSHEY_COMPLEX = 3;
// {$EXTERNALSYM CV_FONT_HERSHEY_COMPLEX}
// const CV_FONT_HERSHEY_TRIPLEX = 4;
// {$EXTERNALSYM CV_FONT_HERSHEY_TRIPLEX}
// const CV_FONT_HERSHEY_COMPLEX_SMALL = 5;
// {$EXTERNALSYM CV_FONT_HERSHEY_COMPLEX_SMALL}
// const CV_FONT_HERSHEY_SCRIPT_SIMPLEX = 6;
// {$EXTERNALSYM CV_FONT_HERSHEY_SCRIPT_SIMPLEX}
// const CV_FONT_HERSHEY_SCRIPT_COMPLEX = 7;
// {$EXTERNALSYM CV_FONT_HERSHEY_SCRIPT_COMPLEX}
// (* font flags *)
// const CV_FONT_ITALIC = 16;
// {$EXTERNALSYM CV_FONT_ITALIC}
// const CV_FONT_VECTOR0 = CV_FONT_HERSHEY_SIMPLEX;
// {$EXTERNALSYM CV_FONT_VECTOR0}
// (* Font structure *)
// type
//
// = record nameFont: PCVChar; // Qt:nameFont
// color: CvScalar;
/// / Qt:ColorFont -> cvScalar(blue_component, green_component, red\_component[, alpha_component])
// font_face: Integer; // Qt: bool italic         /* =CV_FONT_* */
// ascii: ^Integer; (* font data and metrics *)
// greek: ^Integer; cyrillic: ^Integer; hscale, vscale: single; shear: single;
// (* slope coefficient: 0 - normal, >0 - italic *)
// thickness: Integer; // Qt: weight               /* letters thickness */
// dx: single; (* horizontal interval between letters *)
// line_type: Integer; // Qt: PointSize
// end; CvFont;
//
// (* Initializes font structure used further in cvPutText *)
// procedure cvInitFont(1: v1: ); line_type CV_DEFAULT(8): Integer): Integer;
//
// CV_INLINE CvFont CvFont(Double scale, Integer thickness CV_DEFAULT(1))begin CvFont font;
// cvInitFont(and font, CV_FONT_HERSHEY_PLAIN, scale, scale, 0, thickness, CV_AA): Double;
// result := font; end;
//
// (* Renders text stroke with specified font and color at specified location.
// CvFont should be initialized with cvInitFont *)
// procedure cvPutText(var img: CvArr; text: PCVChar; org: CvPoint; var font: vFont; color: CvScalar);
//
// (* Calculates bounding box of text stroke (useful for alignment) *)
// procedure cvGetTextSize(text_string: PCVChar; var font: CvFont; var text_size: CvSize;
// var baseline: Integer);
//
// (* Unpacks color value, if arrtype is CV_8UC?, <color> is treated as
// packed color value, otherwise the first channels (depending on arrtype)
// of destination scalar are set to the same value = <color> *)
// CVAPI(CvScalar)cvColorToScalar(Double packed_color, Integer arrtype);
//
// (* Returns the polygon points which make up the given ellipse.  The ellipse is define by
// the box of size 'axes' rotated 'angle' around the 'center'.  A partial sweep
// of the ellipse arc can be done by spcifying arc_start and arc_end to be something
// other than 0 and 360, respectively.  The input cArray 'pts' must be large enough to
// hold the cResult.  The total number of points stored into 'pts' is returned by this
// cFunction. *)
// CVAPI(Integer)cvEllipse2Poly(CvPoint center, CvSize axes, Integer angle, Integer arc_start,
// Integer arc_end, CvPoint * pts, Integer delta);
//
// (* Draws contour outlines or filled interiors on the image *)
// procedure cvDrawContours(8: v1: ); offset CV_DEFAULT(CvPoint(0: CvPoint; v3: ))): Integer;
//
// (* Does look-up transformation. Elements of the source array
// (that should be 8uC1 or 8sC1) are used as indexes in lutarr 256-element table *)
// procedure cvLUT(var src: CvArr; var dst: CvArr; var lut: CvArr);
//
// (* ****************** Iteration through the sequence tree **************** *)
// type
//
// = record node: Pointer; level: Integer; max_level: Integer; end; CvTreeNodeIterator;
//
// procedure cvInitTreeNodeIterator(var tree_iterator: CvTreeNodeIterator; first: ointer;
// max_level: Integer);
// procedure cvNextTreeNode(var tree_iterator: CvTreeNodeIterator);
// procedure cvPrevTreeNode(var tree_iterator: CvTreeNodeIterator);
//
// (* Inserts sequence into tree with specified "parent" sequence.
// If parent is equal to frame (e.g. the most external contour),
// then added contour will have null cPointer to parent. *)
// procedure cvInsertNodeIntoTree(node: Pointer; parent: Pointer; frame: Pointer);
//
// (* Removes contour from tree (together with the contour children). *)
// procedure cvRemoveNodeFromTree(node: Pointer; frame: Pointer);
//
// (* Gathers pointers to all the sequences,
// accessible from the <first>, to the single sequence *)
// CVAPI(CvSeq)cvTreeToNodeSeq(Pointer first, Integer header_size, CvMemStorage * storage);
//
// (* The function implements the K-means algorithm for clustering an array of sample
// vectors in a specified number of classes *)
// const CV_KMEANS_USE_INITIAL_LABELS = 1;
// {$EXTERNALSYM CV_KMEANS_USE_INITIAL_LABELS}
// CVAPI(Integer)cvKMeans2(CvArr * samples, Integer cluster_count, CvArr * labels,
// CvTermCriteria termcrit,
// function attempts CV_DEFAULT(v1: 0); flags CV_DEFAULT(0): Integer; _centers CV_DEFAULT(0):
// function; var compactness CV_DEFAULT(0): Double): Integer;
//
// (* ***************************************************************************************\
// *                                    System functions                                    *
// *************************************************************************************** *)
//
// (* Add the function pointers table with associated information to the IPP primitives list *)
// CVAPI(Integer)cvRegisterModule(CvModuleInfo * module_info): CvArr;
//
// (* Loads optimized functions from IPP, MKL etc. or switches back to pure C code *)
// CVAPI(Integer)cvUseOptimized(Integer on_off): CvRNG;
//
// (* Retrieves information about the registered modules and loaded optimized plugins *)
// procedure cvGetModuleInfo(module_name: PCVChar; var version: Char; var loaded_addon_plugins: Char);
//
// type * CvAllocFunc =
// function(size: size_t; userdata: Pointer): Pointer; CV_CDECL; type * CvFreeFunc =
// function(var CvFreetype
// procedure * pptr: CV_CDECL; userdata: Pointer): Integer; CV_CDECL; =
// procedure (* Set user-defined memory managment functions (substitutors for malloc and free) that
// will be called by cvAlloc, cvFree and higher-level functions (e.g. cvCreateImage) *)
// procedure cvSetMemoryManager(v1: 0); free_func CV_DEFAULT(0): CvFreeFunc;
// var (CV_STDCALL * Cv_iplCreateImageHeader)(Integer:
// function userdata CV_DEFAULT(v1: 0)): Pointer; type IplImage; v4: nteger; v5: nteger; teger: Char;
// v7: nteger; v8: nteger; * : IplROI; * : plImage; plTileInfo: ointer);
// type CV_STDCALL * Cv_iplAllocateImageData =
// procedure (* : IplImage; v2: nteger; v3: nteger);
// type CV_STDCALL* Cv_iplDeallocate = procedure(*: IplImage; v2: nteger);
// type IplROI* (CV_STDCALL* Cv_iplCreateROI(Integer,Integer,Integer,Integer,Integer);
// type IplImage* (CV_STDCALL* Cv_iplCloneImage( IplImage);
//
// (* Makes OpenCV use IPL functions for IplImage allocation/deallocation *)
// procedure cvSetIPLAllocators(create_header: Cv_iplCreateImageHeader;
// allocate_data: Cv_iplAllocateImageData; deallocate: Cv_iplDeallocate; create_roi: Cv_iplCreateROI;
// clone_image: Cv_iplCloneImage);
//
/// / >> Following declaration is a macro definition!
// const CV_TURN_ON_IPL_COMPATIBILITY()cvSetIPLAllocators(iplCreateImageHeader, iplAllocateImage,
// iplDeallocate, iplCreateROI, iplCloneImage);
//
// (* ***************************************************************************************\
// *                                    Data Persistence                                    *
// *************************************************************************************** *)
//
// (* ********************************* High-level functions ******************************* *)
//
// (* opens existing or creates new file storage *)
// CVAPI(CvFileStorage)cvOpenFileStorage(PCVChar filename, CvMemStorage * memstorage,
// function flags,
// function encoding CV_DEFAULT(v1: 0)): Integer;
//
// (* closes file storage and deallocates buffers *)
// procedure cvReleaseFileStorage(v1: var Returns attribute value or
// 0(NULL) if there is no such attribute * )CVAPI(Char)cvAttrValue(CvAttrList * attr;
// attr_name: PCVChar);
//
// (* starts writing compound structure (map or sequence) *)
// procedure cvStartWriteStruct(var finishes writing compound structure * )
// procedure cvEndWriteStruct(CvFileStorage * fs): PCVChar; (* writes an integer *)
// procedure cvWriteInt(CvFileStorage * fs: v1: CvAttrList())): Integer; (; name: PCVChar;
// value: Integer);
//
// (* writes a floating-point number *)
// procedure cvWriteReal(var fs: CvFileStorage; name: PCVChar; value: Double);
//
// (* writes a string *)
// procedure cvWriteString(var writes A comment * )
// procedure cvWriteComment(CvFileStorage * fs: v1: 0)): Integer; (; comment: PCVChar;
// var writes instance of A standard type (Matrix: Integer eol_comment): PCVChar; (; v4: image;
// v5: sequence; var)
// procedure cvWrite(CvFileStorage * fs: graph etc.) or user - defined cType; name: Pointer;
// var ptr: void; attributes CV_DEFAULT(CvAttrList()): CvAttrList);
//
// (* starts the next stream *)
// procedure cvStartNextStream(var fs: CvFileStorage);
//
// (* helper function: writes multiple integer or floating-point numbers *)
// procedure cvWriteRawData(var fs: CvFileStorage; var src: void; len: nteger; dt: PCVChar);
//
// (* returns the hash entry corresponding to the specified literal key string or 0
// if there is no such a key in the storage *)
// CVAPI(CvStringHashNode)cvGetHashedKey(CvFileStorage * fs, PCVChar name,
// function len CV_DEFAULT(v1: 0)): Integer;
//
// (* returns file node with the specified key within the specified map
// (collection of named nodes) *)
// CVAPI(CvFileNode)cvGetRootFileNode(CvFileStorage * fs,
// function stream_index CV_DEFAULT(v1: 0)): Integer;
//
// (* returns file node with the specified key within the specified map
// (collection of named nodes) *)
// CVAPI(CvFileNode)cvGetFileNode(CvFileStorage * fs, CvFileNode * map, CvStringHashNode * key,
// function create_missing CV_DEFAULT(v1: 0)): Integer;
//
// (* this is a slower version of cvGetFileNode that takes the key as a literal string *)
// CVAPI(CvFileNode)cvGetFileNodeByName(CvFileStorage * fs, CvFileNode * map, PCVChar name);
//
// CV_INLINE
// function cvReadInt(var fs: 0))begin result := not node ? default_value: CV_NODE_IS_INT(node^.tag)
// ? node^.data.i: CV_NODE_IS_REAL(node^.tag)? cvRound(node^.data.f): $7FFFFFFF; end;
// CV_INLINE Integer cvReadIntByName(CvFileStorage; var map: CvFileNode; name: unction;
// default_value CV_DEFAULT(0))begin result := cvReadInt(cvGetFileNodeByName(fs: Integer; v5: map;
// ): name; v7: default_value): Integer; end;
//
// CV_INLINE
// function cvReadReal(var fs: 0. ))begin result := not node ? default_value: CV_NODE_IS_INT(node^.tag)
// ?(Double)node^.data.i: CV_NODE_IS_REAL(node^.tag)? node^.data.f: 1E300; end;
// CV_INLINE Double cvReadRealByName(CvFileStorage; var map: CvFileNode; name: Char;
// default_value CV_DEFAULT(0. ))begin result := cvReadReal(cvGetFileNodeByName(fs: Double; v5: map;
// ): name; v7: default_value): Double; end;
//
// CV_INLINE PCVChar cvReadString(CvFileNode * node, PCVChar default_value CV_DEFAULT(0))begin result :=
// not node ? default_value: CV_NODE_IS_STRING(node^.tag)? node^.data.str.ptr: 0; end;
//
// CV_INLINE PCVChar cvReadStringByName(CvFileStorage * fs, CvFileNode * map,
// const PCVChar = default_value CV_DEFAULT(0(): PCVChar;
// {$EXTERNALSYM PCVChar}
// begin result := cvReadString(cvGetFileNodeByName(fs, map, name), default_value); end;
//
// (* decodes standard or user-defined object and returns it *)
// procedure cvRead(v1: 0));
//
// (* decodes standard or user-defined object and returns it *)
// CV_INLINE
// function cvReadByName()begin result := cvRead(fs: 0); fs: cvGetFileNodeByName(; v3: map;): name;
// v5: attributes): PCVChar; end;
//
// (* starts reading data from sequence or scalar numeric node *)
// procedure cvStartReadRawData(v1: var reads multiple numbers and stores them to array * )
// procedure cvReadRawDataSlice(CvFileStorage * fs; var reader: CvSeqReader; count: nteger;
// dst: Pointer; dt: PCVChar);
//
// (* combination of two previous functions for easier reading of whole sequences *)
// procedure cvReadRawData(var fs: CvFileStorage; var src: CvFileNode; dst: ointer; dt: PCVChar);
//
// (* writes a copy of file node to file storage *)
// procedure cvWriteFileNode(var fs: CvFileStorage; new_node_name: PCVChar; var node: vFileNode;
// embed: Integer);
//
// (* returns name of file node *)
// CVAPI(Char)cvGetFileNodeName(CvFileNode * node);
//
// (* ********************************** Adding own types ********************************** *)
//
// procedure cvRegisterType(var info: CvTypeInfo); CVAPI({$EXTERNALSYM }
// procedure)cvUnregisterType(type_name: PCVChar); CVAPI(CvTypeInfo)cvFirstType(
// procedure); CVAPI(type_name: PCVChar);
// CVAPI(CvTypeInfo)cvTypeOf(Pointer type cvTypeOf(Pointer struct_ptr) = record end:;
// rocedure)cvRelease(Pointer * type end:; r type end:;
// simple API for reading / writing data * )attributes CV_DEFAULT(CvAttrList())): CvAttrList;
// real_name CV_DEFAULT(0)): ^PCVChar; cvGetTickCount(): CVAPI(int64);
// cvGetTickFrequency(): CVAPI(Double); CV_CPU_NONE = 0: const; CV_CPU_MMX = 1: const;
// CV_CPU_SSE = 2: const; CV_CPU_SSE2 = 3: const; CV_CPU_SSE3 = 4: const; CV_CPU_SSSE3 = 5: const;
// CV_CPU_SSE4_1 = 6: const; CV_CPU_SSE4_2 = 7: const; CV_CPU_POPCNT = 8: const;
// CV_CPU_AVX = 10: const; CV_HARDWARE_MAX_FEATURE = 255: const;
// cvCheckHardwareSupport(Integer feature): CVAPI(Integer); cvGetNumThreads(): CVAPI(Integer);
// procedure cvSetNumThreads(v1: 0)); cvGetThreadNum(): CVAPI(Integer);
// cvGetErrStatus(): CVAPI(Integer);
// procedure cvSetErrStatus(status: Integer); CV_ErrModeLeaf = 0: const;
// (* Print error and exit program *)
// CV_ErrModeParent = 1: const; (* Print error and continue *)
// CV_ErrModeSilent = 2: const; (* Don't print and continue */
// cvGetErrMode(  ): CVAPI(Integer);
// cvSetErrMode( Integer mode ): CVAPI(Integer);
// PCVChar = file_name, Integer line ): const;;
// char) cvErrorStr( Integer status ): CVAPI(;
// filename, Integer* line ): ^PCVChar;
// cvErrorFromIppStatus( Integer ipp_status ): CVAPI(Integer);
// PCVChar = file_name, Integer line, Pointer  userdata ): const;;
// prev_userdata CV_DEFAULT(0) ): ^Pointer;
// = ): const;
// file_name, Integer line, Pointer  userdata ): PCVChar;
// file_name, Integer line, Pointer  userdata ): PCVChar;
// file_name, Integer line, Pointer  userdata ): PCVChar;
// OPENCV_ERROR(CV_StsBackTrace,(func),(context)): begin;
// end; end;
//
// {$define OPENCV_ASSERT(expr,func,context)}
// begin if ( not  (expr)) then
// begin OPENCV_ERROR(CV_StsInternal,(func),(context)); end; end;
//
// {$define OPENCV_RSTERR((cvSetErrStatus(CV_StsOk))}
//
// // >> Following declaration is a macro definition!
// const OPENCV_CALL( Func );
// begin
// Func;
// end;
//
//
// (* CV_FUNCNAME macro defines icvFuncName constant which is used by CV_ERROR macro *)
// {$IFDEF CV_NO_FUNC_NAMES}
/// / >> Following declaration is a macro definition!
// const CV_FUNCNAME(Name); const cvFuncName = '';
// {$EXTERNALSYM cvFuncName}
// {$ELSE}
/// / >> Following declaration is a macro definition!
// const CV_FUNCNAME(Name); Char cvFuncName[] = Name
// {$ENDIF}
// (*
// CV_ERROR macro unconditionally raises error with passed code and message.
// After raising error, control will be transferred to the exit cLabel.
// *)
/// / >> Following declaration is a macro definition!
// const CV_ERROR(Code, Msg); begin cvError((Code), cvFuncName, Msg, __FILE__, __LINE__);
// __CV_EXIT__; end;
//
// (* Simplified form of CV_ERROR *)
/// / >> Following declaration is a macro definition!
// const CV_ERROR_FROM_CODE(Code)CV_ERROR(Code, '');
//
// (*
// CV_CHECK macro checks error status after CV (or IPL)
// cFunction call. If error detected, control will be transferred to the exit
// cLabel.
// *)
// {$DEFINE CV_CHECK()}
// begin if (cvGetErrStatus() < 0)CV_ERROR(CV_StsBackTrace, 'Inner function failed.') then; end;
//
// (*
// CV_CALL macro calls CV (or IPL) cFunction, checks error status and
// signals a error if the cFunction failed. Useful in 'parent node'
// error procesing mode
// *)
/// / >> Following declaration is a macro definition!
// const CV_CALL(func); begin func; CV_CHECK(); end;
//
// (* Runtime assertion macro *)
/// / >> Following declaration is a macro definition!
// const CV_ASSERT(Condition); begin if (not(Condition))CV_ERROR(CV_StsInternal,
// 'Assertion: ' # Condition ' failed') then; end;
//
// const __CV_BEGIN__ = begin;
// {$EXTERNALSYM __CV_BEGIN__}
// const __CV_END__ = goto exit; exit:; end;;
// {$EXTERNALSYM __CV_END__}
// const __CV_EXIT__ = goto exit;
// {$EXTERNALSYM __CV_EXIT__}
// {$IFDEF __cplusplus}
// end;
//
/// / classes for automatic module/RTTI data registration/unregistration
// CV_EXPORTS CvModule begin CvModule(CvModuleInfo * _info); ~ CvModule(); CvModuleInfo * info;
//
// CvModuleInfo * first; CvModuleInfo * last;);
//
// CV_EXPORTS CvType begin CvType(PCVChar type_name, CvIsInstanceFunc is_instance,
// CvReleaseFunc release = 0, CvReadFunc read := 0, CvWriteFunc write = 0, CvCloneFunc clone = 0);
// ~ CvType(); CvTypeInfo * info;
//
// CvTypeInfo * first; CvTypeInfo * last;);
//
// {$ENDIF}
// {$ENDIF}
implementation

const
{$IFDEF DEBUG}
  DllName = 'opencv_core242d.dll';
{$ELSE}
  DllName = 'opencv_core242.dll';
{$ENDIF}
procedure cvAlloc(size: size_t); external DllName; cdecl;
procedure cvFree_(ptr: Pointer); external DllName; cdecl;
function cvCreateImageHeader(size: CvSize; depth: Integer; channels: Integer): IplImage;
  external DllName; cdecl;
function cvCreateImage(size: CvSize; depth, channels: Integer): pIplImage; external DllName; cdecl;
procedure cvReleaseImageHeader(image: array of IplImage); external DllName; cdecl;
// procedure cvReleaseImage(image: array of IplImage); external DllName; cdecl;
procedure cvReleaseImage(var image: pIplImage); external DllName; cdecl;
function cvCloneImage(image: pIplImage): IplImage; external DllName; cdecl;
procedure cvSetImageCOI(var image: IplImage; coi: Integer); external DllName; cdecl;
function cvGetImageCOI(image: pIplImage): Integer; external DllName; cdecl;
procedure cvSetImageROI(var image: IplImage; rect: CvRect); external DllName; cdecl;
procedure cvResetImageROI(var image: IplImage); external DllName; cdecl;
function cvGetImageROI(image: pIplImage): CvRect; external DllName; cdecl;
function cvCreateMatHeader(rows: Integer; cols: Integer; cType: Integer): CvMat;
  external DllName; cdecl;

//function cvGetSize(const arr: CvArr): CvSize; external DllName; cdecl;
 function cvGetSize(const image: pIplImage): CvSize;
 begin
 if assigned(Image^.roi) then
 begin
 Result.width := Image^.roi^.width;
 Result.height := Image^.roi^.height;
 end
 else
 begin
 Result.width := image^.width;
 Result.height := image^.height;
 end; // if/else
 end;

end.

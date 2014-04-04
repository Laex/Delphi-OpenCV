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
  //  opencv\modules\imgproc\src\smooth.cpp
  //  ************************************************************************************************* *)

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
unit smooth;

{$POINTERMATH ON}

interface

Uses Core.types_c;

{$IFDEF CV_SSE2}

const
  MEDIAN_HAVE_SIMD = true;

  // static inline void histogram_add_simd(const HT x[16], HT y[16])
  // {
  // const __m128i* rx = (const __m128i*)x;
  // __m128i* ry = (__m128i*)y;
  // __m128i r0 = _mm_add_epi16(_mm_load_si128(ry+0),_mm_load_si128(rx+0));
  // __m128i r1 = _mm_add_epi16(_mm_load_si128(ry+1),_mm_load_si128(rx+1));
  // _mm_store_si128(ry+0, r0);
  // _mm_store_si128(ry+1, r1);
  // }
  //
  // static inline void histogram_sub_simd(const HT x[16], HT y[16])
  // {
  // const __m128i* rx = (const __m128i*)x;
  // __m128i* ry = (__m128i*)y;
  // __m128i r0 = _mm_sub_epi16(_mm_load_si128(ry+0),_mm_load_si128(rx+0));
  // __m128i r1 = _mm_sub_epi16(_mm_load_si128(ry+1),_mm_load_si128(rx+1));
  // _mm_store_si128(ry+0, r0);
  // _mm_store_si128(ry+1, r1);
  // }

{$ELSE}

const
  MEDIAN_HAVE_SIMD = false;
{$ENDIF}

procedure medianBlur(src0: pIplImage; dst: pIplImage; ksize: Integer);

implementation

Uses core_c, imgproc_c, imgproc, Math, System.Generics.Defaults, Windows, imgproc.types_c;

// ****************************************************************************************
// *                                      Median Filter                                   *
// ****************************************************************************************

Type
  HT = ushort;

  /// **
  // * This structure represents a two-tier histogram. The first tier (known as the
  // * "coarse" level) is 4 bit wide and the second tier (known as the "fine" level)
  // * is 8 bit wide. Pixels inserted in the fine level also get inserted into the
  // * coarse bucket designated by the 4 MSBs of the fine bucket value.
  // *
  // * The structure is aligned on 16 bits, which is a prerequisite for SIMD
  // * instructions. Each bucket is 16 bit wide, which means that extra care must be
  // * taken to prevent overflow.
  // */

  pHistogramArray = ^THistogramArray;
  THistogramArray = array [0 .. 15] of HT;

  THistogram = packed record
    coarse: THistogramArray;
    fine: array [0 .. 15] of THistogramArray;
  end;

{$IFDEF CV_SSE2}
  // static inline void histogram_add_simd(const HT x[16], HT y[16])
  // {
  // const __m128i* rx = (const __m128i*)x;
  // __m128i* ry = (__m128i*)y;
  // __m128i r0 = _mm_add_epi16(_mm_load_si128(ry+0),_mm_load_si128(rx+0));
  // __m128i r1 = _mm_add_epi16(_mm_load_si128(ry+1),_mm_load_si128(rx+1));
  // _mm_store_si128(ry+0, r0);
  // _mm_store_si128(ry+1, r1);
  // }
  //
  // static inline void histogram_sub_simd(const HT x[16], HT y[16])
  // {
  // const __m128i* rx = (const __m128i*)x;
  // __m128i* ry = (__m128i*)y;
  // __m128i r0 = _mm_sub_epi16(_mm_load_si128(ry+0),_mm_load_si128(rx+0));
  // __m128i r1 = _mm_sub_epi16(_mm_load_si128(ry+1),_mm_load_si128(rx+1));
  // _mm_store_si128(ry+0, r0);
  // _mm_store_si128(ry+1, r1);
  // }
{$ELSE}

procedure histogram_add(const x, y: pHistogramArray); inline;
var
  i: Integer;
begin
  for i := 0 to 15 do
    y^[i] := y^[i] + x^[i];
end;

procedure histogram_sub(const x, y: pHistogramArray); inline;
var
  i: Integer;
begin
  for i := 0 to 15 do
    y^[i] := y^[i] - x^[i];
end;

procedure histogram_muladd(a: Integer; const x, y: pHistogramArray); inline;
var
  i: Integer;
begin
  for i := 0 to 15 do
    y^[i] := y^[i] + a * x^[i];
end;

{$ENDIF}

procedure medianBlur_8u_Om(const _src: pIplImage; _dst: pIplImage; m: Integer);
Const
  N = 16;
Var
  zone0: array [0 .. 3, 0 .. N - 1] of Integer;
  zone1: array [0 .. 3, 0 .. N * N - 1] of Integer;
  x, y: Integer;
  n2: Integer;
  size: TcvSize;
  src, dst: PByte;
  src_step, dst_step, cn: Integer;
  src_max: PByte;
  dst_cur, src_top, src_bottom: PByte;
  k, c: Integer;
  src_step1, dst_step1: Integer;
  s: Integer;
  t: Integer;
  p, q: Integer;

  procedure UPDATE_ACC01(pix, cn: Integer; op: Integer);
  begin
    zone1[cn][pix] := zone1[cn][pix] + op;
    zone0[cn][pix shr 4] := zone0[cn][pix shr 4] + op;
  end;

begin
  n2 := m * m div 2;
  size := cvGetSize(_dst);
  src := _src^.imagedata;
  dst := _dst^.imagedata;
  src_step := _src.widthstep;
  dst_step := _dst.widthstep;
  cn := _src^.nchannels;
  src_max := src + size.height * src_step;

  // CV_Assert( size.height >= nx and size.width >= nx );
  for x := 0 to size.width - 1 do
  // src + = cn, dst + = cn
  begin
    dst_cur := dst;
    src_top := src;
    src_bottom := src;
    src_step1 := src_step;
    dst_step1 := dst_step;

    if (x mod 2 <> 0) then
    begin
      src_top := src_top + src_step * (size.height - 1);
      src_bottom := src_top;
      dst_cur := dst_cur + dst_step * (size.height - 1);
      src_step1 := -src_step1;
      dst_step1 := -dst_step1;
    end;

    // init accumulator
    FillChar(zone0, sizeof(zone0[0]) * cn, 0);
    FillChar(zone1, sizeof(zone1[0]) * cn, 0);

    for y := 0 to (m div 2) do
    begin
      for c := 0 to cn - 1 do
      begin
        if (y > 0) then
        begin
          k := 0;
          while k < m * cn do
          begin
            UPDATE_ACC01(src_bottom[k + c], c, 1);
            k := k + cn;
          end;
        end
        else
        begin
          k := 0;
          while k < m * cn do
          begin
            UPDATE_ACC01(src_bottom[k + c], c, m div 2 + 1);
            k := k + cn;
          end;
        end;
      end;

      if ((src_step1 > 0) and (y < size.height - 1)) or ((src_step1 < 0) and (size.height - y - 1 > 0)) then
        src_bottom := src_bottom + src_step1;
    end;

    for y := 0 to size.height - 1 do
    begin
      // find median
      for c := 0 to cn - 1 do
      begin
        s := 0;
        k := 0;
        while true do
        begin
          t := s + zone0[c][k];
          if (t > n2) then
            break;
          s := t;
          Inc(k);
        end;

        k := k * N;
        while true do
        begin
          s := s + zone1[c][k];
          if (s > n2) then
            break;
          Inc(k);
        end;
        dst_cur[c] := k;
        dst_cur := dst_cur + dst_step1;
      end;

      if (y + 1) = size.height then
        break;

      if (cn = 1) then
      begin
        for k := 0 to m - 1 do
        begin
          p := src_top[k];
          q := src_bottom[k];
          Dec(zone1[0][p]);
          Dec(zone0[0][p shr 4]);
          Inc(zone1[0][q]);
          Inc(zone0[0][q shr 4]);
        end;
      end
      else if (cn = 3) then
      begin
        k := 0;
        while k < m * 3 do
        begin
          UPDATE_ACC01(src_top[k], 0, -1);
          UPDATE_ACC01(src_top[k + 1], 1, -1);
          UPDATE_ACC01(src_top[k + 2], 2, -1);

          UPDATE_ACC01(src_bottom[k], 0, 1);
          UPDATE_ACC01(src_bottom[k + 1], 1, 1);
          UPDATE_ACC01(src_bottom[k + 2], 2, 1);
          k := k + 3;
        end;
      end
      else
      begin
        assert(cn = 4);
        k := 0;
        while k < m * 4 do
        begin
          UPDATE_ACC01(src_top[k], 0, -1);
          UPDATE_ACC01(src_top[k + 1], 1, -1);
          UPDATE_ACC01(src_top[k + 2], 2, -1);
          UPDATE_ACC01(src_top[k + 3], 3, -1);

          UPDATE_ACC01(src_bottom[k], 0, 1);
          UPDATE_ACC01(src_bottom[k + 1], 1, 1);
          UPDATE_ACC01(src_bottom[k + 2], 2, 1);
          UPDATE_ACC01(src_bottom[k + 3], 3, 1);
          k := k + 4;
        end;
      end;

      if ((src_step1 > 0) and (src_bottom + src_step1 < src_max)) or
        ((src_step1 < 0) and (src_bottom + src_step1 >= src)) then
        src_bottom := src_bottom + src_step1;

      if (y >= m div 2) then
        src_top := src_top + src_step1;
    end;
  end;
end;

procedure medianBlur_8u_O1(const _src: pIplImage; _dst: pIplImage; ksize: Integer);

// **
// * HOP is short for Histogram OPeration. This macro makes an operation \a op on
// * histogram \a h for pixel value \a x. It takes care of handling both levels.
// */
// procedure HOP(h,x,op)
// h.coarse[x shr 4] op,
// *((HT*)h.fine + x) op
var
  cn, m, r: Integer;
  sstep, dstep: size_t;
  H: array [0 .. 3] of THistogram;
  luc: array [0 .. 3] of array [0 .. 15] of HT;
  STRIPE_SIZE: Integer;
  _h_coarse, _h_fine: TArray<HT>;
  h_coarse, h_fine: ^HT;
{$IFDEF MEDIAN_HAVE_SIMD}
  useSIMD: Boolean;
{$ENDIF}
  x: Integer;
  i, j, k, c, N: Integer;
  src, dst: PByte;

  procedure COP(c, j, x, op: Integer);
  begin
    h_coarse[16 * (N * c + j) + (x shr 4)] := h_coarse[16 * (N * c + j) + (x shr 4)] + op;
    h_fine[16 * (N * (16 * c + (x shr 4)) + j) + (x and $F)] :=
      h_fine[16 * (N * (16 * c + (x shr 4)) + j) + (x and $F)] + op;
  end;

var
  p: PByte;
  p0, p1: PByte;
  t, b, sum: Integer;
  segment: ^HT;

begin
  cn := _dst^.nchannels;
  m := _dst^.height;
  r := (ksize - 1) div 2;
  sstep := _src.widthstep;
  dstep := _dst.widthstep;
  STRIPE_SIZE := min(_dst.width, 512 div cn);

  SetLength(_h_coarse, 1 * 16 * (STRIPE_SIZE + 2 * r) * cn + 16);
  SetLength(_h_fine, 16 * 16 * (STRIPE_SIZE + 2 * r) * cn + 16);
  h_coarse := @_h_coarse[0];
  h_fine := @_h_fine[0];
{$IFDEF MEDIAN_HAVE_SIMD}
  useSIMD := cvCheckHardwareSupport(CV_CPU_SSE2);
{$ENDIF}
  x := 0;
  while x < _dst^.width do
  // x + = STRIPE_SIZE
  begin
    N := min(_dst.width - x, STRIPE_SIZE) + r * 2;
    src := _src.imagedata + x * cn;
    dst := _dst.imagedata + (x - r) * cn;
    ZeroMemory(h_coarse, 16 * N * cn * sizeof(h_coarse[0]));
    ZeroMemory(h_fine, 16 * 16 * N * cn * sizeof(h_fine[0]));

    // First row initialization
    for c := 0 to cn - 1 do
    begin
      for j := 0 to N - 1 do
        COP(c, j, src[cn * j + c], (r + 2));

      for i := 1 to r - 1 do
      begin
        p := src + sstep * min(i, m - 1);
        for j := 0 to N - 1 do
          COP(c, j, p[cn * j + c], 1);
      end;
    end;

    for i := 0 to m - 1 do
    begin
      p0 := src + sstep * max(0, i - r - 1);
      p1 := src + sstep * min(m - 1, i + r);

      ZeroMemory(@H, cn * sizeof(H[0]));
      ZeroMemory(@luc, cn * sizeof(luc[0]));
      for c := 0 to cn - 1 do
      begin
        // Update column histograms for the entire row.
        for j := 0 to N - 1 do
        begin
          COP(c, j, p0[j * cn + c], -1);
          COP(c, j, p1[j * cn + c], 1);
        end;

        // First column initialization
        for k := 1 to 16 do
          histogram_muladd(2 * r + 1, @h_fine[16 * N * (16 * c + k)], @H[c].fine[k][0]);

{$IFDEF MEDIAN_HAVE_SIMD}
        // if (useSIMD) then
        // begin
        // for j := 0 to 2 * r-1 do
        // histogram_add_simd(&h_coarse[16 * (N * c + j)], H[c].coarse);
        //
        // for (j := r; j < N - r; j + +)begin int t := 2 * r * r + 2 * r, b, sum := 0;
        // HT * segment;
        //
        // histogram_add_simd(&h_coarse[16 * (N * c + std: : min(j + r, N - 1))], H[c].coarse);
        //
        // // Find median at coarse level
        // for (k := 0; k < 16; + + k)begin sum + = H[c].coarse[k];
        // if (sum > t)begin sum - = H[c].coarse[k];
        // break;
        // end;
        // end;
        // assert(k < 16);
        //
        // / * Update corresponding Histogram segment * /
        // if (luc[c][k] <= j - r)begin memset(&H[c].fine[k], 0, 16 * sizeof(HT));
        // for (luc[c][k] := cv: : HT(j - r); luc[c][k] < min(j + r + 1, N); + + luc[c][k])
        // histogram_add_simd(&h_fine[16 * (N * (16 * c + k) + luc[c][k])], H[c].fine[k]);
        //
        // if (luc[c][k] < j + r + 1)begin histogram_muladd(j + r + 1 - N, &h_fine[16 * (N * (16 * c + k) + (N - 1))],
        // &H[c].fine[k][0]);
        // luc[c][k] := (HT)(j + r + 1);
        // end;
        // end;
        // else
        // begin
        // for (; luc[c][k] < j + r + 1; + + luc[c][k])begin histogram_sub_simd
        // (&h_fine[16 * (N * (16 * c + k) + max(luc[c][k] - 2 * r - 1, 0))], H[c].fine[k]);
        // histogram_add_simd(&h_fine[16 * (N * (16 * c + k) + min(luc[c][k], N - 1))], H[c].fine[k]);
        // end;
        // end;
        //
        // histogram_sub_simd(&h_coarse[16 * (N * c + max(j - r, 0))], H[c].coarse);
        //
        /// * Find median in segment * / segment := H[c].fine[k];
        // for (b := 0; b < 16; b + +)begin sum + = segment[b];
        // if (sum > t)begin dst[dstep * i + cn * j + c] := (uchar)(16 * k + b);
        // break;
        // end;
        // end;
        // assert(b < 16);
        // end;
        // end;
        // else
{$ENDIF}
        begin
          for j := 0 to 2 * r - 1 do
            histogram_add(@h_coarse[16 * (N * c + j)], @H[c].coarse);

          for j := r to (N - r) - 1 do
          begin
            t := 2 * r * r + 2 * r;
            sum := 0;
            histogram_add(@h_coarse[16 * (N * c + min(j + r, N - 1))], @H[c].coarse);

            // Find median at coarse level
            for k := 0 to 15 do
            begin
              sum := sum + H[c].coarse[k];
              if (sum > t) then
              begin
                sum := sum - H[c].coarse[k];
                break;
              end;
            end;
            assert(k < 16);

            // * Update corresponding Histogram segment * /
            if (luc[c][k] <= j - r) then
            begin
              ZeroMemory(@H[c].fine[k], 16 * sizeof(HT));
              luc[c][k] := (j - r);
              while luc[c][k] < min(j + r + 1, N) do
              begin
                histogram_add(@h_fine[16 * (N * (16 * c + k) + luc[c][k])], @H[c].fine[k]);
                Inc(luc[c][k]);
              end;

              if (luc[c][k] < j + r + 1) then
              begin
                histogram_muladd(j + r + 1 - N, @h_fine[16 * (N * (16 * c + k) + (N - 1))], @H[c].fine[k][0]);
                luc[c][k] := j + r + 1;
              end;
            end
            else
            begin
              while luc[c][k] < j + r + 1 do
              begin
                histogram_sub(@h_fine[16 * (N * (16 * c + k) + max(luc[c][k] - 2 * r - 1, 0))], @H[c].fine[k]);
                histogram_add(@h_fine[16 * (N * (16 * c + k) + min(luc[c][k], N - 1))], @H[c].fine[k]);
                Inc(luc[c][k]);
              end;
            end;

            histogram_sub(@h_coarse[16 * (N * c + max(j - r, 0))], @H[c].coarse);

            // * Find median in segment * / segment := H[c].fine[k];
            for b := 0 to 15 do
            begin
              sum := sum + segment[b];
              if (sum > t) then
              begin
                dst[dstep * i + cn * j + c] := 16 * k + b;
                break;
              end;
            end;
            assert(b < 16);
          end;
        end;
      end;
    end;
  end;
end;

Type
MinMax < t: record >= record

const
  _SIZE = 1;

function load(const ptr: Pointer): t;

  procedure store(ptr: Pointer; val: t);
    procedure op(var a, b: t);
    end;

mm8u = Byte; // CV_8U
mm16u = Word; // CV_16U
mm16s = SmallInt; // CV_16S
mm32f = Single; // CV_32F

TmedianBlur<t: record > = class public class
procedure SortNet(const _src: pIplImage; _dst: pIplImage; m: Integer);
end;

procedure medianBlur(src0: pIplImage; dst: pIplImage; ksize: Integer);
Var
  useSortNet: Boolean;
  src: pIplImage;
  cn: Integer;
  img_size_mp: double;
begin
  if (ksize <= 1) then
  begin
    cvCopy(src0, dst);
    Exit;
  end;

  assert((ksize mod 2 = 1));

{$IFDEF HAVE_TEGRA_OPTIMIZATION}
  if (tegra_medianBlur(src0, dst, ksize)) then
    Exit;
{$ENDIF}
  useSortNet := (ksize = 3) or (ksize = 5
{$IFNDEF CV_SSE2}
    ) and (src0^.depth > CV_8U
{$ENDIF}
    );

  if useSortNet then
  begin
    if (dst^.imagedata <> src0^.imagedata) then
      src := src0
    else
      cvCopy(src0, src);

    if (src^.depth = CV_8U) then
      TmedianBlur<mm8u>.SortNet(src, dst, ksize)
    else if (src^.depth = CV_16U) then
      TmedianBlur<mm16u>.SortNet(src, dst, ksize)
    else if (src^.depth = CV_16S) then
      TmedianBlur<mm16s>.SortNet(src, dst, ksize)
    else if (src^.depth = CV_32F) then
      TmedianBlur<mm32f>.SortNet(src, dst, ksize)
    else
      assert(false, 'CV_StsUnsupportedFormat');
    Exit;
  end
  else
  begin
    cvCopyMakeBorder(src0, src, CvPoint(0, 0), { ksize / 2, ksize / 2, } BORDER_REPLICATE, cvScalarAll(0));
    cn := src0^.nchannels;
    assert((src^.depth = CV_8U) and ((cn = 1) or (cn = 3) or (cn = 4)));

    img_size_mp := (src0^.width * src0^.height) / (1 shl 20);
    if ksize <= (3 + (iif(img_size_mp < 1, 12, iif(img_size_mp < 4, 6, 2)) *
      (iif(MEDIAN_HAVE_SIMD and (cvCheckHardwareSupport(CV_CPU_SSE2) <> 0), 1, 3)))) then
      medianBlur_8u_Om(src, dst, ksize)
    else
      medianBlur_8u_O1(src, dst, ksize);
  end;
end;

{ MinMax<T> }

function MinMax<t>.load(const ptr: Pointer): t;
begin
  Result := t(ptr^);
end;

procedure MinMax<t>.op(var a, b: t);
var
  _t: t;
  Comparer: IComparer<t>;
begin
  _t := a;
  Comparer := TComparer<t>.Default;
  if Comparer.Compare(a, b) > 0 then // min
    a := b;
  if Comparer.Compare(b, _t) < 0 then // max
    b := _t;
end;

procedure MinMax<t>.store(ptr: Pointer; val: t);
begin
  t(ptr^) := val;
end;

{ TmedianBlur<T> }

class procedure TmedianBlur<t>.SortNet(const _src: pIplImage; _dst: pIplImage; m: Integer);
type
  pT = ^t;
Var
  MM: MinMax<t>;
  src, dst: pT;
  sstep, dstep: Integer;
  size: TcvSize;
  i, j, k, cn: Integer;
  useSIMD: Boolean;
  len, sdelta, sdelta0, ddelta: Integer;
  p0, p1, p2, p3, p4, p5, p6, p7, p8: t;
  row0, row1, row2: pT;
  limit: Integer;
  j0, j1, j2, j3, j4: Integer;
  i1, i0, i3, i4: Integer;
  row: array [0 .. 4] of pT;
  p: array [0 .. 24] of t;
  rowk: pT;

begin
  src := pT(_src.imagedata);
  dst := pT(_dst.imagedata);
  sstep := _src^.widthstep div sizeof(t);
  dstep := _dst.widthstep div sizeof(t);
  size := cvGetSize(_dst);
  cn := _src^.nchannels;
  useSIMD := cvCheckHardwareSupport(CV_CPU_SSE2) <> 0;

  if (m = 3) then
  begin
    if (size.width = 1) or (size.height = 1) then
    begin
      len := size.width + size.height - 1;
      sdelta := iif(size.height = 1, cn, sstep);
      sdelta0 := iif(size.height = 1, 0, sstep - cn);
      ddelta := iif(size.height = 1, cn, dstep);

      for i := 0 to len - 1 do
      begin
        for j := 0 to cn - 1 do
        begin
          p0 := src[Integer(iif(i > 0, -sdelta, 0))];
          p1 := src[0];
          p2 := src[Integer(iif(i < len - 1, sdelta, 0))];

          MM.op(p0, p1);
          MM.op(p1, p2);
          MM.op(p0, p1);
          dst[j] := p1;
          src := src + sizeof(t);
        end;
        src := src + sdelta0;
        dst := dst + ddelta;
      end;
      Exit;
    end;

    size.width := size.width * cn;
    for i := 0 to size.height - 1 do
    begin
      row0 := src + max(i - 1, 0) * sstep;
      row1 := src + i * sstep;
      row2 := src + min(i + 1, size.height - 1) * sstep;
      limit := iif(useSIMD, cn, size.width);

      j := 0;
      while true do
      begin
        while j < limit do
        begin
          j0 := iif(j >= cn, j - cn, j);
          j2 := iif(j < size.width - cn, j + cn, j);
          p0 := row0[j0];
          p1 := row0[j];
          p2 := row0[j2];
          p3 := row1[j0];
          p4 := row1[j];
          p5 := row1[j2];
          p6 := row2[j0];
          p7 := row2[j];
          p8 := row2[j2];

          MM.op(p1, p2);
          MM.op(p4, p5);
          MM.op(p7, p8);
          MM.op(p0, p1);
          MM.op(p3, p4);
          MM.op(p6, p7);
          MM.op(p1, p2);
          MM.op(p4, p5);
          MM.op(p7, p8);
          MM.op(p0, p3);
          MM.op(p5, p8);
          MM.op(p4, p7);
          MM.op(p3, p6);
          MM.op(p1, p4);
          MM.op(p2, p5);
          MM.op(p4, p7);
          MM.op(p4, p2);
          MM.op(p6, p4);
          MM.op(p4, p2);
          dst[j] := p4;
          Inc(j);
        end;

        if (limit = size.width) then
          break;

        While j <= size.width - MM._SIZE - cn do
        begin
          p0 := MM.load(row0 + j - cn);
          p1 := MM.load(row0 + j);
          p2 := MM.load(row0 + j + cn);
          p3 := MM.load(row1 + j - cn);
          p4 := MM.load(row1 + j);
          p5 := MM.load(row1 + j + cn);
          p6 := MM.load(row2 + j - cn);
          p7 := MM.load(row2 + j);
          p8 := MM.load(row2 + j + cn);

          MM.op(p1, p2);
          MM.op(p4, p5);
          MM.op(p7, p8);
          MM.op(p0, p1);
          MM.op(p3, p4);
          MM.op(p6, p7);
          MM.op(p1, p2);
          MM.op(p4, p5);
          MM.op(p7, p8);
          MM.op(p0, p3);
          MM.op(p5, p8);
          MM.op(p4, p7);
          MM.op(p3, p6);
          MM.op(p1, p4);
          MM.op(p2, p5);
          MM.op(p4, p7);
          MM.op(p4, p2);
          MM.op(p6, p4);
          MM.op(p4, p2);
          MM.store(dst + j, p4);
          j := j + MM._SIZE
        end;

        limit := size.width;
      end;
      dst := dst + dstep;
    end;
  end
  else if (m = 5) then
  begin
    if (size.width = 1) or (size.height = 1) then
    begin
      len := size.width + size.height - 1;
      sdelta := iif(size.height = 1, cn, sstep);
      sdelta0 := iif(size.height = 1, 0, sstep - cn);
      ddelta := iif(size.height = 1, cn, dstep);

      for i := 0 to len - 1 do
      begin
        for j := 0 to cn - 1 do
        begin
          i1 := iif(i > 0, -sdelta, 0);
          i0 := iif(i > 1, -sdelta * 2, i1);
          i3 := iif(i < len - 1, sdelta, 0);
          i4 := iif(i < len - 2, sdelta * 2, i3);
          p0 := src[i0];
          p1 := src[i1];
          p2 := src[0];
          p3 := src[i3];
          p4 := src[i4];

          MM.op(p0, p1);
          MM.op(p3, p4);
          MM.op(p2, p3);
          MM.op(p3, p4);
          MM.op(p0, p2);
          MM.op(p2, p4);
          MM.op(p1, p3);
          MM.op(p1, p2);
          dst[j] := p2;
          src := src + sizeof(t);
        end;
        src := src + sdelta0;
        dst := dst + ddelta;
      end;
      Exit;
    end;

    size.width := size.width * cn;
    for i := 0 to size.height - 1 do
    begin
      row[0] := src + max(i - 2, 0) * sstep;
      row[1] := src + max(i - 1, 0) * sstep;
      row[2] := src + i * sstep;
      row[3] := src + min(i + 1, size.height - 1) * sstep;
      row[4] := src + min(i + 2, size.height - 1) * sstep;
      limit := iif(useSIMD, cn * 2, size.width);

      j := 0;
      while true do
      begin
        while j < limit do
        begin
          j1 := iif(j >= cn, j - cn, j);
          j0 := iif(j >= cn * 2, j - cn * 2, j1);
          j3 := iif(j < size.width - cn, j + cn, j);
          j4 := iif(j < size.width - cn * 2, j + cn * 2, j3);
          for k := 0 to 4 do
          begin
            rowk := row[k];
            p[k * 5] := rowk[j0];
            p[k * 5 + 1] := rowk[j1];
            p[k * 5 + 2] := rowk[j];
            p[k * 5 + 3] := rowk[j3];
            p[k * 5 + 4] := rowk[j4];
          end;

          MM.op(p[1], p[2]);
          MM.op(p[0], p[1]);
          MM.op(p[1], p[2]);
          MM.op(p[4], p[5]);
          MM.op(p[3], p[4]);
          MM.op(p[4], p[5]);
          MM.op(p[0], p[3]);
          MM.op(p[2], p[5]);
          MM.op(p[2], p[3]);
          MM.op(p[1], p[4]);
          MM.op(p[1], p[2]);
          MM.op(p[3], p[4]);
          MM.op(p[7], p[8]);
          MM.op(p[6], p[7]);
          MM.op(p[7], p[8]);
          MM.op(p[10], p[11]);
          MM.op(p[9], p[10]);
          MM.op(p[10], p[11]);
          MM.op(p[6], p[9]);
          MM.op(p[8], p[11]);
          MM.op(p[8], p[9]);
          MM.op(p[7], p[10]);
          MM.op(p[7], p[8]);
          MM.op(p[9], p[10]);
          MM.op(p[0], p[6]);
          MM.op(p[4], p[10]);
          MM.op(p[4], p[6]);
          MM.op(p[2], p[8]);
          MM.op(p[2], p[4]);
          MM.op(p[6], p[8]);
          MM.op(p[1], p[7]);
          MM.op(p[5], p[11]);
          MM.op(p[5], p[7]);
          MM.op(p[3], p[9]);
          MM.op(p[3], p[5]);
          MM.op(p[7], p[9]);
          MM.op(p[1], p[2]);
          MM.op(p[3], p[4]);
          MM.op(p[5], p[6]);
          MM.op(p[7], p[8]);
          MM.op(p[9], p[10]);
          MM.op(p[13], p[14]);
          MM.op(p[12], p[13]);
          MM.op(p[13], p[14]);
          MM.op(p[16], p[17]);
          MM.op(p[15], p[16]);
          MM.op(p[16], p[17]);
          MM.op(p[12], p[15]);
          MM.op(p[14], p[17]);
          MM.op(p[14], p[15]);
          MM.op(p[13], p[16]);
          MM.op(p[13], p[14]);
          MM.op(p[15], p[16]);
          MM.op(p[19], p[20]);
          MM.op(p[18], p[19]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[21], p[23]);
          MM.op(p[22], p[24]);
          MM.op(p[22], p[23]);
          MM.op(p[18], p[21]);
          MM.op(p[20], p[23]);
          MM.op(p[20], p[21]);
          MM.op(p[19], p[22]);
          MM.op(p[22], p[24]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[12], p[18]);
          MM.op(p[16], p[22]);
          MM.op(p[16], p[18]);
          MM.op(p[14], p[20]);
          MM.op(p[20], p[24]);
          MM.op(p[14], p[16]);
          MM.op(p[18], p[20]);
          MM.op(p[22], p[24]);
          MM.op(p[13], p[19]);
          MM.op(p[17], p[23]);
          MM.op(p[17], p[19]);
          MM.op(p[15], p[21]);
          MM.op(p[15], p[17]);
          MM.op(p[19], p[21]);
          MM.op(p[13], p[14]);
          MM.op(p[15], p[16]);
          MM.op(p[17], p[18]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[0], p[12]);
          MM.op(p[8], p[20]);
          MM.op(p[8], p[12]);
          MM.op(p[4], p[16]);
          MM.op(p[16], p[24]);
          MM.op(p[12], p[16]);
          MM.op(p[2], p[14]);
          MM.op(p[10], p[22]);
          MM.op(p[10], p[14]);
          MM.op(p[6], p[18]);
          MM.op(p[6], p[10]);
          MM.op(p[10], p[12]);
          MM.op(p[1], p[13]);
          MM.op(p[9], p[21]);
          MM.op(p[9], p[13]);
          MM.op(p[5], p[17]);
          MM.op(p[13], p[17]);
          MM.op(p[3], p[15]);
          MM.op(p[11], p[23]);
          MM.op(p[11], p[15]);
          MM.op(p[7], p[19]);
          MM.op(p[7], p[11]);
          MM.op(p[11], p[13]);
          MM.op(p[11], p[12]);
          dst[j] := p[12];
          Inc(j);
        end;

        if (limit = size.width) then
          break;

        While j <= size.width - MM._SIZE - cn * 2 do
        begin
          for k := 0 to 4 do
          begin
            rowk := row[k];
            p[k * 5] := MM.load(rowk + j - cn * 2);
            p[k * 5 + 1] := MM.load(rowk + j - cn);
            p[k * 5 + 2] := MM.load(rowk + j);
            p[k * 5 + 3] := MM.load(rowk + j + cn);
            p[k * 5 + 4] := MM.load(rowk + j + cn * 2);
          end;
          MM.op(p[1], p[2]);
          MM.op(p[0], p[1]);
          MM.op(p[1], p[2]);
          MM.op(p[4], p[5]);
          MM.op(p[3], p[4]);
          MM.op(p[4], p[5]);
          MM.op(p[0], p[3]);
          MM.op(p[2], p[5]);
          MM.op(p[2], p[3]);
          MM.op(p[1], p[4]);
          MM.op(p[1], p[2]);
          MM.op(p[3], p[4]);
          MM.op(p[7], p[8]);
          MM.op(p[6], p[7]);
          MM.op(p[7], p[8]);
          MM.op(p[10], p[11]);
          MM.op(p[9], p[10]);
          MM.op(p[10], p[11]);
          MM.op(p[6], p[9]);
          MM.op(p[8], p[11]);
          MM.op(p[8], p[9]);
          MM.op(p[7], p[10]);
          MM.op(p[7], p[8]);
          MM.op(p[9], p[10]);
          MM.op(p[0], p[6]);
          MM.op(p[4], p[10]);
          MM.op(p[4], p[6]);
          MM.op(p[2], p[8]);
          MM.op(p[2], p[4]);
          MM.op(p[6], p[8]);
          MM.op(p[1], p[7]);
          MM.op(p[5], p[11]);
          MM.op(p[5], p[7]);
          MM.op(p[3], p[9]);
          MM.op(p[3], p[5]);
          MM.op(p[7], p[9]);
          MM.op(p[1], p[2]);
          MM.op(p[3], p[4]);
          MM.op(p[5], p[6]);
          MM.op(p[7], p[8]);
          MM.op(p[9], p[10]);
          MM.op(p[13], p[14]);
          MM.op(p[12], p[13]);
          MM.op(p[13], p[14]);
          MM.op(p[16], p[17]);
          MM.op(p[15], p[16]);
          MM.op(p[16], p[17]);
          MM.op(p[12], p[15]);
          MM.op(p[14], p[17]);
          MM.op(p[14], p[15]);
          MM.op(p[13], p[16]);
          MM.op(p[13], p[14]);
          MM.op(p[15], p[16]);
          MM.op(p[19], p[20]);
          MM.op(p[18], p[19]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[21], p[23]);
          MM.op(p[22], p[24]);
          MM.op(p[22], p[23]);
          MM.op(p[18], p[21]);
          MM.op(p[20], p[23]);
          MM.op(p[20], p[21]);
          MM.op(p[19], p[22]);
          MM.op(p[22], p[24]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[12], p[18]);
          MM.op(p[16], p[22]);
          MM.op(p[16], p[18]);
          MM.op(p[14], p[20]);
          MM.op(p[20], p[24]);
          MM.op(p[14], p[16]);
          MM.op(p[18], p[20]);
          MM.op(p[22], p[24]);
          MM.op(p[13], p[19]);
          MM.op(p[17], p[23]);
          MM.op(p[17], p[19]);
          MM.op(p[15], p[21]);
          MM.op(p[15], p[17]);
          MM.op(p[19], p[21]);
          MM.op(p[13], p[14]);
          MM.op(p[15], p[16]);
          MM.op(p[17], p[18]);
          MM.op(p[19], p[20]);
          MM.op(p[21], p[22]);
          MM.op(p[23], p[24]);
          MM.op(p[0], p[12]);
          MM.op(p[8], p[20]);
          MM.op(p[8], p[12]);
          MM.op(p[4], p[16]);
          MM.op(p[16], p[24]);
          MM.op(p[12], p[16]);
          MM.op(p[2], p[14]);
          MM.op(p[10], p[22]);
          MM.op(p[10], p[14]);
          MM.op(p[6], p[18]);
          MM.op(p[6], p[10]);
          MM.op(p[10], p[12]);
          MM.op(p[1], p[13]);
          MM.op(p[9], p[21]);
          MM.op(p[9], p[13]);
          MM.op(p[5], p[17]);
          MM.op(p[13], p[17]);
          MM.op(p[3], p[15]);
          MM.op(p[11], p[23]);
          MM.op(p[11], p[15]);
          MM.op(p[7], p[19]);
          MM.op(p[7], p[11]);
          MM.op(p[11], p[13]);
          MM.op(p[11], p[12]);
          MM.store(dst + j, p[12]);
          j := j + MM._SIZE
        end;
        limit := size.width;
      end;
      dst := dst + dstep;
    end;
  end;
end;

end.

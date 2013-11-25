unit swscale;

{$include ffmpeg.inc}

interface

uses
  pixfmt, ctypes;

(*
  * Copyright (C) 2001-2011 Michael Niedermayer <michaelni@gmx.at>
  *
  * This file is part of FFmpeg.
  *
  * FFmpeg is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Lesser General Public
  * License as published by the Free Software Foundation; either
  * version 2.1 of the License, or (at your option) any later version.
  *
  * FFmpeg is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Lesser General Public License for more details.
  *
  * You should have received a copy of the GNU Lesser General Public
  * License along with FFmpeg; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(*
  * @file
  * @ingroup lsws
  * external API header
*)

(*
  * @defgroup lsws Libswscale
  * @{
*)

(*
  * Return the LIBSWSCALE_VERSION_INT constant.
*)
// unsigned swscale_version(void);

(*
  * Return the libswscale build-time configuration.
*)
// const char *swscale_configuration(void);

(*
  * Return the libswscale license.
*)
// const char *swscale_license(void);

Const
  (* values for the flags, the stuff on the command line is different *)
  SWS_FAST_BILINEAR = 1;
  SWS_BILINEAR = 2;
  SWS_BICUBIC = 4;
  SWS_X = 8;
  SWS_POINT = $10;
  SWS_AREA = $20;
  SWS_BICUBLIN = $40;
  SWS_GAUSS = $80;
  SWS_SINC = $100;
  SWS_LANCZOS = $200;
  SWS_SPLINE = $400;

  SWS_SRC_V_CHR_DROP_MASK = $30000;
  SWS_SRC_V_CHR_DROP_SHIFT = 16;

  SWS_PARAM_DEFAULT = 123456;

  SWS_PRINT_INFO = $1000;

  // the following 3 flags are not completely implemented
  // internal chrominace subsampling info
  SWS_FULL_CHR_H_INT = $2000;
  // input subsampling info
  SWS_FULL_CHR_H_INP = $4000;
  SWS_DIRECT_BGR = $8000;
  SWS_ACCURATE_RND = $40000;
  SWS_BITEXACT = $80000;
  SWS_ERROR_DIFFUSION = $800000;

{$IFDEF FF_API_SWS_CPU_CAPS}
  (*
    * CPU caps are autodetected now, those flags
    * are only provided for API compatibility.
  *)
  SWS_CPU_CAPS_MMX = $80000000;
  SWS_CPU_CAPS_MMXEXT = $20000000;
  SWS_CPU_CAPS_MMX2 = $20000000;
  SWS_CPU_CAPS_3DNOW = $40000000;
  SWS_CPU_CAPS_ALTIVEC = $10000000;
  SWS_CPU_CAPS_BFIN = $01000000;
  SWS_CPU_CAPS_SSE2 = $02000000;
{$ENDIF}
  SWS_MAX_REDUCE_CUTOFF = 0.002;

  SWS_CS_ITU709 = 1;
  SWS_CS_FCC = 4;
  SWS_CS_ITU601 = 5;
  SWS_CS_ITU624 = 5;
  SWS_CS_SMPTE170M = 5;
  SWS_CS_SMPTE240M = 7;
  SWS_CS_DEFAULT = 5;

  (*
    * Return a pointer to yuv<->rgb coefficients for the given colorspace
    * suitable for sws_setColorspaceDetails().
    *
    * @param colorspace One of the SWS_CS_* macros. If invalid,
    * SWS_CS_DEFAULT is used.
  *)
  // const int *sws_getCoefficients(int colorspace);

Type
  // when used for filters they must have an odd number of elements
  // coeffs cannot be shared between vectors
  pSwsVector = ^TSwsVector;

  TSwsVector = {packed} record
    coeff: pDouble;
    /// < pointer to the list of coefficients
    length: Integer;
    /// < number of coefficients in the vector
  end;

  // vectors can be shared
  pSwsFilter = ^TSwsFilter;

  TSwsFilter = {packed} record
    lumH: pSwsVector;
    lumV: pSwsVector;
    chrH: pSwsVector;
    chrV: pSwsVector;
  end;

  pSwsContext = ^TSwsContext;

  TSwsContext = {packed} record

  end;

  (*
    * Return a positive value if pix_fmt is a supported input format, 0
    * otherwise.
  *)
  // int sws_isSupportedInput(enum AVPixelFormat pix_fmt);

  (*
    * Return a positive value if pix_fmt is a supported output format, 0
    * otherwise.
  *)
  // int sws_isSupportedOutput(enum AVPixelFormat pix_fmt);

  (*
    * @param[in]  pix_fmt the pixel format
    * @return a positive value if an endianness conversion for pix_fmt is
    * supported, 0 otherwise.
  *)
  // int sws_isSupportedEndiannessConversion(enum AVPixelFormat pix_fmt);

  (*
    * Allocate an empty SwsContext. This must be filled and passed to
    * sws_init_context(). For filling see AVOptions, options.c and
    * sws_setColorspaceDetails().
  *)
  // struct SwsContext *sws_alloc_context(void);

  (*
    * Initialize the swscaler context sws_context.
    *
    * @return zero or positive value on success, a negative value on
    * error
  *)
  // int sws_init_context(struct SwsContext *sws_context, SwsFilter *srcFilter, SwsFilter *dstFilter);

  (*
    * Free the swscaler context swsContext.
    * If swsContext is NULL, then does nothing.
  *)
  // void sws_freeContext(struct SwsContext *swsContext);
procedure sws_freeContext(swsContext: pSwsContext); cdecl;

{$IFDEF FF_API_SWS_GETCONTEXT}
(*
  * Allocate and return an SwsContext. You need it to perform
  * scaling/conversion operations using sws_scale().
  *
  * @param srcW the width of the source image
  * @param srcH the height of the source image
  * @param srcFormat the source image format
  * @param dstW the width of the destination image
  * @param dstH the height of the destination image
  * @param dstFormat the destination image format
  * @param flags specify which algorithm and options to use for rescaling
  * @return a pointer to an allocated context, or NULL in case of error
  * @note this function is to be removed after a saner alternative is
  *       written
  * @deprecated Use sws_getCachedContext() instead.
*)
// struct SwsContext *sws_getContext(int srcW, int srcH, enum AVPixelFormat srcFormat,
// int dstW, int dstH, enum AVPixelFormat dstFormat,
// int flags, SwsFilter *srcFilter,
// SwsFilter *dstFilter, const double *param);
function sws_getContext(srcW: Integer; srcH: Integer; srcFormat: TAVPixelFormat; dstW: Integer; dstH: Integer; dstFormat: TAVPixelFormat;
  flags: Integer; srcFilter: pSwsFilter; dstFilter: pSwsFilter; const param: pDouble): pSwsContext; cdecl;
{$ENDIF}
(*
  * Scale the image slice in srcSlice and put the resulting scaled
  * slice in the image in dst. A slice is a sequence of consecutive
  * rows in an image.
  *
  * Slices have to be provided in sequential order, either in
  * top-bottom or bottom-top order. If slices are provided in
  * non-sequential order the behavior of the function is undefined.
  *
  * @param c         the scaling context previously created with
  *                  sws_getContext()
  * @param srcSlice  the array containing the pointers to the planes of
  *                  the source slice
  * @param srcStride the array containing the strides for each plane of
  *                  the source image
  * @param srcSliceY the position in the source image of the slice to
  *                  process, that is the number (counted starting from
  *                  zero) in the image of the first row of the slice
  * @param srcSliceH the height of the source slice, that is the number
  *                  of rows in the slice
  * @param dst       the array containing the pointers to the planes of
  *                  the destination image
  * @param dstStride the array containing the strides for each plane of
  *                  the destination image
  * @return          the height of the output slice
*)
// int sws_scale(struct SwsContext *c, const uint8_t *const srcSlice[],
// const int srcStride[], int srcSliceY, int srcSliceH,
// uint8_t *const dst[], const int dstStride[]);

Type
  TCintArray = array [0 .. 0] of cint;
  PCintArray = ^TCintArray;
  TPCuint8Array = array [0 .. 0] of pByte;
  PPCuint8Array = ^TPCuint8Array;

function sws_scale( //
  c: pSwsContext; // struct SwsContext *c
  const srcSlice: PPCuint8Array; // const uint8_t *const srcSlice[]
  const srcStride: PCintArray; // const int srcStride[]
  srcSliceY: cint; // int srcSliceY
  srcSliceH: cint; // int srcSliceH
  dst: PPCuint8Array; // uint8_t *const dst[]
  const dstStride: PCintArray // const int dstStride[]
  ): cint; cdecl;

(*
  * @param dstRange flag indicating the while-black range of the output (1=jpeg / 0=mpeg)
  * @param srcRange flag indicating the while-black range of the input (1=jpeg / 0=mpeg)
  * @param table the yuv2rgb coefficients describing the output yuv space, normally ff_yuv2rgb_coeffs[x]
  * @param inv_table the yuv2rgb coefficients describing the input yuv space, normally ff_yuv2rgb_coeffs[x]
  * @param brightness 16.16 fixed point brightness correction
  * @param contrast 16.16 fixed point contrast correction
  * @param saturation 16.16 fixed point saturation correction
  * @return -1 if not supported
*)
// int sws_setColorspaceDetails(struct SwsContext *c, const int inv_table[4],
// int srcRange, const int table[4], int dstRange,
// int brightness, int contrast, int saturation);

(*
  * @return -1 if not supported
*)
// int sws_getColorspaceDetails(struct SwsContext *c, int **inv_table,
// int *srcRange, int **table, int *dstRange,
// int *brightness, int *contrast, int *saturation);

(*
  * Allocate and return an uninitialized vector with length coefficients.
*)
// SwsVector *sws_allocVec(int length);

(*
  * Return a normalized Gaussian curve used to filter stuff
  * quality = 3 is high quality, lower is lower quality.
*)
// SwsVector *sws_getGaussianVec(double variance, double quality);

(*
  * Allocate and return a vector with length coefficients, all
  * with the same value c.
*)
// SwsVector *sws_getConstVec(double c, int length);

(*
  * Allocate and return a vector with just one coefficient, with
  * value 1.0.
*)
// SwsVector *sws_getIdentityVec(void);

(*
  * Scale all the coefficients of a by the scalar value.
*)
// void sws_scaleVec(SwsVector *a, double scalar);

(*
  * Scale all the coefficients of a so that their sum equals height.
*)
// void sws_normalizeVec(SwsVector *a, double height);
// void sws_convVec(SwsVector *a, SwsVector *b);
// void sws_addVec(SwsVector *a, SwsVector *b);
// void sws_subVec(SwsVector *a, SwsVector *b);
// void sws_shiftVec(SwsVector *a, int shift);

(*
  * Allocate and return a clone of the vector a, that is a vector
  * with the same coefficients as a.
*)
// SwsVector *sws_cloneVec(SwsVector *a);

(*
  * Print with av_log() a textual representation of the vector a
  * if log_level <= av_log_level.
*)
// void sws_printVec2(SwsVector *a, AVClass *log_ctx, int log_level);

// void sws_freeVec(SwsVector *a);

// SwsFilter *sws_getDefaultFilter(float lumaGBlur, float chromaGBlur,
// float lumaSharpen, float chromaSharpen,
// float chromaHShift, float chromaVShift,
// int verbose);
// void sws_freeFilter(SwsFilter *filter);

(*
  * Check if context can be reused, otherwise reallocate a new one.
  *
  * If context is NULL, just calls sws_getContext() to get a new
  * context. Otherwise, checks if the parameters are the ones already
  * saved in context. If that is the case, returns the current
  * context. Otherwise, frees context and gets a new context with
  * the new parameters.
  *
  * Be warned that srcFilter and dstFilter are not checked, they
  * are assumed to remain the same.
*)
// struct SwsContext *sws_getCachedContext(struct SwsContext *context,
// int srcW, int srcH, enum AVPixelFormat srcFormat,
// int dstW, int dstH, enum AVPixelFormat dstFormat,
// int flags, SwsFilter *srcFilter,
// SwsFilter *dstFilter, const double *param);

function sws_getCachedContext(context:pSwsContext;
 srcW:Integer; srcH:Integer; srcFormat:TAVPixelFormat;
 dstW:Integer; dstH:Integer; dstFormat:TAVPixelFormat;
 flags:Integer; srcFilter:pSwsFilter;
 dstFilter:pSwsFilter; const param:pDouble):pSwsContext;cdecl;

(*
  * Convert an 8-bit paletted frame into a frame with a color depth of 32 bits.
  *
  * The output frame will have the same {packed} format as the palette.
  *
  * @param src        source frame buffer
  * @param dst        destination frame buffer
  * @param num_pixels number of pixels to convert
  * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
*)
// void sws_convertPalette8Topacked32(const uint8_t *src, uint8_t *dst, int num_pixels, const uint8_t *palette);

(*
  * Convert an 8-bit paletted frame into a frame with a color depth of 24 bits.
  *
  * With the palette format "ABCD", the destination frame ends up with the format "ABC".
  *
  * @param src        source frame buffer
  * @param dst        destination frame buffer
  * @param num_pixels number of pixels to convert
  * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
*)
// void sws_convertPalette8Topacked24(const uint8_t *src, uint8_t *dst, int num_pixels, const uint8_t *palette);

(*
  * Get the AVClass for swsContext. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass *sws_get_class(void);

implementation

uses ffmpeglib;

{$IFDEF FF_API_SWS_GETCONTEXT}
function sws_getContext; external swscale_dll;
{$ENDIF}
function sws_scale; external swscale_dll;
procedure sws_freeContext; external swscale_dll;
function sws_getCachedContext; external swscale_dll;

end.

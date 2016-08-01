// *****************************************************************************
// Contrast Limited Adaptive Histogram Equalization (CLAHE) for OpenCV
// -----------------------------------------------------------------------------
// Original CLAHE implementation by Karel Zuiderveld, karel@cv.ruu.nl
// in 'Graphics Gems IV', Academic Press, 1994.
// -----------------------------------------------------------------------------
// Converted to OpenCV format by Toby Breckon, toby.breckon@cranfield.ac.uk
// Copyright (c) 2009 School of Engineering, Cranfield University
// License : LGPL - http://www.gnu.org/licenses/lgpl.html
// -----------------------------------------------------------------------------
// Improved by Shervin Emami on 17th Nov 2010, shervin.emami@gmail.com
// http://www.shervinemami.co.cc/
// ************************************************************************************************************************************************ *)*******************************
// Project Delphi-OpenCV
// **************************************************************************************************
// Contributor:
  // Laentir Valetov
// email:laex@bk.ru
// **************************************************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// **************************************************************************************************
// License:
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the 'License');
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an 'AS IS' basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU Lesser General Public License (the  'LGPL License'), in which case the
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
// The Initial Developer of the Original Code:
// OpenCV: open source computer vision library
// Homepage:    http://ocv.org
// Online docs: http://docs.ocv.org
// Q&A forum:   http://answers.ocv.org
// Dev zone:    http://code.ocv.org
// **************************************************************************************************
// Original: https://github.com/joshdoe/opencv-clahe/blob/master/clahe.cpp
// **************************************************************************************************

{$POINTERMATH ON}

unit uclahe;

interface

Uses
 core.types_c;

const
// CLAHE input/output range flag definitions

CV_CLAHE_RANGE_FULL =0;
CV_CLAHE_RANGE_INPUT= 1;

procedure cvCLAdaptEqualize(src:pIplImage; dst:pIplImage;
xdivs:Cardinal; ydivs:Cardinal; bins:Cardinal;
limit:float; range:Integer);

implementation

Uses
core_c,
imgproc_c;

type
// type defs. for Graphic Gemms Code - see later
kz_pixel_t = byte; (* for 8 bit-per-pixel images *)
pkz_pixel_t=^kz_pixel_t;
const
uiNR_OF_GREY = 256;

(************* Prototype of Graphic Gemms CLAHE function. *********************)


function CLAHE(pImage:pkz_pixel_t; uiXRes:Cardinal;
 uiYRes:Cardinal; Min:kz_pixel_t;
          Max:kz_pixel_t; uiNrX:Cardinal; uiNrY:Cardinal;
          uiNrBins:Cardinal; fCliplimit:float):Integer; forward;

// *****************************************************************************

// General Notes:
//
// The number of 'effective' greylevels in the output image is set by bins; selecting
// a small value (eg. 128) speeds up processing and still produce an output image of
// good quality. The output image will have the same minimum and maximum value as the input
// image. A clip limit smaller than 1 (?? is this correct ) results in
// standard (non-contrast limited) AHE.


// cvAdaptEqualize(src, dst, xdivs, ydivs, bins)
//
// perform adaptive histogram equalization (AHE)
//
// src - pointer to source image (must be single channel 8-bit)
// dst - pointer to destination image (must be single channel 8-bit)
// xdivs - number of cell divisions to use in vertical (x) direction (MIN=2 MAX := 16)
// ydivs - number of cell divisions to use in vertical (y) direction (MIN=2 MAX := 16)
// bins - number of histogram bins to use per division
// range - either of CV_CLAHE_RANGE_INPUT or CV_CLAHE_RANGE_FULL to limit the output
// pixel range to the min/max range of the input image or the full range of the
// pixel depth (8-bit in this case)

procedure cvAdaptEqualize(src:pIplImage; dst:pIplImage;
xdivs:Cardinal; ydivs:Cardinal; bins:Cardinal; range:Integer);
begin
// call CLAHE with negative limit (as flag value) to perform AHE (no limits)
cvCLAdaptEqualize(src, dst, xdivs, ydivs, bins, -1.0, range);
end;

// cvCLAdaptEqualize(src, dst, xdivs, ydivs, bins, limit)
//
// perform contrast limited adaptive histogram equalization (CLAHE)
//
// src - pointer to source image (must be single channel 8-bit)
// dst - pointer to destination image (must be single channel 8-bit)
// xdivs - number of cell divisions to use in vertical (x) direction (MIN=2 MAX := 16)
// ydivs - number of cell divisions to use in vertical (y) direction (MIN=2 MAX := 16)
// bins - number of histogram bins to use per division (MIN=2 MAX := 256)
// limit - contrast limit for localised changes in contrast
// (limit >= 0 gives standard AHE without contrast limiting)
// range - either of CV_CLAHE_RANGE_INPUT or CV_CLAHE_RANGE_FULL to limit the output
// pixel range to the min/max range of the input image or the full range of the
// pixel depth (8-bit in this case)


procedure cvCLAdaptEqualize(src:pIplImage; dst:pIplImage;
xdivs:Cardinal; ydivs:Cardinal; bins:Cardinal;
limit:float; range:Integer);
Var
  min_val, max_val:Double;
  min, max:Byte;
  _type:Integer;
  enlarged :Integer;
origW : integer;
origH :integer;
tmpDst :pIplImage;
newW:Integer;
newH:Integer;
enlargedDst:pIplImage;
begin
// check the inputs to the function

if (src = nil) or (dst = nil) then
        cvError( CV_StsUnsupportedFormat, 'cvCLAdaptEqualize','nil value passed as image to function',nil,0 );

     _type := cvGetElemType( src );
     if( _type <> CV_8UC1 ) then
         cvError( CV_StsUnsupportedFormat, 'cvCLAdaptEqualize', 'Only 8uC1 images are supported' );
_type := cvGetElemType( src );
     if( _type <> CV_8UC1 ) then
         cvError( CV_StsUnsupportedFormat, 'cvCLAdaptEqualize', 'Only 8uC1 images are supported' );

//if( !CV_ARE_SIZES_EQ( src, dst )) // Modified by Shervin Emami, 17Nov2010.
if (src^.width <> dst^.width )or( src^.height <> dst^.height) then
         cvError( CV_StsUnmatchedSizes, 'cvCLAdaptEqualize', 'src and dst images must be equal sizes' );

if (((xdivs < 2) )or( (xdivs > 16)) )or( ((ydivs < 2) )or( (ydivs > 16))) then
cvError( CV_StsBadFlag, 'cvCLAdaptEqualize', 'xdivs and ydivs must in range (MIN=2 MAX := 16)' );

if ((bins < 2) )or( (bins > 256)) then
cvError( CV_StsBadFlag, 'cvCLAdaptEqualize', 'bins must in range (MIN=2 MAX := 256)' );

// copy src to dst for in-place CLAHE.
cvCopy(src, dst);


// If the dimensions of the image are not a multiple of the xdivs and ydivs, then enlarge the image to be a correct size and then shrink the image.
// Also make sure the image is aligned to 8 pixels width, so that OpenCV won't add extra padding to the image.
// Added by Shervin Emami, 17Nov2010.
enlarged := 0;
origW := dst^.width;
origH := dst^.height;
tmpDst := nil;
if ((dst^.width and (8-1))<>0 )or( (dst^.height and (8-1))<>0 )or( (dst^.width mod xdivs)<>0 )or( (dst^.height mod ydivs)<>0) then begin
newW := ((dst^.width + 8-1) and -8);	// Align to 8 pixels, so that widthStep hopefully equals width.
newW := ((newW + xdivs-1) and -xdivs);	// Also align for CLAHE.
newH := ((dst^.height + ydivs-1) and -ydivs);
//std::cout << 'w=' << dst^.width << ', h=' << dst^.height << '. new w := ' << newW << ', h := ' << newH << std::endl;
enlargedDst := cvCreateImage(cvSize(newW, newH), dst^.depth, dst^.nChannels);
cvResize(dst, enlargedDst, CV_INTER_CUBIC);
//cvReleaseImage(&dst);
tmpDst := dst;
dst := enlargedDst;	// Use the enlarged image instead of the original dst image.
enlarged := 1;	// signal that we need to shrink later!
end;
// Check if OpenCV adds padding bytes on each row. Added by Shervin Emami, 17Nov2010.
if (dst^.width <> dst^.widthStep) then
cvError( CV_StsBadFlag, 'cvCLAdaptEqualize', 'dst^.widthStep should be the same as dst^.width. The IplImage has padding, so use a larger image.' );


// check number of xdivs and ydivs is a multiple of image dimensions
if (dst^.width mod xdivs)<>0 then
cvError( CV_StsBadFlag, 'cvCLAdaptEqualize', 'xdiv must be an integer multiple of image width ' );
if (dst^.height mod ydivs)<>0 then
cvError( CV_StsBadFlag, 'cvCLAdaptEqualize', 'ydiv must be an integer multiple of image height ' );

// get the min and max values of the image

if (range = CV_CLAHE_RANGE_INPUT) then begin
cvMinMaxLoc(dst, @min_val, @max_val);
min := Trunc(min_val);
max := Trunc(max_val);
end else begin
min := 0;
max := 255;
end;
// call CLHAHE for in-place CLAHE

//int rcode =
CLAHE(pkz_pixel_t(dst^.imageData), dst^.width,
dst^.height, min, max, xdivs, ydivs,
              bins, limit);

//printf('RCODE %i\n', rcode);

// If the dst image was enlarged to fit the alignment, then shrink it back now.
// Added by Shervin Emami, 17Nov2010.
if (enlarged<>0) then begin
//std::cout << 'w=' << dst^.width << ', h=' << dst^.height << '. orig w=' << origW << ', h=' << origH << std::endl;
cvResize(dst, tmpDst, CV_INTER_CUBIC);	// Shrink the enlarged image back into the original dst image.
cvReleaseImage(dst);	// Free the enlarged image.
end;
end;

// *****************************************************************************
(*
* ANSI C code from the article
* 'Contrast Limited Adaptive Histogram Equalization'
* by Karel Zuiderveld, karel@cv.ruu.nl
* in 'Graphics Gems IV', Academic Press, 1994
*
*
* These functions implement Contrast Limited Adaptive Histogram Equalization.
* The main routine (CLAHE) expects an input image that is stored contiguously in
* memory; the CLAHE output image overwrites the original input image and has the
* same minimum and maximum values (which must be provided by the user).
* This implementation assumes that the X- and Y image resolutions are an integer
* multiple of the X- and Y sizes of the contextual regions. A check on various other
* error conditions is performed.
*
* #define the symbol BYTE_IMAGE to make this implementation suitable for
* 8-bit images. The maximum number of contextual regions can be redefined
* by changing uiMAX_REG_X and/or uiMAX_REG_Y; the use of more than 256
* contextual regions is not recommended.
*
* The code is ANSI-C and is also C++ compliant.
*
* Author: Karel Zuiderveld, Computer Vision Research Group,
* Utrecht, The Netherlands (karel@cv.ruu.nl)
*)

(*

EULA: The Graphics Gems code is copyright-protected. In other words, you cannot
claim the text of the code as your own and resell it. Using the code is permitted
in any program, product, or library, non-commercial or commercial. Giving credit
is not required, though is a nice gesture. The code comes as-is, and if there are
any flaws or problems with any Gems code, nobody involved with Gems - authors,
editors, publishers, or webmasters - are to be held responsible. Basically,
don't be a jerk, and remember that anything free comes with no guarantee.

- http://tog.acm.org/resources/GraphicsGems/ (August 2009)

*)

(*********************** Local prototypes ************************)
procedure ClipHistogram (pulHistogram:PCardinal;
                     uiNrGreylevels:Cardinal; ulClipLimit:Cardinal);forward;
procedure MakeHistogram (pImage:pkz_pixel_t; uiXRes:Cardinal;
                uiSizeX:Cardinal; uiSizeY:Cardinal;
                pulHistogram:pCardinal;
                uiNrGreylevels:Cardinal; pLookupTable:pkz_pixel_t);forward;
procedure MapHistogram (pulHistogram:pCardinal;  Min:kz_pixel_t; Max:kz_pixel_t;
               uiNrGreylevels:Cardinal; ulNrOfPixels:Cardinal);forward;
procedure MakeLut (pLUT:pkz_pixel_t; Min:kz_pixel_t; Max:kz_pixel_t; uiNrBins:cardinal); forward;
procedure Interpolate (pImage:pkz_pixel_t; uiXRes:Integer; pulMapLU:pCardinal;
     pulMapRU:PCardinal; pulMapLB:pCardinal; pulMapRB:pCardinal;
     uiXSize:Cardinal; uiYSize:Cardinal; pLUT:pkz_pixel_t);forward;

// *****************************************************************************

(************** Start of actual code **************)

const
uiMAX_REG_X :Cardinal= 16; (* max. # contextual regions in x-direction *)
uiMAX_REG_Y :Cardinal= 16; (* max. # contextual regions in y-direction *)

(************************** main function CLAHE ******************)
function CLAHE(pImage:pkz_pixel_t; uiXRes:Cardinal;
 uiYRes:Cardinal; Min:kz_pixel_t;
          Max:kz_pixel_t; uiNrX:Cardinal; uiNrY:Cardinal;
          uiNrBins:Cardinal; fCliplimit:float):Integer;
(* pImage - Pointer to the input/output image
* uiXRes - Image resolution in the X direction
* uiYRes - Image resolution in the Y direction
* Min - Minimum greyvalue of input image (also becomes minimum of output image)
* Max - Maximum greyvalue of input image (also becomes maximum of output image)
* uiNrX - Number of contextial regions in the X direction (min 2, max uiMAX_REG_X)
* uiNrY - Number of contextial regions in the Y direction (min 2, max uiMAX_REG_Y)
* uiNrBins - Number of greybins for histogram ('dynamic range')
* float fCliplimit - Normalized cliplimit (higher values give more contrast)
* The number of 'effective' greylevels in the output image is set by uiNrBins; selecting
* a small value (eg. 128) speeds up processing and still produce an output image of
* good quality. The output image will have the same minimum and maximum value as the input
* image. A clip limit smaller than 1 results in standard (non-contrast limited) AHE.
*)
Var
    uiX, uiY:Cardinal; (* counters *)
    uiXSize, uiYSize, uiSubX, uiSubY:Cardinal; (* size of context. reg. and subimages *)
    uiXL, uiXR, uiYU, uiYB:Cardinal; (* auxiliary variables interpolation routine *)
    ulClipLimit, ulNrPixels:Cardinal;(* clip limit and region pixel count *)
    pImPointer:pkz_pixel_t; (* pointer to image *)
    aLUT:array[0..uiNR_OF_GREY] of kz_pixel_t; (* lookup table used for scaling of input image *)
    pulHist, pulMapArray:pCardinal; (* pointer to histogram and mappings*)
    pulLU, pulLB, pulRU, pulRB:pCardinal; (* auxiliary pointers interpolation *)


begin
    if (uiNrX > uiMAX_REG_X) then Exit(-1); (* # of regions x-direction too large *)
    if (uiNrY > uiMAX_REG_Y) then Exit(-2); (* # of regions y-direction too large *)
    if (uiXRes mod uiNrX)<>0 then Exit(-3); (* x-resolution no multiple of uiNrX *)
    if (uiYRes mod uiNrY)<>0 then Exit(-4); (* y-resolution no multiple of uiNrY #TPB FIX *)
//    #ifndef BYTE_IMAGE	(* #TPB FIX *)
//if (Max >= uiNR_OF_GREY) return -5; (* maximum too large *)
//#endif
    if (Min >= Max) then Exit(-6); (* minimum equal or larger than maximum *)
    if (uiNrX < 2 )or( uiNrY < 2) then Exit(-7);(* at least 4 contextual regions required *)
    if (fCliplimit = 1.0) then Exit(0); (* is OK, immediately returns original image. *)
    if (uiNrBins = 0) then uiNrBins := 128; (* default value when not specified *)

    GetMem(pulMapArray,sizeof(Cardinal)*uiNrX*uiNrY*uiNrBins);
    if (pulMapArray = nil) then Exit(-8); (* Not enough memory! (try reducing uiNrBins) *)

    uiXSize := uiXRes div uiNrX;
    uiYSize := uiYRes div uiNrY; (* Actual size of contextual regions *)
    ulNrPixels := uiXSize * uiYSize;

    if(fCliplimit > 0.0) then begin (* Calculate actual cliplimit *)
       ulClipLimit :=  Trunc(fCliplimit * (uiXSize * uiYSize) / uiNrBins);
       if ulClipLimit < 1 then
       ulClipLimit := 1;
    end
    else ulClipLimit := 1 shl 14; (* Large value, do not clip (AHE) *)
    MakeLut(@aLUT, Min, Max, uiNrBins); (* Make lookup table for mapping of greyvalues *)
    (* Calculate greylevel mappings for each contextual region *)
    pImPointer := pImage;
    for uiY := 0 to uiNrY-1 do
    begin
        for uiX := 0 to uiNrX-1 do
        begin
            pulHist := @pulMapArray[uiNrBins * (uiY * uiNrX + uiX)];
            MakeHistogram(pImPointer,uiXRes,uiXSize,uiYSize,pulHist,uiNrBins,@aLUT);
            ClipHistogram(pulHist, uiNrBins, ulClipLimit);
            MapHistogram(pulHist, Min, Max, uiNrBins, ulNrPixels);
            pImPointer :=pImPointer+ uiXSize;
        end;
        pImPointer :=pImPointer+ (uiYSize - 1) * uiXRes; (* skip lines, set pointer *)
    end;

    (* Interpolate greylevel mappings to get CLAHE image *)
    pImPointer := pImage;
    for uiY := 0 to uiNrY do begin
        if (uiY = 0) then begin (* special case: top row *)
            uiSubY := uiYSize shr 1;
            uiYU := 0;
            uiYB := 0;
        end
        else begin
            if (uiY = uiNrY) then begin (* special case: bottom row *)
                uiSubY := uiYSize shr 1;
                uiYU := uiNrY-1;
                uiYB := uiYU;
            end
            else begin (* default values *)
                uiSubY := uiYSize;
                uiYU := uiY - 1;
                uiYB := uiYU + 1;
            end;
        end;
        for uiX := 0 to uiNrX do begin
            if (uiX = 0) then begin (* special case: left column *)
                uiSubX := uiXSize shr 1; uiXL := 0; uiXR := 0;
            end
            else begin
                if (uiX = uiNrX) then begin (* special case: right column *)
                    uiSubX := uiXSize shr 1; uiXL := uiNrX - 1; uiXR := uiXL;
                end
                else begin (* default values *)
                    uiSubX := uiXSize; uiXL := uiX - 1; uiXR := uiXL + 1;
                end;
            end;

            pulLU := @pulMapArray[uiNrBins * (uiYU * uiNrX + uiXL)];
            pulRU := @pulMapArray[uiNrBins * (uiYU * uiNrX + uiXR)];
            pulLB := @pulMapArray[uiNrBins * (uiYB * uiNrX + uiXL)];
            pulRB := @pulMapArray[uiNrBins * (uiYB * uiNrX + uiXR)];
            Interpolate(pImPointer,uiXRes,pulLU,pulRU,pulLB,pulRB,uiSubX,uiSubY,@aLUT);
            pImPointer :=pImPointer+ uiSubX; (* set pointer on next matrix *)
        end;
        pImPointer :=pImPointer+ (uiSubY - 1) * uiXRes;
    end;
    freemem(pulMapArray); (* free space for histograms *)
    Result:=0; (* return status OK *)
end;


procedure ClipHistogram (pulHistogram:PCardinal;
                     uiNrGreylevels:Cardinal; ulClipLimit:Cardinal);
(* This function performs clipping of the histogram and redistribution of bins.
* The histogram is clipped and the number of excess pixels is counted. Afterwards
* the excess pixels are equally redistributed across the whole histogram (providing
* the bin count is smaller than the cliplimit).
*)
Var
    pulBinPointer, pulEndPointer, pulHisto:pCardinal;
    ulNrExcess, ulUpper, ulBinIncr, ulStepSize, i:Cardinal;
    ulOldNrExcess:CArdinal; // #IAC Modification

    lBinExcess:Integer;
begin

    ulNrExcess := 0; pulBinPointer := pulHistogram;
    for i := 0 to uiNrGreylevels-1 do
    begin (* calculate total number of excess pixels *)
        lBinExcess :=  Integer(Int64(pulBinPointer[i]) - Int64(ulClipLimit));
        if (lBinExcess > 0) then ulNrExcess :=ulNrExcess+lBinExcess; (* excess in current bin *)
    end;;

    (* Second part: clip histogram and redistribute excess pixels in each bin *)
    ulBinIncr := ulNrExcess div uiNrGreylevels; (* average binincrement *)
    ulUpper := ulClipLimit - ulBinIncr; (* Bins larger than ulUpper set to cliplimit *)

    for i := 0 to uiNrGreylevels-1 do begin
      if (pulHistogram[i] > ulClipLimit) then pulHistogram[i] := ulClipLimit (* clip bin *)
      else begin
          if (pulHistogram[i] > ulUpper) then begin (* high bin count *)
              ulNrExcess :=ulNrExcess- pulHistogram[i] - ulUpper; pulHistogram[i]:=ulClipLimit;
          end
          else begin (* low bin count *)
              ulNrExcess :=ulNrExcess- ulBinIncr; pulHistogram[i] :=pulHistogram[i]+ ulBinIncr;
          end;
       end;
    end;

    // while (ulNrExcess) begin (* Redistribute remaining excess *)
    // pulEndPointer := &pulHistogram[uiNrGreylevels]; pulHisto := pulHistogram;
//
    // while (ulNrExcess )and( pulHisto < pulEndPointer) begin
    // ulStepSize := uiNrGreylevels / ulNrExcess;
    // if (ulStepSize < 1) ulStepSize := 1; (* stepsize at least 1 *)
    // for (pulBinPointer=pulHisto; pulBinPointer < pulEndPointer )and( ulNrExcess;
    // pulBinPointer += ulStepSize) begin
    // if (*pulBinPointer < ulClipLimit) begin
    // (*pulBinPointer)++; ulNrExcess--; (* reduce excess *)
    // end;
    // end;
    // pulHisto++; (* restart redistributing on other bin location *)
    // end;
    //end;

(* ####
IAC Modification:
In the original version of the loop below (commented out above) it was possible for an infinite loop to get
created. If there was more pixels to be redistributed than available space then the
while loop would never end. This problem has been fixed by stopping the loop when all
pixels have been redistributed OR when no pixels where redistributed in the previous iteration.
This change allows very low clipping levels to be used.
*)

     repeat (* Redistribute remaining excess *)
         pulEndPointer := @pulHistogram[uiNrGreylevels]; pulHisto := pulHistogram;

         ulOldNrExcess := ulNrExcess; (* Store number of excess pixels for test later. *)

         while (ulNrExcess<>0)and( pulHisto < pulEndPointer) do
         begin
             ulStepSize := uiNrGreylevels div ulNrExcess;
             if (ulStepSize < 1) then
                 ulStepSize := 1; (* stepsize at least 1 *)
             pulBinPointer:=pulHisto;
             while (pulBinPointer < pulEndPointer )and( ulNrExcess<>0) do
             begin
                 if (pulBinPointer^ < ulClipLimit) then
                 begin
                     Inc(pulBinPointer^); Dec(ulNrExcess); (* reduce excess *)
                 end;
              pulBinPointer :=pulBinPointer+ ulStepSize;
            end;
             Inc(pulHisto); (* restart redistributing on other bin location *)
         end;
     until (ulNrExcess=0) or (ulNrExcess >= ulOldNrExcess);
     (* Finish loop when we have no more pixels or we can't redistribute any more pixels *)


end;

procedure MakeHistogram (pImage:pkz_pixel_t; uiXRes:Cardinal;
                uiSizeX:Cardinal; uiSizeY:Cardinal;
                pulHistogram:pCardinal;
                uiNrGreylevels:Cardinal; pLookupTable:pkz_pixel_t);
(* This function classifies the greylevels present in the array image into
* a greylevel histogram. The pLookupTable specifies the relationship
* between the greyvalue of the pixel (typically between 0 and 4095) and
* the corresponding bin in the histogram (usually containing only 128 bins).
*)
Var
  pImagePointer:pkz_pixel_t;
    i:Cardinal;
begin
    for i := 0 to uiNrGreylevels-1 do pulHistogram[i] := 0; (* clear histogram *)

    for i := 0 to uiSizeY-1 do begin
        pImagePointer := @pImage[uiSizeX];
        while (pImage < pImagePointer) do
        begin
        Inc(pulHistogram[pLookupTable[pImage^]]);
        Inc(pImage);
        end;
        pImagePointer := pImagePointer+uiXRes;
        pImage := @pImagePointer[-uiSizeX];
    end;
end;

procedure MapHistogram (pulHistogram:pCardinal;  Min:kz_pixel_t; Max:kz_pixel_t;
               uiNrGreylevels:Cardinal; ulNrOfPixels:Cardinal);
(* This function calculates the equalized lookup table (mapping) by
* cumulating the input histogram. Note: lookup table is rescaled in range [Min..Max].
*)
Var
  i, ulSum :Cardinal;
  fScale:float;
  ulMin:CArdinal;
begin
    ulSum := 0;
    fScale := (Max - Min) / ulNrOfPixels;
    ulMin := Min;

    for i := 0 to uiNrGreylevels-1 do
    begin
        ulSum :=ulSum+ pulHistogram[i]; pulHistogram[i]:=Trunc(ulMin+ulSum*fScale);
        if (pulHistogram[i] > Max) then pulHistogram[i] := Max;
    end;
end;

procedure MakeLut (pLUT:pkz_pixel_t; Min:kz_pixel_t; Max:kz_pixel_t; uiNrBins:cardinal);
(* To speed up histogram clipping, the input image [Min,Max] is scaled down to
* [0,uiNrBins-1]. This function calculates the LUT.
*)
Var
 i:Integer;
    BinSize:kz_pixel_t;
begin
    BinSize := Trunc((1 + (Max - Min) / uiNrBins));

    for i := Min to Max do pLUT[i] := (i - Min) div BinSize;
end;

procedure Interpolate (pImage:pkz_pixel_t; uiXRes:Integer; pulMapLU:pCardinal;
     pulMapRU:PCardinal; pulMapLB:pCardinal; pulMapRB:pCardinal;
     uiXSize:Cardinal; uiYSize:Cardinal; pLUT:pkz_pixel_t);
(* pImage - pointer to input/output image
* uiXRes - resolution of image in x-direction
* pulMap* - mappings of greylevels from histograms
* uiXSize - uiXSize of image submatrix
* uiYSize - uiYSize of image submatrix
* pLUT - lookup table containing mapping greyvalues to bins
* This function calculates the new greylevel assignments of pixels within a submatrix
* of the image with size uiXSize and uiYSize. This is done by a bilinear interpolation
* between four different mappings in order to eliminate boundary artifacts.
* It uses a division; since division is often an expensive operation, I added code to
* perform a logical shift instead when feasible.
*)
VAr
 uiIncr:Cardinal;
 GreyValue:kz_pixel_t;
 uiNum:Cardinal;

 uiXCoef, uiYCoef, uiXInvCoef, uiYInvCoef, uiShift:Cardinal;
begin
    uiIncr := uiXRes-uiXSize; (* Pointer increment after processing row *)
    uiNum := uiXSize*uiYSize; (* Normalization factor *)

    uiShift := 0;

    if (uiNum and (uiNum - 1))<>0 then (* If uiNum is not a power of two, use division *)
    begin
     uiYInvCoef := uiYSize;
    for uiYCoef := 0 to uiYSize-1 do
    begin
        uiXInvCoef := uiXSize;
        for uiXCoef := 0 to uiXSize-1 do
        begin
            GreyValue := pLUT[pImage^]; (* get histogram bin value *)
            pImage^ := ((uiYInvCoef * (uiXInvCoef*pulMapLU[GreyValue]
                                      + uiXCoef * pulMapRU[GreyValue])
                                + uiYCoef * (uiXInvCoef * pulMapLB[GreyValue]
                                      + uiXCoef * pulMapRB[GreyValue])) div uiNum);
        Inc(pImage);
        Dec(uiXInvCoef);
        end;
        Dec(uiYInvCoef);
        pImage :=pImage+uiIncr;
    end;
    end
    else begin (* avoid the division and use a right shift instead *)
        while uiNum <>0 do (* Calculate 2log of uiNum *)
        begin
        Inc(uiShift);
        uiNum:=uiNum shr 1;
        end;
        uiYInvCoef := uiYSize;
        for uiYCoef := 0 to uiYSize-1 do
        begin
             uiXInvCoef := uiXSize;
             for uiXCoef := 0 to uiXSize-1 do
             begin
               GreyValue := pLUT[pImage^]; (* get histogram bin value *)
               pImage^ := ((uiYInvCoef* (uiXInvCoef * pulMapLU[GreyValue]
                                      + uiXCoef * pulMapRU[GreyValue])
                                + uiYCoef * (uiXInvCoef * pulMapLB[GreyValue]
                                      + uiXCoef * pulMapRB[GreyValue])) shr uiShift);
             Inc(pImage);
             Dec(uiXInvCoef);
            end;
            Dec(uiYInvCoef);
            pImage:=pImage+uiIncr;
        end;
    end;
end;

end.

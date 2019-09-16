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
  opencv\modules\photo\include\opencv2\photo\photo_c.h
  *************************************************************************************************
*)

{$I OpenCV.inc}
unit ocv.photo_c;

interface

uses ocv.core_c, ocv.core.types_c;

const
  (*
    Inpainting algorithms
  *)
  CV_INPAINT_NS = 0;
  CV_INPAINT_TELEA = 1;

  (*
    Inpaints the selected region in the image

    CVAPI(void) cvInpaint( const CvArr* src, const CvArr* inpaint_mask,
    CvArr* dst, double inpaintRange, int flags );
  *)

{$IFDEF SAFELOADLIB}

Type
  TcvInpaint = procedure(const src: pCvArr; const inpaint_mask: pCvArr; dst: pCvArr; inpaintRange: double; flags: Integer); cdecl;

Var
  cvInpaint: TcvInpaint = nil;
{$ELSE}
procedure cvInpaint(const src: pCvArr; const inpaint_mask: pCvArr; dst: pCvArr; inpaintRange: double; flags: Integer); cdecl;
{$ENDIF}

{$IF DEFINED(SAFELOADLIB) AND DEFINED(DEBUG)}
procedure Init_opencv_photo_lib;
{$ENDIF}

implementation

uses ocv.lib;

{$IFDEF SAFELOADLIB}

Var
  PhotoDLL: Cardinal;

procedure Init_opencv_photo_lib;
begin
  PhotoDLL := ocvLoadLibrary(opencv_photo_lib);
  Assert(PhotoDLL <> 0, 'Can not init ' + opencv_photo_lib);
  cvInpaint := ocvGetProcAddress('cvInpaint', PhotoDLL);
end;

initialization

Init_opencv_photo_lib;

{$ELSE}
procedure cvInpaint(const src: pCvArr; const inpaint_mask: pCvArr; dst: pCvArr; inpaintRange: double; flags: Integer); cdecl; external opencv_photo_lib;
{$ENDIF}

end.

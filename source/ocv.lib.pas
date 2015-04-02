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

*)
unit ocv.lib;

interface

const

  CV_VERSION_EPOCH    = '2';
  CV_VERSION_MAJOR    = '4';
  CV_VERSION_MINOR    = '11';
  CV_VERSION_REVISION = '0';

  CV_VERSION = CV_VERSION_EPOCH + '.' + CV_VERSION_MAJOR + '.' + CV_VERSION_MINOR + '.' + CV_VERSION_REVISION;

  // * old  style version constants*/
  CV_MAJOR_VERSION    = CV_VERSION_EPOCH;
  CV_MINOR_VERSION    = CV_VERSION_MAJOR;
  CV_SUBMINOR_VERSION = CV_VERSION_MINOR;

  CV_VERSION_DLL = CV_VERSION_EPOCH + CV_VERSION_MAJOR + CV_VERSION_MINOR;

{$IFDEF MSWINDOWS}
{$IFDEF CPUX86}
  CV_DLL_DIR = ''; // 'Win32\';
{$ELSE}
  CV_DLL_DIR = ''; // 'Win64\';
{$ENDIF}
{$ELSE}
{$ENDIF}
  core_lib = {$IFDEF MSWINDOWS}
    CV_DLL_DIR + 'opencv_core' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_core.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_core.so';
{$ELSE}
  'libopencv_core.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
highgui_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_highgui' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_highgui.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_highgui.so';
{$ELSE}
  'libopencv_highgui.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
imgproc_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_imgproc' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_imgproc.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_imgproc.so';
{$ELSE}
  'libopencv_imgproc.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
objdetect_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_objdetect' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_objdetect.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_objdetect.so';
{$ELSE}
  'libopencv_objdetect.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
legacy_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_legacy' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_legacy.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_legacy.so';
{$ELSE}
  'libopencv_legacy.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
calib3d_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_calib3d' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_calib3d.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_calib3d.so';
{$ELSE}
  'libopencv_calib3d.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
tracking_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_video' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_video.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_video.so';
{$ELSE}
  'libopencv_video.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
nonfree_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_nonfree' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_nonfree.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_nonfree.so';
{$ELSE}
  'libopencv_nonfree.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
opencv_classes_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_classes' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_classes.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_classes.so';
{$ELSE}
  'libopencv_classes.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
opencv_photo_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_photo' + CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_photo.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_photo.so';
{$ELSE}
  'libopencv_photo.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

end.

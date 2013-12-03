// **************************************************************************************************
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
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU Lesser General Public License (the  "LGPL License"), in which case the
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

unit uLibName;

interface

const

  CV_VERSION_EPOCH    = '2';
  CV_VERSION_MAJOR    = '4';
  CV_VERSION_MINOR    = '7';
  CV_VERSION_REVISION = '0';

  CV_VERSION = CV_VERSION_EPOCH + '.' + CV_VERSION_MAJOR + '.' + CV_VERSION_MINOR + '.' + CV_VERSION_REVISION;

  // * old  style version constants*/
  CV_MAJOR_VERSION    = CV_VERSION_EPOCH;
  CV_MINOR_VERSION    = CV_VERSION_MAJOR;
  CV_SUBMINOR_VERSION = CV_VERSION_MINOR;

  CV_VERSION_DLL = CV_VERSION_EPOCH + CV_VERSION_MAJOR + CV_VERSION_MINOR;

{$IFDEF DEBUG}
  Core_Dll           = 'opencv_core' + CV_VERSION_DLL + 'd.dll';
  highgui_Dll        = 'opencv_highgui' + CV_VERSION_DLL + 'd.dll';
  imgproc_Dll        = 'opencv_imgproc' + CV_VERSION_DLL + 'd.dll';
  objdetect_dll      = 'opencv_objdetect' + CV_VERSION_DLL + 'd.dll';
  legacy_dll         = 'opencv_legacy' + CV_VERSION_DLL + 'd.dll';
  calib3d_dll        = 'opencv_calib3d' + CV_VERSION_DLL + 'd.dll';
  tracking_DLL       = 'opencv_video' + CV_VERSION_DLL + 'd.dll';
  Nonfree_DLL        = 'opencv_nonfree' + CV_VERSION_DLL + 'd.dll';
  OpenCV_Classes_DLL = 'OpenCV_Classes.dll';
{$ELSE}
  Core_Dll           = 'opencv_core' + CV_VERSION_DLL + '.dll';
  highgui_Dll        = 'opencv_highgui' + CV_VERSION_DLL + '.dll';
  imgproc_Dll        = 'opencv_imgproc' + CV_VERSION_DLL + '.dll';
  objdetect_dll      = 'opencv_objdetect' + CV_VERSION_DLL + '.dll';
  legacy_dll         = 'opencv_legacy' + CV_VERSION_DLL + '.dll';
  calib3d_dll        = 'opencv_calib3d' + CV_VERSION_DLL + '.dll';
  tracking_DLL       = 'opencv_video' + CV_VERSION_DLL + '.dll';
  Nonfree_DLL        = 'opencv_nonfree' + CV_VERSION_DLL + '.dll';
  OpenCV_Classes_DLL = 'OpenCV_Classes.dll';
{$ENDIF}

implementation

end.

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
  //  ************************************************************************************************* *)
unit uLibName;

interface

const
  CV_Version = '244';

{$IFDEF DEBUG}
  Core_Dll = 'opencv_core' + CV_Version + 'd.dll';
  highgui_Dll = 'opencv_highgui' + CV_Version + 'd.dll';
  imgproc_Dll = 'opencv_imgproc' + CV_Version + 'd.dll';
  objdetect_dll = 'opencv_objdetect' + CV_Version + 'd.dll';
  legacy_dll = 'opencv_legacy' + CV_Version + 'd.dll';
  calib3d_dll = 'opencv_calib3d' + CV_Version + 'd.dll';
  tracking_DLL = 'opencv_video' + CV_Version + 'd.dll';
{$ELSE}
  Core_Dll = 'opencv_core' + CV_Version + '.dll';
  highgui_Dll = 'opencv_highgui' + CV_Version + '.dll';
  imgproc_Dll = 'opencv_imgproc' + CV_Version + '.dll';
  objdetect_dll = 'opencv_objdetect' + CV_Version + '.dll';
  legacy_dll = 'opencv_legacy' + CV_Version + '.dll';
  calib3d_dll = 'opencv_calib3d' + CV_Version + '.dll';
  tracking_DLL = 'opencv_video' + CV_Version + '.dll';
{$ENDIF}

implementation

end.

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

*)

unit ocv.lib;

{$I OpenCV.inc}

interface

const
{$IFDEF DelphiOCVVersion_29}
  CV_VERSION_EPOCH = '2';
  CV_VERSION_MAJOR = '4';
  CV_VERSION_MINOR = '13';
  CV_VERSION_REVISION = '3';
{$ELSE}
{$IFDEF DelphiOCVVersion_30}
  CV_VERSION_EPOCH = '3';
  CV_VERSION_MAJOR = '0';
  CV_VERSION_MINOR = '0';
  CV_VERSION_REVISION = '0';
{$ENDIF}
{$ENDIF}
  CV_VERSION = CV_VERSION_EPOCH + '.' + CV_VERSION_MAJOR + '.' + CV_VERSION_MINOR + '.' + CV_VERSION_REVISION;

  // * old  style version constants*/
  CV_MAJOR_VERSION = CV_VERSION_EPOCH;
  CV_MINOR_VERSION = CV_VERSION_MAJOR;
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
  // -------------------------------
  core_lib =
{$IFDEF MSWINDOWS}
    CV_DLL_DIR + 'opencv_' +
 {$IF DEFINED(DelphiOCVVersion_29)}
    'core' +
 {$ELSEIF DEFINED(DelphiOCVVersion_30)}
    'world' +
 {$ENDIF}
    CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
highgui_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'highgui' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
features2d_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'features2d' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_features2d.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_features2d.so';
{$ELSE}
  'libopencv_features2d.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
// -------------------------------
imgproc_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'imgproc' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
objdetect_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'objdetect' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
legacy_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'legacy' +
{$ELSEIF DEFINED(DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
calib3d_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'calib3d' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
tracking_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'video' +
{$ELSEIF DEFINED(DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
nonfree_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'nonfree' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
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
// -------------------------------
opencv_photo_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'photo' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
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
// -------------------------------
opencv_contrib_lib = {$IFDEF MSWINDOWS}
  CV_DLL_DIR + 'opencv_' +
{$IF DEFINED(DelphiOCVVersion_29)}
  'contrib' +
{$ELSEIF DEFINED( DelphiOCVVersion_30)}
  'world' +
{$IFEND}
  CV_VERSION_DLL {$IFDEF DEBUG} + 'd'{$ENDIF} + '.dll';
{$ELSE}
{$IFDEF MACOS}
  'opencv_contrib.dylib';
{$ELSE}
{$IFDEF ANDROID}
  'libopencv_contrib.so';
{$ELSE}
  'libopencv_contrib.so';
{$ENDIF}
{$ENDIF}
{$ENDIF}
// -------------------------------
//
//
// -------------------------------
//
{$IFDEF SAFELOADLIB}
function ocvLoadLibrary(const Name: String): Cardinal;
function ocvFreeLibrary(const LibHandle: Cardinal; const Remove: Boolean = true): Boolean;
function ocvGetProcAddress(const ProcName: String; const LibHandle: Cardinal = 0; const Check: Boolean = true): Pointer;
procedure ocvErrorMessage(const ErrorText: String);
{$ENDIF}

implementation

{$IFDEF SAFELOADLIB}

Uses
  Winapi.Windows,
  System.Generics.Collections,
  ocv.utils;

procedure ocvErrorMessage(const ErrorText: String);
begin
  if IsConsole then
  begin
    Writeln(ErrorText);
    Writeln('Press ENTER for exit');
    Readln;
  end
  else
    MessageBox(0, LPCWSTR(ErrorText), 'Error', MB_OK);
  Halt(1);
end;

{$IFDEF USE_STUB_FOR_MISS_FUNC}
procedure STUB_PROC;
begin
  ocvErrorMessage('STUB: Call missing functions');
end;
{$ENDIF}

Type
  TOCVLibHandles = TDictionary<String, Cardinal>;

Var
  OCVLibHandles: TOCVLibHandles;

function ocvLoadLibrary(const Name: String): Cardinal;
begin
  if not OCVLibHandles.TryGetValue(Name, Result) then
  begin
{$R-}
    Result := LoadLibrary(LPCWSTR(@Name[1]));
{$R+}
    if Result = 0 then
      ocvErrorMessage('Can not load DLL: ' + Name);
    OCVLibHandles.Add(Name, Result);
  end;
end;

function ocvFreeLibrary(const LibHandle: Cardinal; const Remove: Boolean): Boolean;
Var
  P: TPair<String, Cardinal>;
begin
  if LibHandle = 0 then
    Result := False
  else
  begin
    Result := FreeLibrary(LibHandle);
    if Remove and OCVLibHandles.ContainsValue(LibHandle) then
      for P in OCVLibHandles do
        if P.Value = LibHandle then
        begin
          OCVLibHandles.Remove(P.Key);
          Break;
        end;
  end;
end;

function ocvGetProcAddress(const ProcName: String; const LibHandle: Cardinal; const Check: Boolean): Pointer;
begin
  if LibHandle = 0 then
    Result := nil
  else
    Result := GetProcAddress(LibHandle, c_str(ProcName));
  if not Assigned(Result) then
{$IFDEF USE_STUB_FOR_MISS_FUNC}
    Result := @STUB_PROC;
{$ELSE}
    ocvErrorMessage('Can not load procedure or function: ' + ProcName);
{$ENDIF}
end;

procedure ocvFreeLibraries;
Var
  P: TPair<String, Cardinal>;
begin
  for P in OCVLibHandles do
    ocvFreeLibrary(P.Value, False);
  OCVLibHandles.Clear;
end;

initialization

OCVLibHandles := TOCVLibHandles.Create;

finalization

ocvFreeLibraries;
OCVLibHandles.Free;

{$ENDIF}

end.

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
  Original file:
  opencv\modules\highgui\include\opencv2\highgui.hpp
  *************************************************************************************************
*)

unit ocv.highgui;

{$I OpenCV.inc}

interface

uses
//{$IFDEF MSWINDOWS}
//  Winapi.Windows,
//{$ENDIF MSWINDOWS}
  ocv.mat,
  ocv.core.types_c,
  ocv.highgui_c;

type

  // Attention!
  // The sequence of function declarations interface must match the
  // sequence of function declarations in the project "opencv_classes" (C++)

  TccvVideoCapture = class
    function open(device: Integer): boolean; overload; virtual; stdcall; abstract;
    function openfile(filename: pAnsiChar): boolean; overload; virtual; stdcall; abstract;
    function isOpened(): boolean; virtual; stdcall; abstract;
    procedure release(); virtual; stdcall; abstract;
    function grab(): boolean; virtual; stdcall; abstract;
    function retrieve(Var image: TccvMat; flag: Integer): boolean; virtual; stdcall; abstract;
    function read(Var image: TccvMat): boolean; virtual; stdcall; abstract;
    function setValue(propId: Integer; value: double): boolean; virtual; stdcall; abstract;
    function getValue(propId: Integer): double; virtual; stdcall; abstract;
    // ---------------------------
    class function Create: TccvVideoCapture; overload;
    class function Create(device: Integer): TccvVideoCapture; overload;
    class function Create(filename: pAnsiChar): TccvVideoCapture; overload;
    procedure Free; reintroduce;
  end;

  // Flags for namedWindow
const
  WINDOW_NORMAL = $00000000;
  // the user can resize the window (no constraint) / also use to switch a fullscreen window to a normal size
  WINDOW_AUTOSIZE = $00000001; // the user cannot resize the window, the size is constrainted by the image displayed
  WINDOW_OPENGL   = $00001000; // window with opengl support

  WINDOW_FULLSCREEN = 1; // change the window to fullscreen
  WINDOW_FREERATIO  = $00000100; // the image expends as much as it can (no ratio constraint)
  WINDOW_KEEPRATIO  = $00000000; // the ratio of the image is respected

  // CV_EXPORTS_W void namedWindow(const String& winname, int flags = WINDOW_AUTOSIZE);
procedure namedWindow(const winname: String; const flags: Integer = WINDOW_AUTOSIZE);
// CV_EXPORTS_W void destroyWindow(const String& winname);
procedure destroyWindow(const winname: String);
// CV_EXPORTS_W void destroyAllWindows();
procedure destroyAllWindows();
// CV_EXPORTS_W int startWindowThread();
function startWindowThread(): Integer;
// CV_EXPORTS_W int waitKey(int delay = 0);
function waitKey(const delay: Integer = 0): Integer;
// CV_EXPORTS_W void imshow(const String& winname, InputArray mat);
procedure imshow(const winname: String; const mat: TccvMat);
// CV_EXPORTS_W void resizeWindow(const String& winname, int width, int height);
procedure resizeWindow(const winname: String; const width, height: Integer);
// CV_EXPORTS_W void moveWindow(const String& winname, int x, int y);
procedure moveWindow(const winname: String; const x, y: Integer);
// CV_EXPORTS_W void setWindowProperty(const String& winname, int prop_id, double prop_value);
procedure setWindowProperty(const winname: String; const prop_id: Integer; const prop_value: double);
// CV_EXPORTS_W double getWindowProperty(const String& winname, int prop_id);
function getWindowProperty(const winname: String; const prop_id: Integer): double;
// CV_EXPORTS int createTrackbar(const String& trackbarname, const String& winname,
// int* value, int count,
// TrackbarCallback onChange = 0,
// void* userdata = 0);
function createTrackbar(const trackbarname: String; const winname: String; value: PInteger; count: Integer;
  onChange: CvTrackbarCallback2 = nil; userdata: Pointer = nil): Integer;

// CV_EXPORTS_W Mat imread( const string& filename, int flags=1 );
function imread(const filename: string; flag: Integer = 1): TccvMat;
// CV_EXPORTS_W boolean imwrite( const string& filename, InputArray img, const vector<int>& params=vector<int>());
// function imwrite(const filename: String; const img: TccvMat): boolean;

type
  TIplImageRecordHelper = record helper for TIplImage
    function InitFromMat(const mat: TccvMat): TIplImage;
  end;

implementation

uses
  ocv.lib,
  ocv.utils,
  ocv.core_c;

function CreateVideoCapture: TccvVideoCapture; stdcall; external opencv_classes_lib name 'CreateVideoCapture'; overload;
function CreateVideoCapture(device: Integer): TccvVideoCapture; stdcall;
  external opencv_classes_lib name 'CreateVideoCaptureDevice'; overload;
function CreateVideoCapture(filename: pAnsiChar): TccvVideoCapture; stdcall;
  external opencv_classes_lib name 'CreateVideoCaptureFileName'; overload;
procedure ReleaseVideoCapture(ex: TccvVideoCapture); stdcall; external opencv_classes_lib;

// function _imread(const filename: pCvChar; flag: Integer): TccvMat; external opencv_classes_lib name '_imread';
// function _imwrite(const filename: pCvChar; const img: TccvMat): boolean; external opencv_classes_lib name '_imwrite';

function imread(const filename: string; flag: Integer): TccvMat;
begin
  // Result := _imread(c_str(filename), flag);
  Result := TccvMat.Create(cvLoadImage(pAnsiChar(AnsiString(filename)), flag));
end;

// function imwrite(const filename: String; const img: TccvMat): boolean;
// begin
// Result := _imwrite(c_str(filename), img);
// end;

procedure namedWindow(const winname: String; const flags: Integer = WINDOW_AUTOSIZE);
begin
  cvNamedWindow(c_str(winname), flags);
end;

procedure destroyWindow(const winname: String);
begin
  cvDestroyWindow(c_str(winname));
end;

procedure destroyAllWindows();
begin
  cvDestroyAllWindows();
end;

function startWindowThread(): Integer;
begin
  Result := cvStartWindowThread();
end;

function waitKey(const delay: Integer = 0): Integer;
begin
  Result := cvWaitKey(delay);
end;

procedure imshow(const winname: String; const mat: TccvMat);
Var
  IplImage: TIplImage;
begin
  IplImage.InitFromMat(mat);
  cvShowImage(c_str(winname), @IplImage);
end;

procedure resizeWindow(const winname: String; const width, height: Integer);
begin
  cvResizeWindow(c_str(winname), width, height);
end;

procedure moveWindow(const winname: String; const x, y: Integer);
begin
  cvMoveWindow(c_str(winname), x, y);
end;

procedure setWindowProperty(const winname: String; const prop_id: Integer; const prop_value: double);
begin
  cvSetWindowProperty(c_str(winname), prop_id, prop_value);
end;

function getWindowProperty(const winname: String; const prop_id: Integer): double;
begin
  Result := cvGetWindowProperty(c_str(winname), prop_id);
end;

function createTrackbar(const trackbarname: String; const winname: String; value: PInteger; count: Integer;
  onChange: CvTrackbarCallback2 = nil; userdata: Pointer = nil): Integer;
begin
  Result := cvCreateTrackbar2(c_str(trackbarname), c_str(winname), value, count, onChange, userdata);
end;

{ TIplImageRecordHelper }

function TIplImageRecordHelper.InitFromMat(const mat: TccvMat): TIplImage;
begin
  Assert(mat.dims <= 2);
  cvInitImageHeader(@Self, CvSize(mat.cols, mat.rows), cvIplDepth(mat.flags), mat.channels);
  cvSetData(@Self, mat.data, mat.step1);
end;

{ TocvVideoCapture }

class function TccvVideoCapture.Create: TccvVideoCapture;
begin
  Result := CreateVideoCapture;
end;

class function TccvVideoCapture.Create(device: Integer): TccvVideoCapture;
begin
  Result := CreateVideoCapture(device);
end;

class function TccvVideoCapture.Create(filename: pAnsiChar): TccvVideoCapture;
begin
  Result := CreateVideoCapture(filename);
end;

procedure TccvVideoCapture.Free;
begin
  ReleaseVideoCapture(Self);
end;

end.

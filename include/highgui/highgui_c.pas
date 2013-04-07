(* ///////////////////////////////////////////////////////////////////////////////////////
  //    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //    By downloading, copying, installing or using the software you agree to this license.
  //    If you do not agree to this license, do not download, install,
  //    copy or use the software.
  //
  //
  //                          Intel License Agreement
  //                  For Open Source Computer Vision Library
  //
  //   Copyright (C) 2000, Intel Corporation, all rights reserved.
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
  //     * The name of Intel Corporation may not be used to endorse or promote products
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
  //   the use of this software, even if advised of the possibility of such damage.

  Translated from:

  opencv2/core/core_c.h

*)

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
unit highgui_c;

interface

uses
  core.types_c, Core_C;

(* ***************************************************************************************\
  *                                  Basic GUI functions                                   *
  *************************************************************************************** *)

(* For font *)
const
  CV_FONT_LIGHT = 25; // QFont::Light;
  CV_FONT_NORMAL = 50; // QFont::Normal;
  CV_FONT_DEMIBOLD = 63; // QFont::DemiBold;
  CV_FONT_BOLD = 75; // QFont::Bold;
  CV_FONT_BLACK = 87; // QFont::Black;
  CV_STYLE_NORMAL = 0; // QFont::StyleNormal;
  CV_STYLE_ITALIC = 1; // QFont::StyleItalic;
  CV_STYLE_OBLIQUE = 2; // QFont::StyleOblique;
  (* --------- *)

  // for color cvScalar(blue_component, green_component, red\_component[, alpha_component])
  // and alpha= 0 <-> 0xFF (not transparent <-> transparent)
  // function cvFontQt(nameFont:PCVChar  ; pointSize : Integer=CV_DEFAULT(-1), CvScalar color CV_DEFAULT(cvScalarAll(0)), Integer weight CV_DEFAULT(CV_FONT_NORMAL),  Integer style CV_DEFAULT(CV_STYLE_NORMAL), Integer spacing CV_DEFAULT(0)):CvFont;
  //
  // CVAPI(procedure) cvAddText(var img: CvArr; text: PCVChar; org: CvPoint; var arg2: CvFont);
  //
  // CVAPI(procedure) cvDisplayOverlay(v1: 0));
  // CVAPI(procedure) cvDisplayStatusBar(v1: 0));
  //
  // CVAPI(procedure) cvSaveWindowParameters(name: PCVChar);
  // CVAPI(procedure) cvLoadWindowParameters(name: PCVChar);
  // CVAPI(Integer) cvStartLoop(Integer (pt2Func(Integer argc, PCVChar argv[]), Integer argc, PCVChar  argv[]);
  // CVAPI(procedure) cvStopLoop();
  //
  // type CV_CDECL *CvButtonCallback = procedure(state: Integer; userdata: Pointer);
  // const CV_PUSH_BUTTON := 0; CV_CHECKBOX = 1; CV_RADIOBOX = 2
  //
  // CVAPI(Integer) cvCreateButton(  PCVChar  button_name CV_DEFAULT(0),CvButtonCallback on_change CV_DEFAULT(0), Pointer  userdata CV_DEFAULT(0) , Integer button_type CV_DEFAULT(CV_PUSH_BUTTON), Integer initial_button_state CV_DEFAULT(0));
  // ----------------------
  (* this function is used to set some external parameters in case of X Window *)
  // CVAPI(Integer) cvInitSystem( Integer argc, PCVChar * argv );
  // CVAPI(Integer) cvStartWindowThread(  );
  // ---------  YV ---------
  // These 3 flags are used by cvSet/GetWindowProperty;
const
  CV_WND_PROP_FULLSCREEN = 0; // to change/get window's fullscreen property
  CV_WND_PROP_AUTOSIZE = 1; // to change/get window's autosize property
  CV_WND_PROP_ASPECTRATIO = 2; // to change/get window's aspectratio property
  CV_WND_PROP_OPENGL = 3; // to change/get window's opengl support
  // These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty;
  CV_WINDOW_NORMAL = $00000000;
  // the user can resize the window (no raint)  / also use to switch a fullscreen window to a normal size
  CV_WINDOW_AUTOSIZE = $00000001;
  // the user cannot resize the window; the size is rainted by the image displayed
  CV_WINDOW_OPENGL = $00001000; // window with opengl support
  // Those flags are only for Qt;
  CV_GUI_EXPANDED = $00000000; // status bar and tool bar
  CV_GUI_NORMAL = $00000010; // old fashious way
  // These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty;
  CV_WINDOW_FULLSCREEN = 1; // change the window to fullscreen
  CV_WINDOW_FREERATIO = $00000100; // the image expends as much as it can (no ratio raint)
  CV_WINDOW_KEEPRATIO = $00000000; // the ration image is respected.;

  (* create window *)
function cvNamedWindow(const name: pCVChar; flags: Integer = CV_WINDOW_AUTOSIZE): Integer; cdecl;

// Set and Get Property of the window
procedure cvSetWindowProperty(name: pCVChar; prop_id: Integer; prop_value: Double); cdecl;
function cvGetWindowProperty(name: pCVChar; prop_id: Integer): Double; cdecl;

{
  //display image within window (highgui windows remember their content)
  CVAPI(void) cvShowImage( const char* name, const CvArr* image );
}
procedure cvShowImage(const name: pCVChar; const image: pCvArr); cdecl;
//procedure cvShowImage(const name: pCVChar; const image: pIplImage); cdecl; overload;
//procedure cvShowImage(const name: pCVChar; const image: pCvMat); cdecl; overload;

(* resize/move window *)
procedure cvResizeWindow(name: pCVChar; width: Integer; height: Integer); cdecl;
// CVAPI(void) cvMoveWindow( const char* name, int x, int y );
procedure cvMoveWindow(const name: pCVChar; x: Integer; y: Integer); cdecl;

(* destroy window and all the trackers associated with it *)
procedure cvDestroyWindow(const name: pCVChar); cdecl;
procedure cvDestroyAllWindows; cdecl;
(* get native window handle (HWND in case of Win32 and Widget in case of X Window) *)
// CVAPI(procedure)cvGetWindowHandle(name: PCVChar);
(* get name of highgui window given its native handle *)
// CVAPI(char)cvGetWindowName(Pointer window_handle);

type
  TCvTrackbarCallback = procedure(pos: Integer); cdecl;

  (* create trackbar and display it on top of given window, set callback *)
function cvCreateTrackbar(const trackbar_name: pCVChar; const window_name: pCVChar; value: PInteger; count: Integer;
  on_change: TCvTrackbarCallback): Integer; cdecl;

type
  CvTrackbarCallback2 = procedure(pos: Integer; userdata: Pointer); cdecl;

  // CVAPI(Integer)cvCreateTrackbar2(PCVChar trackbar_name, PCVChar window_name, Integer * value,
  // Integer count, CvTrackbarCallback2 on_change,
  // function userdata CV_DEFAULT(v1: 0)): Pointer;

  (* retrieve or set trackbar position *)
  // CVAPI(Integer)cvGetTrackbarPos(PCVChar trackbar_name, PCVChar window_name);
  // CVAPI(procedure)cvSetTrackbarPos(trackbar_name: PCVChar; window_name: PCVChar; pos: Integer);

const
  CV_EVENT_MOUSEMOVE = 0;
  CV_EVENT_LBUTTONDOWN = 1;
  CV_EVENT_RBUTTONDOWN = 2;
  CV_EVENT_MBUTTONDOWN = 3;
  CV_EVENT_LBUTTONUP = 4;
  CV_EVENT_RBUTTONUP = 5;
  CV_EVENT_MBUTTONUP = 6;
  CV_EVENT_LBUTTONDBLCLK = 7;
  CV_EVENT_RBUTTONDBLCLK = 8;
  CV_EVENT_MBUTTONDBLCLK = 9;
  CV_EVENT_FLAG_LBUTTON = 1;
  CV_EVENT_FLAG_RBUTTON = 2;
  CV_EVENT_FLAG_MBUTTON = 4;
  CV_EVENT_FLAG_CTRLKEY = 8;
  CV_EVENT_FLAG_SHIFTKEY = 16;
  CV_EVENT_FLAG_ALTKEY = 32;

type
  CvMouseCallback = procedure(event: Integer; x: Integer; y: Integer; flags: Integer; param: Pointer); cdecl;

  (* assign callback for mouse events *)
  // CVAPI(procedure)cvSetMouseCallback(var 8 bit = 0; color or not * )
  {
    CVAPI(void) cvSetMouseCallback( const char* window_name, CvMouseCallback on_mouse,
    void* param CV_DEFAULT(NULL));
  }

Type
  // typedef void (CV_CDECL *CvMouseCallback )(int event, int x, int y, int flags, void* param);
  TCvMouseCallback = procedure(event: Integer; x, y, flags: Integer; param: Pointer); cdecl;

procedure cvSetMouseCallback(const window_name: pCVChar; on_mouse: TCvMouseCallback; param: Pointer = nil); cdecl;

const
  CV_LOAD_IMAGE_UNCHANGED = -1;
  (* 8bit= 1; gray *)
  CV_LOAD_IMAGE_GRAYSCALE = 0;
  (* ?= 2; color *)
  CV_LOAD_IMAGE_COLOR = 1;
  (* any depth= 3; ? *)
  CV_LOAD_IMAGE_ANYDEPTH = 2;
  (* ?= 4; any color *)
  CV_LOAD_IMAGE_ANYCOLOR = 4;

  (* load image from file  iscolor can be a combination of above flags where
    CV_LOAD_IMAGE_UNCHANGED  overrides the other flags
    using CV_LOAD_IMAGE_ANYCOLOR alone is equivalent to CV_LOAD_IMAGE_UNCHANGED
    unless CV_LOAD_IMAGE_ANYDEPTH is specified images are converted to 8bit *)
  // CVAPI(IplImage*) cvLoadImage(const char* filename,int iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));
function cvLoadImage(const filename: pCVChar; iscolor: Integer = CV_LOAD_IMAGE_COLOR): pIplImage; cdecl;

// const (;
// iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR): Integer); CVAPI(CvMat)cvLoadImageM(PCVChar filename,
// Integer iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));

const
  CV_IMWRITE_JPEG_QUALITY = 1;
  CV_IMWRITE_PNG_COMPRESSION = 16;
  CV_IMWRITE_PNG_STRATEGY = 17;
  CV_IMWRITE_PNG_STRATEGY_DEFAULT = 0;
  CV_IMWRITE_PNG_STRATEGY_FILTERED = 1;
  CV_IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY = 2;
  CV_IMWRITE_PNG_STRATEGY_RLE = 3;
  CV_IMWRITE_PNG_STRATEGY_FIXED = 4;
  CV_IMWRITE_PXM_BINARY = 32;

  (* save image to file *)
  // CVAPI(Integer)cvSaveImage(PCVChar filename, CvArr * image,
  // function params CV_DEFAULT(v1: 0)): Integer;
  {
    CVAPI(int) cvSaveImage( const char* filename, const CvArr* image, const int* params CV_DEFAULT(0) );
  }

  // function cvSaveImage(const filename: pCVChar; const image: pIplImage; const params: PInteger = nil): Integer; cdecl;
function cvSaveImage(const filename: pCVChar; const image: Pointer; const params: PInteger = nil): Integer; cdecl;

(* decode image stored in the buffer *)
// CVAPI(IplImage)cvDecodeImage(CvMat * buf, Integer iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));
// CVAPI(CvMat)cvDecodeImageM(CvMat * buf, Integer iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));

(* encode image and store the result as a byte vector (single-row 8uC1 matrix) *)
// CVAPI(CvMat)cvEncodeImage(PCVChar ext, CvArr * image,
// function params CV_DEFAULT(v1: 0)): Integer;

const
  CV_CVTIMG_FLIP = 1;
  CV_CVTIMG_SWAP_RB = 2;

  {
    /* utility function: convert one image to another with optional vertical flip */
    CVAPI(void) cvConvertImage( const CvArr* src, CvArr* dst, int flags CV_DEFAULT(0));
  }
procedure cvConvertImage(const src: pIplImage; dst: pIplImage; flags: Integer = 0); cdecl;

(* wait for key event infinitely (delay<=0) or for "delay" milliseconds *)
function cvWaitKey(delay: Integer = 0): Integer; cdecl;

// OpenGL support

// type
// CvOpenGlDrawCallback = procedure(v1:var userdata); cdecl;
// CVAPI( procedure)cvSetOpenGlDrawCallback(v1: 0));

// CVAPI( procedure)cvSetOpenGlContext(window_name: PCVChar);
// CVAPI(procedure)cvUpdateWindow(window_name: PCVChar);

(* ***************************************************************************************\
  *                         Working with Video Files and Cameras                           *
  *************************************************************************************** *)

(* "black box" capture structure *)
type
  TCvCapture = Integer;
  pCvCapture = ^TCvCapture;

  (* start capturing frames from video file *)
  // CVAPI(CvCapture*) cvCreateFileCapture( const char* filename );
function cvCreateFileCapture(const filename: pCVChar): pCvCapture; cdecl;

const
  CV_CAP_ANY = 0; // autodetect
  CV_CAP_MIL = 100; // MIL proprietary drivers
  CV_CAP_VFW = 200; // platform native
  CV_CAP_V4L = 200;
  CV_CAP_V4L2 = 200;
  CV_CAP_FIREWARE = 300; // IEEE 1394 drivers
  CV_CAP_FIREWIRE = 300;
  CV_CAP_IEEE1394 = 300;
  CV_CAP_DC1394 = 300;
  CV_CAP_CMU1394 = 300;
  CV_CAP_STEREO = 400; // TYZX proprietary drivers
  CV_CAP_TYZX = 400;
  CV_TYZX_LEFT = 400;
  CV_TYZX_RIGHT = 401;
  CV_TYZX_COLOR = 402;
  CV_TYZX_Z = 403;
  CV_CAP_QT = 500; // QuickTime
  CV_CAP_UNICAP = 600; // Unicap drivers
  CV_CAP_DSHOW = 700; // DirectShow (via videoInput)
  CV_CAP_PVAPI = 800; // PvAPI; Prosilica GigE SDK
  CV_CAP_OPENNI = 900; // OpenNI (for Kinect)
  CV_CAP_OPENNI_ASUS = 910; // OpenNI (for Asus Xtion)
  CV_CAP_ANDROID = 1000; // Android
  CV_CAP_XIAPI = 1100; // XIMEA Camera API
  CV_CAP_AVFOUNDATION = 1200;
  // AVFoundation framework for iOS (OS X Lion will have the same API);

  (* start capturing frames from camera: index = camera_index + domain_offset (CV_CAP_ *)
  // CVAPI(CvCapture)cvCreateCameraCapture(Integer index);
function cvCreateCameraCapture(index: Longint): pCvCapture; cdecl;

(* grab a frame, return 1 on success, 0 on fail.
  this cFunction is thought to be fast *)
// CVAPI(Integer)cvGrabFrame(CvCapture * capture);

(* get the frame grabbed with cvGrabFrame(..)
  This cFunction may apply some frame processing like
  frame decompression, flipping etc.
  not  not  not DO NOT RELEASE or MODIFY the retrieved frame not  not  not *)
// CVAPI(IplImage)cvRetrieveFrame(CvCapture * capture, Integer streamIdx CV_DEFAULT(0));

(* Just a combination of cvGrabFrame and cvRetrieveFrame
  not  not  not DO NOT RELEASE or MODIFY the retrieved frame not  not  not *)
// CVAPI(IplImage*) cvQueryFrame( CvCapture* capture );
function cvQueryFrame(capture: pCvCapture): pIplImage; cdecl;

(* stop capturing/reading and free resources *)
procedure cvReleaseCapture(Var capture: pCvCapture); cdecl;

// modes of the controlling registers (can be: auto; manual; auto single push; absolute Latter allowed with any other mode)
// every feature can have only one mode turned on at a time;
const
  CV_CAP_PROP_DC1394_OFF = -4;

  // turn the feature off (not controlled manually nor automatically)

  CV_CAP_PROP_DC1394_MODE_MANUAL = -3;

  // set automatically when a value of the feature is set by the user

  CV_CAP_PROP_DC1394_MODE_AUTO = -2;

  CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO = -1;

  CV_CAP_PROP_POS_MSEC = 0;

  CV_CAP_PROP_POS_FRAMES = 1;

  CV_CAP_PROP_POS_AVI_RATIO = 2;

  CV_CAP_PROP_FRAME_WIDTH = 3;

  CV_CAP_PROP_FRAME_HEIGHT = 4;

  CV_CAP_PROP_FPS = 5;

  CV_CAP_PROP_FOURCC = 6;

  CV_CAP_PROP_FRAME_COUNT = 7;

  CV_CAP_PROP_FORMAT = 8;

  CV_CAP_PROP_MODE = 9;

  CV_CAP_PROP_BRIGHTNESS = 10;

  CV_CAP_PROP_CONTRAST = 11;

  CV_CAP_PROP_SATURATION = 12;

  CV_CAP_PROP_HUE = 13;

  CV_CAP_PROP_GAIN = 14;

  CV_CAP_PROP_EXPOSURE = 15;

  CV_CAP_PROP_CONVERT_RGB = 16;

  CV_CAP_PROP_WHITE_BALANCE_BLUE_U = 17;

  CV_CAP_PROP_RECTIFICATION = 18;

  CV_CAP_PROP_MONOCROME = 19;

  CV_CAP_PROP_SHARPNESS = 20;

  CV_CAP_PROP_AUTO_EXPOSURE = 21; // exposure control done by camera;

  // user can adjust refernce level;
  // using this feature;

  CV_CAP_PROP_GAMMA = 22;

  CV_CAP_PROP_TEMPERATURE = 23;

  CV_CAP_PROP_TRIGGER = 24;

  CV_CAP_PROP_TRIGGER_DELAY = 25;

  CV_CAP_PROP_WHITE_BALANCE_RED_V = 26;

  CV_CAP_PROP_ZOOM = 27;

  CV_CAP_PROP_FOCUS = 28;

  CV_CAP_PROP_GUID = 29;

  CV_CAP_PROP_ISO_SPEED = 30;

  CV_CAP_PROP_MAX_DC1394 = 31;

  CV_CAP_PROP_BACKLIGHT = 32;

  CV_CAP_PROP_PAN = 33;

  CV_CAP_PROP_TILT = 34;

  CV_CAP_PROP_ROLL = 35;

  CV_CAP_PROP_IRIS = 36;

  CV_CAP_PROP_SETTINGS = 37;

  CV_CAP_PROP_AUTOGRAB = 1024; // property for highgui class CvCapture_Android only

  CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING = 1025;

  // readonly; tricky property; returns cpnst char* indeed

  CV_CAP_PROP_PREVIEW_FORMAT = 1026; // readonly; tricky property; returns cpnst char* indeed

  // OpenNI map generators;

  CV_CAP_OPENNI_DEPTH_GENERATOR = 1 shl 31;

  CV_CAP_OPENNI_IMAGE_GENERATOR = 1 shl 30;

  CV_CAP_OPENNI_GENERATORS_MASK = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_OPENNI_IMAGE_GENERATOR;

  // Properties of cameras available through OpenNI interfaces;

  CV_CAP_PROP_OPENNI_OUTPUT_MODE = 100;

  CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH = 101; // in mm

  CV_CAP_PROP_OPENNI_BASELINE = 102; // in mm

  CV_CAP_PROP_OPENNI_FOCAL_LENGTH = 103; // in pixels

  CV_CAP_PROP_OPENNI_REGISTRATION = 104; // flag

  CV_CAP_PROP_OPENNI_REGISTRATION_ON = CV_CAP_PROP_OPENNI_REGISTRATION;

  // flag that synchronizes the remapping depth map to image map
  // by changing depth generator's view point (if the flag is "on") or;
  // sets this view point to its normal one (if the flag is "off").;

  CV_CAP_PROP_OPENNI_APPROX_FRAME_SYNC = 105;

  CV_CAP_PROP_OPENNI_MAX_BUFFER_SIZE = 106;

  CV_CAP_PROP_OPENNI_CIRCLE_BUFFER = 107;

  CV_CAP_PROP_OPENNI_MAX_TIME_DURATION = 108;

  CV_CAP_PROP_OPENNI_GENERATOR_PRESENT = 109;

  CV_CAP_OPENNI_IMAGE_GENERATOR_PRESENT = CV_CAP_OPENNI_IMAGE_GENERATOR + CV_CAP_PROP_OPENNI_GENERATOR_PRESENT;

  CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE = CV_CAP_OPENNI_IMAGE_GENERATOR + CV_CAP_PROP_OPENNI_OUTPUT_MODE;

  CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_BASELINE;

  CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_FOCAL_LENGTH;

  CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION = CV_CAP_OPENNI_DEPTH_GENERATOR + CV_CAP_PROP_OPENNI_REGISTRATION;

  CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON = CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION;

  // Properties of cameras available through GStreamer interface;

  CV_CAP_GSTREAMER_QUEUE_LENGTH = 200; // default is 1

  CV_CAP_PROP_PVAPI_MULTICASTIP = 300;

  // ip for anable multicast master mode. 0 for disable multicast
  // Properties of cameras available through XIMEA SDK interface;

  CV_CAP_PROP_XI_DOWNSAMPLING = 400; // Change image resolution by binning or skipping.

  CV_CAP_PROP_XI_DATA_FORMAT = 401; // Output data format.

  CV_CAP_PROP_XI_OFFSET_X = 402;

  // Horizontal offset from the origin to the area of interest (in pixels).

  CV_CAP_PROP_XI_OFFSET_Y = 403;

  // Vertical offset from the origin to the area of interest (in pixels).

  CV_CAP_PROP_XI_TRG_SOURCE = 404; // Defines source of trigger.

  CV_CAP_PROP_XI_TRG_SOFTWARE = 405;

  // Generates an internal trigger. PRM_TRG_SOURCE must be set to TRG_SOFTWARE.

  CV_CAP_PROP_XI_GPI_SELECTOR = 406; // Selects general purpose input

  CV_CAP_PROP_XI_GPI_MODE = 407; // Set general purpose input mode

  CV_CAP_PROP_XI_GPI_LEVEL = 408; // Get general purpose level

  CV_CAP_PROP_XI_GPO_SELECTOR = 409; // Selects general purpose output

  CV_CAP_PROP_XI_GPO_MODE = 410; // Set general purpose output mode

  CV_CAP_PROP_XI_LED_SELECTOR = 411; // Selects camera signalling LED

  CV_CAP_PROP_XI_LED_MODE = 412; // Define camera signalling LED functionality

  CV_CAP_PROP_XI_MANUAL_WB = 413; // Calculates White Balance(must be called during acquisition)

  CV_CAP_PROP_XI_AUTO_WB = 414; // Automatic white balance

  CV_CAP_PROP_XI_AEAG = 415; // Automatic exposure/gain

  CV_CAP_PROP_XI_EXP_PRIORITY = 416; // Exposure priority (0.5 - exposure 50%; gain 50%).

  CV_CAP_PROP_XI_AE_MAX_LIMIT = 417;

  // Maximum limit of exposure in AEAG procedure  CV_CAP_PROP_XI_AG_MAX_LIMIT  = 418;      // Maximum limit of gain in AEAG procedure

  CV_CAP_PROP_XI_AEAG_LEVEL = 419;

  // Average intensity of output signal AEAG should achieve(in %)

  CV_CAP_PROP_XI_TIMEOUT = 420; // Image capture timeout in milliseconds

  // Properties for Android cameras;

  CV_CAP_PROP_ANDROID_FLASH_MODE = 8001;

  CV_CAP_PROP_ANDROID_FOCUS_MODE = 8002;

  CV_CAP_PROP_ANDROID_WHITE_BALANCE = 8003;

  CV_CAP_PROP_ANDROID_ANTIBANDING = 8004;

  CV_CAP_PROP_ANDROID_FOCAL_LENGTH = 8005;

  CV_CAP_PROP_ANDROID_FOCUS_DISTANCE_NEAR = 8006;

  CV_CAP_PROP_ANDROID_FOCUS_DISTANCE_OPTIMAL = 8007;

  CV_CAP_PROP_ANDROID_FOCUS_DISTANCE_FAR = 8008;

  // Properties of cameras available through AVFOUNDATION interface;

  CV_CAP_PROP_IOS_DEVICE_FOCUS = 9001;

  CV_CAP_PROP_IOS_DEVICE_EXPOSURE = 9002;

  CV_CAP_PROP_IOS_DEVICE_FLASH = 9003;

  CV_CAP_PROP_IOS_DEVICE_WHITEBALANCE = 9004;

  CV_CAP_PROP_IOS_DEVICE_TORCH = 9005;

  // Data given from depth generator.;

  CV_CAP_OPENNI_DEPTH_MAP = 0; // Depth values in mm (CV_16UC1)

  CV_CAP_OPENNI_POINT_CLOUD_MAP = 1; // XYZ in meters (CV_32FC3)

  CV_CAP_OPENNI_DISPARITY_MAP = 2; // Disparity in pixels (CV_8UC1)

  CV_CAP_OPENNI_DISPARITY_MAP_32F = 3; // Disparity in pixels (CV_32FC1)

  CV_CAP_OPENNI_VALID_DEPTH_MASK = 4; // CV_8UC1

  // Data given from RGB image generator.;

  CV_CAP_OPENNI_BGR_IMAGE = 5;

  CV_CAP_OPENNI_GRAY_IMAGE = 6;

  // Supported output modes of OpenNI image generator

  CV_CAP_OPENNI_VGA_30HZ = 0;

  CV_CAP_OPENNI_SXGA_15HZ = 1;

  CV_CAP_OPENNI_SXGA_30HZ = 2;

  // supported by Android camera output formats

  CV_CAP_ANDROID_COLOR_FRAME_BGR = 0; // BGR

  CV_CAP_ANDROID_COLOR_FRAME = CV_CAP_ANDROID_COLOR_FRAME_BGR;

  CV_CAP_ANDROID_GREY_FRAME = 1; // Y

  CV_CAP_ANDROID_COLOR_FRAME_RGB = 2;

  CV_CAP_ANDROID_COLOR_FRAME_BGRA = 3;

  CV_CAP_ANDROID_COLOR_FRAME_RGBA = 4;

  // supported Android camera flash modes

  CV_CAP_ANDROID_FLASH_MODE_AUTO = 0;

  CV_CAP_ANDROID_FLASH_MODE_OFF = 0;

  CV_CAP_ANDROID_FLASH_MODE_ON = 1;

  CV_CAP_ANDROID_FLASH_MODE_RED_EYE = 2;

  CV_CAP_ANDROID_FLASH_MODE_TORCH = 3;

  // supported Android camera focus modes

  CV_CAP_ANDROID_FOCUS_MODE_AUTO = 0;

  CV_CAP_ANDROID_FOCUS_MODE_CONTINUOUS_VIDEO = 0;

  CV_CAP_ANDROID_FOCUS_MODE_EDOF = 1;

  CV_CAP_ANDROID_FOCUS_MODE_FIXED = 2;

  CV_CAP_ANDROID_FOCUS_MODE_INFINITY = 3;

  CV_CAP_ANDROID_FOCUS_MODE_MACRO = 4;

  // supported Android camera white balance modes

  CV_CAP_ANDROID_WHITE_BALANCE_AUTO = 0;

  CV_CAP_ANDROID_WHITE_BALANCE_CLOUDY_DAYLIGHT = 0;

  CV_CAP_ANDROID_WHITE_BALANCE_DAYLIGHT = 1;

  CV_CAP_ANDROID_WHITE_BALANCE_FLUORESCENT = 2;

  CV_CAP_ANDROID_WHITE_BALANCE_INCANDESCENT = 3;

  CV_CAP_ANDROID_WHITE_BALANCE_SHADE = 4;

  CV_CAP_ANDROID_WHITE_BALANCE_TWILIGHT = 5;

  CV_CAP_ANDROID_WHITE_BALANCE_WARM_FLUORESCENT = 6;

  // supported Android camera antibanding modes

  CV_CAP_ANDROID_ANTIBANDING_50HZ = 0;

  CV_CAP_ANDROID_ANTIBANDING_60HZ = 0;

  CV_CAP_ANDROID_ANTIBANDING_AUTO = 1;

  CV_CAP_ANDROID_ANTIBANDING_OFF = 2;

  (* retrieve or set capture properties *)
function cvGetCaptureProperty(capture: pCvCapture; property_id: Integer): Double; cdecl;
function cvSetCaptureProperty(capture: pCvCapture; property_id: Integer; value: Double): Integer; cdecl;

// Return the type of the capturer (eg, CV_CAP_V4W, CV_CAP_UNICAP), which is unknown if created with CV_CAP_ANY
// CVAPI(Integer)cvGetCaptureDomain(CvCapture * capture);

(* "black box" video file writer structure *)
// type
// type
// CvVideoWriter = deoWriter;
type
  TCvVideoWriter = Integer;
  pCvVideoWriter = ^TCvVideoWriter;
  ppCvVideoWriter = ^pCvVideoWriter;
  //
  // CV_INLINE
  // function CV_FOURCC(and 255) + ((c2 and 255) shl 8) + ((c3 and 255) shl 16) +
  // ((c4 and 255) shl 24: c1): Integer;
  // end;

const
  CV_FOURCC_PROMPT = -1; (* Open Codec Selection Dialog (Windows only) *)
{$EXTERNALSYM CV_FOURCC_PROMPT}
  // const
  // CV_FOURCC_DEFAULT = CV_FOURCC('I', 'Y', 'U',
  // 'V'( / * Use default codec for specified filename(Linux only) * /;
  // {$EXTERNALSYM CV_FOURCC_DEFAULT}
  // (* initialize video file writer *)

  // CVAPI(CvVideoWriter)cvCreateVideoWriter(PCVChar filename, Integer fourcc, Double fps,
  // CvSize frame_size,
  // function is_color CV_DEFAULT(v1: 1)): Integer;
  {
    CVAPI(CvVideoWriter*) cvCreateVideoWriter( const char* filename, int fourcc,
    double fps, CvSize frame_size,
    int is_color CV_DEFAULT(1));
  }
function cvCreateVideoWriter(const filename: pCVChar; fourcc: Integer; fps: Double; frame_size: TCvSize;
  is_color: Integer = 1): pCvVideoWriter; cdecl;

function CV_FOURCC(const c1, c2, c3, c4: CVChar): Integer; inline;


// CVAPI(CvVideoWriter*) cvCreateImageSequenceWriter( const char* filename,
// int is_color CV_DEFAULT(1));

(* write frame to video file *)
// CVAPI(Integer)cvWriteFrame(CvVideoWriter * writer, IplImage * image);
function cvWriteFrame(writer: pCvVideoWriter; image: pIplImage): Integer; cdecl;

(* close video file writer *)
// CVAPI(procedure)cvReleaseVideoWriter(writer: array of CvVideoWriter);
procedure cvReleaseVideoWriter(Var writer: pCvVideoWriter); cdecl;

(* ***************************************************************************************\
  *                              Obsolete functions/synonyms                               *
  *************************************************************************************** *)

// const cvCaptureFromFile = cvCreateFileCapture;
// {$EXTERNALSYM cvCaptureFromFile}
// const cvCaptureFromCAM = cvCreateCameraCapture;
// {$EXTERNALSYM cvCaptureFromCAM}
// const cvCaptureFromAVI = cvCaptureFromFile;
// {$EXTERNALSYM cvCaptureFromAVI}
// const cvCreateAVIWriter = cvCreateVideoWriter;
// {$EXTERNALSYM cvCreateAVIWriter}
// const cvWriteToAVI = cvWriteFrame;
// {$EXTERNALSYM cvWriteToAVI}
// {$DEFINE cvAddSearchPath(path)}
// const cvvInitSystem = cvInitSystem;
// {$EXTERNALSYM cvvInitSystem}
// const cvvNamedWindow = cvNamedWindow;
// {$EXTERNALSYM cvvNamedWindow}
// const cvvShowImage = cvShowImage;
// {$EXTERNALSYM cvvShowImage}
// const cvvResizeWindow = cvResizeWindow;
// {$EXTERNALSYM cvvResizeWindow}
// const cvvDestroyWindow = cvDestroyWindow;
// {$EXTERNALSYM cvvDestroyWindow}
// const cvvCreateTrackbar = cvCreateTrackbar;
// {$EXTERNALSYM cvvCreateTrackbar}
/// / >> Following declaration is a macro definition!
// const cvvLoadImage(name)cvLoadImage((name), 1);
//
// const cvvSaveImage = cvSaveImage;
// {$EXTERNALSYM cvvSaveImage}
// const cvvAddSearchPath = cvAddSearchPath;
// {$EXTERNALSYM cvvAddSearchPath}
/// / >> Following declaration is a macro definition!
// const cvvWaitKey(name)cvWaitKey(0);
//
/// / >> Following declaration is a macro definition!
// const cvvWaitKeyEx(name, delay)cvWaitKey(delay);
//
// const cvvConvertImage = cvConvertImage;
// {$EXTERNALSYM cvvConvertImage}
// const HG_AUTOSIZE = CV_WINDOW_AUTOSIZE;
// {$EXTERNALSYM HG_AUTOSIZE}
// const set_preprocess_func = cvSetPreprocessFuncWin32;
// {$EXTERNALSYM set_preprocess_func}
// const set_postprocess_func = cvSetPostprocessFuncWin32;
// {$EXTERNALSYM set_postprocess_func}
// {$IFNDEF  WIN32 || defined _WIN32}
// CVAPI(
// procedure)cvSetPreprocessFuncWin32_(callback: Pointer); CVAPI(
// procedure)cvSetPostprocessFuncWin32_(callback: Pointer);
//
/// / >> Following declaration is a macro definition!
// const cvSetPreprocessFuncWin32(callback)cvSetPreprocessFuncWin32_
// ((# define cvSetPreprocessFuncWin32(callback)cvSetPreprocessFuncWin32_((
// procedure((callback): ();

// {$ENDIF}
// {$IFDEF __cplusplus}
// end;

// {$ENDIF}
// {$ENDIF}
implementation

uses
  uLibName;

function cvNamedWindow; external highgui_Dll;
procedure cvShowImage; external highgui_Dll;
//procedure cvShowImage(const name: pCVChar; const image: pIplImage); external highgui_Dll name 'cvShowImage';
//procedure cvShowImage(const name: pCVChar; const image: pCvMat); external highgui_Dll name 'cvShowImage';
function cvWaitKey; external highgui_Dll;
procedure cvDestroyWindow; external highgui_Dll;
procedure cvDestroyAllWindows; external highgui_Dll;
function cvLoadImage; external highgui_Dll;
function cvCreateFileCapture; external highgui_Dll;
function cvQueryFrame; external highgui_Dll;
procedure cvReleaseCapture; external highgui_Dll;
function cvSetCaptureProperty; external highgui_Dll;
function cvGetCaptureProperty; external highgui_Dll;
function cvCreateTrackbar; external highgui_Dll;
function cvCreateCameraCapture; external highgui_Dll;
function cvSaveImage; external highgui_Dll;
function cvCreateVideoWriter; external highgui_Dll;
function cvWriteFrame; external highgui_Dll;
procedure cvReleaseVideoWriter; external highgui_Dll;
procedure cvSetMouseCallback; external highgui_Dll;
procedure cvConvertImage; external highgui_Dll;
procedure cvMoveWindow; external highgui_Dll;

function CV_FOURCC(const c1, c2, c3, c4: CVChar): Integer; inline;
begin
  Result := Integer(c1) + (Integer(c2) shl 8) + (Integer(c3) shl 16) + (Integer(c4) shl 24);
end;

procedure cvResizeWindow; external highgui_Dll;
procedure cvSetWindowProperty; external highgui_Dll;
function cvGetWindowProperty; external highgui_Dll;

end.

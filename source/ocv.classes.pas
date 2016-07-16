(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)

unit ocv.classes;

{$I OpenCV.inc}

interface

Uses
  Winapi.Windows,
  System.Generics.Collections,
  ocv.core.types_c,
  ocv.highgui_c;

Type
  cbool        = bytebool;
  TOpenCVClass = Pointer;

  IOCVCommon = interface
    function _InternalData: TOpenCVClass;
  end;

  TOCVCommon = class(TInterfacedObject, IOCVCommon)
  private
    FData: TOpenCVClass;
  public
    function _InternalData: TOpenCVClass;
  end;

  IMat = interface(IOCVCommon)
    ['{2CFB1B8E-4D18-4C1D-839F-0AFE4213F57D}']
    function elemSize(): size_t;            // 0
    function elemSize1(): size_t;           // 1
    function _type(): integer;              // 2
    function depth(): integer;              // 3
    function channels(): integer;           // 4
    function step1(i: integer = 0): size_t; // 5
    function empty(): cbool;
    function total(): size_t;  // 6
    function flags(): integer; // 7
    function dims(): integer;  // 8
    function rows(): integer;  // 9
    function cols(): integer;  // 10
    function data(): Pointer;  // 11
  end;

  TMat = class(TOCVCommon, IMat)
  private
    FNeedDestroy: boolean;
  public
    constructor Create(const _M: TOpenCVClass = nil; const NeedDestroy: boolean = True); overload;
    constructor Create(const Image: pIplImage); overload;
    destructor Destroy; override;
    function elemSize(): size_t;            // 0
    function elemSize1(): size_t;           // 1
    function _type(): integer;              // 2
    function depth(): integer;              // 3
    function channels(): integer;           // 4
    function step1(i: integer = 0): size_t; // 5
    function empty(): cbool;
    function total(): size_t;  // 6
    function flags(): integer; // 7
    function dims(): integer;  // 8
    function rows(): integer;  // 9
    function cols(): integer;  // 10
    function data(): Pointer;  // 11
  end;

  // ---------------------------- VideoCapture --------------------------
  IVideoCapture = interface(IOCVCommon)
    ['{86599C0A-D6CF-4F53-8AE3-336B0ED948D3}']
    function Open(const CamNumber: integer = CV_CAP_ANY): cbool;
    function isOpened: cbool;
    function Read(Var Mat: IMat): cbool;

    function getProp(const propId: integer): double;
    procedure setProp(const propId: integer; const Value: double);

    function PropSet(const propId: integer; const Value: double): cbool;
    property Prop[const propId: integer]: double read getProp write setProp;
  end;

  TOpenCVVideoCaptureClass = Pointer;

  TVideoCapture = class(TOCVCommon, IVideoCapture)
  private
    function getProp(const propId: integer): double;
    procedure setProp(const propId: integer; const Value: double);
  public
    constructor Create; overload;
    constructor Create(const CamNumber: integer); overload;
    constructor Create(const FileName: String); overload;
    destructor Destroy; override;

    function Open(const CamNumber: integer = CV_CAP_ANY): cbool; overload;
    function Open(const FileName: String): cbool; overload;
    function isOpened: cbool;
    function Read(Var Mat: IMat): cbool;
    function PropSet(const propId: integer; const Value: double): cbool;
    property Prop[const propId: integer]: double read getProp write setProp;
  end;

const
  // Camera API
  CAP_ANY          = 0;   // autodetect
  CAP_VFW          = 200; // platform native
  CAP_V4L          = 200;
  CAP_V4L2         = CAP_V4L;
  CAP_FIREWARE     = 300; // IEEE 1394 drivers
  CAP_FIREWIRE     = CAP_FIREWARE;
  CAP_IEEE1394     = CAP_FIREWARE;
  CAP_DC1394       = CAP_FIREWARE;
  CAP_CMU1394      = CAP_FIREWARE;
  CAP_QT           = 500;  // QuickTime
  CAP_UNICAP       = 600;  // Unicap drivers
  CAP_DSHOW        = 700;  // DirectShow (via videoInput)
  CAP_PVAPI        = 800;  // PvAPI; Prosilica GigE SDK
  CAP_OPENNI       = 900;  // OpenNI (for Kinect)
  CAP_OPENNI_ASUS  = 910;  // OpenNI (for Asus Xtion)
  CAP_ANDROID      = 1000; // Android - not used
  CAP_XIAPI        = 1100; // XIMEA Camera API
  CAP_AVFOUNDATION = 1200; // AVFoundation framework for iOS (OS X Lion will have the same API)
  CAP_GIGANETIX    = 1300; // Smartek Giganetix GigEVisionSDK
  CAP_MSMF         = 1400; // Microsoft Media Foundation (via videoInput)
  CAP_WINRT        = 1410; // Microsoft Windows Runtime using Media Foundation
  CAP_INTELPERC    = 1500; // Intel Perceptual Computing SDK
  CAP_OPENNI2      = 1600; // OpenNI2 (for Kinect)
  CAP_OPENNI2_ASUS = 1610; // OpenNI2 (for Asus Xtion and Occipital Structure sensors)
  CAP_GPHOTO2      = 1700; // gPhoto2 connection

  // generic properties (based on DC1394 properties)
  CAP_PROP_POS_MSEC             = 0;
  CAP_PROP_POS_FRAMES           = 1;
  CAP_PROP_POS_AVI_RATIO        = 2;
  CAP_PROP_FRAME_WIDTH          = 3;
  CAP_PROP_FRAME_HEIGHT         = 4;
  CAP_PROP_FPS                  = 5;
  CAP_PROP_FOURCC               = 6;
  CAP_PROP_FRAME_COUNT          = 7;
  CAP_PROP_FORMAT               = 8;
  CAP_PROP_MODE                 = 9;
  CAP_PROP_BRIGHTNESS           = 10;
  CAP_PROP_CONTRAST             = 11;
  CAP_PROP_SATURATION           = 12;
  CAP_PROP_HUE                  = 13;
  CAP_PROP_GAIN                 = 14;
  CAP_PROP_EXPOSURE             = 15;
  CAP_PROP_CONVERT_RGB          = 16;
  CAP_PROP_WHITE_BALANCE_BLUE_U = 17;
  CAP_PROP_RECTIFICATION        = 18;
  CAP_PROP_MONOCHROME           = 19;
  CAP_PROP_SHARPNESS            = 20;
  CAP_PROP_AUTO_EXPOSURE        = 21;
  // DC1394: exposure control done by camera; user can adjust refernce level using this feature
  CAP_PROP_GAMMA               = 22;
  CAP_PROP_TEMPERATURE         = 23;
  CAP_PROP_TRIGGER             = 24;
  CAP_PROP_TRIGGER_DELAY       = 25;
  CAP_PROP_WHITE_BALANCE_RED_V = 26;
  CAP_PROP_ZOOM                = 27;
  CAP_PROP_FOCUS               = 28;
  CAP_PROP_GUID                = 29;
  CAP_PROP_ISO_SPEED           = 30;
  CAP_PROP_BACKLIGHT           = 32;
  CAP_PROP_PAN                 = 33;
  CAP_PROP_TILT                = 34;
  CAP_PROP_ROLL                = 35;
  CAP_PROP_IRIS                = 36;
  CAP_PROP_SETTINGS            = 37;

  // Generic camera output modes.
  // Currently; these are supported through the libv4l interface only.
  CAP_MODE_BGR  = 0; // BGR24 (default)
  CAP_MODE_RGB  = 1; // RGB24
  CAP_MODE_GRAY = 2; // Y8
  CAP_MODE_YUYV = 3; // YUYV

  // DC1394 only
  // modes of the controlling registers (can be: auto; manual; auto single push; absolute Latter allowed with any other mode)
  // every feature can have only one mode turned on at a time
  CAP_PROP_DC1394_OFF                = -4; // turn the feature off (not controlled manually nor automatically)
  CAP_PROP_DC1394_MODE_MANUAL        = -3; // set automatically when a value of the feature is set by the user
  CAP_PROP_DC1394_MODE_AUTO          = -2;
  CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO = -1;
  CAP_PROP_DC1394_MAX                = 31;

  // OpenNI map generators
  CAP_OPENNI_DEPTH_GENERATOR = 1 shl 31;
  CAP_OPENNI_IMAGE_GENERATOR = 1 shl 30;
  CAP_OPENNI_GENERATORS_MASK = CAP_OPENNI_DEPTH_GENERATOR + CAP_OPENNI_IMAGE_GENERATOR;

  // Properties of cameras available through OpenNI interfaces
  CAP_PROP_OPENNI_OUTPUT_MODE     = 100;
  CAP_PROP_OPENNI_FRAME_MAX_DEPTH = 101; // in mm
  CAP_PROP_OPENNI_BASELINE        = 102; // in mm
  CAP_PROP_OPENNI_FOCAL_LENGTH    = 103; // in pixels
  CAP_PROP_OPENNI_REGISTRATION    = 104; // flag that synchronizes the remapping depth map to image map
  // by changing depth generator's view point (if the flag is "on") or
  // sets this view point to its normal one (if the flag is "off").
  CAP_PROP_OPENNI_REGISTRATION_ON   = CAP_PROP_OPENNI_REGISTRATION;
  CAP_PROP_OPENNI_APPROX_FRAME_SYNC = 105;
  CAP_PROP_OPENNI_MAX_BUFFER_SIZE   = 106;
  CAP_PROP_OPENNI_CIRCLE_BUFFER     = 107;
  CAP_PROP_OPENNI_MAX_TIME_DURATION = 108;
  CAP_PROP_OPENNI_GENERATOR_PRESENT = 109;
  CAP_PROP_OPENNI2_SYNC             = 110;
  CAP_PROP_OPENNI2_MIRROR           = 111;

  // OpenNI shortcats
  CAP_OPENNI_IMAGE_GENERATOR_PRESENT         = CAP_OPENNI_IMAGE_GENERATOR + CAP_PROP_OPENNI_GENERATOR_PRESENT;
  CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE     = CAP_OPENNI_IMAGE_GENERATOR + CAP_PROP_OPENNI_OUTPUT_MODE;
  CAP_OPENNI_DEPTH_GENERATOR_BASELINE        = CAP_OPENNI_DEPTH_GENERATOR + CAP_PROP_OPENNI_BASELINE;
  CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH    = CAP_OPENNI_DEPTH_GENERATOR + CAP_PROP_OPENNI_FOCAL_LENGTH;
  CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION    = CAP_OPENNI_DEPTH_GENERATOR + CAP_PROP_OPENNI_REGISTRATION;
  CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON = CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION;

  // OpenNI data given from depth generator
  CAP_OPENNI_DEPTH_MAP         = 0; // Depth values in mm (CV_16UC1)
  CAP_OPENNI_POINT_CLOUD_MAP   = 1; // XYZ in meters (CV_32FC3)
  CAP_OPENNI_DISPARITY_MAP     = 2; // Disparity in pixels (CV_8UC1)
  CAP_OPENNI_DISPARITY_MAP_32F = 3; // Disparity in pixels (CV_32FC1)
  CAP_OPENNI_VALID_DEPTH_MASK  = 4; // CV_8UC1

  // Data given from RGB image generator
  CAP_OPENNI_BGR_IMAGE  = 5;
  CAP_OPENNI_GRAY_IMAGE = 6;

  // Supported output modes of OpenNI image generator
  CAP_OPENNI_VGA_30HZ  = 0;
  CAP_OPENNI_SXGA_15HZ = 1;
  CAP_OPENNI_SXGA_30HZ = 2;
  CAP_OPENNI_QVGA_30HZ = 3;
  CAP_OPENNI_QVGA_60HZ = 4;

  // GStreamer
  CAP_PROP_GSTREAMER_QUEUE_LENGTH = 200; // default is 1

  // PVAPI
  CAP_PROP_PVAPI_MULTICASTIP           = 300; // ip for anable multicast master mode. 0 for disable multicast
  CAP_PROP_PVAPI_FRAMESTARTTRIGGERMODE = 301; // FrameStartTriggerMode: Determines how a frame is initiated
  CAP_PROP_PVAPI_DECIMATIONHORIZONTAL  = 302; // Horizontal sub-sampling of the image
  CAP_PROP_PVAPI_DECIMATIONVERTICAL    = 303; // Vertical sub-sampling of the image
  CAP_PROP_PVAPI_BINNINGX              = 304; // Horizontal binning factor
  CAP_PROP_PVAPI_BINNINGY              = 305; // Vertical binning factor
  CAP_PROP_PVAPI_PIXELFORMAT           = 306; // Pixel format

  // PVAPI: FrameStartTriggerMode
  CAP_PVAPI_FSTRIGMODE_FREERUN   = 0; // Freerun
  CAP_PVAPI_FSTRIGMODE_SYNCIN1   = 1; // SyncIn1
  CAP_PVAPI_FSTRIGMODE_SYNCIN2   = 2; // SyncIn2
  CAP_PVAPI_FSTRIGMODE_FIXEDRATE = 3; // FixedRate
  CAP_PVAPI_FSTRIGMODE_SOFTWARE  = 4; // Software

  // PVAPI: DecimationHorizontal; DecimationVertical
  CAP_PVAPI_DECIMATION_OFF      = 1; // Off
  CAP_PVAPI_DECIMATION_2OUTOF4  = 2; // 2 out of 4 decimation
  CAP_PVAPI_DECIMATION_2OUTOF8  = 4; // 2 out of 8 decimation
  CAP_PVAPI_DECIMATION_2OUTOF16 = 8; // 2 out of 16 decimation

  // PVAPI: PixelFormat
  CAP_PVAPI_PIXELFORMAT_MONO8   = 1; // Mono8
  CAP_PVAPI_PIXELFORMAT_MONO16  = 2; // Mono16
  CAP_PVAPI_PIXELFORMAT_BAYER8  = 3; // Bayer8
  CAP_PVAPI_PIXELFORMAT_BAYER16 = 4; // Bayer16
  CAP_PVAPI_PIXELFORMAT_RGB24   = 5; // Rgb24
  CAP_PVAPI_PIXELFORMAT_BGR24   = 6; // Bgr24
  CAP_PVAPI_PIXELFORMAT_RGBA32  = 7; // Rgba32
  CAP_PVAPI_PIXELFORMAT_BGRA32  = 8; // Bgra32

  // Properties of cameras available through XIMEA SDK interface
  CAP_PROP_XI_DOWNSAMPLING = 400; // Change image resolution by binning or skipping.
  CAP_PROP_XI_DATA_FORMAT  = 401; // Output data format.
  CAP_PROP_XI_OFFSET_X     = 402; // Horizontal offset from the origin to the area of interest (in pixels).
  CAP_PROP_XI_OFFSET_Y     = 403; // Vertical offset from the origin to the area of interest (in pixels).
  CAP_PROP_XI_TRG_SOURCE   = 404; // Defines source of trigger.
  CAP_PROP_XI_TRG_SOFTWARE = 405; // Generates an internal trigger. PRM_TRG_SOURCE must be set to TRG_SOFTWARE.
  CAP_PROP_XI_GPI_SELECTOR = 406; // Selects general purpose input
  CAP_PROP_XI_GPI_MODE     = 407; // Set general purpose input mode
  CAP_PROP_XI_GPI_LEVEL    = 408; // Get general purpose level
  CAP_PROP_XI_GPO_SELECTOR = 409; // Selects general purpose output
  CAP_PROP_XI_GPO_MODE     = 410; // Set general purpose output mode
  CAP_PROP_XI_LED_SELECTOR = 411; // Selects camera signalling LED
  CAP_PROP_XI_LED_MODE     = 412; // Define camera signalling LED functionality
  CAP_PROP_XI_MANUAL_WB    = 413; // Calculates White Balance(must be called during acquisition)
  CAP_PROP_XI_AUTO_WB      = 414; // Automatic white balance
  CAP_PROP_XI_AEAG         = 415; // Automatic exposure/gain
  CAP_PROP_XI_EXP_PRIORITY = 416; // Exposure priority (0.5 - exposure 50%; gain 50%).
  CAP_PROP_XI_AE_MAX_LIMIT = 417; // Maximum limit of exposure in AEAG procedure
  CAP_PROP_XI_AG_MAX_LIMIT = 418; // Maximum limit of gain in AEAG procedure
  CAP_PROP_XI_AEAG_LEVEL   = 419; // Average intensity of output signal AEAG should achieve(in %)
  CAP_PROP_XI_TIMEOUT      = 420; // Image capture timeout in milliseconds

  // Properties of cameras available through AVFOUNDATION interface
  CAP_PROP_IOS_DEVICE_FOCUS        = 9001;
  CAP_PROP_IOS_DEVICE_EXPOSURE     = 9002;
  CAP_PROP_IOS_DEVICE_FLASH        = 9003;
  CAP_PROP_IOS_DEVICE_WHITEBALANCE = 9004;
  CAP_PROP_IOS_DEVICE_TORCH        = 9005;

  // Properties of cameras available through Smartek Giganetix Ethernet Vision interface
  // * --- Vladimir Litvinenko (litvinenko.vladimir@gmail.com) --- */
  CAP_PROP_GIGA_FRAME_OFFSET_X   = 10001;
  CAP_PROP_GIGA_FRAME_OFFSET_Y   = 10002;
  CAP_PROP_GIGA_FRAME_WIDTH_MAX  = 10003;
  CAP_PROP_GIGA_FRAME_HEIGH_MAX  = 10004;
  CAP_PROP_GIGA_FRAME_SENS_WIDTH = 10005;
  CAP_PROP_GIGA_FRAME_SENS_HEIGH = 10006;

  CAP_PROP_INTELPERC_PROFILE_COUNT              = 11001;
  CAP_PROP_INTELPERC_PROFILE_IDX                = 11002;
  CAP_PROP_INTELPERC_DEPTH_LOW_CONFIDENCE_VALUE = 11003;
  CAP_PROP_INTELPERC_DEPTH_SATURATION_VALUE     = 11004;
  CAP_PROP_INTELPERC_DEPTH_CONFIDENCE_THRESHOLD = 11005;
  CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_HORZ    = 11006;
  CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_VERT    = 11007;

  // Intel PerC streams
  CAP_INTELPERC_DEPTH_GENERATOR = 1 shl 29;
  CAP_INTELPERC_IMAGE_GENERATOR = 1 shl 28;
  CAP_INTELPERC_GENERATORS_MASK = CAP_INTELPERC_DEPTH_GENERATOR + CAP_INTELPERC_IMAGE_GENERATOR;

  CAP_INTELPERC_DEPTH_MAP = 0;
  // Each pixel is a 16-bit integer. The value indicates the distance from an object to the camera's XY plane or the Cartesian depth.
  CAP_INTELPERC_UVDEPTH_MAP = 1;
  // Each pixel contains two 32-bit floating point values in the range of 0-1; representing the mapping of depth coordinates to the color coordinates.
  CAP_INTELPERC_IR_MAP = 2;
  // Each pixel is a 16-bit integer. The value indicates the intensity of the reflected laser beam.
  CAP_INTELPERC_IMAGE = 3;

  VIDEOWRITER_PROP_QUALITY    = 1; // Quality (0..100%) of the videostream encoded
  VIDEOWRITER_PROP_FRAMEBYTES = 2; // (Read-only): Size of just encoded video frame

  // gPhoto2 properties; if propertyId is less than 0 then work on widget with that __additive inversed__ camera setting ID
  // Get IDs by using CAP_PROP_GPHOTO2_WIDGET_ENUMERATE.
  // @see CvCaptureCAM_GPHOTO2 for more info
  CAP_PROP_GPHOTO2_PREVIEW          = 17001; // Capture only preview from liveview mode.
  CAP_PROP_GPHOTO2_WIDGET_ENUMERATE = 17002; // Readonly; returns (const char *).
  CAP_PROP_GPHOTO2_RELOAD_CONFIG    = 17003; // Trigger; only by set. Reload camera settings.
  CAP_PROP_GPHOTO2_RELOAD_ON_CHANGE = 17004; // Reload all settings on set.
  CAP_PROP_GPHOTO2_COLLECT_MSGS     = 17005; // Collect messages with details.
  CAP_PROP_GPHOTO2_FLUSH_MSGS       = 17006; // Readonly; returns (const char *).
  CAP_PROP_SPEED                    = 17007; // Exposure speed. Can be readonly; depends on camera program.
  CAP_PROP_APERTURE                 = 17008; // Aperture. Can be readonly; depends on camera program.
  CAP_PROP_EXPOSUREPROGRAM          = 17009; // Camera exposure program.
  CAP_PROP_VIEWFINDER               = 17010; // Enter liveview mode.

  // Flags for namedWindow
const
  WINDOW_NORMAL = CV_WINDOW_NORMAL;
  // the user can resize the window (no constraint) / also use to switch a fullscreen window to a normal size
  WINDOW_AUTOSIZE = CV_WINDOW_AUTOSIZE;
  // the user cannot resize the window, the size is constrainted by the image displayed
  WINDOW_OPENGL = CV_WINDOW_OPENGL; // window with opengl support

  WINDOW_FULLSCREEN = CV_WINDOW_FULLSCREEN; // change the window to fullscreen
  WINDOW_FREERATIO  = CV_WINDOW_FREERATIO;  // the image expends as much as it can (no ratio constraint)
  WINDOW_KEEPRATIO  = CV_WINDOW_KEEPRATIO;  // the ratio of the image is respected

  // CV_EXPORTS_W void namedWindow(const String& winname, int flags = WINDOW_AUTOSIZE);
procedure namedWindow(const winname: String; const flags: integer = WINDOW_AUTOSIZE);
// CV_EXPORTS_W void destroyWindow(const String& winname);
procedure destroyWindow(const winname: String);
// CV_EXPORTS_W void destroyAllWindows();
procedure destroyAllWindows();
// CV_EXPORTS_W int startWindowThread();
function startWindowThread(): integer;
// CV_EXPORTS_W int waitKey(int delay = 0);
function waitKey(const delay: integer = 0): integer;
// CV_EXPORTS_W void imshow(const String& winname, InputArray mat);
procedure imshow(const winname: String; const Mat: IMat);
// CV_EXPORTS_W void resizeWindow(const String& winname, int width, int height);
procedure resizeWindow(const winname: String; const width, height: integer);
// CV_EXPORTS_W void moveWindow(const String& winname, int x, int y);
procedure moveWindow(const winname: String; const x, y: integer);
// CV_EXPORTS_W void setWindowProperty(const String& winname, int prop_id, double prop_value);
procedure setWindowProperty(const winname: String; const prop_id: integer; const prop_value: double);
// CV_EXPORTS_W double getWindowProperty(const String& winname, int prop_id);
function getWindowProperty(const winname: String; const prop_id: integer): double;
// CV_EXPORTS int createTrackbar(const String& trackbarname, const String& winname,
// int* value, int count,
// TrackbarCallback onChange = 0,
// void* userdata = 0);
function createTrackbar(const trackbarname: String; const winname: String; Value: PInteger; count: integer;
  onChange: TCvTrackbarCallback2 = nil; userdata: Pointer = nil): integer;
// CV_EXPORTS_W Mat imread( const string& filename, int flags=1 );
function imread(const FileName: string; flag: integer = 1): IMat;
// CV_EXPORTS_W cboolean imwrite( const string& filename, InputArray img, const vector<int>& params=vector<int>());
// function imwrite(const filename: String; const img: TccvMat): cboolean;

Type
  IMaskGenerator = interface(IOCVCommon)
    ['{DABEB77F-A919-49EB-999F-C1876812A330}']
  end;

  TMaskGenerator = class(TOCVCommon, IMaskGenerator)

  end;

  // IFileNode = interface(IOCVCommon)
  // ['{8BB056BF-08AE-4512-9861-4906D770C2A0}']
  // end;
  // TFileNode = class(TOCVCommon, IFileNode)
  // end;

  ISize = interface(IOCVCommon)
    ['{33DA805D-9B45-4EF9-A657-C53DB60CDE9A}']
  end;

  TSize = class(TOCVCommon, ISize)
  public
    constructor Create(const OpenCVClass: TOpenCVClass);
    destructor Destroy; override;
  end;

  IRect2i = interface(IOCVCommon)
    ['{417FF1CA-D2A6-46BC-8FBA-84EF5642CE63}']
  end;

  TRect2i = class(TOCVCommon, IRect2i)

  end;

  IRect         = IRect2i;
  TVectorRect   = TArray<IRect>;
  TVectorInt    = TArray<integer>;
  TVectorDouble = TArray<double>;

  ICascadeClassifier = interface(IOCVCommon)
    ['{700C3DEC-F156-4014-9676-5BD9ECC3B01B}']
    function empty(): cbool;
    function load(const FileName: String): cbool;
    // function Read(const node: IFileNode): cbool;
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; scaleFactor: double { = 1.1 };
      minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var numDetections: TVectorInt;
      scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var rejectLevels: TVectorInt;
      Var levelWeights: TVectorDouble; scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 };
      flags: integer { = 0 }; minSize: ISize { = Size() }; maxSize: ISize { = Size() };
      outputRejectLevels: cbool = false); overload;
    function isOldFormatCascade(): cbool;
    function getOriginalWindowSize(): ISize;
    function getFeatureType(): integer;
    function convert(const oldcascade: String; const newcascade: String): cbool;
  end;

  TCascadeClassifier = class(TOCVCommon, ICascadeClassifier)
  private
  public
    constructor Create; overload;
    (* * @brief Loads a classifier from a file.

      @param filename Name of the file from which the classifier is loaded.
    *)
    constructor Create(const FileName: String); overload;
    destructor Destroy; override;
    (* * @brief Checks whether the classifier has been loaded.
    *)
    function empty(): cbool;
    (* * @brief Loads a classifier from a file.

      @param filename Name of the file from which the classifier is loaded. The file may contain an old
      HAAR classifier trained by the haartraining application or a new cascade classifier trained by the
      traincascade application.
    *)
    function load(const FileName: String): cbool;
    (* * @brief Reads a classifier from a FileStorage node.

      @note The file may contain a new cascade classifier (trained traincascade application) only.
    *)
    // function Read(const node: IFileNode): cbool;

    (* * @brief Detects objects of different sizes in the input image. The detected objects are returned as a list
      of rectangles.

      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored.

      The function is parallelized with the TBB library.

      @note
      -   (Python) A face detection example using cascade classifiers can be found at
      opencv_source_code/samples/python2/facedetect.py
    *)
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; scaleFactor: double { = 1.1 };
      minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;

    (* * @overload
      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param numDetections Vector of detection numbers for the corresponding objects. An object's number
      of detections is the number of neighboring positively classified rectangles that were joined
      together to form the object.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored.
    *)
    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var numDetections: TVectorInt;
      scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: ISize { = Size() };
      maxSize: ISize { = Size() } ); overload;

    (* * @overload
      if `outputRejectLevels` is `true` returns `rejectLevels` and `levelWeights`
    *)

    procedure detectMultiScale(Image: IMat; Var objects: TVectorRect; var rejectLevels: TVectorInt;
      Var levelWeights: TVectorDouble; scaleFactor: double { = 1.1 }; minNeighbors: integer { = 3 };
      flags: integer { = 0 }; minSize: ISize { = Size() }; maxSize: ISize { = Size() };
      outputRejectLevels: cbool = false); overload;

    function isOldFormatCascade(): cbool;
    function getOriginalWindowSize(): ISize;
    function getFeatureType(): integer;
    function convert(const oldcascade: String; const newcascade: String): cbool;
  end;

Type
  TIplImageRecordHelper = record helper for TIplImage
    function InitFromMat(const Mat: IMat): TIplImage;
  end;

  TStringAnsiHelper = record helper for
    String
    function AsPAnsiChar: PAnsiChar;
  end;

implementation

Uses
  ocv.core_c, ocv.utils, ocv.lib;

// ------------------------------ Mat ------------------------------
function _CreateMat: TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateMat@0';
function _GetMatData(const e: TOpenCVClass; index: integer; param: integer = 0): integer; stdcall;
  external opencv_classes_lib name '_GetMatData@12';
function _MatEmpty(const e: TOpenCVClass): cbool; stdcall; external opencv_classes_lib name '_MatEmpty@4';
function _CreateMatFromImage(const Image: pIplImage): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_CreateMatFromImage@4';
procedure _DestroyMat(const M: TOpenCVClass); stdcall; external opencv_classes_lib name '_DestroyMat@4';

// ------------------------------ VideoCapture ------------------------------
function _CreateVideoCapture: TOpenCVVideoCaptureClass; stdcall;
  external opencv_classes_lib name '_CreateVideoCapture@0';
function _VideoCaptureOpen(const e: TOpenCVVideoCaptureClass; CamNumber: integer): cbool; stdcall;
  external opencv_classes_lib name '_VideoCaptureOpen@8';
function _VideoCaptureOpenFileName(const e: TOpenCVVideoCaptureClass; FileName: PAnsiChar): cbool; stdcall;
  external opencv_classes_lib name '_VideoCaptureOpenFileName@8';
function _VideoCaptureisOpened(const e: TOpenCVVideoCaptureClass): cbool; stdcall;
  external opencv_classes_lib name '_VideoCaptureisOpened@4';
function _VideoCaptureRead(const e: TOpenCVVideoCaptureClass; var M: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_VideoCaptureRead@8';
function _VideoCaptureSet(const e: TOpenCVVideoCaptureClass; propId: integer; Value: double): cbool; stdcall;
  external opencv_classes_lib name '_VideoCaptureSet@16';
function _VideoCaptureGet(const e: TOpenCVVideoCaptureClass; propId: integer): double; stdcall;
  external opencv_classes_lib name '_VideoCaptureGet@8';
procedure _DestroyVideoCapture(const e: TOpenCVVideoCaptureClass); stdcall;
  external opencv_classes_lib name '_DestroyVideoCapture@4';

// ------------------------------ CascadeClassifier ------------------------------
function _CreateCascadeClassifier: TOpenCVClass; stdcall; external opencv_classes_lib name '_CreateCascadeClassifier@0';
procedure _DestroyCascadeClassifier(const CascadeClassifier: TOpenCVClass); stdcall;
  external opencv_classes_lib name '_DestroyCascadeClassifier@4';
function _get_CascadeClassifier_empty(CascadeClassifier: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_get_CascadeClassifier_empty@4';
function _get_CascadeClassifier_load(CascadeClassifier: TOpenCVClass; FileName: PAnsiChar): cbool; stdcall;
  external opencv_classes_lib name '_get_CascadeClassifier_load@8';
function _CascadeClassifier_isOldFormatCascade(CascadeClassifier: TOpenCVClass): cbool; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_isOldFormatCascade@4';
function _CascadeClassifier_getOriginalWindowSize(CascadeClassifier: TOpenCVClass): TOpenCVClass; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_getOriginalWindowSize@4';
function _CascadeClassifier_getFeatureType(CascadeClassifier: TOpenCVClass): integer; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_getFeatureType@4';
function _CascadeClassifier_convert(CascadeClassifier: TOpenCVClass; oldcascade, newcascade: PAnsiChar): cbool; stdcall;
  external opencv_classes_lib name '_CascadeClassifier_convert@12';

procedure namedWindow(const winname: String; const flags: integer = WINDOW_AUTOSIZE);
begin
  cvNamedWindow(winname.AsPAnsiChar, flags);
end;

procedure destroyWindow(const winname: String);
begin
  cvDestroyWindow(winname.AsPAnsiChar);
end;

procedure destroyAllWindows();
begin
  cvDestroyAllWindows();
end;

function startWindowThread(): integer;
begin
  Result := cvStartWindowThread();
end;

function waitKey(const delay: integer = 0): integer;
begin
  Result := cvWaitKey(delay);
end;

procedure imshow(const winname: String; const Mat: IMat);
Var
  IplImage: TIplImage;
begin
  IplImage.InitFromMat(Mat);
  cvShowImage(winname.AsPAnsiChar, @IplImage);
end;

procedure resizeWindow(const winname: String; const width, height: integer);
begin
  cvResizeWindow(winname.AsPAnsiChar, width, height);
end;

procedure moveWindow(const winname: String; const x, y: integer);
begin
  cvMoveWindow(winname.AsPAnsiChar, x, y);
end;

procedure setWindowProperty(const winname: String; const prop_id: integer; const prop_value: double);
begin
  cvSetWindowProperty(winname.AsPAnsiChar, prop_id, prop_value);
end;

function getWindowProperty(const winname: String; const prop_id: integer): double;
begin
  Result := cvGetWindowProperty(winname.AsPAnsiChar, prop_id);
end;

function createTrackbar(const trackbarname: String; const winname: String; Value: PInteger; count: integer;
  onChange: TCvTrackbarCallback2 = nil; userdata: Pointer = nil): integer;
begin
  Result := cvCreateTrackbar2(trackbarname.AsPAnsiChar, winname.AsPAnsiChar, Value, count, onChange, userdata);
end;

function imread(const FileName: string; flag: integer): IMat;
begin
  Result := TMat.Create(cvLoadImage(FileName.AsPAnsiChar, flag));
end;

{ ------------------------------ TMat ------------------------------ }

function TMat.channels: integer;
begin
  Result := _GetMatData(FData, 4);
end;

function TMat.cols: integer;
begin
  Result := _GetMatData(FData, 10);
end;

constructor TMat.Create(const Image: pIplImage);
begin
  FData := _CreateMatFromImage(Image);
end;

constructor TMat.Create(const _M: TOpenCVClass; const NeedDestroy: boolean);
begin
  FNeedDestroy := NeedDestroy;
  if Assigned(_M) then
    FData := _M
  else
    FData := _CreateMat;
end;

function TMat.data: Pointer;
begin
  Result := Pointer(_GetMatData(FData, 11));
end;

function TMat.depth: integer;
begin
  Result := _GetMatData(FData, 3);
end;

destructor TMat.Destroy;
begin
  if Assigned(FData) and FNeedDestroy then
    _DestroyMat(FData);
  inherited;
end;

function TMat.dims: integer;
begin
  Result := _GetMatData(FData, 8);
end;

function TMat.elemSize: size_t;
begin
  Result := _GetMatData(FData, 0);
end;

function TMat.elemSize1: size_t;
begin
  Result := _GetMatData(FData, 1);
end;

function TMat.empty: cbool;
begin
  Result := _MatEmpty(FData);
end;

function TMat.flags: integer;
begin
  Result := _GetMatData(FData, 7);
end;

function TMat.rows: integer;
begin
  Result := _GetMatData(FData, 9);
end;

function TMat.step1(i: integer): size_t;
begin
  Result := _GetMatData(FData, 5, i);
end;

function TMat.total: size_t;
begin
  Result := _GetMatData(FData, 6);
end;

function TMat._type: integer;
begin
  Result := _GetMatData(FData, 2);
end;

{ ------------------------------ TVideoCapture ------------------------------ }

constructor TVideoCapture.Create(const CamNumber: integer);
begin
  Create;
  if Assigned(FData) then
    _VideoCaptureOpen(FData, CamNumber);
end;

constructor TVideoCapture.Create(const FileName: String);
begin
  Create;
  if Assigned(FData) then
    _VideoCaptureOpenFileName(FData, FileName.AsPAnsiChar);
end;

constructor TVideoCapture.Create;
begin
  FData := _CreateVideoCapture;
end;

destructor TVideoCapture.Destroy;
begin
  if Assigned(FData) then
    _DestroyVideoCapture(FData);
  inherited;
end;

function TVideoCapture.getProp(const propId: integer): double;
begin
  Result := _VideoCaptureGet(FData, propId);
end;

function TVideoCapture.isOpened: cbool;
begin
  Result := _VideoCaptureisOpened(FData);
end;

function TVideoCapture.Open(const FileName: String): cbool;
begin
  if Assigned(FData) then
    Result := _VideoCaptureOpenFileName(FData, FileName.AsPAnsiChar)
  else
    Result := false;
end;

function TVideoCapture.PropSet(const propId: integer; const Value: double): cbool;
begin
  Result := _VideoCaptureSet(FData, propId, Value);
end;

function TVideoCapture.Open(const CamNumber: integer): cbool;
begin
  Result := _VideoCaptureOpen(FData, CamNumber);
end;

function TVideoCapture.Read(var Mat: IMat): cbool;
Var
  _M: TOpenCVClass;
begin
  Result := _VideoCaptureRead(FData, _M);
  Mat    := TMat.Create(_M);
end;

procedure TVideoCapture.setProp(const propId: integer; const Value: double);
begin
  _VideoCaptureSet(FData, propId, Value);
end;

{ ------------------------------ TIplImageRecordHelper ------------------------------ }

function TIplImageRecordHelper.InitFromMat(const Mat: IMat): TIplImage;
begin
  Assert(Mat.dims <= 2);
  cvInitImageHeader(@Self, CvSize(Mat.cols, Mat.rows), cvIplDepth(Mat.flags), Mat.channels);
  cvSetData(@Self, Mat.data, Mat.step1);
end;

{ ------------------------------ TStringAnsiHelper ------------------------------ }

function TStringAnsiHelper.AsPAnsiChar: PAnsiChar;
begin
  Result := c_str(Self);
end;

{ ------------------------------ TCascadeClassifier ------------------------------ }

function TCascadeClassifier.convert(const oldcascade, newcascade: String): cbool;
begin
  Result := _CascadeClassifier_convert(FData, oldcascade.AsPAnsiChar, newcascade.AsPAnsiChar);
end;

constructor TCascadeClassifier.Create;
begin
  FData := _CreateCascadeClassifier;
end;

constructor TCascadeClassifier.Create(const FileName: String);
begin
  FData := _CreateCascadeClassifier;
  load(FileName);
end;

destructor TCascadeClassifier.Destroy;
begin
  if Assigned(FData) then
    _DestroyCascadeClassifier(FData);
  inherited;
end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TVectorRect; var numDetections: TVectorInt;
  scaleFactor: double; minNeighbors, flags: integer; minSize, maxSize: ISize);
begin

end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TVectorRect; var rejectLevels: TVectorInt;
  var levelWeights: TVectorDouble; scaleFactor: double; minNeighbors, flags: integer; minSize, maxSize: ISize;
  outputRejectLevels: cbool);
begin

end;

procedure TCascadeClassifier.detectMultiScale(Image: IMat; var objects: TVectorRect; scaleFactor: double;
  minNeighbors, flags: integer; minSize, maxSize: ISize);
begin

end;

function TCascadeClassifier.empty: cbool;
begin
  Result := _get_CascadeClassifier_empty(FData);
end;

function TCascadeClassifier.getFeatureType: integer;
begin
  Result := _CascadeClassifier_getFeatureType(FData);
end;

function TCascadeClassifier.getOriginalWindowSize: ISize;
begin
  Result := TSize.Create(_CascadeClassifier_getOriginalWindowSize(FData));
end;

function TCascadeClassifier.isOldFormatCascade: cbool;
begin
  Result := _CascadeClassifier_isOldFormatCascade(FData);
end;

function TCascadeClassifier.load(const FileName: String): cbool;
begin
  Result := _get_CascadeClassifier_load(FData, FileName.AsPAnsiChar);
end;

{ TOCVCommon }

function TOCVCommon._InternalData: TOpenCVClass;
begin
  Result := FData;
end;

{ TSize }

constructor TSize.Create(const OpenCVClass: TOpenCVClass);
begin
  FData := OpenCVClass;
end;

destructor TSize.Destroy;
begin

  inherited;
end;

end.

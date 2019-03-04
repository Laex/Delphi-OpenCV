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
{$IFNDEF CLR}
{$I Opencv.inc}
unit ocv.comp.ImageOperation;
{$ENDIF}

interface

uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  Vcl.Graphics,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Types,
  System.ZLib,
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Graphics,
  SysUtils,
  Classes,
  SyncObjs,
{$IFNDEF DELPHI5}
  Types,
{$ENDIF}
  ZLib,
{$ENDIF}
  ocv.comp.Types,
  ocv.comp.proc,
  ocv.objdetect_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.editor;

type
{$IFDEF DELPHIXE3_UP} // XE3..XE6
  TArrayDouble = TArray<Double>;
  TArrayInteger = TArray<Integer>;
  TArrayBoolean = TArray<Boolean>;
{$ELSE} // D7...XE2
  TArrayDouble = Array of Double;
  TArrayInteger = Array of Integer;
  TArrayBoolean = Array of Boolean;
{$ENDIF}

  TocvCustomImageOperation = class(TComponent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FCriticalSection: TCriticalSection;
    FOwner: TPersistent;
    FFloatParams: TArrayDouble;
    FIntParams: TArrayInteger;
    FBoolParams: TArrayBoolean;
    FOnAfterTransform: TOnOcvAfterTransform;
    FOnBeforeTransform: TOnOcvBeforeTransform;
  protected
    function GetFloatParam(const index: Integer): Double; virtual;
    function GetIntParam(const index: Integer): Integer; virtual;
    function GetBoolParam(const index: Integer): Boolean; virtual;
    procedure SetFloatParam(const index: Integer; const Value: Double);
    procedure SetIntParam(const index: Integer; const Value: Integer);
    procedure SetBoolParam(const index: Integer; const Value: Boolean);
    function LockTransform: Boolean;
    procedure UnlockTransform;
    function GetOwner: TPersistent; override;
    property FloatParams[const index: Integer]: Double Read GetFloatParam write SetFloatParam;
    property IntParams[const index: Integer]: Integer Read GetIntParam write SetIntParam;
    property BoolParams[const index: Integer]: Boolean Read GetBoolParam write SetBoolParam;
  public
    constructor Create(AOwner: TPersistent); reintroduce; virtual;
    destructor Destroy; override;
    function Transform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
    function GetNamePath: string; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; virtual;
    property Name;
  published
    property OnAfterTransform: TOnOcvAfterTransform read FOnAfterTransform write FOnAfterTransform;
    property OnBeforeTransform: TOnOcvBeforeTransform read FOnBeforeTransform write FOnBeforeTransform;
  end;

  TocvImageOperationClass = class of TocvCustomImageOperation;

  IocvEditorPropertiesContainer = interface
    ['{418F88DD-E35D-4425-BF24-E753E83D35D6}']
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
  end;

  TocvCustomImageOperationWithNestedOperation = class(TocvCustomImageOperation, IocvEditorPropertiesContainer)
  private
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    CS: TCriticalSection;
  protected
    function LockTransform: Boolean;
    procedure UnlockTransform;
    // ---------------------------------------------
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    // ---------------------------------------------
    function GetPropertiesClassName: string;
    procedure SetPropertiesClassName(const Value: string);

    function GetProperties: TocvCustomImageOperation;
    procedure SetProperties(const Value: TocvCustomImageOperation);

    function GetPropertiesClass: TocvImageOperationClass; virtual;
    procedure SetPropertiesClass(Value: TocvImageOperationClass);

    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
  end;

  TocvNoneOperation = class(TocvCustomImageOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TocvInterpolationMethod = (INTER_NN, INTER_LINEAR, INTER_CUBIC, INTER_AREA, INTER_LANCZOS4);

  TocvResizeOperation = class(TocvCustomImageOperation)
  private
    FInterpolation: TocvInterpolationMethod;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property DestWidth: Integer index 0 Read GetIntParam write SetIntParam;
    property DestHeight: Integer index 1 Read GetIntParam write SetIntParam;
    property Interpolation: TocvInterpolationMethod read FInterpolation write FInterpolation default INTER_LINEAR;
  end;

  TocvColorConversion = (BGR2BGRA, RGB2RGBA, BGRA2BGR, RGBA2RGB, BGR2RGBA, RGB2BGRA, RGBA2BGR, BGRA2RGB, BGR2RGB,
    RGB2BGR, BGRA2RGBA, RGBA2BGRA, BGR2GRAY, RGB2GRAY, GRAY2BGR, GRAY2RGB, GRAY2BGRA, GRAY2RGBA, BGRA2GRAY, RGBA2GRAY,
    BGR2BGR565, RGB2BGR565, BGR5652BGR, BGR5652RGB, BGRA2BGR565, RGBA2BGR565, BGR5652BGRA, BGR5652RGBA, GRAY2BGR565,
    BGR5652GRAY, BGR2BGR555, RGB2BGR555, BGR5552BGR, BGR5552RGB, BGRA2BGR555, RGBA2BGR555, BGR5552BGRA, BGR5552RGBA,
    GRAY2BGR555, BGR5552GRAY, BGR2XYZ, RGB2XYZ, XYZ2BGR, XYZ2RGB, BGR2YCrCb, RGB2YCrCb, YCrCb2BGR, YCrCb2RGB, BGR2HSV,
    RGB2HSV, BGR2Lab, RGB2Lab, BayerBG2BGR, BayerGB2BGR, BayerRG2BGR, BayerGR2BGR, BayerBG2RGB, BayerGB2RGB,
    BayerRG2RGB, BayerGR2RGB, BGR2Luv, RGB2Luv, BGR2HLS, RGB2HLS, HSV2BGR, HSV2RGB, Lab2BGR, Lab2RGB, Luv2BGR, Luv2RGB,
    HLS2BGR, HLS2RGB, BayerBG2BGR_VNG, BayerGB2BGR_VNG, BayerRG2BGR_VNG, BayerGR2BGR_VNG, BayerBG2RGB_VNG,
    BayerGB2RGB_VNG, BayerRG2RGB_VNG, BayerGR2RGB_VNG, BGR2HSV_FULL, RGB2HSV_FULL, BGR2HLS_FULL, RGB2HLS_FULL,
    HSV2BGR_FULL, HSV2RGB_FULL, HLS2BGR_FULL, HLS2RGB_FULL, LBGR2Lab, LRGB2Lab, LBGR2Luv, LRGB2Luv, Lab2LBGR, Lab2LRGB,
    Luv2LBGR, Luv2LRGB, BGR2YUV, RGB2YUV, YUV2BGR, YUV2RGB, BayerBG2GRAY, BayerGB2GRAY, BayerRG2GRAY, BayerGR2GRAY,
    // YUV 4:2:0 formats family;
    YUV2RGB_NV12, YUV2BGR_NV12, YUV2RGB_NV21, YUV2BGR_NV21, YUV420sp2RGB, YUV420sp2BGR, YUV2RGBA_NV12, YUV2BGRA_NV12,
    YUV2RGBA_NV21, YUV2BGRA_NV21, YUV420sp2RGBA, YUV420sp2BGRA, YUV2RGB_YV12, YUV2BGR_YV12, YUV2RGB_IYUV, YUV2BGR_IYUV,
    YUV2RGB_I420, YUV2BGR_I420, YUV420p2RGB, YUV420p2BGR, YUV2RGBA_YV12, YUV2BGRA_YV12, YUV2RGBA_IYUV, YUV2BGRA_IYUV,
    YUV2RGBA_I420, YUV2BGRA_I420, YUV420p2RGBA, YUV420p2BGRA, YUV2GRAY_420, YUV2GRAY_NV21, YUV2GRAY_NV12, YUV2GRAY_YV12,
    YUV2GRAY_IYUV, YUV2GRAY_I420, YUV420sp2GRAY, YUV420p2GRAY,
    // YUV 4:2:2 formats family;
    YUV2RGB_UYVY, YUV2BGR_UYVY, YUV2RGB_Y422, YUV2BGR_Y422, YUV2RGB_UYNV, YUV2BGR_UYNV, YUV2RGBA_UYVY, YUV2BGRA_UYVY,
    YUV2RGBA_Y422, YUV2BGRA_Y422, YUV2RGBA_UYNV, YUV2BGRA_UYNV, YUV2RGB_YUY2, YUV2BGR_YUY2, YUV2RGB_YVYU, YUV2BGR_YVYU,
    YUV2RGB_YUYV, YUV2BGR_YUYV, YUV2RGB_YUNV, YUV2BGR_YUNV, YUV2RGBA_YUY2, YUV2BGRA_YUY2, YUV2RGBA_YVYU, YUV2BGRA_YVYU,
    YUV2RGBA_YUYV, YUV2BGRA_YUYV, YUV2RGBA_YUNV, YUV2BGRA_YUNV, YUV2GRAY_UYVY, YUV2GRAY_YUY2, YUV2GRAY_Y422,
    YUV2GRAY_UYNV, YUV2GRAY_YVYU, YUV2GRAY_YUYV, YUV2GRAY_YUNV,
    // alpha premultiplication;
    RGBA2mRGBA, mRGBA2RGBA, COLORCVT_MAX);

  TocvIPLDepth = (DEPTH_1U, DEPTH_8U, DEPTH_16U, DEPTH_32F, DEPTH_64F, DEPTH_8S, DEPTH_16S, DEPTH_32S);

  TocvCvtColorOperation = class(TocvCustomImageOperation)
  private const
    cIPLDepth: array [TocvIPLDepth] of Integer = (IPL_DEPTH_1U, IPL_DEPTH_8U, IPL_DEPTH_16U, IPL_DEPTH_32F,
      IPL_DEPTH_64F, (IPL_DEPTH_SIGN or 8), IPL_DEPTH_16S, IPL_DEPTH_32S);

    cColorConversion: array [TocvColorConversion] of Integer = (CV_BGR2BGRA, CV_RGB2RGBA, CV_BGRA2BGR, CV_RGBA2RGB,
      CV_BGR2RGBA, CV_RGB2BGRA, CV_RGBA2BGR, CV_BGRA2RGB, CV_BGR2RGB, CV_RGB2BGR, CV_BGRA2RGBA, CV_RGBA2BGRA,
      CV_BGR2GRAY, CV_RGB2GRAY, CV_GRAY2BGR, CV_GRAY2RGB, CV_GRAY2BGRA, CV_GRAY2RGBA, CV_BGRA2GRAY, CV_RGBA2GRAY,
      CV_BGR2BGR565, CV_RGB2BGR565, CV_BGR5652BGR, CV_BGR5652RGB, CV_BGRA2BGR565, CV_RGBA2BGR565, CV_BGR5652BGRA,
      CV_BGR5652RGBA, CV_GRAY2BGR565, CV_BGR5652GRAY, CV_BGR2BGR555, CV_RGB2BGR555, CV_BGR5552BGR, CV_BGR5552RGB,
      CV_BGRA2BGR555, CV_RGBA2BGR555, CV_BGR5552BGRA, CV_BGR5552RGBA, CV_GRAY2BGR555, CV_BGR5552GRAY, CV_BGR2XYZ,
      CV_RGB2XYZ, CV_XYZ2BGR, CV_XYZ2RGB, CV_BGR2YCrCb, CV_RGB2YCrCb, CV_YCrCb2BGR, CV_YCrCb2RGB, CV_BGR2HSV,
      CV_RGB2HSV, CV_BGR2Lab, CV_RGB2Lab, CV_BayerBG2BGR, CV_BayerGB2BGR, CV_BayerRG2BGR, CV_BayerGR2BGR,
      CV_BayerBG2RGB, CV_BayerGB2RGB, CV_BayerRG2RGB, CV_BayerGR2RGB, CV_BGR2Luv, CV_RGB2Luv, CV_BGR2HLS, CV_RGB2HLS,
      CV_HSV2BGR, CV_HSV2RGB, CV_Lab2BGR, CV_Lab2RGB, CV_Luv2BGR, CV_Luv2RGB, CV_HLS2BGR, CV_HLS2RGB,
      CV_BayerBG2BGR_VNG, CV_BayerGB2BGR_VNG, CV_BayerRG2BGR_VNG, CV_BayerGR2BGR_VNG, CV_BayerBG2RGB_VNG,
      CV_BayerGB2RGB_VNG, CV_BayerRG2RGB_VNG, CV_BayerGR2RGB_VNG, CV_BGR2HSV_FULL, CV_RGB2HSV_FULL, CV_BGR2HLS_FULL,
      CV_RGB2HLS_FULL, CV_HSV2BGR_FULL, CV_HSV2RGB_FULL, CV_HLS2BGR_FULL, CV_HLS2RGB_FULL, CV_LBGR2Lab, CV_LRGB2Lab,
      CV_LBGR2Luv, CV_LRGB2Luv, CV_Lab2LBGR, CV_Lab2LRGB, CV_Luv2LBGR, CV_Luv2LRGB, CV_BGR2YUV, CV_RGB2YUV, CV_YUV2BGR,
      CV_YUV2RGB, CV_BayerBG2GRAY, CV_BayerGB2GRAY, CV_BayerRG2GRAY, CV_BayerGR2GRAY,
      // YUV 4:2:0 formats family;
      CV_YUV2RGB_NV12, CV_YUV2BGR_NV12, CV_YUV2RGB_NV21, CV_YUV2BGR_NV21, CV_YUV420sp2RGB, CV_YUV420sp2BGR,
      CV_YUV2RGBA_NV12, CV_YUV2BGRA_NV12, CV_YUV2RGBA_NV21, CV_YUV2BGRA_NV21, CV_YUV420sp2RGBA, CV_YUV420sp2BGRA,
      CV_YUV2RGB_YV12, CV_YUV2BGR_YV12, CV_YUV2RGB_IYUV, CV_YUV2BGR_IYUV, CV_YUV2RGB_I420, CV_YUV2BGR_I420,
      CV_YUV420p2RGB, CV_YUV420p2BGR, CV_YUV2RGBA_YV12, CV_YUV2BGRA_YV12, CV_YUV2RGBA_IYUV, CV_YUV2BGRA_IYUV,
      CV_YUV2RGBA_I420, CV_YUV2BGRA_I420, CV_YUV420p2RGBA, CV_YUV420p2BGRA, CV_YUV2GRAY_420, CV_YUV2GRAY_NV21,
      CV_YUV2GRAY_NV12, CV_YUV2GRAY_YV12, CV_YUV2GRAY_IYUV, CV_YUV2GRAY_I420, CV_YUV420sp2GRAY, CV_YUV420p2GRAY,
      // YUV 4:2:2 formats family;
      CV_YUV2RGB_UYVY, CV_YUV2BGR_UYVY, CV_YUV2RGB_Y422, CV_YUV2BGR_Y422, CV_YUV2RGB_UYNV, CV_YUV2BGR_UYNV,
      CV_YUV2RGBA_UYVY, CV_YUV2BGRA_UYVY, CV_YUV2RGBA_Y422, CV_YUV2BGRA_Y422, CV_YUV2RGBA_UYNV, CV_YUV2BGRA_UYNV,
      CV_YUV2RGB_YUY2, CV_YUV2BGR_YUY2, CV_YUV2RGB_YVYU, CV_YUV2BGR_YVYU, CV_YUV2RGB_YUYV, CV_YUV2BGR_YUYV,
      CV_YUV2RGB_YUNV, CV_YUV2BGR_YUNV, CV_YUV2RGBA_YUY2, CV_YUV2BGRA_YUY2, CV_YUV2RGBA_YVYU, CV_YUV2BGRA_YVYU,
      CV_YUV2RGBA_YUYV, CV_YUV2BGRA_YUYV, CV_YUV2RGBA_YUNV, CV_YUV2BGRA_YUNV, CV_YUV2GRAY_UYVY, CV_YUV2GRAY_YUY2,
      CV_YUV2GRAY_Y422, CV_YUV2GRAY_UYNV, CV_YUV2GRAY_YVYU, CV_YUV2GRAY_YUYV, CV_YUV2GRAY_YUNV,
      // alpha premultiplication;
      CV_RGBA2mRGBA, CV_mRGBA2RGBA, CV_COLORCVT_MAX);
  private
    FColorConversion: TocvColorConversion;
    // FAutoCalcParams: Boolean;
    FChannels: Integer;
    FDepth: TocvIPLDepth;
  public
    constructor Create(AOwner: TPersistent); override;
    // procedure CalculateImageParams(const Source: IocvImage);
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    // property AutoCalcParams: Boolean read FAutoCalcParams write FAutoCalcParams default True;
    property ColorConversion: TocvColorConversion read FColorConversion write FColorConversion default RGB2GRAY;
    property Depth: TocvIPLDepth Read FDepth write FDepth;
    property Channels: Integer read FChannels write FChannels;
  end;

  TocvGrayScaleOperation = class(TocvCvtColorOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
    property ColorConversion;
    property Depth;
    property Channels;
  end;

  TocvCannyOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Threshold1: Double index 0 Read GetFloatParam write SetFloatParam;
    property Threshold2: Double index 1 Read GetFloatParam write SetFloatParam;
    property ApertureSize: Integer index 0 Read GetIntParam write SetIntParam;
  end;

  TocvErodeDilateMode = (SHAPE_RECT, SHAPE_CROSS, SHAPE_ELLIPSE, SHAPE_CUSTOM);

  TocvCustomErodeDilateOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FMorphOp: TocvErodeDilateMode;
    procedure SetMorphOp(const Value: TocvErodeDilateMode);
  protected
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Radius: Integer index 0 Read GetIntParam write SetIntParam;
    property Iterations: Integer index 1 Read GetIntParam write SetIntParam;
    property MorphOp: TocvErodeDilateMode read FMorphOp write SetMorphOp default SHAPE_RECT;
  end;

  TocvErodeOperation = class(TocvCustomErodeDilateOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TocvDilateOperation = class(TocvCustomErodeDilateOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TocvLaplaceOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Aperture: Integer index 0 Read GetIntParam write SetIntParam;
  end;

  TocvScalar = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FCvScalar: TCvScalar;
    function GetScalar(const index: Integer): Double;
    procedure SetScalar(const index: Integer; const Value: Double);
  public
    property CvScalar: TCvScalar read FCvScalar write FCvScalar;
    property Scalar[const Index: Integer]: Double read GetScalar write SetScalar;
  published
    property Val0: Double index 0 read GetScalar write SetScalar;
    property Val1: Double index 1 read GetScalar write SetScalar;
    property Val2: Double index 2 read GetScalar write SetScalar;
    property Val3: Double index 3 read GetScalar write SetScalar;
  end;

  TOnGetImage = procedure(Sender: TObject; Var Source2Image: IocvImage) of object;

  TocvCommonMathOperation = class(TocvCustomImageOperation, IocvDataReceiver)
  private
    FocvVideoSource: IocvDataSource;
    FSrource2Image: IocvImage;
    FOnGetImage: TOnGetImage;
    FOnGetMaskImage: TOnGetImage;
    FTransformInterpolation: TocvInterpolationMethod;
    procedure SetVideoSource_Source2(const Value: IocvDataSource);
    procedure DoGetSourceImage(Var Image: IocvImage);
    procedure DoGetMaskImage(Var Image: IocvImage);
    procedure GetImagesForTransorm(out Source1: IocvImage; out Source2, Mask: IocvImage);
    // --------------------------------------
    property VideoSource: IocvDataSource Read FocvVideoSource write SetVideoSource_Source2;
    property OnGetSourceImage: TOnGetImage read FOnGetImage write FOnGetImage;
    property OnGetMaskImage: TOnGetImage read FOnGetMaskImage write FOnGetMaskImage;
    property TransformInterpolation: TocvInterpolationMethod read FTransformInterpolation write FTransformInterpolation
      default INTER_CUBIC;
  public
    constructor Create(AOwner: TPersistent); override;
  protected
    procedure TakeImage(const IplImage: IocvImage);
    procedure SetVideoSource(const Value: TObject);
  published
  end;

  TocvAddWeightedOperation = class(TocvCommonMathOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property VideoSource;
    property OnGetSourceImage;
    property TransformInterpolation;

    property Alpha: Double index 0 Read GetFloatParam write SetFloatParam;
    property Beta: Double index 1 Read GetFloatParam write SetFloatParam;
    property Gamma: Double index 2 Read GetFloatParam write SetFloatParam;
  end;

  TocvLogicType = (ioAdd, ioSub, ioAnd, ioOr, ioXor);

  TocvLogicOperation = class(TocvCommonMathOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FOperation: TocvLogicType;
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property VideoSource;
    property OnGetSourceImage;
    property OnGetMaskImage;
    property TransformInterpolation;

    property Operation: TocvLogicType read FOperation write FOperation default ioAdd;
  end;

  TocvLogicSType = (ioAddS, ioSubS, ioSubRS, ioXorS);

  TocvLogicSOperation = class(TocvCommonMathOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FOperation: TocvLogicSType;
    FValue: TocvScalar;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property OnGetMaskImage;
    property TransformInterpolation;

    property Operation: TocvLogicSType read FOperation write FOperation default ioAddS;
    property Value: TocvScalar read FValue write FValue;
  end;

  TocvSobelOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property XOrder: Integer index 0 Read GetIntParam write SetIntParam;
    property YOrder: Integer index 1 Read GetIntParam write SetIntParam;
    property Aperture: Integer index 2 Read GetIntParam write SetIntParam;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TocvSmoothOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSmoothOperation: TocvSmoothOperations;
    procedure SetSmoothOperation(const Value: TocvSmoothOperations);
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property sigma1: Double index 0 Read GetFloatParam write SetFloatParam;
    property sigma2: Double index 1 Read GetFloatParam write SetFloatParam;
    property size1: Integer index 0 Read GetIntParam write SetIntParam;
    property size2: Integer index 1 Read GetIntParam write SetIntParam;
    property SmoothType: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
  end;

  TocvThresholdType = (THRESH_BINARY, THRESH_BINARY_INV, THRESH_TRUNC, THRESH_TOZERO, THRESH_TOZERO_INV, THRESH_MASK,
    THRESH_OTSU);

  TocvCustomThresholdOperation = class(TocvCustomImageOperation)
  private
    function GetThresholdType: TocvThresholdType;
    procedure SetThresholdType(const Value: TocvThresholdType);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property MaxValue: Double index 0 Read GetFloatParam write SetFloatParam; // default 250;
    property ThresholdType: TocvThresholdType read GetThresholdType write SetThresholdType default THRESH_BINARY;
    // index 0
  end;

  TocvThresholdOperation = class(TocvCustomThresholdOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Threshold: Double index 1 Read GetFloatParam write SetFloatParam; // default 50;
  end;

  TocvAdaptiveThresholdType = (ADAPTIVE_THRESH_MEAN_C, ADAPTIVE_THRESH_GAUSSIAN_C);

  TocvAdaptiveThresholdOperation = class(TocvCustomThresholdOperation)
  private
    function GetAdaptiveThresholdType: TocvAdaptiveThresholdType;
    procedure SetAdaptiveThresholdType(const Value: TocvAdaptiveThresholdType);
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property AdaptiveThresholdType: TocvAdaptiveThresholdType read GetAdaptiveThresholdType
      write SetAdaptiveThresholdType default ADAPTIVE_THRESH_MEAN_C; // index 1
    property BlockSize: Integer index 2 Read GetIntParam write SetIntParam; // 3
    property Param: Double index 1 Read GetFloatParam write SetFloatParam; // 5;
  end;

  TocvPoint2D32i = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSize: TcvSize;
  public
    constructor Create(const AX: Integer = 0; const AY: Integer = 0);
    property Size: TcvSize read FSize write FSize;
  published
    property X: Integer read FSize.Width write FSize.Width;
    property Y: Integer read FSize.height write FSize.height;
  end;

  TocvPoint2D32f = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FX, FY: Single;
  public
    constructor Create(const AX: Single = 0; const AY: Single = 0);
  published
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
  end;

  TocvRect32i = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FRight: Integer;
    FBottom: Integer;
    FTop: Integer;
    FLeft: Integer;
    function GetHeight: Integer;
    function GetOcvRect: TocvRect;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetOcvRect(const Value: TocvRect);
    procedure SetWidth(const Value: Integer);
    function GetCvRect: TCvRect;
    procedure SetCvRect(const Value: TCvRect);
  public
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Bottom: Integer read FBottom write FBottom;
    property Right: Integer read FRight write FRight;
    property Width: Integer read GetWidth write SetWidth;
    property height: Integer read GetHeight write SetHeight;
    property ocvRect: TocvRect read GetOcvRect write SetOcvRect;
    property cvRect: TCvRect read GetCvRect write SetCvRect;
  end;

  TocvCropOperation = class(TocvCustomImageOperation)
  private
    FCropRect: TocvRect32i;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property CropRect: TocvRect32i read FCropRect write FCropRect;
  end;

  TocvInRangeSOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FLower: TocvScalar;
    FUpper: TocvScalar;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Lower: TocvScalar read FLower write FLower;
    property Upper: TocvScalar read FUpper write FUpper;
  end;

  // TocvLogicType =
  // (
  // ioAdd,
  // ioAddS,
  // ioSub,
  // ioSubS,
  // ioSubRS,
  // ioMul,
  // ioDiv,
  // ioScaleAdd,
  // ioAXPY
  // );
  //
  // TocvMathLogicOperation = class(TocvCustomImageOperation)
  // public
  // constructor Create(AOwner: TPersistent); override;
  // destructor Destroy; override;
  // function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  // published
  // end;

  TocvInterpolationWarpingFlag = (WARP_FILL_OUTLIERS, WARP_INVERSE_MAP);
  TocvInterpolationWarpingFlagSet = set of TocvInterpolationWarpingFlag;

  TocvRotateOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FCenter: TocvPoint2D32f;
    FMethod: TocvInterpolationMethod;
    FWarpingFlag: TocvInterpolationWarpingFlagSet;
    FFillColor: TColor;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Angle: Integer index 0 Read GetIntParam write SetIntParam;
    property RotateAroundCenter: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property CustomCenter: TocvPoint2D32f Read FCenter write FCenter;
    property Method: TocvInterpolationMethod read FMethod write FMethod default INTER_LINEAR;
    property WarpingFlag: TocvInterpolationWarpingFlagSet read FWarpingFlag write FWarpingFlag
      default [WARP_FILL_OUTLIERS];
    property Scale: Double index 0 Read GetFloatParam write SetFloatParam;
    property FillColor: TColor read FFillColor write FFillColor default clBlack;
  end;

  TocvQuad = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public type
    TOcvQuadPoints = array [0 .. 3] of TocvPoint2D32f;
    TCvPoint2D32fArray = array [0 .. 3] of TCvPoint2D32f;
  protected
    FPoints: TOcvQuadPoints;
    function ShaIsConvexQuadrangle: Boolean;
  private
    function GetPoints(const index: Integer): TocvPoint2D32f;
    procedure SetPoints(const index: Integer; const Value: TocvPoint2D32f);
    function GetCvQuad: TCvPoint2D32fArray;
  public
    constructor Create;
    destructor Destroy; override;
    property Points[const Index: Integer]: TocvPoint2D32f read GetPoints write SetPoints;
    property Quad: TOcvQuadPoints read FPoints;
    property cvQuad: TCvPoint2D32fArray read GetCvQuad;
  published
    property TopLeft: TocvPoint2D32f index 0 read GetPoints write SetPoints;
    property TopRight: TocvPoint2D32f index 1 read GetPoints write SetPoints;
    property BottomLeft: TocvPoint2D32f index 2 read GetPoints write SetPoints;
    property BottomRight: TocvPoint2D32f index 3 read GetPoints write SetPoints;
  end;

  TocvWarpPerspective = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSourceQuad: TocvQuad;
    FDestQuad: TocvQuad;
    FMethod: TocvInterpolationMethod;
    FWarpingFlag: TocvInterpolationWarpingFlagSet;
    FFillColor: TColor;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property SourceQuad: TocvQuad read FSourceQuad write FSourceQuad;
    property DestQuad: TocvQuad read FDestQuad write FDestQuad;
    property Method: TocvInterpolationMethod read FMethod write FMethod default INTER_LINEAR;
    property WarpingFlag: TocvInterpolationWarpingFlagSet read FWarpingFlag write FWarpingFlag
      default [WARP_FILL_OUTLIERS];
    property FullSourceImage: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property FillColor: TColor Read FFillColor write FFillColor;
  end;

  TocvAbsDiff = class(TocvCustomImageOperation)
  protected
    FPrevFrame: IocvImage;
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TocvDraw = class(TPersistent)
  protected
    FColor: TColor;
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  private
    FOwner: TPersistent;
    FOffset: TocvPoint2D32i;
    FEnabled: Boolean;
    FThickness: Integer;
    FLineType: TocvLineType;
    FShift: Integer;
    function GetCvLineType: Integer;
    function GetCvColor: TCvScalar;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    property Color: TColor read FColor write FColor default clGreen;
    property Shift: Integer read FShift write FShift default 0;
    property cvLineType: Integer read GetCvLineType;
    property cvColor: TCvScalar read GetCvColor;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Thickness: Integer read FThickness write FThickness default 2;
    property LineType: TocvLineType read FLineType write FLineType default LT_AA;
    property Offset: TocvPoint2D32i read FOffset write FOffset;
  end;

  TocvDrawColor = class(TocvDraw)
  published
    property Color;
  end;

  TocvMatchTemplateMethod = (TM_SQDIFF, TM_SQDIFF_NORMED, TM_CCORR, TM_CCORR_NORMED, TM_CCOEFF, TM_CCOEFF_NORMED);

  TocvMatchTemplate = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FMethod: TocvMatchTemplateMethod;
    FIPLTemplate: pIplImage;
    FTemplate: TPicture;
    FOnMathTemplateRect: TOnOcvRect;
    FDrawRect: TocvDraw;
    procedure SetFIPLTemplate(const Value: pIplImage);
    function GetIPLTemplate: pIplImage;
    procedure TemplateOnChange(Sender: TObject);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
    property IPLTemplate: pIplImage read GetIPLTemplate write SetFIPLTemplate;
  published
    property Method: TocvMatchTemplateMethod read FMethod write FMethod default TM_CCOEFF_NORMED;
    property Template: TPicture Read FTemplate write FTemplate;
    property DrawRect: TocvDraw read FDrawRect write FDrawRect;
    property OnMathTemplateRect: TOnOcvRect read FOnMathTemplateRect write FOnMathTemplateRect;
  end;

  TocvMotionDetectCalcRectType = (mdBoundingRect, mdMinAreaRect);

  TocvContourApproximationMethods = (CHAIN_CODE, CHAIN_APPROX_NONE, CHAIN_APPROX_SIMPLE, CHAIN_APPROX_TC89_L1,
    CHAIN_APPROX_TC89_KCOS, LINK_RUNS);

  TocvDrawMotionRect = TocvDrawColor;

  TocvMotionDetect = class(TocvCustomImageOperationWithNestedOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetPropertiesClass: TocvImageOperationClass; override;
  private
    FCalcRectType: TocvMotionDetectCalcRectType;
    FPrevFrame: IocvImage;
    FSmoothOperation: TocvSmoothOperations;
    FDrawMotionRect: TocvDrawMotionRect;
    FOnMotion: TOnOcvRects;
    FContours: pCvSeq;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
    property MotionRects: pCvSeq Read FContours;
  published
    property RemoveSmallObject: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property MinObjectSize: Integer index 0 Read GetIntParam write SetIntParam;
    property CalcRectType: TocvMotionDetectCalcRectType read FCalcRectType write FCalcRectType default mdBoundingRect;
    property Smooth: TocvSmoothOperations read FSmoothOperation write FSmoothOperation default BLUR;
    property Threshold: TocvCustomImageOperation read GetProperties write SetProperties;
    property DrawMotionRect: TocvDrawMotionRect read FDrawMotionRect Write FDrawMotionRect;
    property OnMotion: TOnOcvRects read FOnMotion write FOnMotion;
    property NotifyOnlyWhenFound: Boolean index 1 Read GetBoolParam write SetBoolParam;
  end;

  TocvHoughTransform = (HOUGH_STANDARD, HOUGH_PROBABILISTIC, HOUGH_MULTI_SCALE, HOUGH_GRADIENT);

  TocvDrawHoughCircles = TocvDrawColor;

  TocvHoughCirclesSmooth = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FSmoothOperation: TocvSmoothOperations;
    Fsigma1: Double;
    Fsigma2: Double;
    Fsize1: Integer;
    Fsize2: Integer;
    FEnabled: Boolean;
  public
    constructor Create;
  published
    property sigma1: Double Read Fsigma1 write Fsigma1;
    property sigma2: Double Read Fsigma2 write Fsigma2;
    property size1: Integer Read Fsize1 write Fsize1 default 0;
    property size2: Integer Read Fsize2 write Fsize2 default 0;
    property SmoothType: TocvSmoothOperations read FSmoothOperation write FSmoothOperation default GAUSSIAN;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TocvHoughCircles = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FHoughTransform: TocvHoughTransform;
    FDrawCircle: TocvDrawHoughCircles;
    FOnCircles: TOnOcvCircles;
    FSmooth: TocvHoughCirclesSmooth;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Method: TocvHoughTransform read FHoughTransform write FHoughTransform default HOUGH_GRADIENT;
    property InverseRatio: Double index 0 Read GetFloatParam write SetFloatParam;
    property MinDist: Double index 1 Read GetFloatParam write SetFloatParam;
    property Param1: Double index 2 Read GetFloatParam write SetFloatParam;
    property Param2: Double index 3 Read GetFloatParam write SetFloatParam;
    property MinRadius: Integer index 0 Read GetIntParam write SetIntParam;
    property MaxRadius: Integer index 1 Read GetIntParam write SetIntParam;
    property DrawCircle: TocvDrawHoughCircles read FDrawCircle write FDrawCircle;
    property OnCircles: TOnOcvCircles read FOnCircles write FOnCircles;
    property NotifyOnlyWhenFound: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property Smooth: TocvHoughCirclesSmooth read FSmooth write FSmooth;
  end;

  TocvHoughLinesCanny = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FThreshold1: Double;
    FThreshold2: Double;
    FApertureSize: Integer;
  public
    constructor Create;
  published
    property Threshold1: Double read FThreshold1 write FThreshold1;
    property Threshold2: Double read FThreshold2 write FThreshold2;
    property ApertureSize: Integer read FApertureSize write FApertureSize default 3;
  end;

  TocvDrawHoughLines = TocvDrawColor;

  TocvHoughLines = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FHoughTransform: TocvHoughTransform;
    FCanny: TocvHoughLinesCanny;
    FOnLines: TOnOcvLines;
    FDrawLines: TocvDrawHoughLines;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Method: TocvHoughTransform read FHoughTransform write FHoughTransform default HOUGH_PROBABILISTIC;
    property Rho: Double index 0 Read GetFloatParam write SetFloatParam;
    property Theta: Double index 1 Read GetFloatParam write SetFloatParam;
    property Param1: Double index 2 Read GetFloatParam write SetFloatParam;
    property Param2: Double index 3 Read GetFloatParam write SetFloatParam;
    property Threshold: Integer index 0 Read GetIntParam write SetIntParam;
    property Canny: TocvHoughLinesCanny Read FCanny write FCanny;
    property OnLines: TOnOcvLines read FOnLines write FOnLines;
    property DrawLines: TocvDrawHoughLines read FDrawLines write FDrawLines;
    property NotifyOnlyWhenFound: Boolean index 0 Read GetBoolParam write SetBoolParam;
  end;

  TocvHaarCascadeDraw = class(TocvDraw)
  published
    property Color;
    property Shift;
  end;

  TocvHaarCascade = class(TocvCustomImageOperation)
  private
    FHaarCascade: TocvHaarCascadeType;
    FLockFrontalFaceChange: TCriticalSection;
    FCascade: pCvHaarClassifierCascade;
    FMinSize: TocvPoint2D32i;
    FMaxSize: TocvPoint2D32i;
    FDrawHaarCascade: TocvHaarCascadeDraw;
    FCascadeFlags: TocvHaarCascadeFlagSet;
    FOnHaarCascade: TOnOcvHaarCascade;
    FCustomHaarCascade: TFileName;
    FHaarRects: TocvRects;
    procedure SetHaarCascade(const Value: TocvHaarCascadeType);
    procedure ReleaseCascade;
    procedure SetCustomHaarCascade(const Value: TFileName);
    procedure DoLoadHaarCascade(const FileName: String);
  protected
    FPrevFrame: IocvImage;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
    property HaarRects: TocvRects read FHaarRects;
  published
    property CustomHaarCascade: TFileName read FCustomHaarCascade write SetCustomHaarCascade;
    property HaarCascade: TocvHaarCascadeType read FHaarCascade write SetHaarCascade default hcFrontalFaceAlt;
    property Equalize: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property Scale: Double index 0 Read GetFloatParam write SetFloatParam; // 1.3
    property MinNeighbors: Integer index 0 Read GetIntParam write SetIntParam; // 3
    property MinSize: TocvPoint2D32i read FMinSize write FMinSize; // CV_DEFAULT(cvSize(0,0))
    property MaxSize: TocvPoint2D32i read FMaxSize write FMaxSize; // {CV_DEFAULT(cvSize(0,0))}
    property DrawHaarCascade: TocvHaarCascadeDraw read FDrawHaarCascade write FDrawHaarCascade;
    property CascadeFlags: TocvHaarCascadeFlagSet read FCascadeFlags write FCascadeFlags default [];
    property OnHaarCascade: TOnOcvHaarCascade read FOnHaarCascade write FOnHaarCascade;
    property NotifyOnlyWhenFound: Boolean index 1 Read GetBoolParam write SetBoolParam;
  end;

  TocvEditorOperation = (eopNone, eopSature, eopExpo, eopHue, eopTemperature, eopWhite, eopShadow, eopContrast,
    eopClarity);

  TocvEditor = class(TocvCustomImageOperation)
  private
    FStep: Integer;
    FEditorOperation: TocvEditorOperation;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Step: Integer read FStep Write FStep;
    property EditorOperation: TocvEditorOperation read FEditorOperation write FEditorOperation;
  end;

  TocvContourDraw = class(TocvDraw)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FHoleColor: TColor;
    FMaxLevel: Integer;
  public
    constructor Create(AOwner: TPersistent);
  published
    property ExternalColor: TColor read FColor write FColor default clGreen;
    property HoleColor: TColor read FHoleColor write FHoleColor default clRed;
    property MaxLevel: Integer read FMaxLevel write FMaxLevel default 2;
  end;

  TocvContourRetrievalModes = (RETR_EXTERNAL, RETR_LIST, RETR_CCOMP, RETR_TREE, RETR_FLOODFILL);

  TocvContourApprox = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  private
    FOwner: TPersistent;
    FEnabled: Boolean;
    FRecursive: Boolean;
    FEps: Double;
  public
    constructor Create(AOwner: TPersistent);
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Eps: Double read FEps write FEps;
    property Recursive: Boolean read FRecursive write FRecursive default True;
  end;

  TocvContoursOperation = class(TocvCustomImageOperationWithNestedOperation)
  private
    FRetrievalMode: TocvContourRetrievalModes;
    FApproximationMethod: TocvContourApproximationMethods;
    FOffset: TocvPoint2D32i;
    FContourDraw: TocvContourDraw;
    FApprox: TocvContourApprox;
    FOnContour: TOnOcvContour;
    FContours: pCvSeq;
    procedure DoNotifyContours(const Image: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
    property Contours: pCvSeq read FContours;
  published
    property Preprocessing: TocvCustomImageOperation read GetProperties write SetProperties;
    property RetrievalMode: TocvContourRetrievalModes read FRetrievalMode write FRetrievalMode default RETR_LIST;
    property ApproximationMethod: TocvContourApproximationMethods read FApproximationMethod write FApproximationMethod
      default CHAIN_APPROX_SIMPLE;
    property Offset: TocvPoint2D32i read FOffset write FOffset;
    property MinArea: Integer index 0 Read GetIntParam write SetIntParam;
    property ContourDraw: TocvContourDraw read FContourDraw write FContourDraw;
    property ApproxPoly: TocvContourApprox read FApprox write FApprox;
    property OnContour: TOnOcvContour read FOnContour write FOnContour;
  end;

  TocvImageOperationCollectionItem = class(TCollectionItem, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    FOwner: TCollection;
    FOnAfterTransform: TOnOcvAfterTransform;
    FOnBeforeTransform: TOnOcvBeforeTransform;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
  protected
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
    function GetDisplayName: string; override;
    function GetOwner: TPersistent; override;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetNamePath: string; override;
    procedure Assign(Source: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; virtual;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
    property OnAfterTransform: TOnOcvAfterTransform read FOnAfterTransform write FOnAfterTransform;
    property OnBeforeTransform: TOnOcvBeforeTransform read FOnBeforeTransform write FOnBeforeTransform;
  end;

  TocvImageOperationCollection = class(TOwnedCollection)
  protected
    FOnBeforeEachOperation: TOnOcvNotifyCollectionItem;
    FOnAfterEachOperation: TOnOcvNotifyCollectionItem;
  public
    function Transform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
  end;

  TocvImageOperation = class(TocvDataSourceAndReceiver, IocvEditorPropertiesContainer)
  private
    CS: TCriticalSection;
    FOperation: TocvCustomImageOperation;
    FOperationClass: TocvImageOperationClass;
    FOperations: TocvImageOperationCollection;
    FUseCollection: Boolean;
    FOnAfterEachOperation: TOnOcvNotifyCollectionItem;
    FOnBeforeEachOperation: TOnOcvNotifyCollectionItem;
    FEnabled: Boolean;
    function LockTransform: Boolean;
    procedure UnlockTransform;
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TocvCustomImageOperation);
    procedure SetPropertiesClass(Value: TocvImageOperationClass);
    procedure SetPropertiesClassName(const Value: string);
    procedure SetUseCollection(const Value: Boolean);
    procedure SetOnAfterEachOperation(const Value: TOnOcvNotifyCollectionItem);
    procedure SetOnBeforeEachOperation(const Value: TOnOcvNotifyCollectionItem);
  protected
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TakeImage(const IplImage: IocvImage); override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
    property Operations: TocvImageOperationCollection Read FOperations write FOperations;
    property OperationsEnabled: Boolean read FUseCollection write SetUseCollection default True;
    property OnBeforeEachOperation: TOnOcvNotifyCollectionItem read FOnBeforeEachOperation
      write SetOnBeforeEachOperation;
    property OnAfterEachOperation: TOnOcvNotifyCollectionItem read FOnAfterEachOperation write SetOnAfterEachOperation;
    property Enabled: Boolean read FEnabled Write FEnabled default True;
  end;

  TRegisteredImageOperations = class(TStringList)
  public
    function FindByClassName(const ClassName: String): TocvImageOperationClass;
    function FindByName(const Name: String): TocvImageOperationClass;
    function GetNameByClass(const IOClass: TClass): String;
    procedure RegisterIOClass(const IOClass: TClass; const ClassName: String);
  end;

function GetRegisteredImageOperations: TRegisteredImageOperations;

implementation

uses
  ocv.core_c,
  ocv.imgproc_c,
  ocv.utils,
{$IFDEF HAS_UNITSCOPE}
  System.Math;
{$ELSE}
Math;
{$ENDIF}
/// /
/// / -------------------------
/// /
// {$IFNDEF haarcascadeinc}
// {$DEFINE haarcascadeinc}
// ///
// // Run utils\CompressHaar\uCompressHaar.dpr
// // Add to serarch path \Delphi-OpenCV\resource\facedetectxml\
// ///
// {$R haarcascade.rc haarcascade.res}
// {$R haarcascade.res}
// {$I haarcascade.inc}
// {$ENDIF}

type
  TPersistentAccessProtected = class(TPersistent);

Var
  _RegisteredImageOperations: TRegisteredImageOperations = nil;

function GetRegisteredImageOperations: TRegisteredImageOperations;
begin
  if not Assigned(_RegisteredImageOperations) then
    _RegisteredImageOperations := TRegisteredImageOperations.Create;
  Result := _RegisteredImageOperations;
end;

{ TocvImageOperation }

procedure TocvImageOperation.SetOnAfterEachOperation(const Value: TOnOcvNotifyCollectionItem);
begin
  FOnAfterEachOperation := Value;
  Operations.FOnAfterEachOperation := Value;
end;

procedure TocvImageOperation.SetOnBeforeEachOperation(const Value: TOnOcvNotifyCollectionItem);
begin
  FOnBeforeEachOperation := Value;
  Operations.FOnBeforeEachOperation := Value;
end;

procedure TocvImageOperation.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvImageOperation.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvImageOperation.CreateProperties;
begin
  if FOperationClass <> nil then
    FOperation := FOperationClass.Create(Self);
end;

procedure TocvImageOperation.DestroyProperties;
begin
  FreeAndNil(FOperation);
end;

procedure TocvImageOperation.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvImageOperation.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

procedure TocvImageOperation.SetUseCollection(const Value: Boolean);
begin
  if FUseCollection <> Value then
  begin
    CS.Enter;
    try
      FUseCollection := Value;
    finally
      CS.Leave;
    end;
  end;
end;

constructor TocvImageOperation.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  FOperations := TocvImageOperationCollection.Create(Self, TocvImageOperationCollectionItem);
  FUseCollection := True;
  FEnabled := True;
end;

destructor TocvImageOperation.Destroy;
begin
  if LockTransform then
    if Assigned(FOperation) then
      FreeAndNil(FOperation);
  FOperations.Free;
  CS.Free;
  inherited;
end;

function TocvImageOperation.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvNoneOperation.Create(Self);
  Result := FOperation;
end;

function TocvImageOperation.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Operation.ClassType);
end;

function TocvImageOperation.GetPropertiesClassName: string;
begin
  Result := Operation.ClassName;
end;

function TocvImageOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

procedure TocvImageOperation.TakeImage(const IplImage: IocvImage);
var
  Destanation: IocvImage;
  ContinueTransform: Boolean;
begin
  if LockTransform then
    try
      if Enabled then
      begin
        if (OperationsEnabled and (Operations.Count > 0) and Operations.Transform(IplImage, Destanation)) then
          NotifyReceiver(Destanation)
        else
        begin
          ContinueTransform := True;
          if Assigned(OnBeforeEachOperation) then
            OnBeforeEachOperation(nil, Operation, nil, IplImage, ContinueTransform);
          if not ContinueTransform then
            NotifyReceiver(IplImage)
          else if Operation.Transform(IplImage.Clone, Destanation) then
          begin
            if Assigned(OnAfterEachOperation) then
              OnAfterEachOperation(nil, Operation, nil, Destanation, ContinueTransform);
            NotifyReceiver(Destanation);
          end
          else
            NotifyReceiver(IplImage);
        end;
      end
      else
        NotifyReceiver(IplImage);
    finally
      Destanation := nil;
      UnlockTransform;
    end;
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{ TocvImageOperationCanny }

constructor TocvCannyOperation.Create { (AOwner: TPersistent) };
begin
  inherited;
  Threshold1 := 10;
  Threshold2 := 100;
  ApertureSize := 3;
end;

function TocvCannyOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvCanny(Source.GrayImage.IpImage, Destanation.IpImage, Threshold1, Threshold2, ApertureSize);
  Result := True;
end;

{ TocvImageOperationGrayScale }

function TocvGrayScaleOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := Source.GrayImage;
  Result := True;
end;

{ TocvImageOperationNone }

function TocvNoneOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := Source;
  Result := True;
end;

{ TCustomOpenCVImgOperation }

procedure TocvCustomImageOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvCustomImageOperation then
  begin
    FFloatParams := (Dest as TocvCustomImageOperation).FFloatParams;
    FIntParams := (Dest as TocvCustomImageOperation).FIntParams;
    FBoolParams := (Dest as TocvCustomImageOperation).FBoolParams;
  end;
end;

constructor TocvCustomImageOperation.Create(AOwner: TPersistent);
begin
  if AOwner is TComponent then
    inherited Create(AOwner as TComponent)
  else
    inherited Create(nil);
  SetSubComponent(True);

  FOwner := AOwner;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TocvCustomImageOperation.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TocvCustomImageOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Result := False;
end;

function TocvCustomImageOperation.GetBoolParam(const index: Integer): Boolean;
begin
  if (index >= 0) and (index < Length(FBoolParams)) then
    Result := FBoolParams[index]
  else
    Result := False;
end;

function TocvCustomImageOperation.GetFloatParam(const index: Integer): Double;
begin
  if (index >= 0) and (index < Length(FFloatParams)) then
    Result := FFloatParams[index]
  else
    Result := 0;
end;

function TocvCustomImageOperation.GetIntParam(const index: Integer): Integer;
begin
  if (index >= 0) and (index < Length(FIntParams)) then
    Result := FIntParams[index]
  else
    Result := 0;
end;

function TocvCustomImageOperation.GetNamePath: string;
var
  S: string;
  lOwner: TPersistent;
begin
  Result := inherited GetNamePath;
  lOwner := GetOwner;
  if
  { } (lOwner <> nil) and
  { } (
    { } (csSubComponent in TComponent(lOwner).ComponentStyle) or
    { } (TPersistentAccessProtected(lOwner).GetOwner <> nil)
    { } ) then
  begin
    S := lOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TocvCustomImageOperation.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TocvCustomImageOperation.LockTransform: Boolean;
begin
  Result := FCriticalSection.TryEnter;
end;

procedure TocvCustomImageOperation.SetBoolParam(const index: Integer; const Value: Boolean);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FBoolParams) then
        SetLength(FBoolParams, index + 1);
      FBoolParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.SetFloatParam(const index: Integer; const Value: Double);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FFloatParams) then
        SetLength(FFloatParams, index + 1);
      FFloatParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.SetIntParam(const index: Integer; const Value: Integer);
begin
  if (index >= 0) and LockTransform then
    try
      if index > High(FIntParams) then
        SetLength(FIntParams, index + 1);
      FIntParams[index] := Value;
    finally
      UnlockTransform;
    end;
end;

function TocvCustomImageOperation.Transform(const Source: IocvImage; var Destanation: IocvImage): Boolean;
Var
  ContinueTransform: Boolean;
begin
  Result := LockTransform;
  if Result then
    try
      ContinueTransform := True;
      if Assigned(OnBeforeTransform) then
        OnBeforeTransform(Self, Source, ContinueTransform);
      if ContinueTransform then
        Result := DoTransform(Source.Clone, Destanation)
      else
      begin
        Destanation := Source;
        Result := True;
      end;
      if Result and Assigned(OnAfterTransform) then
        OnAfterTransform(Self, Destanation);
    finally
      UnlockTransform;
    end
  else
    Destanation := Source;
end;

procedure TocvCustomImageOperation.UnlockTransform;
begin
  FCriticalSection.Leave;
end;

{ TocvImageOperationSmooth }
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

procedure TocvSmoothOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvSmoothOperation then
    FSmoothOperation := (Dest as TocvSmoothOperation).FSmoothOperation;
end;

constructor TocvSmoothOperation.Create { (AOwner: TPersistent) };
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  size1 := 3;
  size2 := 3;
  sigma1 := 0;
  sigma2 := 0;
end;

procedure TocvSmoothOperation.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  if LockTransform then
    try
      FSmoothOperation := Value;
    finally
      UnlockTransform;
    end;
end;

function TocvSmoothOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Image: pIplImage;
begin
  Image := cvCloneImage(Source.IpImage);
  cvSmooth(Source.IpImage, Image, ocvSmoothOperations[SmoothType], size1, size2, sigma1, sigma2);
  Destanation := TocvImage.Create(Image);
  Result := True;
end;

{ TRegisteredImageOperations }

function TRegisteredImageOperations.FindByClassName(const ClassName: String): TocvImageOperationClass;
Var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if TocvImageOperationClass(Objects[i]).ClassName = ClassName then
      Exit(TocvImageOperationClass(Objects[i]));
end;

function TRegisteredImageOperations.FindByName(const Name: String): TocvImageOperationClass;
Var
  i: Integer;
begin
  i := IndexOf(Name);
  if i <> -1 then
    Result := TocvImageOperationClass(Objects[i])
  else
    Result := Nil;
end;

function TRegisteredImageOperations.GetNameByClass(const IOClass: TClass): String;
Var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if Integer(Objects[i]) = Integer(IOClass) then
    begin
      Result := Self[i];
      Break;
    end;
end;

procedure TRegisteredImageOperations.RegisterIOClass(const IOClass: TClass; const ClassName: String);
begin
  AddObject(ClassName, TObject(IOClass));
  RegisterClass(TPersistentClass(IOClass));
end;

{ TocvCustomErodeDilate }

procedure TocvCustomErodeDilateOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvCustomErodeDilateOperation then
    FMorphOp := (Dest as TocvCustomErodeDilateOperation).MorphOp;
end;

constructor TocvCustomErodeDilateOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  Radius := 5;
  Iterations := 5;
  FMorphOp := SHAPE_RECT;
end;

procedure TocvCustomErodeDilateOperation.SetMorphOp(const Value: TocvErodeDilateMode);
begin
  if LockTransform then
    try
      FMorphOp := Value;
    finally
      UnlockTransform;
    end;
end;

const
  EDMorpgOp: array [TocvErodeDilateMode] of Integer = (CV_SHAPE_RECT, CV_SHAPE_CROSS, CV_SHAPE_ELLIPSE,
    CV_SHAPE_CUSTOM);

  { TocvErode }

function TocvErodeOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvErode(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{ TocvDilate }

function TocvDilateOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvDilate(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{ TocvLaplace }

constructor TocvLaplaceOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  Aperture := 3;
end;

function TocvLaplaceOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  TempImg: pIplImage;
begin
  TempImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.Depth,
    Source.IpImage^.nChannels));
  cvLaplace(Source.IpImage, TempImg, Aperture);
  cvConvertScale(TempImg, Destanation.IpImage);
  cvReleaseImage(TempImg);
  Result := True;
end;

{ TocvSobel }

constructor TocvSobelOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  XOrder := 1;
  YOrder := 1;
  Aperture := 3;
end;

function TocvSobelOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  TmpImg: pIplImage;
begin
  TmpImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.Depth,
    Source.IpImage^.nChannels));
  cvSobel(Source.IpImage, TmpImg, XOrder, YOrder, Aperture);
  cvConvertScale(TmpImg, Destanation.IpImage);
  cvReleaseImage(TmpImg);
  Result := True;
end;

{ TocvImageOperationCollectionItem }

procedure TocvImageOperationCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TocvImageOperationCollectionItem then
    Operation.Assign(TocvImageOperationCollectionItem(Source).Operation)
  else
    inherited;
end;

constructor TocvImageOperationCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := Collection;
  CS := TCriticalSection.Create;
end;

procedure TocvImageOperationCollectionItem.CreateProperties;
begin
  if FOperationClass <> nil then
  begin
    FOperation := FOperationClass.Create(Self);
    FOperation.SetParentComponent((GetOwner as TOwnedCollection).Owner as TComponent);
  end;
end;

destructor TocvImageOperationCollectionItem.Destroy;
begin
  if Assigned(FOperation) then
    FOperation.Free;
  CS.Free;
  inherited;
end;

procedure TocvImageOperationCollectionItem.DestroyProperties;
begin
  FreeAndNil(FOperation);
end;

function TocvImageOperationCollectionItem.GetDisplayName: string;
begin
  Result := GetRegisteredImageOperations.GetNameByClass(FOperation.ClassType);
end;

function TocvImageOperationCollectionItem.GetNamePath: string;
begin
  Result := inherited GetNamePath + Format('Operations%d', [Index]);
end;

function TocvImageOperationCollectionItem.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TocvImageOperationCollectionItem.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := TocvNoneOperation.Create(Self);
  Result := FOperation;
end;

function TocvImageOperationCollectionItem.GetPropertiesClass: TocvImageOperationClass;
begin
  Result := TocvImageOperationClass(Operation.ClassType);
end;

function TocvImageOperationCollectionItem.GetPropertiesClassName: string;
begin
  Result := Operation.ClassName;
end;

function TocvImageOperationCollectionItem.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

function TocvImageOperationCollectionItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TocvImageOperationCollectionItem.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvImageOperationCollectionItem.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvImageOperationCollectionItem.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvImageOperationCollectionItem.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

function TocvImageOperationCollectionItem.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  ContinueTransform: Boolean;
begin
  Result := LockTransform;
  if Result then
    try
      ContinueTransform := True;
      if Assigned(OnBeforeTransform) then
        OnBeforeTransform(Self, Source, ContinueTransform);
      Result := ContinueTransform and Operation.DoTransform(Source, Destanation);
      if Result and Assigned(OnAfterTransform) then
        OnAfterTransform(Self, Destanation);
    finally
      UnlockTransform;
    end;
end;

procedure TocvImageOperationCollectionItem.UnlockTransform;
begin
  CS.Leave;
end;

function TocvImageOperationCollectionItem._AddRef: Integer;
begin
  Result := -1;
end;

function TocvImageOperationCollectionItem._Release: Integer;
begin
  Result := -1;
end;

{ TocvThresholdOperation }

Const
  cThreshold: array [TocvThresholdType] of Integer = (CV_THRESH_BINARY, CV_THRESH_BINARY_INV, CV_THRESH_TRUNC,
    CV_THRESH_TOZERO, CV_THRESH_TOZERO_INV, CV_THRESH_MASK, CV_THRESH_OTSU);

constructor TocvThresholdOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  Threshold := 50;
end;

constructor TocvCustomThresholdOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  MaxValue := 250;
  ThresholdType := THRESH_BINARY;
end;

function TocvCustomThresholdOperation.GetThresholdType: TocvThresholdType;
begin
  Result := TocvThresholdType(GetIntParam(0));
end;

procedure TocvCustomThresholdOperation.SetThresholdType(const Value: TocvThresholdType);
begin
  SetIntParam(0, Integer(Value));
end;

function TocvThresholdOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := Source.GrayImage.Same;
  cvThreshold(Source.GrayImage.IpImage, Destanation.IpImage, Threshold, MaxValue, cThreshold[ThresholdType]);
  Result := True;
end;

{ TocvAdaptiveThresholdOperation }

const
  cAdaptiveThresholdType: array [TocvAdaptiveThresholdType] of Integer = (CV_ADAPTIVE_THRESH_MEAN_C,
    CV_ADAPTIVE_THRESH_GAUSSIAN_C);

constructor TocvAdaptiveThresholdOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  AdaptiveThresholdType := ADAPTIVE_THRESH_MEAN_C;
  BlockSize := 3;
  Param := 5;
end;

function TocvAdaptiveThresholdOperation.GetAdaptiveThresholdType: TocvAdaptiveThresholdType;
begin
  Result := TocvAdaptiveThresholdType(GetIntParam(1));
end;

procedure TocvAdaptiveThresholdOperation.SetAdaptiveThresholdType(const Value: TocvAdaptiveThresholdType);
begin
  SetIntParam(1, Integer(Value));
end;

function TocvAdaptiveThresholdOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvAdaptiveThreshold(Source.GrayImage.IpImage, Destanation.IpImage, MaxValue,
    cAdaptiveThresholdType[AdaptiveThresholdType], cThreshold[ThresholdType], BlockSize, Param);
  Result := True;
end;

{ TocvContoursOperation }

constructor TocvContoursOperation.Create { (AOwner: TComponent) };
begin
  inherited;
  FOffset := TocvPoint2D32i.Create;
  FContourDraw := TocvContourDraw.Create(Self);
  FApprox := TocvContourApprox.Create(Self);
  OperationClass := TocvThresholdOperation;
  With Preprocessing as TocvThresholdOperation do
  begin
    Threshold := 128;
    MaxValue := 255;
    ThresholdType := THRESH_BINARY_INV;
  end;
  RetrievalMode := RETR_LIST;
  ApproximationMethod := CHAIN_APPROX_SIMPLE;
  MinArea := 100;
end;

destructor TocvContoursOperation.Destroy;
begin
  FOffset.Free;
  FContourDraw.Free;
  FApprox.Free;
  inherited;
end;

function TocvContoursOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  th_image: IocvImage;
  storage: pCvMemStorage;
  contoursCont: Integer;
  er, eg, eb: byte;
  hr, hg, hb: byte;
  s_contours: pCvSeq;
  area: Double;
begin
  Result := False;
  FContours := nil;
  th_image := nil;
  storage := cvCreateMemStorage(0);
  try
    Destanation := Source; // .Clone;
    if Preprocessing.Transform(Source, th_image) then
    begin
      contoursCont := cvFindContours(th_image.IpImage, storage, @Contours, SizeOf(TCvContour), Integer(RetrievalMode),
        Integer(ApproximationMethod), cvPoint(Offset.X, Offset.Y));
      if ApproxPoly.Enabled then
        FContours := cvApproxPoly(Contours, SizeOf(TCvContour), storage, CV_POLY_APPROX_DP, ApproxPoly.Eps,
          Integer(ApproxPoly.Recursive));
      DoNotifyContours(Destanation, contoursCont, Contours);
      if (contoursCont > 0) and ContourDraw.Enabled then
      begin
        GetRGBValue(ContourDraw.ExternalColor, er, eg, eb);
        GetRGBValue(ContourDraw.HoleColor, hr, hg, hb);
        if MinArea > 0 then
        begin
          s_contours := Contours;
          while (s_contours <> nil) do
          begin
            area := cvContourArea(s_contours, CV_WHOLE_SEQ);
            if abs(area) > MinArea then
              cvDrawContours(Destanation.IpImage, s_contours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb),
                ContourDraw.MaxLevel, ContourDraw.Thickness, cLineType[ContourDraw.LineType],
                cvPoint(ContourDraw.Offset.X, ContourDraw.Offset.Y));
            s_contours := s_contours.h_next;
          end;
        end
        else
          cvDrawContours(Destanation.IpImage, FContours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb), ContourDraw.MaxLevel,
            ContourDraw.Thickness, cLineType[ContourDraw.LineType],
            cvPoint(ContourDraw.Offset.X, ContourDraw.Offset.Y));
      end;
      Result := True;
    end;
  finally
    cvReleaseMemStorage(storage)
  end;
end;

procedure TocvContoursOperation.DoNotifyContours(const Image: IocvImage; const ContourCount: Integer;
  const Contours: pCvSeq);
begin
  if Assigned(OnContour) then
    OnContour(Self, Image, ContourCount, Contours);
end;

{ TocvRotateOperation }

procedure TocvRotateOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvRotateOperation then
  begin
    FCenter := (Dest as TocvRotateOperation).FCenter;
    FMethod := (Dest as TocvRotateOperation).FMethod;
    FWarpingFlag := (Dest as TocvRotateOperation).FWarpingFlag;
    FFillColor := (Dest as TocvRotateOperation).FFillColor
  end;
end;

constructor TocvRotateOperation.Create(AOwner: TPersistent);
begin
  inherited;
  Angle := 90;
  FCenter := TocvPoint2D32f.Create;
  RotateAroundCenter := True;
  Method := INTER_LINEAR;
  WarpingFlag := [WARP_FILL_OUTLIERS];
  Scale := 1;
  FFillColor := clBlack;
end;

destructor TocvRotateOperation.Destroy;
begin
  FCenter.Free;
  inherited;
end;

function TocvRotateOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  rot_mat: pCvMat;
  cvCenter: TCvPoint2D32f;
  D: pIplImage;
  M: Integer;
begin
  // Матрица трансформации
  rot_mat := cvCreateMat(2, 3, CV_32FC1);
  // Вращение относительно центра изображения
  if RotateAroundCenter then
  begin
    cvCenter.X := Source.IpImage^.Width div 2;
    cvCenter.Y := Source.IpImage^.height div 2;
  end
  else
  begin
    cvCenter.X := CustomCenter.X;
    cvCenter.Y := CustomCenter.Y;
  end;
  cv2DRotationMatrix(cvCenter, Angle, Scale, rot_mat);
  // Создаем изображение
  D := cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.Depth, Source.IpImage^.nChannels);
  // Выполняем вращение
  M := Integer(Method);
  if WARP_FILL_OUTLIERS in FWarpingFlag then
    M := M or CV_WARP_FILL_OUTLIERS;
  if WARP_INVERSE_MAP in FWarpingFlag then
    M := M or CV_WARP_INVERSE_MAP;
  cvWarpAffine(Source.IpImage, D, rot_mat, M, ColorToCvRGB(FillColor));
  cvReleaseMat(rot_mat);
  Destanation := TocvImage.Create(D);
  Result := True;
end;

{ TPersistentPoint }

procedure TocvPoint2D32i.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvPoint2D32f then
    FSize := (Dest as TocvPoint2D32i).FSize;
end;

{ TocvCountourDraw }

procedure TocvDraw.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvDraw then
  begin
    FOffset.FSize := (Dest as TocvDraw).FOffset.FSize;
    FEnabled := (Dest as TocvDraw).FEnabled;
    FThickness := (Dest as TocvDraw).FThickness;
    FLineType := (Dest as TocvDraw).FLineType;
    FColor := (Dest as TocvDraw).FColor;
  end;
end;

constructor TocvDraw.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FOffset := TocvPoint2D32i.Create;
  FEnabled := True;
  FThickness := 1;
  FLineType := LT_AA;
  FColor := clGreen;
  FShift := 0;
end;

destructor TocvDraw.Destroy;
begin
  FOffset.Free;
  inherited;
end;

function TocvDraw.GetCvColor: TCvScalar;
begin
  Result := ColorToCvRGB(Color);
end;

function TocvDraw.GetCvLineType: Integer;
begin
  Result := cLineType[LineType];
end;

function TocvDraw.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TocvContourApprox }

procedure TocvContourApprox.AssignTo(Dest: TPersistent);
begin
  if Dest is TocvContourApprox then
  begin
    FEnabled := (Dest as TocvContourApprox).Enabled;
    FEps := (Dest as TocvContourApprox).Eps;
    FRecursive := (Dest as TocvContourApprox).Recursive;
  end
  else
    inherited;
end;

constructor TocvContourApprox.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := True;
  FEps := 3;
  FRecursive := True;
end;

function TocvContourApprox.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TocvAbsDiff }

function TocvAbsDiff.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  GrayImage: IocvImage;
begin
  GrayImage := Source.GrayImage;
  Destanation := GrayImage.Same;
  if Assigned(FPrevFrame) and (FPrevFrame.Width = GrayImage.Width) and (FPrevFrame.height = GrayImage.height) then
    cvAbsDiff(FPrevFrame.IpImage, GrayImage.IpImage, Destanation.IpImage);
  FPrevFrame := GrayImage;
  Result := True;
end;

{ TocvFaceDetect }

constructor TocvHaarCascade.Create(AOwner: TPersistent);
begin
  inherited;
  FLockFrontalFaceChange := TCriticalSection.Create;
  FMinSize := TocvPoint2D32i.Create(30, 30);
  FMaxSize := TocvPoint2D32i.Create;
  HaarCascade := hcFrontalFaceAlt;
  FDrawHaarCascade := TocvHaarCascadeDraw.Create(Self);
  Scale := 1.3;
  MinNeighbors := 3;
  Equalize := True;
  NotifyOnlyWhenFound := False;
end;

destructor TocvHaarCascade.Destroy;
begin
  FLockFrontalFaceChange.Free;
  FMinSize.Free;
  FMaxSize.Free;
  FDrawHaarCascade.Free;
  ReleaseCascade;
  inherited;
end;

function TocvHaarCascade.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  // storage: pCvMemStorage;
  // gray: IocvImage;
  // detected_objects: pCvSeq;
  i: Integer;
  // cvr: pCvRect;
begin
  Destanation := Source;
  Result := ocvHaarCascadeTransform(
    { } Source,
    { } FCascade,
    { } FHaarRects,
    { } MinSize.Size,
    { } MaxSize.Size,
    { } Equalize,
    { } Scale,
    { } MinNeighbors,
    { } CascadeFlags);

  if Result then
  begin
    if Assigned(OnHaarCascade) and ((not NotifyOnlyWhenFound) or (Length(FHaarRects) > 0)) then
      OnHaarCascade(Self, Destanation, FHaarRects);
    if DrawHaarCascade.Enabled then
      for i := 0 to High(FHaarRects) do
        Destanation.Canvas.Rectangle(FHaarRects[i].Left, FHaarRects[i].Top, FHaarRects[i].Right, FHaarRects[i].Bottom,
          DrawHaarCascade.Color, DrawHaarCascade.Thickness, DrawHaarCascade.LineType, DrawHaarCascade.Shift);
  end;
  // if Assigned(FCascade) then
  // begin
  // storage := cvCreateMemStorage(0);
  // try
  // gray := Source.GrayImage;
  // if Equalize then
  // cvEqualizeHist(gray.IpImage, gray.IpImage);
  // detected_objects := cvHaarDetectObjects(gray.IpImage, FCascade, storage, Scale, MinNeighbors, GetHaarCascadeFlag,
  // cvSize(MinSize.X, MinSize.Y), cvSize(MaxSize.X, MaxSize.Y));
  //
  // if Assigned(detected_objects) then
  // begin
  // SetLength(FHaarRects, detected_objects^.total);
  // i := 0;
  // While i < detected_objects^.total do
  // begin
  // cvr := pCvRect(cvGetSeqElem(detected_objects, i));
  // FHaarRects[i] := ocvRect(cvr^.X, cvr^.Y, (cvr^.X) + (cvr^.Width), (cvr^.Y) + (cvr^.height));
  // Inc(i);
  // end;
  //
  // if Assigned(OnHaarCascade) and ((not NotifyOnlyWhenFound) or (detected_objects^.total > 0)) then
  // OnHaarCascade(Self, Destanation, FHaarRects);
  //
  // if DrawHaarCascade.Enabled then
  // begin
  // GetRGBValue(DrawHaarCascade.Color, r, g, b);
  // i := 0;
  // While i < detected_objects^.total do
  // begin
  // cvr := pCvRect(cvGetSeqElem(detected_objects, i));
  // cvRectangle(Destanation.IpImage, cvPoint(cvr^.X, cvr^.Y), cvPoint((cvr^.X) + (cvr^.Width), (cvr^.Y) + (cvr^.height)),
  // CV_RGB(r, g, b), DrawHaarCascade.Thickness, cLineType[DrawHaarCascade.LineType], DrawHaarCascade.Shift);
  // Inc(i);
  // end;
  // end;
  // end;
  // Result := True;
  // finally
  // cvReleaseMemStorage(storage);
  // end;
  // end
  // else
  // Result := False;
end;

procedure TocvHaarCascade.ReleaseCascade;
begin
  if Assigned(FCascade) then
    cvReleaseHaarClassifierCascade(FCascade);
  FCascade := nil;
end;

procedure TocvHaarCascade.SetCustomHaarCascade(const Value: TFileName);
begin
  if FCustomHaarCascade <> Value then
  begin
    FCustomHaarCascade := Value;
    DoLoadHaarCascade(FCustomHaarCascade);
  end;
end;

procedure TocvHaarCascade.DoLoadHaarCascade(const FileName: String);
begin
  ReleaseCascade;
  if FileExists(FileName) then
    FCascade := cvLoad(c_str(FileName), nil, nil, nil);
end;

procedure TocvHaarCascade.SetHaarCascade(const Value: TocvHaarCascadeType);

  function TempPath: string;
  var
    BufSize: Cardinal;
  begin
{$IFDEF MSWINDOWS}
    BufSize := GetTempPath(0, nil);
    SetLength(Result, BufSize);
    GetTempPath(BufSize, PChar(Result));
    Result := Trim(Result);
{$ENDIF MSWINDOWS}
  end;

// Var
// FullFileName: String;
// RS: TResourceStream;
// DC: TZDecompressionStream;
// FS: TFileStream;
begin
  if FHaarCascade <> Value then
  begin
    FLockFrontalFaceChange.Enter;
    try
      FHaarCascade := Value;
      ReleaseCascade;
      if not(csDesigning in ComponentState) then
      begin
        ReleaseCascade;
        FCascade := ocvLoadHaarCascade(FHaarCascade);
      end;
    finally
      FLockFrontalFaceChange.Leave;
    end;
  end;
end;

constructor TocvPoint2D32i.Create(const AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

{ TocvContourDraw }

procedure TocvContourDraw.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvContourDraw then
  begin
    FHoleColor := (Dest as TocvContourDraw).FHoleColor;
    FMaxLevel := (Dest as TocvContourDraw).FMaxLevel;
  end;
end;

constructor TocvContourDraw.Create(AOwner: TPersistent);
begin
  inherited;
  FHoleColor := clRed;
  FMaxLevel := 2;
end;

{ TocvMatchTemplate }

procedure TocvMatchTemplate.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvMatchTemplate then
  begin
    FMethod := (Dest as TocvMatchTemplate).FMethod;
  end;
end;

constructor TocvMatchTemplate.Create(AOwner: TPersistent);
begin
  inherited;
  FTemplate := TPicture.Create;
  FTemplate.OnChange := TemplateOnChange;
  FDrawRect := TocvDraw.Create(Self);
  FMethod := TM_CCOEFF_NORMED;
end;

destructor TocvMatchTemplate.Destroy;
begin
  if Assigned(FIPLTemplate) then
    cvReleaseImage(FIPLTemplate);
  FTemplate.Free;
  FDrawRect.Free;
  inherited;
end;

procedure TocvMatchTemplate.TemplateOnChange(Sender: TObject);
begin
  IPLTemplate := nil;
end;

function TocvMatchTemplate.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  imgMat: pIplImage;
  P1, P2: TCvPoint;
  min: Double;
  r, g, b: byte;
begin
  Destanation := Source;
  if Assigned(IPLTemplate) then
  begin
    imgMat := cvCreateImage(cvSize(Source.IpImage^.Width - IPLTemplate^.Width + 1,
      Source.IpImage^.height - IPLTemplate^.height + 1), IPL_DEPTH_32F, 1);
    cvMatchTemplate(Source.IpImage, IPLTemplate, imgMat, Integer(FMethod));

    if Assigned(OnMathTemplateRect) or DrawRect.Enabled then
    begin
      cvMinMaxLoc(imgMat, @min, @min, nil, @P1, nil);
      P2.X := P1.X + IPLTemplate^.Width - 1;
      P2.Y := P1.Y + IPLTemplate^.height - 1;

      if Assigned(OnMathTemplateRect) then
        OnMathTemplateRect(Self, Source, ocvRect(P1.X, P1.Y, P2.X, P2.Y));

      if DrawRect.Enabled then
      begin
        GetRGBValue(DrawRect.Color, r, g, b);
        cvRectangle(Destanation.IpImage, P1, P2, CV_RGB(r, g, b));
      end;

    end;

    cvReleaseImage(imgMat);

    Result := True;
  end
  else
    Result := False;
end;

function TocvMatchTemplate.GetIPLTemplate: pIplImage;
begin
  if not Assigned(FIPLTemplate) then
  begin
    if not Template.Bitmap.Empty then
      FIPLTemplate := BitmapToIplImage(Template.Bitmap);
  end;
  Result := FIPLTemplate;
end;

procedure TocvMatchTemplate.SetFIPLTemplate(const Value: pIplImage);
begin
  if FIPLTemplate <> Value then
  begin
    if Assigned(FIPLTemplate) then
      cvReleaseImage(FIPLTemplate);
    FIPLTemplate := Value;
  end;
end;

{ TocvMotionDetect }

procedure TocvMotionDetect.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvMotionDetect then
  begin
    FCalcRectType := (Dest as TocvMotionDetect).FCalcRectType;
  end;
end;

constructor TocvMotionDetect.Create(AOwner: TPersistent);
begin
  inherited;
  RemoveSmallObject := True;
  MinObjectSize := 100;
  FSmoothOperation := BLUR;
  FDrawMotionRect := TocvDrawMotionRect.Create(Self);
  OperationClass := TocvThresholdOperation;
  With (Operation as TocvThresholdOperation) do
  begin
    Threshold := 25;
    MaxValue := 255;
  end;
  NotifyOnlyWhenFound := False;
end;

destructor TocvMotionDetect.Destroy;
begin
  FDrawMotionRect.Free;
  inherited;
end;

function TocvMotionDetect.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  CurrentGrayImage: IocvImage;
  DifferenceImage: IocvImage;
  storage: pCvMemStorage;
  area: Double;
  ThresholdImage: IocvImage;
  black, white: TCvScalar;
  c: pCvSeq;
  Rects: TocvRects;
  Rect: TCvRect;
  Rect2d: TCvBox2D;
  i: Integer;
  r, g, b: byte;
begin
  Destanation := Source;
  CurrentGrayImage := Source.GrayImage;

  if (not Assigned(FPrevFrame)) or (FPrevFrame.Width <> CurrentGrayImage.Width) or (FPrevFrame.height = CurrentGrayImage.height) then
    FPrevFrame := CurrentGrayImage;

  DifferenceImage := CurrentGrayImage.Same;
  cvAbsDiff(FPrevFrame.IpImage, CurrentGrayImage.IpImage, DifferenceImage.IpImage);
  cvSmooth(DifferenceImage.IpImage, DifferenceImage.IpImage, Integer(Smooth));

  if Threshold.DoTransform(DifferenceImage, ThresholdImage) then
  begin
    // img_out := DifferenceImage.Clone;
    storage := cvCreateMemStorage(0);
    // FContours := AllocMem(SizeOf(TCvSeq));
    try
      FContours := nil;
      cvFindContours(ThresholdImage.IpImage, storage, @FContours, SizeOf(TCvContour), CV_RETR_LIST,
        CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));

      black := CV_RGB(0, 0, 0);
      white := CV_RGB(255, 255, 255);

      while (FContours <> nil) do
      begin
        area := cvContourArea(FContours, CV_WHOLE_SEQ);
        if (abs(area) <= MinObjectSize) and RemoveSmallObject then // Если площадь меньше порога, то удаляем
          cvDrawContours(ThresholdImage.IpImage, FContours, black, black, -1, CV_FILLED, 8, cvPoint(0, 0))
        else
          cvDrawContours(ThresholdImage.IpImage, FContours, white, white, -1, CV_FILLED, 8, cvPoint(0, 0));

        FContours := FContours.h_next; // Переходим к следующему контуру
      end;

      cvClearMemStorage(storage);
      SetLength(Rects, 0);

      FContours := nil;
      cvFindContours(ThresholdImage.IpImage, storage, @FContours, SizeOf(TCvContour), CV_RETR_LIST,
        CV_CHAIN_APPROX_NONE, cvPoint(0, 0));

      if Assigned(FContours) then
      begin
        c := FContours;
        i := 0;
        while (c <> nil) do
        begin
          SetLength(Rects, i + 1);
          if CalcRectType = mdBoundingRect then
          begin
            Rect := cvBoundingRect(c, 0);
            Rects[i] := ocvRect(Rect.X, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.height);
          end
          else if CalcRectType = mdMinAreaRect then
          begin
            Rect2d := cvMinAreaRect2(c);
            Rects[i] := ocvRect(Round(Rect2d.Center.X - Rect2d.Size.Width / 2),
              Round(Rect2d.Center.Y - Rect2d.Size.height / 2), Round(Rect2d.Center.X + Rect2d.Size.Width / 2),
              Round(Rect2d.Center.Y + Rect2d.Size.height / 2));
          end;

          if DrawMotionRect.Enabled then
          begin
            GetRGBValue(DrawMotionRect.Color, r, g, b);
            cvRectangle(Destanation.IpImage, cvPoint(Rects[i].Left, Rects[i].Top),
              cvPoint(Rects[i].Right, Rects[i].Bottom), CV_RGB(r, g, b), DrawMotionRect.Thickness,
              cLineType[DrawMotionRect.LineType], DrawMotionRect.Shift);
          end;
          Inc(i);
          c := c.h_next;
        end;
      end;

      if Assigned(OnMotion) and ((not NotifyOnlyWhenFound) or (Length(Rects) > 0)) then
        OnMotion(Self, Destanation, Rects);

    finally
      cvReleaseMemStorage(storage);
    end;
  end;

  FPrevFrame := CurrentGrayImage;

  Result := True;
end;

function TocvMotionDetect.GetPropertiesClass: TocvImageOperationClass;
begin
  if not Assigned(FOperation) then
    Result := TocvThresholdOperation
  else
    Result := inherited;
end;

{ TocvCustomImageOperationWithNestedOperation }

constructor TocvCustomImageOperationWithNestedOperation.Create(AOwner: TPersistent);
begin
  inherited;
  CS := TCriticalSection.Create;
end;

procedure TocvCustomImageOperationWithNestedOperation.CreateProperties;
begin
  FOperation := FOperationClass.Create(Self);
end;

function TocvCustomImageOperationWithNestedOperation.GetProperties: TocvCustomImageOperation;
begin
  if not Assigned(FOperation) then
    FOperation := OperationClass.Create(Self);
  Result := FOperation;
end;

function TocvCustomImageOperationWithNestedOperation.GetPropertiesClass: TocvImageOperationClass;
begin
  if Assigned(FOperation) then
    Result := TocvImageOperationClass(FOperation.ClassType)
  else
    Result := TocvNoneOperation;
end;

destructor TocvCustomImageOperationWithNestedOperation.Destroy;
begin
  CS.Free;
  DestroyProperties;
  inherited;
end;

procedure TocvCustomImageOperationWithNestedOperation.DestroyProperties;
begin
  if Assigned(FOperation) then
    FreeAndNil(FOperation);
end;

function TocvCustomImageOperationWithNestedOperation.GetPropertiesClassName: string;
begin
  Result := Operation.ClassName;
end;

procedure TocvCustomImageOperationWithNestedOperation.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TocvCustomImageOperationWithNestedOperation.SetProperties(const Value: TocvCustomImageOperation);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TocvCustomImageOperationWithNestedOperation.SetPropertiesClass(Value: TocvImageOperationClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TocvCustomImageOperationWithNestedOperation.SetPropertiesClassName(const Value: string);
begin
  OperationClass := TocvImageOperationClass(GetRegisteredImageOperations.FindByClassName(Value));
end;

function TocvCustomImageOperationWithNestedOperation.LockTransform: Boolean;
begin
  Result := CS.TryEnter;
end;

procedure TocvCustomImageOperationWithNestedOperation.UnlockTransform;
begin
  CS.Leave;
end;

{ TocvRectPersistent }

procedure TocvRect32i.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvRect32i then
  begin
    FRight := (Dest as TocvRect32i).FRight;
    FBottom := (Dest as TocvRect32i).FBottom;
    FTop := (Dest as TocvRect32i).FTop;
    FLeft := (Dest as TocvRect32i).FLeft;
  end;
end;

function TocvRect32i.GetCvRect: TCvRect;
begin
  Result := ocv.core.types_c.cvRect(Left, Top, Width, height);
end;

function TocvRect32i.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TocvRect32i.GetOcvRect: TocvRect;
begin
  Result := ocv.comp.Types.ocvRect(Left, Top, Right, Bottom);
end;

function TocvRect32i.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TocvRect32i.SetCvRect(const Value: TCvRect);
begin
  Left := Value.X;
  Top := Value.Y;
  Width := Value.Width;
  height := Value.height;
end;

procedure TocvRect32i.SetHeight(const Value: Integer);
begin
  Bottom := Top + Value;
end;

procedure TocvRect32i.SetOcvRect(const Value: TocvRect);
begin
  FLeft := Value.Left;
  FTop := Value.Top;
  FRight := Value.Right;
  FBottom := Value.Bottom;
end;

procedure TocvRect32i.SetWidth(const Value: Integer);
begin
  FRight := FLeft + Value;
end;

{ TocvCropOperation }

constructor TocvCropOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FCropRect := TocvRect32i.Create;
end;

destructor TocvCropOperation.Destroy;
begin
  FCropRect.Free;
  inherited;
end;

function TocvCropOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
{$IFDEF DELPHIXE2_UP}
  if FCropRect.ocvRect.IsEmpty then
{$ELSE}
  if IsRectEmpty(FCropRect.ocvRect) then
{$ENDIF}
    Destanation := Source
  else
    Destanation := Source.Crop(FCropRect.cvRect);
  Result := True;
end;

{ TocvImageOperationCollection }

function TocvImageOperationCollection.Transform(const Source: IocvImage; out Destanation: IocvImage): Boolean;

  function iif(const index: Integer): TObject;
  begin
    if (index < 0) or (index >= Count) then
      Result := nil
    else
      Result := (Items[index] as TocvImageOperationCollectionItem).Operation;
  end;

Var
  i: Integer;
  ContinueTransform: Boolean;
begin
  Destanation := Source;
  ContinueTransform := True;
  Result := True;
  for i := 0 to Count - 1 do
  begin
    if Assigned(FOnBeforeEachOperation) then
      FOnBeforeEachOperation(iif(i - 1), iif(i), iif(i + 1), Destanation, ContinueTransform);
    if not ContinueTransform then
      Break;
    Result := (Items[i] as TocvImageOperationCollectionItem).DoTransform(Destanation.Clone, Destanation);
    if not Result then
      Break;
    if Assigned(FOnAfterEachOperation) then
    begin
      FOnAfterEachOperation(iif(i - 1), iif(i), iif(i + 1), Destanation, ContinueTransform);
      if not ContinueTransform then
        Break;
    end;
  end;
end;

{ TocvAddWeightedOperation }

constructor TocvCommonMathOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FTransformInterpolation := INTER_CUBIC;
end;

procedure TocvCommonMathOperation.DoGetMaskImage(var Image: IocvImage);
begin
  Image := nil;
  if Assigned(FOnGetMaskImage) then
    FOnGetMaskImage(Self, Image);
end;

procedure TocvCommonMathOperation.DoGetSourceImage(var Image: IocvImage);
begin
  FCriticalSection.Enter;
  try
    Image := FSrource2Image;
  finally
    UnlockTransform;
  end;
  if Assigned(FOnGetImage) then
    FOnGetImage(Self, Image);
end;

procedure TocvCommonMathOperation.GetImagesForTransorm(out Source1: IocvImage; out Source2, Mask: IocvImage);
Var
  s1, s2, M: IocvImage;
begin
  if Assigned(VideoSource) then
  begin
    Source1 := VideoSource.Image;
    DoGetSourceImage(s2);
    DoGetMaskImage(M);
    if Assigned(s2) and ((s1.Width <> s2.Width) or (s1.height <> s2.height)) then
    begin
      Source2 := s1.Same;
      cvResize(s2.IpImage, Source2.IpImage, Integer(TransformInterpolation));
    end
    else
      Source2 := s2;

    if Assigned(M) and ((s1.Width <> M.Width) or (s1.height <> M.height)) then
    begin
      Mask := s1.Same;
      cvResize(M.IpImage, Mask.IpImage, Integer(TransformInterpolation));
    end
    else
      Mask := M;
  end
  else
    Source2 := Source1;
end;

procedure TocvCommonMathOperation.SetVideoSource_Source2(const Value: IocvDataSource);
begin
  if FocvVideoSource <> Value then
  begin
    if Assigned(FocvVideoSource) then
      FocvVideoSource.RemoveReceiver(Self);
    FocvVideoSource := Value;
    if Assigned(FocvVideoSource) then
      FocvVideoSource.AddReceiver(Self);
  end;
end;

procedure TocvCommonMathOperation.SetVideoSource(const Value: TObject);
begin
  VideoSource := Value as TocvDataSource;
end;

procedure TocvCommonMathOperation.TakeImage(const IplImage: IocvImage);
begin
  if LockTransform then
    try
      FSrource2Image := IplImage;
    finally
      UnlockTransform;
    end;
end;

{ TocvWarpAffine }

procedure TocvWarpPerspective.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvWarpPerspective then
  begin
    FMethod := (Dest as TocvWarpPerspective).FMethod;
    FWarpingFlag := (Dest as TocvWarpPerspective).FWarpingFlag;
    FFillColor := (Dest as TocvWarpPerspective).FFillColor;
    FSourceQuad.AssignTo((Dest as TocvWarpPerspective).FSourceQuad);
    FDestQuad.AssignTo((Dest as TocvWarpPerspective).FDestQuad);
  end;
end;

constructor TocvWarpPerspective.Create(AOwner: TPersistent);
begin
  inherited;
  FSourceQuad := TocvQuad.Create;
  FDestQuad := TocvQuad.Create;
  Method := INTER_LINEAR;
  WarpingFlag := [WARP_FILL_OUTLIERS];
  FullSourceImage := True;
  FFillColor := clBlack;
end;

destructor TocvWarpPerspective.Destroy;
begin
  FSourceQuad.Free;
  FDestQuad.Free;
  inherited;
end;

function TocvWarpPerspective.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  dst: pIplImage;
  srcQuad, dstQuad: TocvQuad.TCvPoint2D32fArray;
  warp_matrix: pCvMat;
begin
  if DestQuad.ShaIsConvexQuadrangle then
  begin
    if FullSourceImage or (not SourceQuad.ShaIsConvexQuadrangle) then
    begin
      srcQuad[0].X := 0; // src Top left
      srcQuad[0].Y := 0;
      srcQuad[1].X := Source.Width - 1; // src Top right
      srcQuad[1].Y := 0;
      srcQuad[2].X := 0; // src Bottom left
      srcQuad[2].Y := Source.height - 1;
      srcQuad[3].X := Source.Width - 1; // src Bot right
      srcQuad[3].Y := Source.height - 1;
    end
    else
      srcQuad := SourceQuad.cvQuad;

    dstQuad := DestQuad.cvQuad;
    warp_matrix := cvCreateMat(3, 3, CV_32FC1);
    dst := cvCloneImage(Source.IpImage);
    cvGetPerspectiveTransform(@srcQuad, @dstQuad, warp_matrix);
    cvWarpPerspective(Source.IpImage, dst, warp_matrix, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS,
      ColorToCvRGB(FillColor));
    Destanation := TocvImage.Create(dst);
  end
  else
    Destanation := Source;
  Result := True;
end;

{ TocvAddWeightedOperation }

constructor TocvAddWeightedOperation.Create(AOwner: TPersistent);
begin
  inherited;
  Alpha := 0.5;
  Beta := 0.5;
  Gamma := 0;
end;

function TocvAddWeightedOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  s1, s2, M: IocvImage;
begin
  GetImagesForTransorm(s1, s2, M);
  try
    Destanation := s1.Same;
    // Источники должны иметь один размер или ROI
    cvAddWeighted(s1.IpImage, Alpha, s2.IpImage, Beta, Gamma, Destanation.IpImage);
    Result := True;
  except
    Destanation := Source;
    Result := False;
  end;
end;

{ TocvQuad }

procedure TocvQuad.AssignTo(Dest: TPersistent);
Var
  i: Integer;
begin
  inherited;
  if Dest is TocvQuad then
    for i := 0 to 3 do
      FPoints[i].AssignTo((Dest as TocvQuad).FPoints[i]);
end;

constructor TocvQuad.Create;
Var
  i: Integer;
begin
  inherited;
  for i := 0 to 3 do
    FPoints[i] := TocvPoint2D32f.Create;
end;

destructor TocvQuad.Destroy;
Var
  i: Integer;
begin
  for i := 0 to 3 do
    FPoints[i].Free;
  inherited;
end;

function TocvQuad.GetCvQuad: TCvPoint2D32fArray;
Var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    Result[i].X := FPoints[i].X;
    Result[i].Y := FPoints[i].Y;
  end;
end;

function TocvQuad.GetPoints(const index: Integer): TocvPoint2D32f;
begin
  if (index >= 0) and (index < 4) then
    Result := FPoints[index]
  else
    Result := nil;
end;

function TocvQuad.ShaIsConvexQuadrangle: Boolean;
begin
  Result := True;
  // ---------------------------------
  // Procedure GetVector(const i: byte; var P: TCvPoint2D32f);
  // begin
  // P.X := FPoints[(i + 1) and 3].X - FPoints[i and 3].X;
  // P.Y := FPoints[(i + 1) and 3].Y - FPoints[i and 3].Y;
  // end;
  //
  // Var
  // Q: Boolean;
  // v1, v2: TCvPoint2D32f;
  // T, Z, P: Double;
  // i: Integer;
  // begin
  // GetVector(3, v1);
  // GetVector(0, v2);
  // T := v1.X * v2.Y - v2.X * v1.Y;
  // Z := Sign(T);
  // P := 1.0;
  // i := 0;
  // Q := True;
  // while (Q and (i < 4)) do
  // begin
  // GetVector(i, v1);
  // GetVector(i + 1, v2);
  // T := v1.X * v2.Y - v2.X * v1.Y;
  // P := P * Z * Sign(T);
  // if (P < 0) then
  // Q := False;
  // Inc(i);
  // end;
  // Result := Q;
  // ---------------------------------
  // ---------------------------------
  // function ShaIsSameDirection(const t0, t1, t2: TocvPoint2D32f; var dir: Integer): Boolean;
  // const
  // MinInt = -1 xor MaxInt;
  // var
  // S: Integer;
  // begin;
  // S := Trunc((t1.X - t0.X) * (t2.Y - t0.Y) - (t2.X - t0.X) * (t1.Y - t0.Y));
  // if S = 0 then
  // Result := True
  // else
  // begin;
  // S := S or MaxInt;
  // Result := (S xor dir) <> MinInt;
  // dir := S;
  // end;
  // end;
  // var
  // dir: Integer;
  // begin;
  // dir := 0;
  // Result := ShaIsSameDirection(P0, P1, P2, dir) and ShaIsSameDirection(P1, P2, P3, dir) and ShaIsSameDirection(P2, P3, P0, dir)
  // and ShaIsSameDirection(P3, P0, P1, dir) and (dir <> 0);
  // ---------------------------------
end;

procedure TocvQuad.SetPoints(const index: Integer; const Value: TocvPoint2D32f);
begin
  if (index >= 0) and (index < 4) then
    FPoints[index] := Value;
end;

{ TocvPoint2D32f }

procedure TocvPoint2D32f.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvPoint2D32f then
  begin
    FX := (Dest as TocvPoint2D32f).FX;
    FY := (Dest as TocvPoint2D32f).FY;
  end;
end;

constructor TocvPoint2D32f.Create(const AX, AY: Single);
begin
  FX := AX;
  FY := AY;
end;

{ TocvHoughCircles }

procedure TocvHoughCircles.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvHoughCircles then
  begin
    FHoughTransform := (Dest as TocvHoughCircles).FHoughTransform;
  end;
end;

constructor TocvHoughCircles.Create(AOwner: TPersistent);
begin
  inherited;
  FHoughTransform := HOUGH_GRADIENT;
  InverseRatio := 1;
  MinDist := 100;
  Param1 := 100;
  Param2 := 100;
  MinRadius := 0;
  MaxRadius := 0;
  FDrawCircle := TocvDrawHoughCircles.Create(Self);
  NotifyOnlyWhenFound := False;
  FSmooth := TocvHoughCirclesSmooth.Create;
  FSmooth.SmoothType := GAUSSIAN;
  FSmooth.size1 := 5;
  FSmooth.size2 := 5;
  FSmooth.sigma1 := 0;
  FSmooth.sigma2 := 0;
end;

destructor TocvHoughCircles.Destroy;
begin
  FDrawCircle.Free;
  FSmooth.Free;
  inherited;
end;

function TocvHoughCircles.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
type
  TFloatArray = array [0 .. 10] of Single;
  pFloatArray = ^TFloatArray;

Var
  results: pCvSeq;
  storage: pCvMemStorage;
  Circles: TocvCircles;
  i: Integer;
  p: pFloatArray;
  D: IocvImage;
begin
  Finalize(Circles);
  Destanation := Source;
  try
    storage := cvCreateMemStorage(0);
    // results := nil;
    try

      if Smooth.Enabled then
      begin
        D := Source.Same;
        cvSmooth(Source.IpImage, D.IpImage, ocvSmoothOperations[Smooth.SmoothType], Smooth.size1, Smooth.size2,
          Smooth.sigma1, Smooth.sigma2);
      end
      else
        D := Source;

      results := cvHoughCircles(D.GrayImage.IpImage, storage, Integer(Method), InverseRatio, MinDist, Param1, Param2,
        MinRadius, MaxRadius);
      if Assigned(results) then
      begin
        SetLength(Circles, results^.total);
        for i := 0 to results^.total - 1 do
        begin
          p := pFloatArray(cvGetSeqElem(results, i));
          Circles[i].cX := cvRound(p^[0]);
          Circles[i].cY := cvRound(p^[1]);
          Circles[i].Radius := cvRound(p^[2]);
          if DrawCircle.Enabled then
            cvCircle(Destanation.IpImage, cvPoint(Circles[i].cX, Circles[i].cY), Circles[i].Radius, DrawCircle.cvColor,
              DrawCircle.Thickness, DrawCircle.cvLineType, DrawCircle.Shift);
        end;
      end;
      if Assigned(OnCircles) and ((Length(Circles) > 0) or (not NotifyOnlyWhenFound)) then
        OnCircles(Self, Destanation, Circles);
      Result := True;
    finally
      cvReleaseMemStorage(storage);
    end;
  except
    Result := False;
  end;
end;

{ TocvHoughLines }

procedure TocvHoughLines.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvHoughLines then
  begin
    FHoughTransform := (Dest as TocvHoughLines).FHoughTransform
  end;
end;

constructor TocvHoughLines.Create(AOwner: TPersistent);
begin
  inherited;
  FHoughTransform := HOUGH_PROBABILISTIC;
  Rho := 1;
  Theta := CV_PI / 180;
  Param1 := 50;
  Param2 := 10;
  Threshold := 50;
  FCanny := TocvHoughLinesCanny.Create;
  FDrawLines := TocvDrawHoughLines.Create(Self);
  NotifyOnlyWhenFound := True;
end;

destructor TocvHoughLines.Destroy;
begin
  FCanny.Free;
  FDrawLines.Free;
  inherited;
end;

function TocvHoughLines.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  lines: pCvSeq;
  line: pCvPointArray;
  storage: pCvMemStorage;
  SG, D: IocvImage;
  i: Integer;
  ocvlines: TocvLines;
begin
  // lines := nil;
  Finalize(ocvlines);
  Destanation := Source;
  try
    storage := cvCreateMemStorage(0);
    try
      SG := Source.GrayImage;
      D := SG.Same;
      cvCanny(SG.IpImage, D.IpImage, Canny.Threshold1, Canny.Threshold2, Canny.ApertureSize);
      lines := cvHoughLines2(D.IpImage, storage, Integer(Method), Rho, Theta, Threshold, Param1, Param2);
      if Assigned(lines) then
      begin
        SetLength(ocvlines, lines^.total);
        for i := 0 to lines^.total - 1 do
        begin
          line := pCvPointArray(cvGetSeqElem(lines, i));
          ocvlines[i].S := line^[0];
          ocvlines[i].E := line^[1];
          if DrawLines.Enabled then
            cvLine(Destanation.IpImage, line^[0], line^[1], DrawLines.cvColor, DrawLines.Thickness,
              DrawLines.cvLineType, DrawLines.Shift);
        end;
      end;

      if Assigned(OnLines) and ((Length(ocvlines) > 0) or (not NotifyOnlyWhenFound)) then
        OnLines(Self, Destanation, ocvlines);

      Result := True;
    finally
      cvReleaseMemStorage(storage);
    end;
  except
    Result := False;
  end;
end;

{ TocvHoughCirclesSmooth }

procedure TocvHoughCirclesSmooth.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvHoughCirclesSmooth then
  begin
    FSmoothOperation := (Dest as TocvHoughCirclesSmooth).FSmoothOperation;
    Fsigma1 := (Dest as TocvHoughCirclesSmooth).Fsigma1;
    Fsigma2 := (Dest as TocvHoughCirclesSmooth).Fsigma2;
    Fsize1 := (Dest as TocvHoughCirclesSmooth).Fsize1;
    Fsize2 := (Dest as TocvHoughCirclesSmooth).Fsize2;
    FEnabled := (Dest as TocvHoughCirclesSmooth).FEnabled;
  end;
end;

constructor TocvHoughCirclesSmooth.Create;
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  Fsigma1 := 0;
  Fsigma2 := 0;
  Fsize1 := 5;
  Fsize2 := 5;
  FEnabled := True;
end;

{ TocvHoughLinesCanny }

procedure TocvHoughLinesCanny.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvHoughLinesCanny then
  begin
    FThreshold1 := (Dest as TocvHoughLinesCanny).FThreshold1;
    FThreshold2 := (Dest as TocvHoughLinesCanny).FThreshold2;
    FApertureSize := (Dest as TocvHoughLinesCanny).FApertureSize;
  end;
end;

constructor TocvHoughLinesCanny.Create;
begin
  inherited;
  FThreshold1 := 50;
  FThreshold2 := 200;
  FApertureSize := 3;
end;

{ TocvInRangeOperation }

procedure TocvInRangeSOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvInRangeSOperation then
  begin
    FLower := (Dest as TocvInRangeSOperation).FLower;
    FUpper := (Dest as TocvInRangeSOperation).FUpper;
  end;
end;

constructor TocvInRangeSOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FLower := TocvScalar.Create;
  FUpper := TocvScalar.Create;
end;

destructor TocvInRangeSOperation.Destroy;
begin
  FLower.Free;
  FUpper.Free;
  inherited;
end;

function TocvInRangeSOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Result := FLower.CvScalar < FUpper.CvScalar;
  if Result then
  begin
    Destanation := Source.Clone.GrayImage;
    try
      cvInRangeS(Source.IpImage, FLower.CvScalar, FUpper.CvScalar, Destanation.IpImage);
    except
      Result := False;
    end;
  end;
end;

{ TocvScalar }

procedure TocvScalar.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvScalar then
    FCvScalar := (Dest as TocvScalar).FCvScalar;
end;

function TocvScalar.GetScalar(const index: Integer): Double;
begin
  if (index >= 0) and (index < 4) then
    Result := FCvScalar.val[index]
  else
    Result := 0;
end;

procedure TocvScalar.SetScalar(const index: Integer; const Value: Double);
begin
  if (index >= 0) and (index < 4) then
    FCvScalar.val[index] := Value;
end;

{ TocvCvtColorOperation }

constructor TocvCvtColorOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FColorConversion := RGB2GRAY;
  FDepth := DEPTH_8U;
  FChannels := 1;
  // FAutoCalcParams := True;
end;

function TocvCvtColorOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  iImage: pIplImage;
begin
  // if FAutoCalcParams then
  // CalculateImageParams(Source);
  iImage := cvCreateImage(cvGetSize(Source.IpImage), cIPLDepth[Depth], Channels);
  try
    cvCvtColor(Source.IpImage, iImage, cColorConversion[ColorConversion]);
    Destanation := TocvImage.Create(iImage);
    Result := True;
  except
    Destanation := Source;
    Result := False;
  end;
end;

// procedure TocvCvtColorOperation.CalculateImageParams(const Source: IocvImage);
// Var
// scn, Depth: Integer;
// code: Integer;
// dcn: Integer;
// begin
// scn := Source.IpImage^.nChannels;
// dcn := scn;
// Depth := Source.IpImage^.Depth;
// code := CVColorConversion[ColorConversion];
// case code of
// CV_BGR2BGRA, CV_RGB2BGRA, CV_BGRA2BGR, CV_RGBA2BGR, CV_RGB2BGR, CV_BGRA2RGBA:
// begin
// if (code = CV_BGR2BGRA) or (code = CV_RGB2BGRA) or (code = CV_BGRA2RGBA) then
// dcn := 4
// else
// dcn := 3;
// end;
// CV_BGR2BGR565, CV_BGR2BGR555, CV_RGB2BGR565, CV_RGB2BGR555, CV_BGRA2BGR565, CV_BGRA2BGR555, CV_RGBA2BGR565, CV_RGBA2BGR555:
// begin
// Assert((scn = 3) or (scn = 4) and (Depth = IPL_DEPTH_8U));
// end;
// CV_BGR5652BGR, CV_BGR5552BGR, CV_BGR5652RGB, CV_BGR5552RGB, CV_BGR5652BGRA, CV_BGR5552BGRA, CV_BGR5652RGBA, CV_BGR5552RGBA:
// begin
// if (dcn <= 0) then
// if (code = CV_BGR5652BGRA) or (code = CV_BGR5552BGRA) or (code = CV_BGR5652RGBA) or (code = CV_BGR5552RGBA) then
// dcn := 4
// else
// dcn := 3;
// Assert((scn = 2) and (Depth = IPL_DEPTH_8U));
// end;
// CV_BGR2GRAY, CV_BGRA2GRAY, CV_RGB2GRAY, CV_RGBA2GRAY:
// begin
// Assert(scn = 3) or (scn = 4);
// dcn := 1;
// end;
// CV_BGR5652GRAY, CV_BGR5552GRAY:
// begin
// Assert((scn = 2) and (Depth = IPL_DEPTH_8U));
// dcn := 1;
// end;
// end;
// end;

{ TocvResizeOperation }

procedure TocvResizeOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvResizeOperation then
    FInterpolation := (Dest as TocvResizeOperation).FInterpolation;
end;

constructor TocvResizeOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FInterpolation := INTER_LINEAR;
end;

function TocvResizeOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  D: pIplImage;
begin
  Destanation := Source;
  Result := False;
  if (DestWidth <> 0) and (DestHeight <> 0) then
    try
      D := cvCreateImage(cvSize(DestWidth, DestHeight), Source.IpImage^.Depth, Source.IpImage^.nChannels);
      cvResize(Source.IpImage, D, Integer(Interpolation));
      Destanation := TocvImage.Create(D);
      Result := True;
    except
    end;
end;

{ TocvMathLogicOperation }

procedure TocvLogicOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvLogicOperation then
    FOperation := (Dest as TocvLogicOperation).FOperation;
end;

constructor TocvLogicOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FOperation := ioAdd;
end;

function TocvLogicOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  s1, s2, M: IocvImage;
  _m: pIplImage;
begin
  GetImagesForTransorm(s1, s2, M);
  try
    Destanation := s1.Same;
    if Assigned(M) then
      _m := M.IpImage
    else
      _m := nil;
    case Operation of
      ioAdd:
        cvAdd(s1.IpImage, s2.IpImage, Destanation.IpImage, _m);
      ioSub:
        cvSub(s1.IpImage, s2.IpImage, Destanation.IpImage, _m);
      ioAnd:
        cvAnd(s1.IpImage, s2.IpImage, Destanation.IpImage, _m);
      ioOr:
        cvOr(s1.IpImage, s2.IpImage, Destanation.IpImage, _m);
      ioXor:
        cvXor(s1.IpImage, s2.IpImage, Destanation.IpImage, _m);
    end;
    Result := True;
  except
    Destanation := Source;
    Result := False;
  end;
end;

{ TocvLogicSOperation }

procedure TocvLogicSOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvLogicSOperation then
    FOperation := (Dest as TocvLogicSOperation).FOperation;
end;

constructor TocvLogicSOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FValue := TocvScalar.Create;
  FOperation := ioAddS;
end;

destructor TocvLogicSOperation.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TocvLogicSOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  s1, s2, M: IocvImage;
  _m: pIplImage;
begin
  GetImagesForTransorm(s1, s2, M);
  try
    Destanation := s1.Same;
    if Assigned(M) then
      _m := M.IpImage
    else
      _m := nil;
    case Operation of
      ioAddS:
        cvAddS(s1.IpImage, Value.CvScalar, Destanation.IpImage, _m);
      ioSubS:
        cvSubS(s1.IpImage, Value.CvScalar, Destanation.IpImage, _m);
      ioSubRS:
        cvSubRS(s1.IpImage, Value.CvScalar, Destanation.IpImage, _m);
      ioXorS:
        cvXorS(s1.IpImage, Value.CvScalar, Destanation.IpImage, _m);
    end;
    Result := True;
  except
    Destanation := Source;
    Result := False;
  end;
end;

{ TocvEditor }

constructor TocvEditor.Create(AOwner: TPersistent);
begin
  inherited;
  FStep := 10;
  FEditorOperation := eopNone;
end;

destructor TocvEditor.Destroy;
begin

  inherited;
end;

function TocvEditor.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  iImage: pIplImage;
begin
  try
    case EditorOperation of
      eopNone:
        begin
          Destanation := Source;
          Exit(True);
        end;
      eopSature:
        Sature(Step, Source.IpImage, iImage);
      eopExpo:
        Expo(Step, Source.IpImage, iImage);
      eopHue:
        Hue(Step, Source.IpImage, iImage);
      eopTemperature:
        Temperature(Step, Source.IpImage, iImage);
      eopWhite:
        white(Step, Source.IpImage, iImage);
      eopShadow:
        Shadow(Step, Source.IpImage, iImage);
      eopContrast:
        Contrast(Step, Source.IpImage, iImage);
      eopClarity:
        Clarity(Step, Source.IpImage, iImage);
    end;
    Destanation := TocvImage.Create(iImage);
    Result := True;
  except
    Destanation := Source;
    Result := False;
  end;
end;

initialization

GetRegisteredImageOperations.RegisterIOClass(TocvNoneOperation, 'None');
GetRegisteredImageOperations.RegisterIOClass(TocvGrayScaleOperation, 'GrayScale');
GetRegisteredImageOperations.RegisterIOClass(TocvCannyOperation, 'Canny');
GetRegisteredImageOperations.RegisterIOClass(TocvSmoothOperation, 'Smooth');
GetRegisteredImageOperations.RegisterIOClass(TocvErodeOperation, 'Erode');
GetRegisteredImageOperations.RegisterIOClass(TocvDilateOperation, 'Dilate');
GetRegisteredImageOperations.RegisterIOClass(TocvLaplaceOperation, 'Laplace');
GetRegisteredImageOperations.RegisterIOClass(TocvSobelOperation, 'Sobel');
GetRegisteredImageOperations.RegisterIOClass(TocvThresholdOperation, 'Threshold');
GetRegisteredImageOperations.RegisterIOClass(TocvAdaptiveThresholdOperation, 'AdaptiveThreshold');
GetRegisteredImageOperations.RegisterIOClass(TocvContoursOperation, 'Contours');
GetRegisteredImageOperations.RegisterIOClass(TocvRotateOperation, 'Rotate');
GetRegisteredImageOperations.RegisterIOClass(TocvAbsDiff, 'AbsDiff');
GetRegisteredImageOperations.RegisterIOClass(TocvHaarCascade, 'HaarCascade');
GetRegisteredImageOperations.RegisterIOClass(TocvMatchTemplate, 'MatchTemplate');
GetRegisteredImageOperations.RegisterIOClass(TocvMotionDetect, 'MotionDetect');
GetRegisteredImageOperations.RegisterIOClass(TocvCropOperation, 'Crop');
GetRegisteredImageOperations.RegisterIOClass(TocvAddWeightedOperation, 'AddWeighted');
GetRegisteredImageOperations.RegisterIOClass(TocvWarpPerspective, 'WarpPerspective');
GetRegisteredImageOperations.RegisterIOClass(TocvHoughCircles, 'HoughCircles');
GetRegisteredImageOperations.RegisterIOClass(TocvHoughLines, 'HoughLines');
GetRegisteredImageOperations.RegisterIOClass(TocvInRangeSOperation, 'InRangeS');
GetRegisteredImageOperations.RegisterIOClass(TocvCvtColorOperation, 'ColorOperation');
GetRegisteredImageOperations.RegisterIOClass(TocvResizeOperation, 'Resize');
GetRegisteredImageOperations.RegisterIOClass(TocvLogicOperation, 'Logic');
GetRegisteredImageOperations.RegisterIOClass(TocvLogicSOperation, 'LogicS');
GetRegisteredImageOperations.RegisterIOClass(TocvEditor, 'Editor');

finalization

if Assigned(_RegisteredImageOperations) then
  FreeAndNil(_RegisteredImageOperations);

end.

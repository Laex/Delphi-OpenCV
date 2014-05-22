// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
  // Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

{$IFNDEF CLR}
{$I Opencv.inc}
unit uOCVImageOperation;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Winapi.Windows,
  Vcl.Graphics,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Types,
  System.ZLib,
{$ELSE}
  Windows,
  Graphics,
  SysUtils,
  Classes,
  SyncObjs,
{$IFNDEF VER5}
  Types,
{$ENDIF VER5}
  ZLib,
{$ENDIF VER6P}
  uOCVTypes,
  ocv.objdetect_c,
  ocv.core.types_c;

type

  TocvCustomImageOperation = class(TComponent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FCriticalSection: TCriticalSection;
    FOwner: TPersistent;
    FFloatParams: TArray<Double>;
    FIntParams: TArray<Integer>;
    FBoolParams: TArray<Boolean>;
    FOnAfterPaint: TOnOcvAfterTransform;
    FOnBeforePaint: TOnOcvBeforeTransform;
  protected
    function GetFloatParam(const index: Integer): Double;
    function GetIntParam(const index: Integer): Integer;
    procedure SetFloatParam(const index: Integer; const Value: Double);
    procedure SetIntParam(const index: Integer; const Value: Integer);
    function GetBoolParam(const index: Integer): Boolean;
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
    property OnAfterPaint: TOnOcvAfterTransform read FOnAfterPaint write FOnAfterPaint;
    property OnBeforePaint: TOnOcvBeforeTransform read FOnBeforePaint write FOnBeforePaint;
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

  TocvGrayScaleOperation = class(TocvCustomImageOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TovcCannyOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Threshold1: Double index 0 Read GetFloatParam write SetFloatParam;
    property Threshold2: Double index 1 Read GetFloatParam write SetFloatParam;
    property ApertureSize: Integer index 0 Read GetIntParam write SetIntParam;
  end;

  TocvErodeDilateMode = (SHAPE_RECT, SHAPE_CROSS, SHAPE_ELLIPSE, SHAPE_CUSTOM);

  TovcCustomErodeDilateOperation = class(TocvCustomImageOperation)
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

  TovcErodeOperation = class(TovcCustomErodeDilateOperation)
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TovcDilateOperation = class(TovcCustomErodeDilateOperation)
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

  TovcSobelOperation = class(TocvCustomImageOperation)
  public
    constructor Create(AOwner: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property XOrder: Integer index 0 Read GetIntParam write SetIntParam;
    property YOrder: Integer index 1 Read GetIntParam write SetIntParam;
    property Aperture: Integer index 2 Read GetIntParam write SetIntParam;
  end;

  TocvSmoothOperations = (BLUR_NO_SCALE, BLUR, GAUSSIAN, MEDIAN, BILATERAL);

  TovcSmoothOperation = class(TocvCustomImageOperation)
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
    property SmoothOperation: TocvSmoothOperations read FSmoothOperation write SetSmoothOperation default GAUSSIAN;
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
    property ThresholdType: TocvThresholdType read GetThresholdType write SetThresholdType default THRESH_BINARY; // index 0
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
    property AdaptiveThresholdType: TocvAdaptiveThresholdType read GetAdaptiveThresholdType write SetAdaptiveThresholdType
      default ADAPTIVE_THRESH_MEAN_C; // index 1
    property BlockSize: Integer index 2 Read GetIntParam write SetIntParam; // 3
    property Param: Double index 1 Read GetFloatParam write SetFloatParam; // 5;
  end;

  TocvPoint = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FPoint: TPoint;
  public
    constructor Create(const AX: Integer = 0; const AY: Integer = 0);
  published
    property X: Integer read FPoint.X write FPoint.X;
    property Y: Integer read FPoint.Y write FPoint.Y;
  end;

  TocvRectPersistent = class(TPersistent)
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
    property Height: Integer read GetHeight write SetHeight;
    property ocvRect: TocvRect read GetOcvRect write SetOcvRect;
    property cvRect: TCvRect read GetCvRect write SetCvRect;
  end;

  TovcCropOperation = class(TocvCustomImageOperation)
  private
    FCropRect: TocvRectPersistent;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property CropRect: TocvRectPersistent read FCropRect write FCropRect;
  end;

  TocvInterpolationMethod = (INTER_NN, INTER_LINEAR, INTER_CUBIC, INTER_AREA, INTER_LANCZOS4);

  TocvInterpolationWarpingFlag = (WARP_FILL_OUTLIERS, WARP_INVERSE_MAP);
  TocvInterpolationWarpingFlagSet = set of TocvInterpolationWarpingFlag;

  TocvRotateOperation = class(TocvCustomImageOperation)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FCustomCenter: TocvPoint;
    FMethod: TocvInterpolationMethod;
    FWarpingFlag: TocvInterpolationWarpingFlagSet;
    FScale: Double;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  published
    property Angle: Integer index 0 Read GetIntParam write SetIntParam;
    property RotateAroundCenter: Boolean index 0 Read GetBoolParam write SetBoolParam;
    property CustomCenter: TocvPoint Read FCustomCenter write FCustomCenter;
    property Method: TocvInterpolationMethod read FMethod write FMethod default INTER_LINEAR;
    property WarpingFlag: TocvInterpolationWarpingFlagSet read FWarpingFlag write FWarpingFlag default [WARP_FILL_OUTLIERS];
    property Scale: Double read FScale write FScale;
  end;

  TocvAbsDiff = class(TocvCustomImageOperation)
  protected
    FPrevFrame: IocvImage;
  public
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  end;

  TocvLineType = (LT_FILLED, LT_8, LT_AA);

  TocvDraw = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  private
    FOwner: TPersistent;
    FOffset: TocvPoint;
    FEnabled: Boolean;
    FThickness: Integer;
    FLineType: TocvLineType;
    FColor: TColor;
    FShift: Integer;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    property Color: TColor read FColor write FColor default clGreen;
    property Shift: Integer read FShift write FShift default 0;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Thickness: Integer read FThickness write FThickness default 2;
    property LineType: TocvLineType read FLineType write FLineType default LT_AA;
    property Offset: TocvPoint read FOffset write FOffset;
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

  TocvDrawMotionRect = class(TocvDraw)
  published
    property Color;
  end;

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

  TocvHaarCascadeDraw = class(TocvDraw)
  published
    property Color;
    property Shift;
  end;

  TocvHaarCascadeType = (hcEye, hcEyeTreeEyeGlasses, hcFrontalFaceAlt, hcFrontalFaceAlt2, hcFrontalFaceAltTree,
    hcFrontalFaceDefaut, hcFullBody, hcLeftEye2Splits, hcLowerBody, hcMcsEyePairBig, hcMcsEyePairSmall, hcMcsLeftEar,
    hcMcsLeftEye, hcMcsMouth, hcMcsNose, hcMcsRightEar, hcMcsRightEye, hcMcsUpperBody, hcProfileFace, hcRightEye2Splits, hcSmile,
    hcUpperBody, hcPlateNumberRus);

  TocvHaarCascadeFlag = (HAAR_DO_CANNY_PRUNING, HAAR_SCALE_IMAGE, HAAR_FIND_BIGGEST_OBJECT, HAAR_DO_ROUGH_SEARCH);
  TocvHaarCascadeFlagSet = set of TocvHaarCascadeFlag;

  TocvHaarCascade = class(TocvCustomImageOperation)
  private
    FHaarCascade: TocvHaarCascadeType;
    FLockFrontalFaceChange: TCriticalSection;
    FCascade: pCvHaarClassifierCascade;
    FMinSize: TocvPoint;
    FMaxSize: TocvPoint;
    FDrawHaarCascade: TocvHaarCascadeDraw;
    FCascadeFlags: TocvHaarCascadeFlagSet;
    FOnHaarCascade: TOnOcvHaarCascade;
    FCustomHaarCascade: TFileName;
    FHaarRects: TocvRects;
    procedure SetHaarCascade(const Value: TocvHaarCascadeType);
    procedure ReleaseCascade;
    function GetHaarCascadeFlag: Integer;
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
    property Equalize: Boolean index 1 Read GetBoolParam write SetBoolParam;
    property Scale: Double index 0 Read GetFloatParam write SetFloatParam; // 1.3
    property MinNeighbors: Integer index 0 Read GetIntParam write SetIntParam; // 3
    property MinSize: TocvPoint read FMinSize write FMinSize; // CV_DEFAULT(cvSize(0,0))
    property MaxSize: TocvPoint read FMaxSize write FMaxSize; // {CV_DEFAULT(cvSize(0,0))}
    property DrawHaarCascade: TocvHaarCascadeDraw read FDrawHaarCascade write FDrawHaarCascade;
    property CascadeFlags: TocvHaarCascadeFlagSet read FCascadeFlags write FCascadeFlags default [];
    property OnHaarCascade: TOnOcvHaarCascade read FOnHaarCascade write FOnHaarCascade;
    property NotifyOnlyWhenFound: Boolean index 2 Read GetBoolParam write SetBoolParam;
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
    FOffset: TocvPoint;
    FContourDraw: TocvContourDraw;
    FApprox: TocvContourApprox;
    FOnContour: TOnOcvContour;
    FContours: pCvSeq;
    procedure DoNotifyContours(const Image: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
  protected
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property Contours: pCvSeq read FContours;
  published
    property Preprocessing: TocvCustomImageOperation read GetProperties write SetProperties;
    property RetrievalMode: TocvContourRetrievalModes read FRetrievalMode write FRetrievalMode default RETR_LIST;
    property ApproximationMethod: TocvContourApproximationMethods read FApproximationMethod write FApproximationMethod
      default CHAIN_APPROX_SIMPLE;
    property Offset: TocvPoint read FOffset write FOffset;
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
    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean; virtual;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
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
    procedure TakeImage(const IplImage: IocvImage); override;
    function GetProperties: TocvCustomImageOperation;
    function GetPropertiesClass: TocvImageOperationClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OperationClass: TocvImageOperationClass read GetPropertiesClass write SetPropertiesClass;
  published
    property OperationClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Operation: TocvCustomImageOperation read GetProperties write SetProperties;
    property Operations: TocvImageOperationCollection Read FOperations write FOperations;
    property OperationsEnabled: Boolean read FUseCollection write SetUseCollection default True;
    property OnBeforeEachOperation: TOnOcvNotifyCollectionItem read FOnBeforeEachOperation write SetOnBeforeEachOperation;
    property OnAfterEachOperation: TOnOcvNotifyCollectionItem read FOnAfterEachOperation write SetOnAfterEachOperation;
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

///
// Run utils\CompressHaar\uCompressHaar.dpr
// Add to serarch path \Delphi-OpenCV\bin\facedetectxml\
///
{$R haarcascade.rc haarcascade.res}
{$R haarcascade.res}

uses
  ocv.core_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.cvutils;

type
  TPersistentAccessProtected = class(TPersistent);

  TocvHaarCascadeRecord = record
    Name: String;
    FileName: String;
  end;

  //
  // Run utils\CompressHaar\uCompressHaar.dpr
  // Add to serarch path \Delphi-OpenCV\resource\facedetectxml\
  //
{$I haarcascade.inc}

Var
  _RegisteredImageOperations: TRegisteredImageOperations = nil;

function GetRegisteredImageOperations: TRegisteredImageOperations;
begin
  if not Assigned(_RegisteredImageOperations) then
    _RegisteredImageOperations := TRegisteredImageOperations.Create;
  Result := _RegisteredImageOperations;
end;

const
  cLineType: array [TocvLineType] of Integer = (CV_FILLED, 8, CV_AA);

procedure GetRGBValue(const AColor: TColor; var r, g, b: byte);
Var
  RGBColor: TColor;
begin
  RGBColor := ColorToRGB(AColor);
  r := GetRValue(RGBColor);
  g := GetGValue(RGBColor);
  b := GetBValue(RGBColor);
end;

{TocvImageOperation}

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
    finally
      Destanation := nil;
      UnlockTransform;
    end;
end;

procedure TocvImageOperation.UnlockTransform;
begin
  CS.Leave;
end;

{TovcImageOperationCanny}

constructor TovcCannyOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  Threshold1 := 10;
  Threshold2 := 100;
  ApertureSize := 3;
end;

function TovcCannyOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_8U, 1));
  cvCanny(Source.GrayImage.IpImage, Destanation.IpImage, Threshold1, Threshold2, ApertureSize);
  Result := True;
end;

{TocvImageOperationGrayScale}

function TocvGrayScaleOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := Source.GrayImage;
  Result := True;
end;

{TocvImageOperationNone}

function TocvNoneOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  Destanation := Source;
  Result := True;
end;

{TCustomOpenCVImgOperation}

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
  {} (lOwner <> nil) and
  {} (
    {} (csSubComponent in TComponent(lOwner).ComponentStyle) or
    {} (TPersistentAccessProtected(lOwner).GetOwner <> nil)
    {} ) then
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
      if Assigned(OnBeforePaint) then
        OnBeforePaint(Self, Source, ContinueTransform);
      if ContinueTransform then
        Result := DoTransform(Source.Clone, Destanation)
      else
      begin
        Destanation := Source;
        Result := True;
      end;
      if Result and Assigned(OnAfterPaint) then
        OnAfterPaint(Self, Source);
    finally
      UnlockTransform;
    end;
end;

procedure TocvCustomImageOperation.UnlockTransform;
begin
  FCriticalSection.Leave;
end;

{TovcImageOperationSmooth}
Const
  ocvSmoothOperations: array [TocvSmoothOperations] of Integer = (CV_BLUR_NO_SCALE, CV_BLUR, CV_GAUSSIAN, CV_MEDIAN,
    CV_BILATERAL);

procedure TovcSmoothOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TovcSmoothOperation then
    FSmoothOperation := (Dest as TovcSmoothOperation).FSmoothOperation;
end;

constructor TovcSmoothOperation.Create {(AOwner: TPersistent)};
begin
  inherited;
  FSmoothOperation := GAUSSIAN;
  size1 := 3;
  size2 := 3;
  sigma1 := 0;
  sigma2 := 0;
end;

procedure TovcSmoothOperation.SetSmoothOperation(const Value: TocvSmoothOperations);
begin
  if LockTransform then
    try
      FSmoothOperation := Value;
    finally
      UnlockTransform;
    end;
end;

function TovcSmoothOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Image: pIplImage;
begin
  Image := cvCloneImage(Source.IpImage);
  cvSmooth(Source.IpImage, Image, ocvSmoothOperations[SmoothOperation], size1, size2, sigma1, sigma2);
  Destanation := TocvImage.Create(Image);
  Result := True;
end;

{TRegisteredImageOperations}

function TRegisteredImageOperations.FindByClassName(const ClassName: String): TocvImageOperationClass;
Var
  i: Integer;
begin
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

{TovcCustomErodeDilate}

procedure TovcCustomErodeDilateOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TovcCustomErodeDilateOperation then
    FMorphOp := (Dest as TovcCustomErodeDilateOperation).MorphOp;
end;

constructor TovcCustomErodeDilateOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Radius := 5;
  Iterations := 5;
  FMorphOp := SHAPE_RECT;
end;

procedure TovcCustomErodeDilateOperation.SetMorphOp(const Value: TocvErodeDilateMode);
begin
  if LockTransform then
    try
      FMorphOp := Value;
    finally
      UnlockTransform;
    end;
end;

const
  EDMorpgOp: array [TocvErodeDilateMode] of Integer = (CV_SHAPE_RECT, CV_SHAPE_CROSS, CV_SHAPE_ELLIPSE, CV_SHAPE_CUSTOM);

  {TovcErode}

function TovcErodeOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvErode(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{TovcDilate}

function TovcDilateOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  Kern: pIplConvKernel;
begin
  Destanation := TocvImage.Create(cvCloneImage(Source.IpImage));
  Kern := cvCreateStructuringElementEx(Radius * 2 + 1, Radius * 2 + 1, Radius, Radius, EDMorpgOp[FMorphOp]);
  cvDilate(Source.IpImage, Destanation.IpImage, Kern, Iterations);
  cvReleaseStructuringElement(Kern);
  Result := True;
end;

{TocvLaplace}

constructor TocvLaplaceOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Aperture := 3;
end;

function TocvLaplaceOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  TempImg: pIplImage;
begin
  TempImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.depth, Source.IpImage^.nChannels));
  cvLaplace(Source.IpImage, TempImg, Aperture);
  cvConvertScale(TempImg, Destanation.IpImage);
  cvReleaseImage(TempImg);
  Result := True;
end;

{TovcSobel}

constructor TovcSobelOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  XOrder := 1;
  YOrder := 1;
  Aperture := 3;
end;

function TovcSobelOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  TmpImg: pIplImage;
begin
  TmpImg := cvCreateImage(cvGetSize(Source.IpImage), IPL_DEPTH_16S, Source.IpImage^.nChannels);
  Destanation := TocvImage.Create(cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.depth, Source.IpImage^.nChannels));
  cvSobel(Source.IpImage, TmpImg, XOrder, YOrder, Aperture);
  cvConvertScale(TmpImg, Destanation.IpImage);
  cvReleaseImage(TmpImg);
  Result := True;
end;

{TocvImageOperationCollectionItem}

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
  Transform: Boolean;
begin
  Result := LockTransform;
  if Result then
    try
      Result := Operation.DoTransform(Source, Destanation)
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

{TocvThresholdOperation}

Const
  cThreshold: array [TocvThresholdType] of Integer = (CV_THRESH_BINARY, CV_THRESH_BINARY_INV, CV_THRESH_TRUNC, CV_THRESH_TOZERO,
    CV_THRESH_TOZERO_INV, CV_THRESH_MASK, CV_THRESH_OTSU);

constructor TocvThresholdOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  Threshold := 50;
end;

constructor TocvCustomThresholdOperation.Create {(AOwner: TComponent)};
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

{TocvAdaptiveThresholdOperation}

const
  cAdaptiveThresholdType: array [TocvAdaptiveThresholdType] of Integer = (CV_ADAPTIVE_THRESH_MEAN_C,
    CV_ADAPTIVE_THRESH_GAUSSIAN_C);

constructor TocvAdaptiveThresholdOperation.Create {(AOwner: TComponent)};
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
  cvAdaptiveThreshold(Source.GrayImage.IpImage, Destanation.IpImage, MaxValue, cAdaptiveThresholdType[AdaptiveThresholdType],
    cThreshold[ThresholdType], BlockSize, Param);
  Result := True;
end;

{TocvContoursOperation}

constructor TocvContoursOperation.Create {(AOwner: TComponent)};
begin
  inherited;
  FOffset := TocvPoint.Create;
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
              cvDrawContours(Destanation.IpImage, s_contours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb), ContourDraw.MaxLevel,
                ContourDraw.Thickness, cLineType[ContourDraw.LineType], cvPoint(ContourDraw.Offset.X, ContourDraw.Offset.Y));
            s_contours := s_contours.h_next;
          end;
        end
        else
          cvDrawContours(Destanation.IpImage, FContours, CV_RGB(er, eg, eb), CV_RGB(hr, hg, hb), ContourDraw.MaxLevel,
            ContourDraw.Thickness, cLineType[ContourDraw.LineType], cvPoint(ContourDraw.Offset.X, ContourDraw.Offset.Y));
      end;
      Result := True;
    end;
  finally
    cvReleaseMemStorage(storage)
  end;
end;

procedure TocvContoursOperation.DoNotifyContours(const Image: IocvImage; const ContourCount: Integer; const Contours: pCvSeq);
begin
  if Assigned(OnContour) then
    OnContour(Self, Image, ContourCount, Contours);
end;

{TocvRotateOperation}

procedure TocvRotateOperation.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvRotateOperation then
  begin
    FCustomCenter := (Dest as TocvRotateOperation).FCustomCenter;
    FMethod := (Dest as TocvRotateOperation).FMethod;
    FWarpingFlag := (Dest as TocvRotateOperation).FWarpingFlag;
    FScale := (Dest as TocvRotateOperation).FScale;
  end;
end;

constructor TocvRotateOperation.Create(AOwner: TPersistent);
begin
  inherited;
  Angle := 90;
  FCustomCenter := TocvPoint.Create;
  RotateAroundCenter := True;
  Method := INTER_LINEAR;
  WarpingFlag := [WARP_FILL_OUTLIERS];
  Scale := 1;
end;

destructor TocvRotateOperation.Destroy;
begin
  FCustomCenter.Free;
  inherited;
end;

function TocvRotateOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  rot_mat: pCvMat;
  center: TcvPoint2D32f;
  D: pIplImage;
  M: Integer;
begin
  // Матрица трансформации
  rot_mat := cvCreateMat(2, 3, CV_32FC1);
  // Вращение относительно центра изображения
  if RotateAroundCenter then
  begin
    center.X := Source.IpImage^.Width div 2;
    center.Y := Source.IpImage^.Height div 2;
  end
  else
  begin
    center.X := CustomCenter.X;
    center.Y := CustomCenter.Y;
  end;
  cv2DRotationMatrix(center, Angle, Scale, rot_mat);
  // Создаем изображение
  D := cvCreateImage(cvGetSize(Source.IpImage), Source.IpImage^.depth, Source.IpImage^.nChannels);
  // Выполняем вращение
  M := Integer(Method);
  if WARP_FILL_OUTLIERS in FWarpingFlag then
    M := M or CV_WARP_FILL_OUTLIERS;
  if WARP_INVERSE_MAP in FWarpingFlag then
    M := M or CV_WARP_INVERSE_MAP;
  cvWarpAffine(Source.IpImage, D, rot_mat, M, cvScalarAll(0));
  cvReleaseMat(rot_mat);
  Destanation := TocvImage.Create(D);
  Result := True;
end;

{TPersistentPoint}

procedure TocvPoint.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvPoint then
    FPoint := (Dest as TocvPoint).FPoint;
end;

{TocvCountourDraw}

procedure TocvDraw.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvDraw then
  begin
    FOffset.FPoint := (Dest as TocvDraw).FOffset.FPoint;
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
  FOffset := TocvPoint.Create;
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

function TocvDraw.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{TocvContourApprox}

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

{TocvAbsDiff}

function TocvAbsDiff.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
Var
  GrayImage: IocvImage;
begin
  GrayImage := Source.GrayImage;
  Destanation := GrayImage.Same;
  if Assigned(FPrevFrame) then
    cvAbsDiff(FPrevFrame.IpImage, GrayImage.IpImage, Destanation.IpImage);
  FPrevFrame := GrayImage;
  Result := True;
end;

{TocvFaceDetect}

constructor TocvHaarCascade.Create(AOwner: TPersistent);
begin
  inherited;
  FLockFrontalFaceChange := TCriticalSection.Create;
  FMinSize := TocvPoint.Create(30, 30);
  FMaxSize := TocvPoint.Create;
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
  storage: pCvMemStorage;
  gray: IocvImage;
  detected_objects: pCvSeq;
  i: Integer;
  cvr: pCvRect;
  r, g, b: byte;
begin
  Destanation := Source;
  if Assigned(FCascade) then
  begin
    storage := cvCreateMemStorage(0);
    try
      gray := Source.GrayImage;
      if Equalize then
        cvEqualizeHist(gray.IpImage, gray.IpImage);
      detected_objects := cvHaarDetectObjects(gray.IpImage, FCascade, storage, Scale, MinNeighbors, GetHaarCascadeFlag,
        cvSize(MinSize.X, MinSize.Y), cvSize(MaxSize.X, MaxSize.Y));

      if Assigned(detected_objects) then
      begin
        SetLength(FHaarRects, detected_objects^.total);
        i := 0;
        While i < detected_objects^.total do
        begin
          cvr := pCvRect(cvGetSeqElem(detected_objects, i));
          FHaarRects[i] := ocvRect(cvr^.X, cvr^.Y, (cvr^.X) + (cvr^.Width), (cvr^.Y) + (cvr^.Height));
          Inc(i);
        end;

        if Assigned(OnHaarCascade) and ((not NotifyOnlyWhenFound) or (detected_objects^.total > 0)) then
          OnHaarCascade(Self, Destanation, FHaarRects);

        if DrawHaarCascade.Enabled then
        begin
          GetRGBValue(DrawHaarCascade.Color, r, g, b);
          i := 0;
          While i < detected_objects^.total do
          begin
            cvr := pCvRect(cvGetSeqElem(detected_objects, i));
            cvRectangle(Destanation.IpImage, cvPoint(cvr^.X, cvr^.Y), cvPoint((cvr^.X) + (cvr^.Width), (cvr^.Y) + (cvr^.Height)),
              CV_RGB(r, g, b), DrawHaarCascade.Thickness, cLineType[DrawHaarCascade.LineType], DrawHaarCascade.Shift);
            Inc(i);
          end;
        end;
      end;
      Result := True;
    finally
      cvReleaseMemStorage(storage);
    end;
  end
  else
    Result := False;
end;

function TocvHaarCascade.GetHaarCascadeFlag: Integer;
Var
  i: TocvHaarCascadeFlag;
  j: Integer;
begin
  Result := 0;
  j := 1;
  for i := HAAR_DO_CANNY_PRUNING to HAAR_DO_ROUGH_SEARCH do
  begin
    if i in FCascadeFlags then
      Result := Result or j;
    j := j * 2;
  end;
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
    BufSize := GetTempPath(0, nil);
    SetLength(Result, BufSize);
    GetTempPath(BufSize, PChar(Result));
    Result := Trim(Result);
  end;

Var
  FullFileName: String;
  RS: TResourceStream;
  DC: TZDecompressionStream;
  FS: TFileStream;
begin
  FLockFrontalFaceChange.Enter;
  try
    if FHaarCascade <> Value then
    begin
      FHaarCascade := Value;
      ReleaseCascade;
    end;
    if not(csDesigning in ComponentState) then
    begin
      if not Assigned(FCascade) then
        try
          FullFileName := TempPath + FrontalFaceXML[FHaarCascade].FileName;
          if not FileExists(FullFileName) then
          begin
            RS := TResourceStream.Create(hInstance, FrontalFaceXML[FHaarCascade].Name, RT_RCDATA);
            DC := TZDecompressionStream.Create(RS);
            FS := TFileStream.Create(FullFileName, fmCreate);
            try
              FS.CopyFrom(DC, DC.Size);
            finally
              DC.Free;
              FS.Free;
              RS.Free;
            end;
          end;
          DoLoadHaarCascade(FullFileName);
        except
          ReleaseCascade;
        end;
    end;
  finally
    FLockFrontalFaceChange.Leave;
  end;
end;

constructor TocvPoint.Create(const AX, AY: Integer);
begin
  FPoint.X := AX;
  FPoint.Y := AY;
end;

{TocvContourDraw}

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

{TocvMatchTemplate}

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
  p1, p2: TCvPoint;
  min: Double;
  r, g, b: byte;
begin
  Destanation := Source;
  if Assigned(IPLTemplate) then
  begin
    imgMat := cvCreateImage(cvSize(Source.IpImage^.Width - IPLTemplate^.Width + 1, Source.IpImage^.Height - IPLTemplate^.Height +
      1), IPL_DEPTH_32F, 1);
    cvMatchTemplate(Source.IpImage, IPLTemplate, imgMat, Integer(FMethod));

    if Assigned(OnMathTemplateRect) or DrawRect.Enabled then
    begin
      cvMinMaxLoc(imgMat, @min, @min, nil, @p1, nil);
      p2.X := p1.X + IPLTemplate^.Width - 1;
      p2.Y := p1.Y + IPLTemplate^.Height - 1;

      if Assigned(OnMathTemplateRect) then
        OnMathTemplateRect(Self, Source, ocvRect(p1.X, p1.Y, p2.X, p2.Y));

      if DrawRect.Enabled then
      begin
        GetRGBValue(DrawRect.Color, r, g, b);
        cvRectangle(Destanation.IpImage, p1, p2, CV_RGB(r, g, b));
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

{TocvMotionDetect}

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

  if not Assigned(FPrevFrame) then
    FPrevFrame := CurrentGrayImage;

  DifferenceImage := CurrentGrayImage.Same;
  cvAbsDiff(FPrevFrame.IpImage, CurrentGrayImage.IpImage, DifferenceImage.IpImage);
  cvSmooth(DifferenceImage.IpImage, DifferenceImage.IpImage, Integer(Smooth));

  if Threshold.DoTransform(DifferenceImage, ThresholdImage) then
  begin
    // img_out := DifferenceImage.Clone;
    storage := cvCreateMemStorage(0);
    FContours := AllocMem(SizeOf(TCvSeq));
    try
      cvFindContours(ThresholdImage.IpImage, storage, @FContours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,
        cvPoint(0, 0));

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

      cvFindContours(ThresholdImage.IpImage, storage, @FContours, SizeOf(TCvContour), CV_RETR_LIST, CV_CHAIN_APPROX_NONE,
        cvPoint(0, 0));

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
            Rects[i] := ocvRect(Rect.X, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.Height);
          end
          else if CalcRectType = mdMinAreaRect then
          begin
            Rect2d := cvMinAreaRect2(c);
            Rects[i] := ocvRect(Round(Rect2d.center.X - Rect2d.Size.Width / 2), Round(Rect2d.center.Y - Rect2d.Size.Height / 2),
              Round(Rect2d.center.X + Rect2d.Size.Width / 2), Round(Rect2d.center.Y + Rect2d.Size.Height / 2));
          end;

          if DrawMotionRect.Enabled then
          begin
            GetRGBValue(DrawMotionRect.Color, r, g, b);
            cvRectangle(Destanation.IpImage, cvPoint(Rects[i].Left, Rects[i].Top), cvPoint(Rects[i].Right, Rects[i].Bottom),
              CV_RGB(r, g, b), DrawMotionRect.Thickness, cLineType[DrawMotionRect.LineType], DrawMotionRect.Shift);
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

{TocvCustomImageOperationWithNestedOperation}

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

{TocvRectPersistent}

procedure TocvRectPersistent.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvRectPersistent then
  begin
    FRight := (Dest as TocvRectPersistent).FRight;
    FBottom := (Dest as TocvRectPersistent).FBottom;
    FTop := (Dest as TocvRectPersistent).FTop;
    FLeft := (Dest as TocvRectPersistent).FLeft;
  end;
end;

function TocvRectPersistent.GetCvRect: TCvRect;
begin
  Result := ocv.core.types_c.cvRect(Left, Top, Width, Height);
end;

function TocvRectPersistent.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TocvRectPersistent.GetOcvRect: TocvRect;
begin
  Result := uOCVTypes.ocvRect(Left, Top, Right, Bottom);
end;

function TocvRectPersistent.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TocvRectPersistent.SetCvRect(const Value: TCvRect);
begin
  Left := Value.X;
  Top := Value.Y;
  Width := Value.Width;
  Height := Value.Height;
end;

procedure TocvRectPersistent.SetHeight(const Value: Integer);
begin
  Bottom := Top + Value;
end;

procedure TocvRectPersistent.SetOcvRect(const Value: TocvRect);
begin
  FLeft := Value.Left;
  FTop := Value.Top;
  FRight := Value.Right;
  FBottom := Value.Bottom;
end;

procedure TocvRectPersistent.SetWidth(const Value: Integer);
begin
  FRight := FLeft + Value;
end;

{TovcCropOperation}

constructor TovcCropOperation.Create(AOwner: TPersistent);
begin
  inherited;
  FCropRect := TocvRectPersistent.Create;
end;

destructor TovcCropOperation.Destroy;
begin
  FCropRect.Free;
  inherited;
end;

function TovcCropOperation.DoTransform(const Source: IocvImage; out Destanation: IocvImage): Boolean;
begin
  if FCropRect.ocvRect.IsEmpty then
    Destanation := Source
  else
    Destanation := Source.Crop(FCropRect.cvRect);
  Result := True;
end;

{TocvImageOperationCollection}

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

initialization

GetRegisteredImageOperations.RegisterIOClass(TocvNoneOperation, 'None');
GetRegisteredImageOperations.RegisterIOClass(TocvGrayScaleOperation, 'GrayScale');
GetRegisteredImageOperations.RegisterIOClass(TovcCannyOperation, 'Canny');
GetRegisteredImageOperations.RegisterIOClass(TovcSmoothOperation, 'Smooth');
GetRegisteredImageOperations.RegisterIOClass(TovcErodeOperation, 'Erode');
GetRegisteredImageOperations.RegisterIOClass(TovcDilateOperation, 'Dilate');
GetRegisteredImageOperations.RegisterIOClass(TocvLaplaceOperation, 'Laplace');
GetRegisteredImageOperations.RegisterIOClass(TovcSobelOperation, 'Sobel');
GetRegisteredImageOperations.RegisterIOClass(TocvThresholdOperation, 'Threshold');
GetRegisteredImageOperations.RegisterIOClass(TocvAdaptiveThresholdOperation, 'AdaptiveThreshold');
GetRegisteredImageOperations.RegisterIOClass(TocvContoursOperation, 'Contours');
GetRegisteredImageOperations.RegisterIOClass(TocvRotateOperation, 'Rotate');
GetRegisteredImageOperations.RegisterIOClass(TocvAbsDiff, 'AbsDiff');
GetRegisteredImageOperations.RegisterIOClass(TocvHaarCascade, 'HaarCascade');
GetRegisteredImageOperations.RegisterIOClass(TocvMatchTemplate, 'MatchTemplate');
GetRegisteredImageOperations.RegisterIOClass(TocvMotionDetect, 'MotionDetect');
GetRegisteredImageOperations.RegisterIOClass(TovcCropOperation, 'Crop');

finalization

if Assigned(_RegisteredImageOperations) then
  FreeAndNil(_RegisteredImageOperations);

end.

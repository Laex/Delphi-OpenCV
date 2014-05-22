unit opencv.core;

interface

uses
  opencv.mat;

Type

  TocvVec3<T> = class

  end;

  TocvVec3d = TocvVec3<Double>;
  TocvVec3b = TocvVec3<byte>;

  TocvPoint<T> = class
  public
    function getX(): T; virtual; stdcall; abstract;
    procedure setX(x: T); virtual; stdcall; abstract;
    function getY(): T; virtual; stdcall; abstract;
    procedure setY(y: T); virtual; stdcall; abstract;

    property x: T read getX write setX;
    property y: T read getY write setY;
  end;

  TocvPoint2i = TocvPoint<integer>;
  TocvPoint = TocvPoint2i;

  TocvVectorOfPoint<T> = class
  public
    function size(): integer; virtual; stdcall; abstract;
  end;

  TocvVectorOfPoint2i = TocvVectorOfPoint<TocvPoint2i>;
  TocvVectorOfPoint = TocvVectorOfPoint2i;

  TocvVectorOfVectorOfPoint<T> = class
  public
    function size(): integer; virtual; stdcall; abstract;
    procedure push_back(Val: TocvVectorOfPoint2i); virtual; stdcall; abstract;
  end;

  TocvVectorOfVectorOfPoint2i = TocvVectorOfVectorOfPoint<TocvVectorOfPoint2i>;

  TocvRotatedRect = class

  end;

function CreateVec3d(): TocvVec3d; stdcall; overload;
function CreateVec3d(v0, v1, v2: Double): TocvVec3d; stdcall; overload;
function Vec3d(v0, v1, v2: Double): TocvVec3d; stdcall;
procedure ReleaseVec3d(ex: TocvVec3d); stdcall;

function CreateVec3b(): TocvVec3b; stdcall; overload;
function CreateVec3b(v0, v1, v2: byte): TocvVec3b; stdcall; overload;
function Vec3b(v0, v1, v2: byte): TocvVec3b; stdcall;
procedure ReleaseVec3b(ex: TocvVec3b); stdcall;

procedure ReleasePoint2i(ex: TocvPoint2i); stdcall;
procedure ReleaseVectorOfPoint2i(ex: TocvVectorOfPoint2i); stdcall;
procedure ReleaseVectorOfVectorOfPoint2i(ex: TocvVectorOfVectorOfPoint2i); stdcall;

procedure MSER(m: TocvMat; var ex: TocvVectorOfVectorOfPoint2i); stdcall;

implementation

uses opencv.ulibname;

function CreateVec3d(): TocvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d'; overload;
function CreateVec3d(v0, v1, v2: Double): TocvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d3'; overload;
function Vec3d(v0, v1, v2: Double): TocvVec3d; stdcall; external opencv_classes_lib name 'CreateVec3d3'; overload;
procedure ReleaseVec3d(ex: TocvVec3d); stdcall; external opencv_classes_lib;

function CreateVec3b(): TocvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b'; overload;
function CreateVec3b(v0, v1, v2: byte): TocvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b3'; overload;
function Vec3b(v0, v1, v2: byte): TocvVec3b; stdcall; external opencv_classes_lib name 'CreateVec3b3'; overload;
procedure ReleaseVec3b(ex: TocvVec3b); stdcall; external opencv_classes_lib;

procedure ReleasePoint2i; external opencv_classes_lib;
procedure ReleaseVectorOfPoint2i; external opencv_classes_lib;
procedure ReleaseVectorOfVectorOfPoint2i; external opencv_classes_lib;

procedure MSER; external opencv_classes_lib;

end.

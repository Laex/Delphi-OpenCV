unit calib3d;

interface

Uses Core.types_c, types_c;

{
  /* Reprojects the computed disparity image to the 3D space using the specified 4x4 matrix */
  CVAPI(void)  cvReprojectImageTo3D(
  const CvArr* disparityImage,
  CvArr* _3dImage,
  const CvMat* Q,
  int handleMissingValues CV_DEFAULT(0) );
}

procedure cvReprojectImageTo3D(
  { } const disparityImage: pCvMat;
  { } _3dImage: pIplImage;
  { } const Q: pCvMat;
  { } handleMissingValues: Integer = 0); cdecl;

Const
  CV_CALIB_CB_ADAPTIVE_THRESH = 1;
  CV_CALIB_CB_NORMALIZE_IMAGE = 2;
  CV_CALIB_CB_FILTER_QUADS = 4;
  CV_CALIB_CB_FAST_CHECK = 8;

  {
    /* Detects corners on a chessboard calibration pattern */
    CVAPI(int) cvFindChessboardCorners(
    const void* image,
    CvSize pattern_size,
    CvPoint2D32f* corners,
    int* corner_count CV_DEFAULT(NULL),
    int flags CV_DEFAULT(CV_CALIB_CB_ADAPTIVE_THRESH+CV_CALIB_CB_NORMALIZE_IMAGE) );
  }
function cvFindChessboardCorners(const image: Pointer; pattern_size: TCvSize; corners: pCvPoint2D32f;
  corner_count: pInteger = nil; flags: Integer = CV_CALIB_CB_ADAPTIVE_THRESH + CV_CALIB_CB_NORMALIZE_IMAGE)
  : Integer; cdecl;

const
  CV_CALIB_USE_INTRINSIC_GUESS = 1;
  CV_CALIB_FIX_ASPECT_RATIO = 2;
  CV_CALIB_FIX_PRINCIPAL_POINT = 4;
  CV_CALIB_ZERO_TANGENT_DIST = 8;
  CV_CALIB_FIX_FOCAL_LENGTH = 16;
  CV_CALIB_FIX_K1 = 32;
  CV_CALIB_FIX_K2 = 64;
  CV_CALIB_FIX_K3 = 128;
  CV_CALIB_FIX_K4 = 2048;
  CV_CALIB_FIX_K5 = 4096;
  CV_CALIB_FIX_K6 = 8192;
  CV_CALIB_RATIONAL_MODEL = 16384;
  CV_CALIB_THIN_PRISM_MODEL = 32768;
  CV_CALIB_FIX_S1_S2_S3_S4 = 65536;

  {
    /* Draws individual chessboard corners or the whole chessboard detected */
    CVAPI(void) cvDrawChessboardCorners(
    CvArr* image,
    CvSize pattern_size,
    CvPoint2D32f* corners,
    int count,
    int pattern_was_found );
  }
procedure cvDrawChessboardCorners(image: pIplImage; pattern_size: TCvSize; corners: pCvPoint2D32f; count: Integer;
  pattern_was_found: Integer); cdecl;

{
  /* Finds intrinsic and extrinsic camera parameters
  from a few views of known calibration pattern */
  CVAPI(double) cvCalibrateCamera2(
  const CvMat* object_points,
  const CvMat* image_points,
  const CvMat* point_counts,
  CvSize image_size,
  CvMat* camera_matrix,
  CvMat* distortion_coeffs,
  CvMat* rotation_vectors CV_DEFAULT(NULL),
  CvMat* translation_vectors CV_DEFAULT(NULL),
  int flags CV_DEFAULT(0),
  CvTermCriteria term_crit CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,DBL_EPSILON)) );
}

function cvCalibrateCamera2(
  { } const object_points: pCvMat;
  { } const image_points: pCvMat;
  { } const point_counts: pCvMat;
  { } image_size: TCvSize;
  { } camera_matrix: pCvMat;
  { } distortion_coeffs: pCvMat;
  { } rotation_vectors: pCvMat { =nil };
  { } translation_vectors: pCvMat { =nil };
  { } flags: Integer { =0 };
  { } term_crit: TCvTermCriteria { =cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,30,DBL_EPSILON) }
  ): Double; cdecl;

implementation

Uses uLibName;

procedure cvReprojectImageTo3D; external calib3d_dll;
function cvFindChessboardCorners; external calib3d_dll;
procedure cvDrawChessboardCorners; external calib3d_dll;
function cvCalibrateCamera2; external calib3d_dll;

end.

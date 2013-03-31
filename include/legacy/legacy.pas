unit legacy;

interface

Uses Core.types_c;

Type
  pCvStereoGCState = ^TCvStereoGCState;

  TCvStereoGCState = packed record
    Ithreshold: Integer;
    interactionRadius: Integer;
    K, lambda, lambda1, lambda2: Single;
    occlusionCost: Integer;
    minDisparity: Integer;
    numberOfDisparities: Integer;
    maxIters: Integer;
    left: pCvMat;
    right: pCvMat;
    dispLeft: pCvMat;
    dispRight: pCvMat;
    ptrLeft: pCvMat;
    ptrRight: pCvMat;
    vtxBuf: pCvMat;
    edgeBuf: pCvMat;
  end;

  // CVAPI(CvStereoGCState*) cvCreateStereoGCState( int numberOfDisparities, int maxIters );
function cvCreateStereoGCState(numberOfDisparities: Integer; maxIters: Integer): pCvStereoGCState; cdecl;

{
  CVAPI(void) cvFindStereoCorrespondenceGC(
  const CvArr* left,
  const CvArr* right,
  CvArr* disparityLeft,
  CvArr* disparityRight,
  CvStereoGCState* state,
  int useDisparityGuess CV_DEFAULT(0) );
}

procedure cvFindStereoCorrespondenceGC(const left: pIplImage; const right: pIplImage; disparityLeft: pCvMat;
  disparityRight: pCvMat; state: pCvStereoGCState; useDisparityGuess: Integer = 0); cdecl;

// CVAPI(void) cvReleaseStereoGCState( CvStereoGCState** state );
procedure cvReleaseStereoGCState(Var state: pCvStereoGCState); cdecl;

implementation

Uses LibName;

function cvCreateStereoGCState; external legacy_Dll;
procedure cvFindStereoCorrespondenceGC; external legacy_Dll;
procedure cvReleaseStereoGCState; external legacy_Dll;

end.

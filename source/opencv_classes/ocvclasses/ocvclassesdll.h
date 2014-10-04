#pragma once

#include "stdafx.h"
#include "ocvclasses.h"

#define ICLASS_API __stdcall
#define ICLASS_EXPORT extern "C" 
//__declspec(dllexport)

ICLASS_EXPORT TMat* ICLASS_API CreateMat();
ICLASS_EXPORT TMat* ICLASS_API CreateMatRCT(int rows, int cols, int type);
ICLASS_EXPORT void ICLASS_API ReleaseMat(TMat * ex);
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCapture();
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCaptureDevice(int device);
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCaptureFileName(const char* filename);
ICLASS_EXPORT void ICLASS_API ReleaseVideoCapture(TVideoCapture * ex);
ICLASS_EXPORT TCvKNearest* ICLASS_API CreateCvKNearest();
ICLASS_EXPORT TCvKNearest* ICLASS_API CreateCvKNearestTR(CvMat* _trainData, CvMat* _responses, CvMat* _sampleIdx, BOOL _isRegression, int _max_k);
ICLASS_EXPORT void ICLASS_API ReleaseCvKNearest(TCvKNearest * ex);
ICLASS_EXPORT TVec3d* ICLASS_API CreateVec3d();
ICLASS_EXPORT TVec3d* ICLASS_API CreateVec3d3(double v0, double v1, double v2);
ICLASS_EXPORT void ICLASS_API ReleaseVec3d(TVec3d * ex);
ICLASS_EXPORT TVec3b* ICLASS_API CreateVec3b();
ICLASS_EXPORT TVec3b* ICLASS_API CreateVec3b3(uchar v0, uchar v1, uchar v2);
ICLASS_EXPORT void ICLASS_API ReleaseVec3b(TVec3d * ex);
ICLASS_EXPORT void ICLASS_API ReleasePoint2i(TPoint2i * ex);
ICLASS_EXPORT void ICLASS_API ReleaseVectorOfPoint2i(TVectorOfPoint2i * ex);
ICLASS_EXPORT void ICLASS_API MSER(TMat m, TVectorOfVectorOfPoint2i ** ex);
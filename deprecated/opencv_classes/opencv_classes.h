#pragma once

#include "Ihighgui.h"
#include "Icore.types.h"
#include "IString.h"
#include "Iml.h"

#ifdef ICLASS_EXPORTS
#define ICLASS_API __declspec(dllexport) __stdcall
#else
#define ICLASS_API __declspec(dllimport) __stdcall
#endif

// highgui.h
extern "C" HRESULT ICLASS_API CreateVideoCapture(LPVideoCapture *_VideoCapture);
extern "C" HRESULT ICLASS_API CreateVideoCapture_dvc(int device, LPVideoCapture *_VideoCapture);
extern "C" HRESULT ICLASS_API CreateVideoCapture_fln(char* filename, LPVideoCapture *_VideoCapture);
//core/types.hpp
extern "C" HRESULT ICLASS_API CreateMat(LPMat *_Mat);
extern "C" HRESULT ICLASS_API CreateMat_rct(int rows, int cols, int type, LPMat *_Mat);
extern "C" HRESULT ICLASS_API CreateMat_Mat(cv::Mat& m, LPMat *_Mat);
extern "C" HRESULT ICLASS_API CreatePoint(LPPoint *_Point);
extern "C" HRESULT ICLASS_API CreatePoint_xy(int x,int y, LPPoint *_Point);
extern "C" HRESULT ICLASS_API CreateScalar(LPScalar *_Scalar);
extern "C" HRESULT ICLASS_API CreateScalar_v03(int v0,int v1, int v2, int v3, LPScalar *_Scalar);
extern "C" HRESULT ICLASS_API CreateScalar_v0(int v0, LPScalar *_Scalar);
extern "C" HRESULT ICLASS_API CreateSize2i(LPSize * _Size2i);
extern "C" HRESULT ICLASS_API CreateSize2i_wh(int _width, int _height, LPSize * _Size2i);
//IString.h
extern "C" HRESULT ICLASS_API CString(const char* s, LPString *_String);
//Iml.h
extern "C" HRESULT ICLASS_API CreateCvKNearest(LPCvKNearest *_LPCvKNearest);
extern "C" HRESULT ICLASS_API CreateCvKNearestTR(const CvMat* _trainData, const CvMat* _responses,const CvMat* _sampleIdx, BOOL _isRegression, int _max_k, LPCvKNearest *_LPCvKNearest);
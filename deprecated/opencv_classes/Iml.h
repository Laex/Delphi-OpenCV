#pragma once

#include "stdafx.h"
#include "Unknwn.h"
#include "Icore.types.h"

struct __declspec(uuid("{2F98E12A-AB71-48B5-AACC-025D0D0A3611}")) ICvKNearest : public IUnknown
{
public:	
	virtual BOOL __stdcall train( const CvMat* trainData, const CvMat* responses,
		const CvMat* sampleIdx=0, BOOL is_regression=false,
		int maxK=32, BOOL updateBase=false )=0;

	virtual float __stdcall find_nearest( const CvMat* samples, int k, CV_OUT CvMat* results=0,
		const float** neighbors=0, CV_OUT CvMat* neighborResponses=0, CV_OUT CvMat* dist=0 ) =0;

/*
    virtual BOOL __stdcall open(int device) = 0;
	virtual BOOL __stdcall openfilename(const char* filename) = 0;
    virtual BOOL __stdcall isOpened() = 0;
    virtual void __stdcall release() = 0;

    virtual BOOL __stdcall grab() = 0;
    virtual BOOL __stdcall retrieve(LPMat *image, int flag) = 0;
    virtual BOOL __stdcall read(LPMat *image) = 0;

	virtual BOOL __stdcall setvalue(int propId, double value) = 0;
    virtual double __stdcall getvalue(int propId) = 0;
*/
};

typedef ICvKNearest * LPCvKNearest;
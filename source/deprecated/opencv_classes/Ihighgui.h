#pragma once

#include "stdafx.h"
#include "Unknwn.h"
#include "Icore.types.h"

struct __declspec(uuid("{3F605CF0-ECAC-4230-B30B-AF9BFD516C4F}")) IVideoCapture : public IUnknown
{
public:	
    virtual BOOL __stdcall open(int device) = 0;
	virtual BOOL __stdcall openfilename(const char* filename) = 0;
    virtual BOOL __stdcall isOpened() = 0;
    virtual void __stdcall release() = 0;

    virtual BOOL __stdcall grab() = 0;
    virtual BOOL __stdcall retrieve(LPMat *image, int flag) = 0;
    virtual BOOL __stdcall read(LPMat *image) = 0;

	virtual BOOL __stdcall setvalue(int propId, double value) = 0;
    virtual double __stdcall getvalue(int propId) = 0;
};

typedef IVideoCapture * LPVideoCapture;
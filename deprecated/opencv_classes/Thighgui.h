#pragma once

#include "Ihighgui.h"
#include "Icore.types.h"
#include "opencv_classes.h"
#include "opencv2\highgui\highgui.hpp"

class TVideoCapture : public IVideoCapture
{
public:

	TVideoCapture() : FVideoCapture(), FRefCount(0) { }
	TVideoCapture(int device) : FVideoCapture(device), FRefCount(0) { }
	TVideoCapture(const char* filename) : FVideoCapture(cv::string(filename)), FRefCount(0) { }

	~TVideoCapture() { }

	// Wrapping methods 
	BOOL __stdcall open(int device) { return FVideoCapture.open(device); };
	BOOL __stdcall openfilename(const char* filename) { return FVideoCapture.open(cv::string(filename)); };	
	BOOL __stdcall isOpened() { return FVideoCapture.isOpened(); };
	void __stdcall release() { FVideoCapture.release(); };

	BOOL __stdcall grab() { return FVideoCapture.grab(); };
	BOOL __stdcall retrieve(LPMat* image, int flag)	
	{ 	
		cv::Mat m = cv::Mat();		
		bool r=FVideoCapture.retrieve(m,flag); 
		if (r) CreateMat_Mat(m,image);
		return r;
	};
	BOOL __stdcall read(LPMat* image)  
	{ 		
		cv::Mat* m = new cv::Mat();
		BOOL r=FVideoCapture.read(*m); 
		if (r) CreateMat_Mat(*m,image); else delete m;			
		return r;		
	};

	BOOL __stdcall setvalue(int propId, double value) { return FVideoCapture.set(propId,value); };
	double __stdcall getvalue(int propId) { return FVideoCapture.get(propId); };

	// Methods of IUnknown

	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IVideoCapture)))
		{
			*ppvObject = (void *)this;
			return S_OK;
		}
		else
		{
			*ppvObject = NULL;
			return E_NOINTERFACE;
		}
	}

	ULONG __stdcall AddRef(void)
	{
		return InterlockedIncrement(&FRefCount);
	}


	ULONG __stdcall Release(void)
	{
		ULONG result = InterlockedDecrement(&FRefCount);
		if (!result)
			delete this;
		return result;
	}

private:
	cv::VideoCapture FVideoCapture;	// Delegation instead of inheritance
	ULONG FRefCount;
};
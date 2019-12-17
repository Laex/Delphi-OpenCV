
#include "stdafx.h"

namespace cv
{

	//////////////////// VidoCapture ////////////////////
	ICLASS_API VideoCapture* __stdcall CreateVideoCapture()
	{
		return new VideoCapture;
	};

	ICLASS_API bool __stdcall VideoCaptureOpen(VideoCapture* e, int CamNumber)
	{
		return e->open(CamNumber);
	};

	ICLASS_API bool __stdcall VideoCaptureOpenFileName(VideoCapture* e, char* FileName)
	{
		return e->open(FileName);
	};

	ICLASS_API bool __stdcall VideoCaptureisOpened(VideoCapture* e)
	{
		return e->isOpened();
	};

	ICLASS_API bool __stdcall VideoCaptureRead(VideoCapture* e, Mat** M)
	{
		Mat *_M = new Mat;
		bool r=e->read(*_M);
		*M = _M;
		return r;
				
	};

	ICLASS_API bool __stdcall VideoCaptureSet(VideoCapture* e, int propId, double value)
	{
		return e->set(propId, value);
	}

	ICLASS_API double __stdcall VideoCaptureGet(VideoCapture* e, int propId)
	{
		return e->get(propId);
	}

	ICLASS_API void __stdcall DestroyVideoCapture(VideoCapture* e)
	{
		delete e;
	};	

}

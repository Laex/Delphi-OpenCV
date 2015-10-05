// opencv_classes.cpp: определяет экспортированные функции для приложения DLL.
//

#include "stdafx.h"
#include "opencv2/core/mat.hpp"
#include "opencv2/videoio.hpp"
#include "opencv2/objdetect.hpp"


#define ICLASS_API extern "C" __declspec(dllexport) 
//__stdcall

namespace cv
{
	//////////////////// Mat ////////////////////
	ICLASS_API Mat* __stdcall CreateMat()
	{
		return new Mat;
	};
	ICLASS_API bool __stdcall MatEmpty(Mat* e)
	{
		return e->empty();
	}

	ICLASS_API Mat* __stdcall CreateMatFromImage(IplImage* Image)
	{
		Mat *M = new Mat;
		*M = cvarrToMat(Image);// default additional arguments: don't copy data.
		//*M= Image;
		return M;
	}

	ICLASS_API int __stdcall GetMatData(Mat* e, int index, int param)
	{
		switch (index)
		{
		case 0:
			return (int)e->elemSize();
		case 1:
			return (int)e->elemSize1();
		case 2:
			return (int)e->type();
		case 3:
			return (int)e->depth();
		case 4:
			return (int)e->channels();
		case 5:
			return (int)e->step1(param);
		case 6:
			return (int)e->total();
		case 7:
			return (int)e->flags;
		case 8:
			return (int)e->dims;
		case 9:
			return (int)e->rows;
		case 10:
			return (int)e->cols;
		case 11:
			return (int)e->data;
		}
	};

	ICLASS_API void __stdcall DestroyMat(Mat* e)
	{
		delete e;
	};

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

	//////////////////// CascadeClassifier ////////////////////

	ICLASS_API CascadeClassifier* __stdcall CreateCascadeClassifier()
	{
		return new CascadeClassifier;
	};
	ICLASS_API void __stdcall DestroyCascadeClassifier(CascadeClassifier* e)
	{
		delete e;
	};

	ICLASS_API bool __stdcall get_CascadeClassifier_empty(CascadeClassifier* e)
	{
		return e->empty();
	}

	ICLASS_API bool __stdcall get_CascadeClassifier_load(CascadeClassifier* e, char* filename)
	{
		return e->load(filename);
	}


	ICLASS_API bool __stdcall CascadeClassifier_isOldFormatCascade(CascadeClassifier* e)
	{
		return e->isOldFormatCascade();
	}

	ICLASS_API Size* __stdcall CascadeClassifier_getOriginalWindowSize(CascadeClassifier* e)
	{
		Size *s = new Size(e->getOriginalWindowSize());
		//*s = e->getOriginalWindowSize();
		return s;
	}

	ICLASS_API int __stdcall CascadeClassifier_getFeatureType(CascadeClassifier* e)
	{
		return e->getFeatureType();
	}	

	ICLASS_API bool __stdcall CascadeClassifier_convert(CascadeClassifier* e, char* oldcascade, char* newcascade)
	{
		return e->convert(oldcascade, newcascade);
	}	

}

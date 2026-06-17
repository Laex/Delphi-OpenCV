
#include "stdafx.h"

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
		default:
			return -1;
		}
	};

	ICLASS_API void __stdcall DestroyMat(Mat* e)
	{
		delete e;
	};

	ICLASS_API Size* __stdcall CreateSize()
	{
		return new Size();
	}

	ICLASS_API void __stdcall DestroySize(Size* s)
	{
		delete s;
	}

	ICLASS_API Size* __stdcall CreateSizeFromCvSize(const CvSize *sz)
	{
		return new Size(*sz);
	}


}
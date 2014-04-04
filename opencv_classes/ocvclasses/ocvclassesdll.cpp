
#include "stdafx.h"
#include "ocvclassesdll.h"

///////////////////////////////////////////////////
TMat * __stdcall CreateMat()
{	
	return new TMat();
};

TMat * __stdcall CreateMatRCT(int rows, int cols, int type)
{	
	return new TMat(rows, cols, type);
};

void __stdcall ReleaseMat(TMat * ex)
{
	delete ex;
};

//////////////////////////////////////////////////
TVideoCapture * __stdcall CreateVideoCapture()
{
	return new TVideoCapture();
};

TVideoCapture * __stdcall CreateVideoCaptureDevice(int device)
{
	return new TVideoCapture(device);
};

TVideoCapture * __stdcall CreateVideoCaptureFileName(const char* filename)
{
	return new TVideoCapture(filename);
};

void __stdcall ReleaseVideoCapture(TVideoCapture * ex)
{
	delete ex;
};

////////////////////////////////////////////////
TCvKNearest * __stdcall CreateCvKNearest()
{
	return new TCvKNearest();
};

TCvKNearest * __stdcall CreateCvKNearestTR(CvMat* _trainData, CvMat* _responses, CvMat* _sampleIdx, BOOL _isRegression, int _max_k)
{
	return new TCvKNearest(_trainData, _responses, _sampleIdx, _isRegression, _max_k);
};

void __stdcall ReleaseCvKNearest(TCvKNearest * ex)
{
	delete ex;
};
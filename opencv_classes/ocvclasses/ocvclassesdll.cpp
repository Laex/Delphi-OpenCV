
#include <vector>
#include "stdafx.h"
#include "ocvclassesdll.h"
#include "opencv2\features2d\features2d.hpp"

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

////////////////////////////////////////////////
TVec3d * __stdcall CreateVec3d()
{
	return new TVec3d();
};

TVec3d * __stdcall CreateVec3d3(double v0, double v1, double v2)
{
	return new TVec3d(v0, v1, v2);
};

void __stdcall ReleaseVec3d(TVec3d * ex)
{
	delete ex;
};

////////////////////////////////////////////////

TVec3b * __stdcall CreateVec3b()
{
	return new TVec3b();
};

TVec3b * __stdcall CreateVec3b3(uchar v0, uchar v1, uchar v2)
{
	return new TVec3b(v0, v1, v2);
};

void __stdcall ReleaseVec3b(TVec3d * ex)
{
	delete ex;
};

////////////////////////////////////////////////

void __stdcall ReleasePoint2i(TPoint2i * ex)
{
	delete ex;
};
void __stdcall ReleaseVectorOfPoint2i(TVectorOfPoint2i * ex)
{
	delete ex;
};
void __stdcall ReleaseVectorOfVectorOfPoint2i(TVectorOfVectorOfPoint2i * ex)
{
	delete ex;
};

////////////////////////////////////////////////

void __stdcall MSER(TMat m, TVectorOfVectorOfPoint2i ** ex)
{
	std::vector<std::vector<cv::Point> > contours;
	cv::Mat* mm;
	mm = m.Mat();
	cv::MSER()(*mm, contours);
	*ex = new TVectorOfVectorOfPoint2i();
	for (int i = 0; i < contours.size(); i++)
	{
		(*ex)->push_back(TVectorOfPoint2i(contours[i]));
	}

}
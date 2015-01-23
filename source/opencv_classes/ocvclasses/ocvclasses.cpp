
#include <vector>
#include "stdafx.h"
#include "ocvexport.h"
#include "ocvclasses.h"
#include "opencv2\features2d\features2d.hpp"

///////////////////////////////////////////////////
TMat* ICLASS_API CreateMat()
{	
	return new TMat();
};

TMat* ICLASS_API CreateMatRCT(int rows, int cols, int type)
{	
	return new TMat(rows, cols, type);
};

TMat* ICLASS_API CreateMatFromImage(IplImage* m)
{
	return new TMat(m);
};

void ICLASS_API ReleaseMat(TMat* ex)
{
	delete ex;
};

//////////////////////////////////////////////////
TVideoCapture * ICLASS_API CreateVideoCapture()
{
	return new TVideoCapture();
};

TVideoCapture * ICLASS_API CreateVideoCaptureDevice(int device)
{
	return new TVideoCapture(device);
};

TVideoCapture * ICLASS_API CreateVideoCaptureFileName(const char* filename)
{
	return new TVideoCapture(filename);
};

void ICLASS_API ReleaseVideoCapture(TVideoCapture * ex)
{
	delete ex;
};

////////////////////////////////////////////////
TCvKNearest * ICLASS_API CreateCvKNearest()
{
	return new TCvKNearest();
};

TCvKNearest * ICLASS_API CreateCvKNearestTR(CvMat* _trainData, CvMat* _responses, CvMat* _sampleIdx, BOOL _isRegression, int _max_k)
{
	return new TCvKNearest(_trainData, _responses, _sampleIdx, _isRegression, _max_k);
};

void ICLASS_API ReleaseCvKNearest(TCvKNearest * ex)
{
	delete ex;
};

////////////////////////////////////////////////
TVec3d * ICLASS_API CreateVec3d()
{
	return new TVec3d();
};

TVec3d * ICLASS_API CreateVec3d3(double v0, double v1, double v2)
{
	return new TVec3d(v0, v1, v2);
};

void ICLASS_API ReleaseVec3d(TVec3d * ex)
{
	delete ex;
};

////////////////////////////////////////////////

TVec3b * ICLASS_API CreateVec3b()
{
	return new TVec3b();
};

TVec3b * ICLASS_API CreateVec3b3(uchar v0, uchar v1, uchar v2)
{
	return new TVec3b(v0, v1, v2);
};

void ICLASS_API ReleaseVec3b(TVec3d * ex)
{
	delete ex;
};

////////////////////////////////////////////////

void ICLASS_API ReleasePoint2i(TPoint2i * ex)
{
	delete ex;
};
void ICLASS_API ReleaseVectorOfPoint2i(TVectorOfPoint2i * ex)
{
	delete ex;
};
void ICLASS_API ReleaseVectorOfVectorOfPoint2i(TVectorOfVectorOfPoint2i * ex)
{
	delete ex;
};

////////////////////////////////////////////////

void ICLASS_API MSER(TMat m, TVectorOfVectorOfPoint2i ** ex)
{
	std::vector<std::vector<cv::Point> > contours;
	cv::Mat* mm;
	mm = m.Mat();
	cv::MSER()(*mm, contours);
	*ex = new TVectorOfVectorOfPoint2i();
	for (int i = 0; i < (int)contours.size(); i++)
	{
		(*ex)->push_back(TVectorOfPoint2i(contours[i]));
	}

}

///////////////////////////////////////////////
TCascadeClassifier* ICLASS_API CreateCascadeClassifier()
{
	return new TCascadeClassifier();
};

TCascadeClassifier* ICLASS_API CreateCascadeClassifierFromFile(const char* filename)
{
	return new TCascadeClassifier(filename);
};

void ICLASS_API ReleaseCascadeClassifier(TCascadeClassifier* ex)
{
	delete ex;
};

///////////////////////////////////////////////

TSURF* ICLASS_API CreateSURF()
{
	return new TSURF();
};

TSURF* ICLASS_API CreateSURFFromValue(double hessianThreshold,
	int nOctaves = 4, int nOctaveLayers = 2,
	BOOL extended = true, BOOL upright = false)
{
	return new TSURF(hessianThreshold,nOctaves, nOctaveLayers,extended, upright);
};

void ICLASS_API ReleaseSURF(TSURF* ex)
{
	delete ex;
};
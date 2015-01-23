
//#include <vector>
#include "stdafx.h"
#include "ocvexport.h"
#include "ocvclasses.h"
#include "dnaclasses.h"
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

TSIFT* ICLASS_API CreateSIFT(int nfeatures = 0, int nOctaveLayers = 3,
	double contrastThreshold = 0.04, double edgeThreshold = 10,
	double sigma = 1.6)
{
	return new TSIFT(nfeatures, nOctaveLayers,
		contrastThreshold, edgeThreshold, sigma);
};

void ICLASS_API ReleaseSIFT(TSIFT* ex)
{
	delete ex;
};

TBFMatcher* ICLASS_API CreateBFMatcher(int normType = NORM_L2, BOOL crossCheck = false)
{
	return new TBFMatcher(normType, crossCheck);
};

void ICLASS_API ReleaseBFMatcher(TBFMatcher* ex)
{
	delete ex;
};

// Draws matches of keypints from two images on output image.
void ICLASS_API DrawMatches(
	TMat* img1, TCVectorKeyPoint* keypoints1,
	TMat* img2, TCVectorKeyPoint* keypoints2,
	TCVectorDMatch* matches1to2, TMat** outImg)
{
	vector<KeyPoint> k1, k2;	
	for (size_t i = 0; i < keypoints1->size(); i++)
	{
		TKeyPoint K = *keypoints1->at(i);
		k1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
	}
	for (size_t i = 0; i < keypoints2->size(); i++)
	{
		TKeyPoint K = *keypoints2->at(i);
		k2.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
	}
	
	vector<DMatch> m1to2;
	for (size_t i = 0; i < matches1to2->size(); i++)
	{
		TDMatch K = *matches1to2->at(i);
		m1to2.push_back(DMatch(K.queryIdx, K.trainIdx, K.imgIdx, K.distance));
	}
	
	Mat oImg;
	drawMatches(*img1->Mat(), k1, *img2->Mat(), k2, m1to2, oImg);

	*outImg = new TMat(oImg);
};
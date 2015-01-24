
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

TSURF* ICLASS_API CreateSURF(double hessianThreshold,
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
//----------------------------------------------------
TBRISK* ICLASS_API CreateBRISK(int thresh = 30, int octaves = 3, float patternScale = 1.0f)
{
	return new TBRISK(thresh, octaves, patternScale);

};

void ICLASS_API ReleaseBRISK(TBRISK* ex)
{
	delete ex;
};

TORB* ICLASS_API CreateORB(int nfeatures = 500, float scaleFactor = 1.2f, int nlevels = 8, int edgeThreshold = 31,
	int firstLevel = 0, int WTA_K = 2, int scoreType = ORB::HARRIS_SCORE, int patchSize = 31)
{
	return new TORB(nfeatures, scaleFactor, nlevels, edgeThreshold,
		firstLevel, WTA_K, scoreType, patchSize);
};

void ICLASS_API ReleaseORB(TORB* ex)
{
	delete ex;
};

TFREAK* ICLASS_API CreateFREAK(BOOL orientationNormalized = true,
	BOOL scaleNormalized = true,
	float patternScale = 22.0f,
	int nOctaves = 4)
{
	return new TFREAK(orientationNormalized, scaleNormalized, patternScale, nOctaves);
};

void ICLASS_API ReleaseFREAK(TFREAK* ex)
{
	delete ex;
};

TMSER* ICLASS_API CreateMSER(int _delta = 5, int _min_area = 60, int _max_area = 14400,
	double _max_variation = 0.25, double _min_diversity = .2,
	int _max_evolution = 200, double _area_threshold = 1.01,
	double _min_margin = 0.003, int _edge_blur_size = 5)
{
	return new TMSER(_delta, _min_area, _max_area, _max_variation, _min_diversity,
		_max_evolution, _area_threshold,
		_min_margin, _edge_blur_size);
};

void ICLASS_API ReleaseMSER(TMSER* ex)
{
	delete ex;
};

TStarDetector* ICLASS_API CreateStarDetector(int _maxSize = 45, int _responseThreshold = 30,
	int _lineThresholdProjected = 10,
	int _lineThresholdBinarized = 8,
	int _suppressNonmaxSize = 5)
{
	return new TStarDetector(_maxSize, _responseThreshold,
		_lineThresholdProjected, _lineThresholdBinarized, _suppressNonmaxSize);
};

void ICLASS_API ReleaseStarDetector(TStarDetector* ex)
{
	delete ex;
};

TFastFeatureDetector* ICLASS_API CreateFastFeatureDetector(int threshold = 10, BOOL nonmaxSuppression = true)
{
	return new TFastFeatureDetector(threshold, nonmaxSuppression);
};

void ICLASS_API ReleaseFastFeatureDetector(TFastFeatureDetector* ex)
{
	delete ex;
};

TGFTTDetector* ICLASS_API CreateGFTTDetector(int maxCorners = 1000, double qualityLevel = 0.01, double minDistance = 1,
	int blockSize = 3, BOOL useHarrisDetector = false, double k = 0.04)
{
	return new TGFTTDetector(maxCorners, qualityLevel, minDistance,
		blockSize, useHarrisDetector, k);
};

void ICLASS_API ReleaseGFTTDetector(TGFTTDetector* ex)
{
	delete ex;
};

TSimpleBlobDetector* ICLASS_API CreateSimpleBlobDetector(TSimpleBlobDetectorParams SimpleBlobDetectorParams)
{
	SimpleBlobDetector::Params parameters;
	parameters.thresholdStep = SimpleBlobDetectorParams.thresholdStep;
	parameters.minThreshold = SimpleBlobDetectorParams.minThreshold;
	parameters.maxThreshold = SimpleBlobDetectorParams.maxThreshold;
	parameters.minRepeatability = SimpleBlobDetectorParams.minRepeatability;
	parameters.minDistBetweenBlobs = SimpleBlobDetectorParams.minDistBetweenBlobs;
	parameters.filterByColor = SimpleBlobDetectorParams.filterByColor;
	parameters.blobColor = SimpleBlobDetectorParams.blobColor;
	parameters.filterByArea = SimpleBlobDetectorParams.filterByArea;
	parameters.minArea = SimpleBlobDetectorParams.minArea;
	parameters.maxArea = SimpleBlobDetectorParams.maxArea;
	parameters.filterByCircularity = SimpleBlobDetectorParams.filterByCircularity;
	parameters.minCircularity = SimpleBlobDetectorParams.minCircularity;
	parameters.maxCircularity = SimpleBlobDetectorParams.maxCircularity;
	parameters.filterByInertia = SimpleBlobDetectorParams.filterByInertia;
	parameters.minInertiaRatio = SimpleBlobDetectorParams.minInertiaRatio;
	parameters.maxInertiaRatio = SimpleBlobDetectorParams.maxInertiaRatio;
	parameters.filterByConvexity = SimpleBlobDetectorParams.filterByConvexity;
	parameters.minConvexity = SimpleBlobDetectorParams.minConvexity;
	parameters.maxConvexity = SimpleBlobDetectorParams.maxConvexity;
	return new TSimpleBlobDetector(parameters);
};

TSimpleBlobDetector* ICLASS_API CreateSimpleBlobDetectorDefault()
{	
	return new TSimpleBlobDetector();
};

void ICLASS_API ReleaseSimpleBlobDetector(TSimpleBlobDetector* ex)
{
	delete ex;
};

TDenseFeatureDetector* ICLASS_API CreateDenseFeatureDetector(float initFeatureScale = 1.f, int featureScaleLevels = 1,
	float featureScaleMul = 0.1f,
	int initXyStep = 6, int initImgBound = 0,
	BOOL varyXyStepWithScale = true,
	BOOL varyImgBoundWithScale = false)
{
	return new TDenseFeatureDetector(initFeatureScale, featureScaleLevels,
		featureScaleMul, initXyStep, initImgBound, varyXyStepWithScale, varyImgBoundWithScale);
};

void ICLASS_API ReleaseDenseFeatureDetector(TDenseFeatureDetector* ex)
{
	delete ex;
};

TBriefDescriptorExtractor* ICLASS_API CreateBriefDescriptorExtractor(int bytes = 32)
{
	return new TBriefDescriptorExtractor(bytes);
};

void ICLASS_API ReleaseBriefDescriptorExtractor(TBriefDescriptorExtractor* ex)
{
	delete ex;
};

TFlannBasedMatcher* ICLASS_API CreateFlannBasedMatcher()
{
	return new TFlannBasedMatcher();
};

void ICLASS_API ReleaseFlannBasedMatcher(TFlannBasedMatcher* ex)
{
	delete ex;
};

//----------------------------------------------------

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
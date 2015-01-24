#pragma once

#include "stdafx.h"
#include "ocvexport.h"
#include "ocvclasses.h"
#include "dnaclasses.h"

ICLASS_EXPORT TMat* ICLASS_API CreateMat();
ICLASS_EXPORT TMat* ICLASS_API CreateMatRCT(int rows, int cols, int type);
ICLASS_EXPORT TMat* ICLASS_API CreateMatFromImage(IplImage* m);
ICLASS_EXPORT void ICLASS_API ReleaseMat(TMat * ex);
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCapture();
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCaptureDevice(int device);
ICLASS_EXPORT TVideoCapture* ICLASS_API CreateVideoCaptureFileName(const char* filename);
ICLASS_EXPORT void ICLASS_API ReleaseVideoCapture(TVideoCapture * ex);
ICLASS_EXPORT TCvKNearest* ICLASS_API CreateCvKNearest();
ICLASS_EXPORT TCvKNearest* ICLASS_API CreateCvKNearestTR(CvMat* _trainData, CvMat* _responses, CvMat* _sampleIdx, BOOL _isRegression, int _max_k);
ICLASS_EXPORT void ICLASS_API ReleaseCvKNearest(TCvKNearest * ex);
ICLASS_EXPORT TVec3d* ICLASS_API CreateVec3d();
ICLASS_EXPORT TVec3d* ICLASS_API CreateVec3d3(double v0, double v1, double v2);
ICLASS_EXPORT void ICLASS_API ReleaseVec3d(TVec3d * ex);
ICLASS_EXPORT TVec3b* ICLASS_API CreateVec3b();
ICLASS_EXPORT TVec3b* ICLASS_API CreateVec3b3(uchar v0, uchar v1, uchar v2);
ICLASS_EXPORT void ICLASS_API ReleaseVec3b(TVec3d * ex);
ICLASS_EXPORT void ICLASS_API ReleasePoint2i(TPoint2i * ex);
ICLASS_EXPORT void ICLASS_API ReleaseVectorOfPoint2i(TVectorOfPoint2i * ex);
ICLASS_EXPORT void ICLASS_API MSER(TMat m, TVectorOfVectorOfPoint2i ** ex);

ICLASS_EXPORT TCVectorRect* ICLASS_API CreateCVectorRect();
ICLASS_EXPORT void ICLASS_API ReleaseCVectorRect(TCVectorRect* ex);
ICLASS_EXPORT TCVectorInt* ICLASS_API CreateCVectorInt();
ICLASS_EXPORT void ICLASS_API ReleaseCVectorInt(TCVectorInt* ex);
ICLASS_EXPORT TCVectorDouble* ICLASS_API CreateCVectorDouble();
ICLASS_EXPORT void ICLASS_API ReleaseCVectorDouble(TCVectorDouble* ex);

ICLASS_EXPORT TCascadeClassifier* ICLASS_API CreateCascadeClassifier();
ICLASS_EXPORT TCascadeClassifier* ICLASS_API CreateCascadeClassifierFromFile(const char* filename);
ICLASS_EXPORT void ICLASS_API ReleaseCascadeClassifier(TCascadeClassifier* ex);

ICLASS_EXPORT TSURF* ICLASS_API CreateSURF(double hessianThreshold, int nOctaves, int nOctaveLayers, BOOL extended, BOOL upright);
ICLASS_EXPORT void ICLASS_API ReleaseSURF(TSURF* ex);
ICLASS_EXPORT TCVectorKeyPoint* ICLASS_API CreateCVectorKeyPoint();
ICLASS_EXPORT void ICLASS_API ReleaseCVectorKeyPoint(TCVectorKeyPoint* ex);
ICLASS_EXPORT TCVectorDMatch* ICLASS_API CreateCVectorDMatch();
ICLASS_EXPORT void ICLASS_API ReleaseCVectorDMatch(TCVectorDMatch* ex);
ICLASS_EXPORT TBFMatcher* ICLASS_API CreateBFMatcher(int normType = NORM_L2, BOOL crossCheck = false);
ICLASS_EXPORT void ICLASS_API ReleaseBFMatcher(TBFMatcher* ex);
ICLASS_EXPORT void ICLASS_API DrawMatches(
	TMat* img1, TCVectorKeyPoint* keypoints1,
	TMat* img2, TCVectorKeyPoint* keypoints2,
	TCVectorDMatch* matches1to2, TMat** outImg);
ICLASS_EXPORT TSIFT* ICLASS_API CreateSIFT(int nfeatures = 0, int nOctaveLayers = 3,
	double contrastThreshold = 0.04, double edgeThreshold = 10,
	double sigma = 1.6);
ICLASS_EXPORT void ICLASS_API ReleaseSIFT(TSIFT* ex);

ICLASS_EXPORT TBRISK* ICLASS_API CreateBRISK(int thresh = 30, int octaves = 3, float patternScale = 1.0f);
ICLASS_EXPORT void ICLASS_API ReleaseBRISK(TBRISK* ex);
ICLASS_EXPORT TORB* ICLASS_API CreateORB(int nfeatures = 500, float scaleFactor = 1.2f, int nlevels = 8, int edgeThreshold = 31,
	int firstLevel = 0, int WTA_K = 2, int scoreType = ORB::HARRIS_SCORE, int patchSize = 31);
ICLASS_EXPORT void ICLASS_API ReleaseORB(TORB* ex);
ICLASS_EXPORT TFREAK* ICLASS_API CreateFREAK(BOOL orientationNormalized = true,
	BOOL scaleNormalized = true,
	float patternScale = 22.0f,
	int nOctaves = 4);
ICLASS_EXPORT void ICLASS_API ReleaseFREAK(TFREAK* ex);
ICLASS_EXPORT TMSER* ICLASS_API CreateMSER(int _delta = 5, int _min_area = 60, int _max_area = 14400,
	double _max_variation = 0.25, double _min_diversity = .2,
	int _max_evolution = 200, double _area_threshold = 1.01,
	double _min_margin = 0.003, int _edge_blur_size = 5);
ICLASS_EXPORT void ICLASS_API ReleaseMSER(TMSER* ex);
ICLASS_EXPORT TStarDetector* ICLASS_API CreateStarDetector(int _maxSize = 45, int _responseThreshold = 30,
	int _lineThresholdProjected = 10,
	int _lineThresholdBinarized = 8,
	int _suppressNonmaxSize = 5);
ICLASS_EXPORT void ICLASS_API ReleaseStarDetector(TStarDetector* ex);
ICLASS_EXPORT TFastFeatureDetector* ICLASS_API CreateFastFeatureDetector(int threshold = 10, BOOL nonmaxSuppression = true);
ICLASS_EXPORT void ICLASS_API ReleaseFastFeatureDetector(TFastFeatureDetector* ex);
ICLASS_EXPORT TGFTTDetector* ICLASS_API CreateGFTTDetector(int maxCorners = 1000, double qualityLevel = 0.01, double minDistance = 1,
	int blockSize = 3, BOOL useHarrisDetector = false, double k = 0.04);
ICLASS_EXPORT void ICLASS_API ReleaseGFTTDetector(TGFTTDetector* ex);
ICLASS_EXPORT TSimpleBlobDetector* ICLASS_API CreateSimpleBlobDetector(TSimpleBlobDetectorParams SimpleBlobDetectorParams);
ICLASS_EXPORT TSimpleBlobDetector* ICLASS_API CreateSimpleBlobDetectorDefault();
ICLASS_EXPORT void ICLASS_API ReleaseSimpleBlobDetector(TSimpleBlobDetector* ex);
ICLASS_EXPORT TDenseFeatureDetector* ICLASS_API CreateDenseFeatureDetector(float initFeatureScale = 1.f, int featureScaleLevels = 1,
	float featureScaleMul = 0.1f,
	int initXyStep = 6, int initImgBound = 0,
	BOOL varyXyStepWithScale = true,
	BOOL varyImgBoundWithScale = false);
ICLASS_EXPORT void ICLASS_API ReleaseDenseFeatureDetector(TDenseFeatureDetector* ex);
ICLASS_EXPORT TBriefDescriptorExtractor* ICLASS_API CreateBriefDescriptorExtractor(int bytes = 32);
ICLASS_EXPORT void ICLASS_API ReleaseBriefDescriptorExtractor(TBriefDescriptorExtractor* ex);
ICLASS_EXPORT TFlannBasedMatcher* ICLASS_API CreateFlannBasedMatcher();
ICLASS_EXPORT void ICLASS_API ReleaseFlannBasedMatcher(TFlannBasedMatcher* ex);
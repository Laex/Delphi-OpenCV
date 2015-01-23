#pragma once

#include "stdafx.h"
#include <vector>
#include "ocvexport.h"
#include "dnaclasses.h"
#include "opencv2\core\core.hpp"
#include "opencv2\core\core_c.h"
#include "opencv2\core\mat.hpp"
#include "opencv2\highgui\highgui.hpp"
#include "opencv2\ml\ml.hpp"
#include "opencv2\features2d\features2d.hpp"
#include "opencv2\nonfree\features2d.hpp"
#include "opencv2\objdetect\objdetect.hpp"

using namespace cv;
using namespace std;

//////////////////////////////////////////////////////////////////////
class OCV_CLASS_EXPORT TMat
{
protected:
	cv::Mat FMat;

public:
	TMat() : FMat(){}
	TMat(int rows, int cols, int type) : FMat(rows, cols, type){}
	TMat(cv::Mat& m) : FMat(m) { }
	TMat(IplImage* m) : FMat(m) { }
	~TMat(){}

	virtual size_t ICLASS_API elemSize() { return FMat.elemSize(); };
	virtual size_t ICLASS_API elemSize1() { return FMat.elemSize1(); };
	virtual int ICLASS_API _type() { return FMat.type(); };
	virtual int ICLASS_API depth() { return FMat.depth(); };
	virtual int ICLASS_API channels() { return FMat.channels(); };
	virtual size_t ICLASS_API step1(int i) { return FMat.step1(); };
	virtual BOOL ICLASS_API empty() { return FMat.empty(); };
	virtual size_t ICLASS_API total() { return FMat.total(); };
	virtual int ICLASS_API flags() { return FMat.flags; };
	virtual int ICLASS_API dims() { return FMat.dims; };
	virtual int ICLASS_API rows()  { return FMat.rows; };
	virtual int ICLASS_API cols()  { return FMat.cols; };
	virtual uchar* ICLASS_API data()  { return FMat.data; };
	//! copies the matrix content to "m".
	// It calls m.create(this->size(), this->type()).
	virtual void ICLASS_API copyTo(TMat** m)
	{
		cv::Mat mm = cv::Mat();
		FMat.copyTo(mm);
		*m = new TMat(mm);
	}
	virtual cv::Mat* ICLASS_API Mat()  { return &FMat; };
};

//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TVideoCapture
{

private:
	cv::VideoCapture FVideoCapture;

public:

	TVideoCapture() : FVideoCapture(){};
	TVideoCapture(int device) : FVideoCapture(device){ };
	TVideoCapture(const char* filename) : FVideoCapture(cv::string(filename)){ };
	~TVideoCapture() { };

	virtual BOOL ICLASS_API open(int device) { return FVideoCapture.open(device); };
	virtual BOOL ICLASS_API openfile(const char* filename) { return FVideoCapture.open(cv::string(filename)); };
	virtual BOOL ICLASS_API isOpened() { return FVideoCapture.isOpened(); };
	virtual void ICLASS_API release() { FVideoCapture.release(); };
	virtual BOOL ICLASS_API grab() { return FVideoCapture.grab(); };
	virtual BOOL ICLASS_API retrieve(TMat** image, int flag)
	{
		cv::Mat m = cv::Mat();
		BOOL result = FVideoCapture.retrieve(m, flag);
		if (result) *image = new TMat(m);
		return result;
	};
	virtual BOOL ICLASS_API read(TMat** image)
	{
		cv::Mat m = cv::Mat();
		BOOL result = FVideoCapture.read(m);
		if (result) *image = new TMat(m);
		return result;
	};
	virtual BOOL ICLASS_API setvalue(int propId, double value) { return FVideoCapture.set(propId, value); };
	virtual double ICLASS_API getvalue(int propId) { return FVideoCapture.get(propId); };
};

//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TCvKNearest
{
public:
	TCvKNearest() : FCvKNearest() {}
	TCvKNearest(const CvMat* _trainData, const CvMat* _responses,
		const CvMat* _sampleIdx = 0, BOOL _isRegression = FALSE, int _max_k = 32) : FCvKNearest(_trainData, _responses, _sampleIdx, (bool)_isRegression, _max_k) {}
	~TCvKNearest() { }

	// wrapping methods
	virtual BOOL ICLASS_API train(const CvMat* trainData, const CvMat* responses,
		const CvMat* sampleIdx = 0, BOOL is_regression = false,
		int maxK = 32, BOOL updateBase = false)
	{
		return FCvKNearest.train(trainData, responses, sampleIdx, is_regression, maxK, updateBase);
	};

	virtual float ICLASS_API find_nearest(const CvMat* samples, int k, CV_OUT CvMat* results = 0,
		const float** neighbors = 0, CV_OUT CvMat* neighborResponses = 0, CV_OUT CvMat* dist = 0)
	{
		return FCvKNearest.find_nearest(samples, k, results, neighbors, neighborResponses, dist);
	};

private:
	CvKNearest FCvKNearest;
};

//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TVec3d
{
public:
	TVec3d() : FVec3d() {}
	TVec3d(double v0, double v1, double v2) : FVec3d(v0, v1, v2) {} //!< 3-element vector constructor	
	~TVec3d() { }

private:
	cv::Vec3d FVec3d;
};

//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TVec3b
{
public:
	TVec3b() : FVec3b() {}
	TVec3b(uchar v0, uchar v1, uchar v2) : FVec3b(v0, v1, v2) {} //!< 3-element vector constructor	
	~TVec3b() { }

private:
	cv::Vec3b FVec3b;
};

//////////////////////////////////////////////////////////////////////
class OCV_CLASS_EXPORT TPoint2i
{
public:
	TPoint2i() : FPoint() {}
	TPoint2i(int _x, int _y) : FPoint(_x, _y) {}
	TPoint2i(cv::Point Point) : FPoint(Point) {}
	~TPoint2i() { }
	virtual int ICLASS_API getX() { return FPoint.x; };
	virtual void ICLASS_API setX(int x) { FPoint.x = x; };
	virtual int ICLASS_API getY() { return FPoint.y; };
	virtual void ICLASS_API setY(int y) { FPoint.y = y; };
private:
	cv::Point FPoint;
};
//////////////////////////////////////////////////////////////////////

//TODO: Create template

class OCV_CLASS_EXPORT TVectorOfPoint2i
{
public:
	TVectorOfPoint2i() : FVectorOfPoint2i() {}
	TVectorOfPoint2i(int _Count) : FVectorOfPoint2i(_Count) {}
	TVectorOfPoint2i(std::vector<cv::Point> v) : FVectorOfPoint2i()
	{
		for (size_t i = 0; i < (int)v.size(); i++)
		{
			FVectorOfPoint2i.push_back(TPoint2i(v[i]));
		}
	}
	~TVectorOfPoint2i() { }
private:
	std::vector<TPoint2i> FVectorOfPoint2i;
};
//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TVectorOfVectorOfPoint2i
{
public:
	TVectorOfVectorOfPoint2i() : FVectorOfVectorOfPoint2i() {};
	TVectorOfVectorOfPoint2i(int _Count) : FVectorOfVectorOfPoint2i(_Count) {};
	TVectorOfVectorOfPoint2i(std::vector<TVectorOfPoint2i> v) : FVectorOfVectorOfPoint2i(v) {};
	~TVectorOfVectorOfPoint2i() { };
	virtual int ICLASS_API size() { return FVectorOfVectorOfPoint2i.size(); };
	virtual void ICLASS_API push_back(TVectorOfPoint2i _Val) { FVectorOfVectorOfPoint2i.push_back(_Val); };
private:
	std::vector<TVectorOfPoint2i> FVectorOfVectorOfPoint2i;
};
//////////////////////////////////////////////////////////////////////

class OCV_CLASS_EXPORT TCascadeClassifier
{
private:
	cv::CascadeClassifier FCascadeClassifier;
public:
	TCascadeClassifier() : FCascadeClassifier() {};
	TCascadeClassifier(const char* filename) : FCascadeClassifier(cv::string(filename)){ };
	~TCascadeClassifier(){};

	virtual bool ICLASS_API empty() { return FCascadeClassifier.empty(); };
	virtual bool ICLASS_API load(const char* filename) { return FCascadeClassifier.load(string(filename)); };
	/*
	virtual void ICLASS_API detectMultiScale(const Mat& image,
	CV_OUT vector<Rect>& objects,
	double scaleFactor = 1.1,
	int minNeighbors = 3, int flags = 0,
	Size minSize = Size(),
	Size maxSize = Size());
	*/
	virtual void ICLASS_API detectMultiScale(TMat* image, TCVectorRect* objects,
		double scaleFactor = 1.1,
		int minNeighbors = 3,
		int flags = 0,
		CvSize minSize = CvSize(),
		CvSize maxSize = CvSize())
	{
		vector<Rect> o;
		FCascadeClassifier.detectMultiScale(*image->Mat(), o, scaleFactor, minNeighbors, flags, Size(minSize), Size(maxSize));
		for (size_t i = 0; i < o.size(); i++)
		{
			Rect R = o[i];
			objects->Vector()->push_back(CRect(R.x, R.y, R.width, R.height));
		}
	};

	/*
	CV_WRAP virtual void detectMultiScale(const Mat& image,
	CV_OUT vector<Rect>& objects,
	vector<int>& rejectLevels,
	vector<double>& levelWeights,
	double scaleFactor = 1.1,
	int minNeighbors = 3, int flags = 0,
	Size minSize = Size(),
	Size maxSize = Size(),
	bool outputRejectLevels = false);
	*/

	virtual void ICLASS_API detectMultiScaleLevel(TMat* image,
		TCVectorRect* objects,
		TCVectorInt* rejectLevels,
		TCVectorDouble* levelWeights,
		double scaleFactor = 1.1,
		int minNeighbors = 3, int flags = 0,
		CvSize minSize = CvSize(),
		CvSize maxSize = CvSize(),
		bool outputRejectLevels = false)
	{
		vector<Rect> o;
		FCascadeClassifier.detectMultiScale(*image->Mat(), o, *rejectLevels->Vector(), *levelWeights->Vector(),
			scaleFactor, minNeighbors, flags, Size(minSize), Size(maxSize), outputRejectLevels);
		for (size_t i = 0; i < o.size(); i++)
		{
			Rect R = o[i];
			objects->Vector()->push_back(CRect(R.x, R.y, R.width, R.height));
		}

	};
	virtual bool ICLASS_API isOldFormatCascade() { return FCascadeClassifier.isOldFormatCascade(); };
	/*
	virtual CvSize ICLASS_API getOriginalWindowSize()
	{
	CvSize R = FCascadeClassifier.getOriginalWindowSize();
	return R;
	};
	virtual int ICLASS_API getFeatureType() { return FCascadeClassifier.getFeatureType(); };
	virtual bool ICLASS_API setImage(TMat m) { return FCascadeClassifier.setImage(*m.Mat()); };
	*/
};

class OCV_CLASS_EXPORT TSURF
{
private:
	SURF FSURF;
public:
	TSURF() : FSURF() {};
	TSURF(double hessianThreshold,
		int nOctaves = 4, int nOctaveLayers = 2,
		BOOL extended = true, BOOL upright = false) : FSURF(hessianThreshold, nOctaves, nOctaveLayers, extended, upright) {};
	~TSURF(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			FSURF.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			FSURF.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
	virtual void ICLASS_API compute(TMat* image, TCVectorKeyPoint* keypoints, TMat** descriptors)
	{
		vector<KeyPoint> keypoints1;
		for (size_t i = 0; i < keypoints->size(); i++)
		{
			TKeyPoint K = *keypoints->at(i);
			keypoints1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
		};
		Mat m;
		FSURF.compute(*image->Mat(), keypoints1, m);
		*descriptors = new TMat(m);
	};
};


class OCV_CLASS_EXPORT TBFMatcher
{
private:
	BFMatcher FBFMatcher;
public:
	TBFMatcher() : FBFMatcher() {};
	TBFMatcher(int normType = NORM_L2, BOOL crossCheck = false) : FBFMatcher(normType, crossCheck) {};
	~TBFMatcher(){};
	virtual void ICLASS_API match(TMat* queryDescriptors, TMat* trainDescriptors,
		CV_OUT TCVectorDMatch* matches, TMat* mask = NULL)
	{
		vector<DMatch> m;
		if (mask)
			FBFMatcher.match(*queryDescriptors->Mat(), *trainDescriptors->Mat(), m, *mask->Mat()); else
			FBFMatcher.match(*queryDescriptors->Mat(), *trainDescriptors->Mat(), m);
		for (size_t i = 0; i < m.size(); i++)
		{
			matches->push_back(CDMatch(m[i]));
		}
	};
};

class OCV_CLASS_EXPORT TSIFT
{
private:
	SIFT FSIFT;
public:
	TSIFT() : FSIFT() {};
	TSIFT(int nfeatures = 0, int nOctaveLayers = 3,
		double contrastThreshold = 0.04, double edgeThreshold = 10,
		double sigma = 1.6) : FSIFT(nfeatures, nOctaveLayers,
		contrastThreshold, edgeThreshold,sigma) {};
	~TSIFT(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			FSIFT.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			FSIFT.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
	virtual void ICLASS_API compute(TMat* image, TCVectorKeyPoint* keypoints, TMat** descriptors)
	{
		vector<KeyPoint> keypoints1;
		for (size_t i = 0; i < keypoints->size(); i++)
		{
			TKeyPoint K = *keypoints->at(i);
			keypoints1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
		};
		Mat m;
		FSIFT.compute(*image->Mat(), keypoints1, m);
		*descriptors = new TMat(m);
	};
};

class OCV_CLASS_EXPORT TBRISK
{
private:
	BRISK FBRISK;
public:
	TBRISK() : FBRISK() {};
	TBRISK(int thresh = 30, int octaves = 3, float patternScale = 1.0f) : FBRISK(thresh, octaves, patternScale) {};
	~TBRISK(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			FBRISK.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			FBRISK.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
	virtual void ICLASS_API compute(TMat* image, TCVectorKeyPoint* keypoints, TMat** descriptors)
	{
		vector<KeyPoint> keypoints1;
		for (size_t i = 0; i < keypoints->size(); i++)
		{
			TKeyPoint K = *keypoints->at(i);
			keypoints1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
		};
		Mat m;
		FBRISK.compute(*image->Mat(), keypoints1, m);
		*descriptors = new TMat(m);
	};
};

class OCV_CLASS_EXPORT TORB
{
private:
	ORB Detector;
public:
	TORB() : Detector() {};
	TORB(int nfeatures = 500, float scaleFactor = 1.2f, int nlevels = 8, int edgeThreshold = 31,
		int firstLevel = 0, int WTA_K = 2, int scoreType = ORB::HARRIS_SCORE, int patchSize = 31) : Detector(
		nfeatures, scaleFactor, nlevels, edgeThreshold,
		firstLevel, WTA_K, scoreType, patchSize) {};
	~TORB(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
	virtual void ICLASS_API compute(TMat* image, TCVectorKeyPoint* keypoints, TMat** descriptors)
	{
		vector<KeyPoint> keypoints1;
		for (size_t i = 0; i < keypoints->size(); i++)
		{
			TKeyPoint K = *keypoints->at(i);
			keypoints1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
		};
		Mat m;
		Detector.compute(*image->Mat(), keypoints1, m);
		*descriptors = new TMat(m);
	};
};

class OCV_CLASS_EXPORT TFREAK
{
private:
	FREAK Detector;
public:
	TFREAK() : Detector() {};
	TFREAK(BOOL orientationNormalized = true,
		BOOL scaleNormalized = true,
		float patternScale = 22.0f,
		int nOctaves = 4,
		const vector<int>& selectedPairs = vector<int>()) : Detector(orientationNormalized, scaleNormalized, patternScale, nOctaves, selectedPairs) {};
	~TFREAK(){};

	virtual void ICLASS_API compute(TMat* image, TCVectorKeyPoint* keypoints, TMat** descriptors)
	{
		vector<KeyPoint> keypoints1;
		for (size_t i = 0; i < keypoints->size(); i++)
		{
			TKeyPoint K = *keypoints->at(i);
			keypoints1.push_back(KeyPoint(K.x, K.y, K.size, K.angle, K.response, K.octave, K.class_id));
		};
		Mat m;
		Detector.compute(*image->Mat(), keypoints1, m);
		*descriptors = new TMat(m);
	};
};

class OCV_CLASS_EXPORT TMSER
{
private:
	cv::MSER Detector;
public:
	TMSER() : Detector() {};
	TMSER(int _delta = 5, int _min_area = 60, int _max_area = 14400,
		double _max_variation = 0.25, double _min_diversity = .2,
		int _max_evolution = 200, double _area_threshold = 1.01,
		double _min_margin = 0.003, int _edge_blur_size = 5) : Detector(
		_delta, _min_area, _max_area ,_max_variation, _min_diversity,
		_max_evolution , _area_threshold,
		_min_margin , _edge_blur_size) {};
	~TMSER(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};

class OCV_CLASS_EXPORT TStarDetector
{
private:
	StarDetector Detector;
public:
	TStarDetector() : Detector() {};
	TStarDetector(int _maxSize = 45, int _responseThreshold = 30,
		int _lineThresholdProjected = 10,
		int _lineThresholdBinarized = 8,
		int _suppressNonmaxSize = 5) : Detector(_maxSize, _responseThreshold,
		_lineThresholdProjected,_lineThresholdBinarized,_suppressNonmaxSize) {};
	~TStarDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};

class OCV_CLASS_EXPORT TFastFeatureDetector
{
private:
	FastFeatureDetector Detector;
public:
	TFastFeatureDetector() : Detector() {};
	TFastFeatureDetector(int threshold = 10, BOOL nonmaxSuppression = true) : Detector(threshold, nonmaxSuppression) {};
	~TFastFeatureDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};

};

class OCV_CLASS_EXPORT TGFTTDetector
{
private:
	GFTTDetector Detector;
public:
	TGFTTDetector(int maxCorners = 1000, double qualityLevel = 0.01, double minDistance = 1,
		int blockSize = 3, BOOL useHarrisDetector = false, double k = 0.04) : Detector(maxCorners, qualityLevel, minDistance,
		blockSize, useHarrisDetector, k) {};
	~TGFTTDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};

};

class OCV_CLASS_EXPORT TSimpleBlobDetector
{
private:
	SimpleBlobDetector Detector;
public:	
	TSimpleBlobDetector(SimpleBlobDetector::Params &parameters = SimpleBlobDetector::Params()) : Detector(parameters) {};
	~TSimpleBlobDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};

class OCV_CLASS_EXPORT TDenseFeatureDetector
{
private:
	DenseFeatureDetector Detector;
public:	
	TDenseFeatureDetector(float initFeatureScale = 1.f, int featureScaleLevels = 1,
		float featureScaleMul = 0.1f,
		int initXyStep = 6, int initImgBound = 0,
		BOOL varyXyStepWithScale = true,
		BOOL varyImgBoundWithScale = false) : Detector(initFeatureScale, featureScaleLevels,
		featureScaleMul,initXyStep, initImgBound,varyXyStepWithScale,varyImgBoundWithScale) {};
	~TDenseFeatureDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};

class OCV_CLASS_EXPORT TGridAdaptedFeatureDetector
{
private:
	GridAdaptedFeatureDetector Detector;
public:	
	TGridAdaptedFeatureDetector(const Ptr<FeatureDetector>& detector = 0,
		int maxTotalKeypoints = 1000,
		int gridRows = 4, int gridCols = 4) : Detector(detector,maxTotalKeypoints,gridRows, gridCols) {};
	~TGridAdaptedFeatureDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};

class OCV_CLASS_EXPORT TPyramidAdaptedFeatureDetector
{
private:
	PyramidAdaptedFeatureDetector Detector;
public:
	TPyramidAdaptedFeatureDetector(const Ptr<FeatureDetector>& detector, int maxLevel = 2) : Detector(detector, maxLevel) {};
	~TPyramidAdaptedFeatureDetector(){};

	virtual void ICLASS_API detect(TMat* image, CV_OUT TCVectorKeyPoint* keypoints, TMat* mask = NULL)
	{
		vector<KeyPoint> keypoints1;
		if (mask)
			Detector.detect(*image->Mat(), keypoints1, *mask->Mat()); else
			Detector.detect(*image->Mat(), keypoints1);
		for (size_t i = 0; i < keypoints1.size(); i++)
		{
			keypoints->Vector()->push_back(CKeyPoint(keypoints1[i]));
		}
	};
};
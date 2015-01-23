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
		for (int i = 0; i < (int)v.size(); i++)
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
		for (int i = 0; i < o.size(); i++)
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
		for (int i = 0; i < o.size(); i++)
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
	SurfFeatureDetector FSURF;
public:
	TSURF() : FSURF() {};
	TSURF(double hessianThreshold,
		int nOctaves = 4, int nOctaveLayers = 2,
		BOOL extended = true, BOOL upright = false) : FSURF(hessianThreshold, nOctaves, nOctaveLayers, extended, upright) {};
	~TSURF(){};
};
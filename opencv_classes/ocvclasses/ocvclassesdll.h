#pragma once

#include "stdafx.h"
#include "opencv2\core\core.hpp"
#include "opencv2\core\mat.hpp"
#include "opencv2\highgui\highgui.hpp"
#include "opencv2\ml\ml.hpp"
#include "opencv2\features2d\features2d.hpp"

//////////////////////////////////////////////////////////////////////
class AFX_CLASS_EXPORT TMat
{
protected:
	cv::Mat FMat;

public:
	TMat() : FMat(){}
	TMat(int rows, int cols, int type) : FMat(rows, cols, type){}
	TMat(cv::Mat& m) : FMat(m) { }
	~TMat(){}

	virtual size_t __stdcall elemSize() { return FMat.elemSize(); };
	virtual size_t __stdcall elemSize1() { return FMat.elemSize1(); };
	virtual int __stdcall _type() { return FMat.type(); };
	virtual int __stdcall depth() { return FMat.depth(); };
	virtual int __stdcall channels() { return FMat.channels(); };
	virtual size_t __stdcall step1(int i) { return FMat.step1(); };
	virtual BOOL __stdcall empty() { return FMat.empty(); };
	virtual size_t __stdcall total() { return FMat.total(); };
	virtual int __stdcall flags() { return FMat.flags; };
	virtual int __stdcall dims() { return FMat.dims; };
	virtual int __stdcall rows()  { return FMat.rows; };
	virtual int __stdcall cols()  { return FMat.cols; };
	virtual uchar* __stdcall data()  { return FMat.data; };
	//! copies the matrix content to "m".
	// It calls m.create(this->size(), this->type()).
	virtual void __stdcall copyTo(TMat** m) 
	{ 
		cv::Mat mm = cv::Mat();
		FMat.copyTo(mm);
		*m = new TMat(mm);
	}
	virtual cv::Mat* __stdcall Mat()  { return &FMat; };
};

//////////////////////////////////////////////////////////////////////

class AFX_CLASS_EXPORT TVideoCapture
{

private:
	cv::VideoCapture FVideoCapture;

public:

	TVideoCapture() : FVideoCapture(){}
	TVideoCapture(int device) : FVideoCapture(device){ }
	TVideoCapture(const char* filename) : FVideoCapture(cv::string(filename)){ }
	~TVideoCapture() { }

	virtual BOOL __stdcall open(int device) { return FVideoCapture.open(device); };
	virtual BOOL __stdcall openfile(const char* filename) { return FVideoCapture.open(cv::string(filename)); };
	virtual BOOL __stdcall isOpened() { return FVideoCapture.isOpened(); };
	virtual void __stdcall release() { FVideoCapture.release(); };
	virtual BOOL __stdcall grab() { return FVideoCapture.grab(); };
	virtual BOOL __stdcall retrieve(TMat** image, int flag) 
	{
		cv::Mat m = cv::Mat();
		BOOL result = FVideoCapture.retrieve(m, flag);
		if (result) *image = new TMat(m);
		return result;
	};
	virtual BOOL __stdcall read(TMat** image) 
	{		
		cv::Mat m = cv::Mat();
		BOOL result = FVideoCapture.read(m);
		if (result) *image = new TMat(m);
		return result;
	};
	virtual BOOL __stdcall setvalue(int propId, double value) { return FVideoCapture.set(propId, value); };
	virtual double __stdcall getvalue(int propId) { return FVideoCapture.get(propId); };
};

//////////////////////////////////////////////////////////////////////

class AFX_CLASS_EXPORT TCvKNearest
{
public:
	TCvKNearest() : FCvKNearest() {}
	TCvKNearest(const CvMat* _trainData, const CvMat* _responses,
		const CvMat* _sampleIdx = 0, BOOL _isRegression = false, int _max_k = 32) : FCvKNearest(_trainData, _responses, _sampleIdx, (bool)_isRegression, _max_k) {}
	~TCvKNearest() { }

	// wrapping methods
	virtual BOOL __stdcall train(const CvMat* trainData, const CvMat* responses,
		const CvMat* sampleIdx = 0, BOOL is_regression = false,
		int maxK = 32, BOOL updateBase = false)
	{
		return FCvKNearest.train(trainData, responses, sampleIdx, is_regression, maxK, updateBase);
	};

	virtual float __stdcall find_nearest(const CvMat* samples, int k, CV_OUT CvMat* results = 0,
		const float** neighbors = 0, CV_OUT CvMat* neighborResponses = 0, CV_OUT CvMat* dist = 0)
	{
		return FCvKNearest.find_nearest(samples, k, results, neighbors, neighborResponses, dist);
	};

private:
	CvKNearest FCvKNearest;
};

//////////////////////////////////////////////////////////////////////

class AFX_CLASS_EXPORT TVec3d
{
public:
	TVec3d() : FVec3d() {}
	TVec3d(double v0, double v1, double v2) : FVec3d(v0, v1, v2) {} //!< 3-element vector constructor	
	~TVec3d() { }	

private:
	cv::Vec3d FVec3d;
};

//////////////////////////////////////////////////////////////////////

class AFX_CLASS_EXPORT TVec3b
{
public:
	TVec3b() : FVec3b() {}
	TVec3b(uchar v0, uchar v1, uchar v2) : FVec3b(v0, v1, v2) {} //!< 3-element vector constructor	
	~TVec3b() { }

private:
	cv::Vec3b FVec3b;
};

//////////////////////////////////////////////////////////////////////
class AFX_CLASS_EXPORT TPoint2i
{
public:
	TPoint2i() : FPoint() {}
	TPoint2i(int _x, int _y) : FPoint(_x, _y) {}
	TPoint2i(cv::Point Point) : FPoint(Point) {}
	~TPoint2i() { }
	virtual int __stdcall getX() { return FPoint.x; };
	virtual void __stdcall setX(int x) { FPoint.x=x; };
	virtual int __stdcall getY() { return FPoint.y; };
	virtual void __stdcall setY(int y) { FPoint.y = y; };
private:
	cv::Point FPoint;
};
//////////////////////////////////////////////////////////////////////

//TODO: Create template

class AFX_CLASS_EXPORT TVectorOfPoint2i
{
public:
	TVectorOfPoint2i() : FVectorOfPoint2i() {}
	TVectorOfPoint2i(int _Count) : FVectorOfPoint2i(_Count) {}
	TVectorOfPoint2i(std::vector<cv::Point> v) : FVectorOfPoint2i()
	{
		for (int i = 0; i < v.size(); i++)
		{
			FVectorOfPoint2i.push_back(TPoint2i(v[i]));
		}
	}
	~TVectorOfPoint2i() { }	
private:
	std::vector<TPoint2i> FVectorOfPoint2i;	
};
//////////////////////////////////////////////////////////////////////

class AFX_CLASS_EXPORT TVectorOfVectorOfPoint2i
{
public:
	TVectorOfVectorOfPoint2i() : FVectorOfVectorOfPoint2i() {}
	TVectorOfVectorOfPoint2i(int _Count) : FVectorOfVectorOfPoint2i(_Count) {}
	TVectorOfVectorOfPoint2i(std::vector<TVectorOfPoint2i> v) : FVectorOfVectorOfPoint2i(v) {}
	~TVectorOfVectorOfPoint2i() { }
	virtual int __stdcall size() { return FVectorOfVectorOfPoint2i.size(); };
	virtual void __stdcall push_back(TVectorOfPoint2i _Val) { FVectorOfVectorOfPoint2i.push_back(_Val); };
private:
	std::vector<TVectorOfPoint2i> FVectorOfVectorOfPoint2i;
};
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////

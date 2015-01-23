
#include "stdafx.h"
#include "dnaclasses.h"
#include "ocvexport.h"
#include "opencv2\features2d\features2d.hpp"

using namespace cv;

TCVectorRect* ICLASS_API CreateCVectorRect()
{
	return new TCVectorRect();
};

void ICLASS_API ReleaseCVectorRect(TCVectorRect* ex)
{
	delete ex;
};

TCRect CRect(int X, int Y, int W, int H)
{
	TCRect R;
	R.x = X;
	R.y = Y;
	R.width = W;
	R.height = H;
	return R;
};

TCVectorInt* ICLASS_API CreateCVectorInt()
{
	return new TCVectorInt();
};

void ICLASS_API ReleaseCVectorInt(TCVectorInt* ex)
{
	delete ex;
};

TCVectorDouble* ICLASS_API CreateCVectorDouble()
{
	return new TCVectorDouble();
};

void ICLASS_API ReleaseCVectorDouble(TCVectorDouble* ex)
{
	delete ex;
};

TKeyPoint CKeyPoint(const cv::KeyPoint k)
{
	TKeyPoint R;
	R.x = k.pt.x;
	R.y = k.pt.y;
	R.angle = k.angle;
	R.octave = k.octave;
	R.response = k.response;
	R.size = k.size;
	R.class_id = k.class_id;
	return R;
};

TCVectorKeyPoint* ICLASS_API CreateCVectorKeyPoint()
{
	return new TCVectorKeyPoint();
};

void ICLASS_API ReleaseCVectorKeyPoint(TCVectorKeyPoint* ex)
{
	delete ex;
};

TCVectorDMatch* ICLASS_API CreateCVectorDMatch()
{
	return new TCVectorDMatch();
};

void ICLASS_API ReleaseCVectorDMatch(TCVectorDMatch* ex)
{
	delete ex;
};

TDMatch CDMatch(const cv::DMatch k)
{
	TDMatch R;
	R.distance = k.distance;
	R.imgIdx = k.imgIdx;
	R.queryIdx = k.queryIdx;
	R.trainIdx = k.trainIdx;
	return R;
};
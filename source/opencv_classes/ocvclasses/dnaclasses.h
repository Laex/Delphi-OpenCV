#pragma once

#include <vector>
#include "stdafx.h"
#include "ocvexport.h"
#include "opencv2\objdetect\objdetect.hpp"
#include "opencv2\features2d\features2d.hpp"

template<typename T> OCV_CLASS_EXPORT class TCVector
{
protected:
	std::vector<T> FVector;

public:
	TCVector() : FVector(){}	
	~TCVector(){}

	virtual size_t ICLASS_API size() { return FVector.size(); };
	virtual void ICLASS_API push_back(const T& Val) { FVector.push_back(Val); };
	virtual T* ICLASS_API at(int i) { return &FVector.at(i); };
	virtual std::vector<T>* ICLASS_API Vector()  { return &FVector; };
};

typedef struct TCRect
{	
	int  x;
	int  y;
	int  width;
	int  height;
} TCRect;

typedef struct TKeyPoint
{
	//Point2f pt; //!< coordinates of the keypoints
	float x, y;
	float size; //!< diameter of the meaningful keypoint neighborhood
	float angle; //!< computed orientation of the keypoint (-1 if not applicable);
	//!< it's in [0,360) degrees and measured relative to
	//!< image coordinate system, ie in clockwise.
	float response; //!< the response by which the most strong keypoints have been selected. Can be used for the further sorting or subsampling
	int octave; //!< octave (pyramid layer) from which the keypoint has been extracted
	int class_id; //!< object class (if the keypoints need to be clustered by an object they belong to)
} TKeyPoint;

typedef struct TDMatch
{
	int queryIdx; // query descriptor index
	int trainIdx; // train descriptor index
	int imgIdx;   // train image index
	float distance;
} TDMatch;

typedef TCVector<TCRect> TCVectorRect;
typedef TCVector<int> TCVectorInt;
typedef TCVector<double> TCVectorDouble;
typedef TCVector<TKeyPoint> TCVectorKeyPoint;
typedef TCVector<TDMatch> TCVectorDMatch;

TCRect CRect(int X, int Y, int W, int H);
TKeyPoint CKeyPoint(const cv::KeyPoint k);
TDMatch CDMatch(const cv::DMatch k);
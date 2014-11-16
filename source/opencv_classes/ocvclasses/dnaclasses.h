#pragma once

#include <vector>
#include "stdafx.h"
#include "ocvexport.h"
#include "opencv2\objdetect\objdetect.hpp"

template<typename T> OCV_CLASS_EXPORT class TCVector
{
protected:
	std::vector<T> FVector;

public:
	TCVector() : FVector(){}	
	~TCVector(){}

	virtual size_t __stdcall size() { return FVector.size(); };
	virtual void __stdcall push_back(const T& Val) { FVector.push_back(Val); };
	virtual T* __stdcall at(int i) { return &FVector.at(i); };
	virtual std::vector<T>* __stdcall Vector()  { return &FVector; };
};

typedef struct TCRect
{	
	int  x;
	int  y;
	int  width;
	int  height;
} TCRect;

TCRect CRect(int X, int Y, int W, int H);

typedef TCVector<TCRect> TCVectorRect;
typedef TCVector<int> TCVectorInt;
typedef TCVector<double> TCVectorDouble;
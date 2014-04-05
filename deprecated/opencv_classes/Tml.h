#pragma once

#include "stdafx.h"
#include "Iml.h"
#include "opencv2\ml\ml.hpp"

class TCvKNearest : public ICvKNearest
{
public:

	TCvKNearest() : FCvKNearest(), FRefCount(0) {}	
	TCvKNearest(const CvMat* _trainData, const CvMat* _responses,
                const CvMat* _sampleIdx=0, BOOL _isRegression=false, int _max_k=32 ):	
				FCvKNearest(_trainData, _responses, _sampleIdx, (bool)_isRegression, _max_k), FRefCount(0) {}
	~TCvKNearest() { }

	// wrapping methods
	BOOL __stdcall train( const CvMat* trainData, const CvMat* responses,
		const CvMat* sampleIdx=0, BOOL is_regression=false,
		int maxK=32, BOOL updateBase=false )
		{return FCvKNearest.train(trainData, responses,sampleIdx, is_regression,maxK, updateBase);};

	float __stdcall find_nearest( const CvMat* samples, int k, CV_OUT CvMat* results=0,
		const float** neighbors=0, CV_OUT CvMat* neighborResponses=0, CV_OUT CvMat* dist=0 )
		{return FCvKNearest.find_nearest(samples, k, results,neighbors, neighborResponses, dist);};
	
	// Methods of IUnknown
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IScalar)))
		{
			*ppvObject = (void *)this;
			return S_OK;
		}
		else
		{
			*ppvObject = NULL;
			return E_NOINTERFACE;
		}
	}

	ULONG __stdcall AddRef(void)
	{
		return InterlockedIncrement(&FRefCount);
	}


	ULONG __stdcall Release(void)
	{
		ULONG result = InterlockedDecrement(&FRefCount);
		if (!result)
			delete this;
		return result;
	}

private:
	CvKNearest FCvKNearest;	// Delegation instead of inheritance
	ULONG FRefCount;
};
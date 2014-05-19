#pragma once

#include "stdafx.h"
#include "IString.h"
#include "opencv2\core\core.hpp"

class TString : public IString
{
public:

	TString() : FString(), FRefCount(0) {}
	TString(const char* s) : FString(s), FRefCount(0) { }	
	~TString() { }

	// wrapping methods	
	//-----------------------------------------------	
	cv::String* __stdcall getString() { return &FString; };	
	
	// Methods of IUnknown
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IString)))
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
	cv::String FString;	// Delegation instead of inheritance
	ULONG FRefCount;
};

#pragma once

#include "stdafx.h"
#include "opencv_classes.h"
#include "opencv2\core\core.hpp"
#include "opencv2\core\mat.hpp"
#include "Icore.types.h"

class TPoint : public IPoint
{
public:

	TPoint() : FPoint(), FRefCount(0) {}
	TPoint(int _x, int _y) : FPoint(_x,_y), FRefCount(0) { }	
	~TPoint() { }

	// wrapping methods
	int __stdcall get_x() {return FPoint.x;};
	int __stdcall get_y() {return FPoint.y;};
	void __stdcall set_x(int _x) {FPoint.x=_x;};
	void __stdcall set_y(int _y) {FPoint.y=_y;};
	//-----------------------------------------------	
	cv::Point* __stdcall getPoint() { return &FPoint; };	
	
	// Methods of IUnknown
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IPoint)))
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
	cv::Point FPoint;	// Delegation instead of inheritance
	ULONG FRefCount;
};


class TScalar : public IScalar
{
public:

	TScalar() : FScalar(), FRefCount(0) {}
	TScalar(double v0, double v1, double v2=0, double v3=0): FScalar(v0,v1,v2,v3), FRefCount(0) {}
    TScalar(double v0): FScalar(v0), FRefCount(0) {}
	~TScalar() { }

	// wrapping methods
	BOOL __stdcall isReal() {return FScalar.isReal();};
	void all(double value) {FScalar.all(value);};
	//---------------------------------
	cv::Scalar* __stdcall getScalar() {return &FScalar;};
	
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
	cv::Scalar FScalar;	// Delegation instead of inheritance
	ULONG FRefCount;
};

class TMatInputArray : public IMat, IInputArray
{
public:

	TMatInputArray() : FMat(), FInputArray(FMat), FRefCount(0) {}
	TMatInputArray(int rows, int cols, int type) : FMat(rows,cols,type), FInputArray(FMat), FRefCount(0) { }
	TMatInputArray(cv::Mat& m) : FMat(m), FInputArray(FMat), FRefCount(0) { }

	~TMatInputArray() { }

	// Methods of IMat wrapping Mat 
	size_t __stdcall elemSize() { return FMat.elemSize(); };
	size_t __stdcall elemSize1() { return FMat.elemSize1(); };
	int __stdcall _type() { return FMat.type(); };
	int __stdcall depth() { return FMat.depth(); };
	int __stdcall channels() { return FMat.channels(); };
	size_t __stdcall step1(int i) { return FMat.step1(); };
	BOOL __stdcall empty() { return FMat.empty(); };
	size_t __stdcall total() { return FMat.total(); };
	int __stdcall flags() { return FMat.flags; };
	int __stdcall dims() { return FMat.dims; };
	int __stdcall rows()  { return FMat.rows; };
	int __stdcall cols()  { return FMat.cols; };
	uchar* __stdcall data()  { return FMat.data; };
	void __stdcall copyto(IMat *mat) {*mat=TMatInputArray(FMat);};
	int* __stdcall refcount() { return FMat.refcount; };
	//-----------------------------------------------
	cv::Mat* __stdcall getMat() { return &FMat; };
	cv::_InputArray* __stdcall getInputArray() { return &FInputArray; };	

	// Methods of IUnknown

	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IMat)) || IsEqualGUID(riid, __uuidof(IInputArray)))
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
	cv::Mat FMat;	// Delegation instead of inheritance
	cv::_InputArray FInputArray;	// Delegation instead of inheritance
	ULONG FRefCount;
};

class TOutputArray : public IOutputArray
{
public:

	TOutputArray() : FOutputArray(), FRefCount(0) {}
	TOutputArray(cv::_OutputArray& o): FRefCount(0) {FOutputArray=o;}	
	~TOutputArray() {}

	// wrapping methods	
	BOOL __stdcall fixedSize() {return FOutputArray.fixedSize();}
	BOOL __stdcall fixedType() {return FOutputArray.fixedType();}
	BOOL __stdcall needed()  {return FOutputArray.needed();}
    void __stdcall create(LPSize sz, int type, int i, BOOL allowTransposed, int fixedDepthMask) 
	{
		FOutputArray.create(*sz->getSize(), type, i, (bool)allowTransposed, fixedDepthMask);
	}
    void __stdcall create(int rows, int cols, int type, int i, BOOL allowTransposed, int fixedDepthMask) 
	{
		FOutputArray.create(rows, cols, type, i, (bool)allowTransposed, fixedDepthMask);
	}
    void __stdcall create(int dims, int* size, int type, int i, BOOL allowTransposed, int fixedDepthMask)
	{
		FOutputArray.create(dims, size, type, i, (bool)allowTransposed, fixedDepthMask);
	}
    void __stdcall release()  {FOutputArray.release();}
    void __stdcall clear()  {FOutputArray.clear();}
	//---------------------------------
	LPMat* __stdcall getIMat() 
	{ 
		LPMat * mat=0;
		CreateMat_Mat(FOutputArray.getMat(),mat);
		return mat;
	}
	cv::Mat* __stdcall getMat() 
	{
		return &FOutputArray.getMat();
	}
	cv::_OutputArray* __stdcall getOutputArray() {return &FOutputArray;}
	
	// Methods of IUnknown
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(IOutputArray)))
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
	cv::_OutputArray FOutputArray;	// Delegation instead of inheritance
	ULONG FRefCount;
};

class TSize2i : public ISize2i
{
public:

	TSize2i() : FSize2i(), FRefCount(0) {}
	TSize2i(int _width, int _height): FSize2i(_width,_height), FRefCount(0) {}    
	~TSize2i() { }

	// wrapping methods
	
	//---------------------------------
	cv::Size2i* __stdcall getSize() {return &FSize2i;};
	
	// Methods of IUnknown
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject)
	{
		if (IsEqualGUID(riid, __uuidof(ISize2i)))
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
	cv::Size2i FSize2i;	// Delegation instead of inheritance
	ULONG FRefCount;
};

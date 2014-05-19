#pragma once

#include "stdafx.h"
#include "Unknwn.h"
#include "opencv2\core\core.hpp"

struct __declspec(uuid("{CA53E78F-F6BF-46E2-B034-7BDD72FFD952}")) IPoint : public IUnknown
{
public:
	virtual int __stdcall get_x() = 0;
	virtual int __stdcall get_y() = 0;
	virtual void __stdcall set_x(int _x) = 0;
	virtual void __stdcall set_y(int _y) = 0;
	//---------------------------------
	virtual cv::Point* __stdcall getPoint() = 0;	
};
typedef IPoint * LPPoint;

struct __declspec(uuid("{0887A2A4-3A75-4739-A6E8-F9CB502B8864}")) IScalar : public IUnknown
{
public:
	virtual BOOL __stdcall isReal() = 0;
	virtual void all(double value) = 0;
	//---------------------------------
	virtual cv::Scalar* __stdcall getScalar() = 0;	
};
typedef IScalar * LPScalar;

struct __declspec(uuid("{14B0A243-5E3D-4ECD-9F1E-8AD47CEF0CE0}")) ISize2i : public IUnknown
{
public:	
	//---------------------------------
	virtual cv::Size2i* __stdcall getSize() = 0;	
};
typedef ISize2i * LPSize;
typedef ISize2i ISize;

struct __declspec(uuid("{9C458D5C-F577-4A2D-89A0-FC426B80CC56}")) IMat : public IUnknown
{
public:    
	virtual size_t __stdcall elemSize() = 0;
	virtual size_t __stdcall elemSize1() = 0;
	virtual int __stdcall _type() = 0;
	virtual int __stdcall depth() = 0;
	virtual int __stdcall channels() = 0;	
    virtual size_t __stdcall step1(int i) = 0;
	virtual BOOL __stdcall empty() = 0;
	virtual size_t __stdcall total() = 0;
	virtual int __stdcall flags() = 0;
	virtual int __stdcall dims() = 0;
	virtual int __stdcall rows() = 0;
	virtual int __stdcall cols() = 0;
	virtual uchar* __stdcall data() = 0;
	virtual void __stdcall copyto(IMat *mat) = 0;
	virtual int* __stdcall refcount() = 0;	
	//---------------------------------
	virtual cv::Mat* __stdcall getMat() = 0;	
};

typedef IMat * LPMat;

struct __declspec(uuid("{358B3E6C-DBC7-451A-A906-330417EB6BDB}")) IInputArray : public IUnknown
{
public:	
	//---------------------------------
	virtual cv::_InputArray* __stdcall getInputArray() = 0;	
};
typedef IInputArray * LPInputArray;

struct __declspec(uuid("{023EA1D3-39AF-4BDF-82A0-6AA2AD251B6A}")) IOutputArray : public IUnknown
{
public:
    virtual BOOL __stdcall fixedSize() = 0;
    virtual BOOL __stdcall fixedType() = 0;
    virtual BOOL __stdcall needed()  = 0;      
    virtual void __stdcall create(LPSize sz, int type, int i, BOOL allowTransposed, int fixedDepthMask) = 0;
    virtual void __stdcall create(int rows, int cols, int type, int i, BOOL allowTransposed, int fixedDepthMask) = 0;
    virtual void __stdcall create(int dims, int* size, int type, int i, BOOL allowTransposed, int fixedDepthMask) = 0;
    virtual void __stdcall release()  = 0;
    virtual void __stdcall clear()  = 0;
	//---------------------------------
	virtual LPMat* __stdcall getIMat() = 0;
	virtual cv::Mat* __stdcall getMat() = 0;
	virtual cv::_OutputArray* __stdcall getOutputArray() = 0;
};
typedef IOutputArray * LPOutputArray;
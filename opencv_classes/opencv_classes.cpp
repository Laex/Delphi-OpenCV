// opencv_classes.cpp: определяет экспортированные функции для приложения DLL.
//

#include "stdafx.h"
#define ICLASS_EXPORTS 1
#include "opencv_classes.h"

#include "Ihighgui.h"
#include "Thighgui.h"

#include "Icore.types.h"
#include "Tcore.types.h"

#include "IString.h"
#include "TString.h"



// mat.hpp
HRESULT ICLASS_API CreateMat(LPMat *_Mat)
{
    *_Mat = new TMatInputArray();
    if (*_Mat)
    {
        (*_Mat)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateMat_rct(int rows, int cols, int type, LPMat *_Mat)
{
    *_Mat = new TMatInputArray(rows,cols,type);
    if (*_Mat)
    {
        (*_Mat)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateMat_Mat(cv::Mat& m, LPMat *_Mat)
{
    *_Mat = new TMatInputArray(m);
    if (*_Mat)
    {
        (*_Mat)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

// highgui.hpp

HRESULT ICLASS_API CreateVideoCapture(LPVideoCapture *_VideoCapture)
{
    *_VideoCapture = new TVideoCapture();
    if (*_VideoCapture)
    {
        (*_VideoCapture)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateVideoCapture_dvc(int device, LPVideoCapture *_VideoCapture)
{
    *_VideoCapture = new TVideoCapture(device);
    if (*_VideoCapture)
    {
        (*_VideoCapture)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateVideoCapture_fln(char* filename, LPVideoCapture *_VideoCapture)
{
    *_VideoCapture = new TVideoCapture(filename);
    if (*_VideoCapture)
    {
        (*_VideoCapture)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

//core/types.hpp
HRESULT ICLASS_API CreatePoint(LPPoint *_Point)
{
    *_Point = new TPoint();
    if (*_Point)
    {
        (*_Point)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreatePoint_xy(int x,int y, LPPoint *_Point)
{
    *_Point = new TPoint(x,y);
    if (*_Point)
    {
        (*_Point)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateScalar(LPScalar *_Scalar)
	{
    *_Scalar = new TScalar();
    if (*_Scalar)
    {
        (*_Scalar)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}
HRESULT ICLASS_API CreateScalar_v03(int v0,int v1, int v2, int v3, LPScalar *_Scalar)
	{
    *_Scalar = new TScalar(v0,v1,v2,v3);
    if (*_Scalar)
    {
        (*_Scalar)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}
HRESULT ICLASS_API CreateScalar_v0(int v0, LPScalar *_Scalar)
{
    *_Scalar = new TScalar(v0);
    if (*_Scalar)
    {
        (*_Scalar)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CString(const char* s, LPString *_String)
{
    *_String = new TString(s);
    if (*_String)
    {
        (*_String)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateSize2i(LPSize * _Size2i)
	{
    *_Size2i = new TSize2i();
    if (*_Size2i)
    {
        (*_Size2i)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}
HRESULT ICLASS_API CreateSize2i_wh(int _width, int _height, LPSize * _Size2i)
	{
    *_Size2i = new TSize2i(_width,_height);
    if (*_Size2i)
    {
        (*_Size2i)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT ICLASS_API CreateOutputArray(cv::_OutputArray& o, LPOutputArray * _OutputArray)
	{
    *_OutputArray = new TOutputArray(o);
    if (*_OutputArray)
    {
        (*_OutputArray)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}
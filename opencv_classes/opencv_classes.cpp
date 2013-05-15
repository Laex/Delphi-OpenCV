// opencv_classes.cpp: определяет экспортированные функции для приложения DLL.
//

#include "stdafx.h"
#define IMAT_EXPORTS 1
#include "opencv_classes.h"

#include "TMat.h"
#include "imat.h"
//#include "opencv2\core\mat.hpp"

#include "Thighgui.h"
#include "Ihighgui.h"


// mat.hpp
HRESULT IMAT_API CreateMat(LPMat *_Mat)
{
    *_Mat = new TMat();
    if (*_Mat)
    {
        (*_Mat)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

HRESULT IMAT_API CreateMat_rct(int rows, int cols, int type, LPMat *_Mat)
{
    *_Mat = new TMat(rows,cols,type);
    if (*_Mat)
    {
        (*_Mat)->AddRef();
        return S_OK;
    }
    else
        return E_NOINTERFACE;
}

// highgui.hpp

HRESULT IMAT_API CreateVideoCapture(LPVideoCapture *_VideoCapture)
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

HRESULT IMAT_API CreateVideoCapture_dvc(int device, LPVideoCapture *_VideoCapture)
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
// opencv_classes.cpp: определяет экспортированные функции для приложения DLL.
//

#include "stdafx.h"
#define IMAT_EXPORTS 1
#include "opencv_classes.h"
#include "TMat.h"
#include "imat.h"
#include "opencv2\core\mat.hpp"

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

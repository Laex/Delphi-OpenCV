
#include "stdafx.h"
#include "dnaclasses.h"
#include "ocvexport.h"

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
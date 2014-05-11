#include "stdafx.h"
#include "tesseract_dll.h"

///////////////////////////////////////////////////
TTessBaseAPI * __stdcall CreateTessBaseAPI()
{
	return new TTessBaseAPI();
};

void __stdcall ReleaseTessBaseAPI(TTessBaseAPI * ex)
{
	delete ex;
};

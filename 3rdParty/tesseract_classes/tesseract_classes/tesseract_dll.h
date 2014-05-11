#pragma once

#include "stdafx.h"
#include <tesseract/baseapi.h>
#include <leptonica/allheaders.h>

//////////////////////////////////////////////////////////////////////
class AFX_CLASS_EXPORT TTessBaseAPI
{
protected:
	tesseract::TessBaseAPI api;

public:
	TTessBaseAPI() : api() {}
	~TTessBaseAPI() {}

	virtual int __stdcall Init(const char* datapath, const char* language) { return api.Init(datapath, language); };
	virtual void __stdcall SetImage(const Pix* pix) { api.SetImage(pix); };
	virtual char* __stdcall GetUTF8Text() { return api.GetUTF8Text(); };
	virtual void __stdcall _End() { api.End(); };
};
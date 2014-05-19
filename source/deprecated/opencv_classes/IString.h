#pragma once

#include "stdafx.h"
#include "Unknwn.h"
#include "opencv2\core\core.hpp"

struct __declspec(uuid("{50C8309F-69B6-4E8C-A2B5-F2530007CCA1}")) IString : public IUnknown
{
public:	
	//---------------------------------
	virtual cv::String* __stdcall getString() = 0;	
};

typedef IString * LPString;
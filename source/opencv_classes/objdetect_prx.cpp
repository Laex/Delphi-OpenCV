
#include "stdafx.h"

namespace cv
{
	//////////////////// CascadeClassifier ////////////////////

	ICLASS_API CascadeClassifier* __stdcall CreateCascadeClassifier()
	{
		return new CascadeClassifier;
	};
	ICLASS_API void __stdcall DestroyCascadeClassifier(CascadeClassifier* e)
	{
		delete e;
	};

	ICLASS_API bool __stdcall get_CascadeClassifier_empty(CascadeClassifier* e)
	{
		return e->empty();
	}

	ICLASS_API bool __stdcall get_CascadeClassifier_load(CascadeClassifier* e, char* filename)
	{
		return e->load(filename);
	}


	ICLASS_API bool __stdcall CascadeClassifier_isOldFormatCascade(CascadeClassifier* e)
	{
		return e->isOldFormatCascade();
	}

	ICLASS_API Size* __stdcall CascadeClassifier_getOriginalWindowSize(CascadeClassifier* e)
	{
		Size *s = new Size(e->getOriginalWindowSize());
		//*s = e->getOriginalWindowSize();
		return s;
	}

	ICLASS_API int __stdcall CascadeClassifier_getFeatureType(CascadeClassifier* e)
	{
		return e->getFeatureType();
	}

	ICLASS_API int __stdcall CascadeClassifier_setImage(CascadeClassifier* e, Mat* m)
	{
		return e->setImage(*m);
	}

	//ICLASS_API bool __stdcall CascadeClassifier_convert(CascadeClassifier* e, char* oldcascade, char* newcascade)
	//{
	//		return e->convert(oldcascade, newcascade);
	//	}	
}
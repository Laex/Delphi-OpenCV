#include "stdafx.h"

namespace cv
{

	typedef struct
	{
		Point2f pt;
		float size;
		float angle;
		float response;
		int octave;
		int class_id;
	} _KeyPoint;

	//////////////////// FeatureDetector ////////////////////
	ICLASS_API FeatureDetector* __stdcall Create_FeatureDetector(const char* detectorType)
	{
		Ptr<FeatureDetector> r= FeatureDetector::create(detectorType);		
		r.addref();
		return r;
	}
	ICLASS_API bool __stdcall Empty_FeatureDetector(const FeatureDetector* r)
	{		
		return r->empty();
	}
		
	//std::vector<KeyPoint> kp;
	std::vector<KeyPoint>* kp = new std::vector<KeyPoint>();

	ICLASS_API void __stdcall detect_FeatureDetector(const FeatureDetector* r, IplImage* image, 
						CV_OUT size_t &keypointcount, CV_OUT _KeyPoint* &keypoints, IplImage* mask)
	{		
		//std::vector<KeyPoint>* kp = new std::vector<KeyPoint>();
		//kp.clear();
		//kp = new std::vector<KeyPoint>();
		r->detect(Mat(image), *kp, (mask ? Mat(mask) : Mat()));		
		keypointcount = kp->size();
		if (kp->size())
		{			
			keypoints = (_KeyPoint*)cvAlloc(sizeof(_KeyPoint)*kp->size());
			// need memcpy
			for (int i = 0; i < kp->size(); i++)
			{
				keypoints[i].pt = (*kp)[i].pt;
				keypoints[i].angle = (*kp)[i].angle;
				keypoints[i].class_id = (*kp)[i].class_id;
				keypoints[i].octave = (*kp)[i].octave;
				keypoints[i].response = (*kp)[i].response;
				keypoints[i].size = (*kp)[i].size;
			}
		}
		else keypoints = 0;
		//delete kp;
	}

	//////////////////// DescriptorExtractor ////////////////////
	ICLASS_API DescriptorExtractor* __stdcall Create_DescriptorExtractor(const char* descriptorExtractorType)
	{
		Ptr<DescriptorExtractor> r = DescriptorExtractor::create(descriptorExtractorType);
		r.addref();
		return r;
	}
	ICLASS_API bool __stdcall Empty_DescriptorExtractor(const DescriptorExtractor* r)
	{
		return r->empty();
	}
	ICLASS_API int __stdcall descriptorSize_DescriptorExtractor(const DescriptorExtractor* r)
	{
		return r->descriptorSize();
	}
	ICLASS_API int __stdcall descriptorType_DescriptorExtractor(const DescriptorExtractor* r)
	{
		return r->descriptorType();
	}
	
	ICLASS_API void __stdcall compute_DescriptorExtractor(const DescriptorExtractor* r, IplImage* image, 
		int keypointcount, _KeyPoint* keypoints, CV_OUT Mat* &descriptors)
	{
		std::vector<KeyPoint> kp;		
		for (size_t i = 0; i < keypointcount; i++) {
			kp.push_back(
				KeyPoint(
				keypoints[i].pt, keypoints[i].size, keypoints[i].angle,
					keypoints[i].response, keypoints[i].octave, keypoints[i].class_id
				)
			);

		}		
		descriptors = new Mat;		
		r->compute(Mat(image), kp, *descriptors);		
	}
}
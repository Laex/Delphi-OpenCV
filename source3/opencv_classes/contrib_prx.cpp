
#include "stdafx.h"

namespace cv
{

	//////////////////// FaceRecognizer ////////////////////
	ICLASS_API FaceRecognizer* __stdcall Create_EigenFaceRecognizer(int num_components, double threshold)
	{
		Ptr<FaceRecognizer> r = createEigenFaceRecognizer(num_components, threshold);
		r.addref();
		return r;
	}
	ICLASS_API FaceRecognizer* __stdcall Create_FisherFaceRecognizer(int num_components, double threshold)
	{
		Ptr<FaceRecognizer> r = createFisherFaceRecognizer(num_components, threshold);
		r.addref();
		return r;
	}

	ICLASS_API FaceRecognizer* __stdcall Create_LBPHFaceRecognizer(int radius, int neighbors,
		int grid_x, int grid_y, double threshold)
	{
		Ptr<FaceRecognizer> r = createLBPHFaceRecognizer(radius, neighbors, grid_x, grid_y, threshold);
		r.addref();
		return r;
	}

	ICLASS_API void __stdcall FaceRecognizerLoad(FaceRecognizer* e, char* FileName)
	{
		return e->load(FileName);
	};

	ICLASS_API void __stdcall FaceRecognizerSave(FaceRecognizer* e, char* FileName)
	{
		return e->save(FileName);
	};

	ICLASS_API int __stdcall FaceRecognizerPredict1(FaceRecognizer* e, IplImage* scr)
	{
		return e->predict(Mat(scr));
	};

	ICLASS_API void __stdcall FaceRecognizerPredict2(FaceRecognizer* e, IplImage* scr, CV_OUT int &label, CV_OUT double &confidence)
	{		
		e->predict(Mat(scr), label, confidence);
	};

	ICLASS_API void __stdcall FaceRecognizerTrain(FaceRecognizer* e, int n, IplImage** scr, int* lab)
	{
		vector<Mat> images;
		vector<int> labels;
		for (int i = 0; i < n; i++)
		{
			images.push_back(Mat(scr[i])); 
			labels.push_back(lab[i]);
		}
		e->train(images, labels);
	};

	ICLASS_API void __stdcall FaceRecognizerTrainMat(FaceRecognizer* e, int n, Mat** scr, int* lab)
	{
		vector<Mat> images;
		vector<int> labels;
		for (int i = 0; i < n; i++)
		{
			images.push_back(*scr[i]);
			labels.push_back(lab[i]);
		}
		e->train(images, labels);
	};

	ICLASS_API void __stdcall FaceRecognizerUpdate(FaceRecognizer* e, int n, IplImage** scr, int* lab)
	{
		vector<Mat> images;
		vector<int> labels;
		for (int i = 0; i < n; i++)
		{
			images.push_back(Mat(scr[i]));
			labels.push_back(lab[i]);
		}
		e->update(images, labels);
	};

	ICLASS_API void __stdcall DestroyFaceRecognizer(FaceRecognizer* e)
	{
		//while(e.refcount)
		//e.release();
		//delete e;
	};
}

#include "stdafx.h"

namespace cv
{

ICLASS_API void __stdcall prxCanny(IplImage* image, IplImage* edges,
	double threshold1, double threshold2,
	int apertureSize, bool L2gradient)
{
	Mat _edges;
	Canny(Mat(image), _edges, threshold1, threshold2, apertureSize, L2gradient);
	cvCopy(&(IplImage)_edges, edges);
}

}
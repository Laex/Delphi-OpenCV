// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_CopyMakeBorder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  core_c in '..\..\..\include\сore\core_c.pas',
  Core.types_c in '..\..\..\include\сore\Core.types_c.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  uLibName in '..\..\..\include\uLibName.pas',
  types_c in '..\..\..\include\сore\types_c.pas';

const
  filename = 'Resource\opencv_logo_with_text.png';

Var
 image :pIplImage= nil;
 dst   :pIplImage= nil;
 dst2  :pIplImage= nil;

begin
	// получаем картинку
	image := cvLoadImage(filename,1);
  WriteLn(Format('[i] image: %s', [filename]));
	// создаём картинки
	dst := cvCreateImage( cvSize(image^.width+20, image^.height+20), image^.depth, image^.nChannels);
	dst2 := cvCreateImage( cvSize(image^.width+20, image^.height+20), image^.depth, image^.nChannels);

	// окно для отображения картинки
	cvNamedWindow('original',CV_WINDOW_AUTOSIZE);
	cvNamedWindow('IPL_BORDER_CONSTANT',CV_WINDOW_AUTOSIZE);
	cvNamedWindow('IPL_BORDER_REPLICATE',CV_WINDOW_AUTOSIZE);

	// обрамляем границей
	cvCopyMakeBorder(image, dst, cvPoint(10,10), IPL_BORDER_CONSTANT, cvScalar(250));
	cvCopyMakeBorder(image, dst2, cvPoint(10,10), IPL_BORDER_REPLICATE, cvScalar(250));


	// показываем картинку
	cvShowImage('original',image);
	cvShowImage('IPL_BORDER_CONSTANT',dst);
	cvShowImage('IPL_BORDER_REPLICATE',dst2);

	// ждём нажатия клавиши
	cvWaitKey(0);

	// освобождаем ресурсы
	cvReleaseImage(&image);
	cvReleaseImage(&dst);
	cvReleaseImage(&dst2);
	// удаляем окна
	cvDestroyAllWindows();
end.

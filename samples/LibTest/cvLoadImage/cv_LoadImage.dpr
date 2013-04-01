// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program cv_LoadImage;

{$APPTYPE CONSOLE}

uses
  core_c in '..\..\..\include\ñore\core_c.pas',
  core.types_c in '..\..\..\include\ñore\core.types_c.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  uLibName in '..\..\..\include\uLibName.pas',
  types_c in '..\..\..\include\ñore\types_c.pas';

const
  // declare image filename
  IMAGE_FILE_NAME = 'Resource\opencv_logo_with_text.png';

var
  // declare an opencv image pointer variable
  image: pIplImage;

begin
  // load image from file
  // REMARK: all opencv strings are PAnsiChar, pay attention to this
  // when using with Delphi 2010/2009
  image := cvLoadImage(IMAGE_FILE_NAME);
  // create display window
  cvNamedWindow('image');
  // display image inside "image" window
  cvShowImage('image', image);
  // wait until user keypress
  cvWaitKey();
  // release image memory
  cvReleaseImage(image);
  // close and release all display windows
  cvDestroyAllWindows;

end.

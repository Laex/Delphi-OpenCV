unit LibName;

interface

const
  CV_Version = '244';

{$IFDEF DEBUG}
  Core_Dll = 'opencv_core' + CV_Version + 'd.dll';
  highgui_Dll = 'opencv_highgui' + CV_Version + 'd.dll';
  imgproc_Dll = 'opencv_imgproc' + CV_Version + 'd.dll';
  objdetect_dll ='opencv_objdetect' + CV_Version + 'd.dll';
{$ELSE}
  Core_Dll = 'opencv_core' + CV_Version + '.dll';
  highgui_Dll = 'opencv_highgui' + CV_Version + '.dll';
  imgproc_Dll = 'opencv_imgproc' + CV_Version + '.dll';
  objdetect_dll ='opencv_objdetect' + CV_Version + '.dll';
{$ENDIF}

implementation

end.

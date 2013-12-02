unit uOCVRegister;

interface

procedure Register;

implementation

Uses
  DesignIntf,
  System.Classes,
  uOCVCamera,
  uOCVView,
  uOCVImageOperation,
  uOCVSplitter;

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvImageOperation]);
  RegisterComponents('OpenCV', [TocvCamera]);
  RegisterComponents('OpenCV', [TocvCamera]);
  RegisterComponents('OpenCV', [TocvView]);
  RegisterComponents('OpenCV', [TocvSplitter]);
  RegisterClasses([TocvImageOperation_None, TocvImageOperation_GrayScale, TovcImageOperation_Canny, TovcImageOperation_Smooth,
    TocvChannel]);
end;

end.

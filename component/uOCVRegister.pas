unit uOCVRegister;

interface

procedure Register;

implementation

Uses
  DesignIntf,
  System.Classes,
  uOCVCamera,
  uOCVView,
  uOCVImageOperation;

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvImageOperation]);
  UnlistPublishedProperty(TocvCustomImageOperations, 'Tag');
  UnlistPublishedProperty(TocvCustomImageOperations, 'Name');
//  RegisterClasses([TocvImageOperationNone,TocvImageOperationGrayScale,TovcImageOperationCanny,TovcImageOperationSmooth]);

  RegisterComponents('OpenCV', [TocvCamera]);
  RegisterComponents('OpenCV', [TocvView]);
end;

end.

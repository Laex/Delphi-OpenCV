unit uOCVRegister;

interface

procedure Register;

implementation

Uses
  DesignIntf,
  System.Classes,
  uOCVCamera,
  uOCVView,
  uOCVImageOperation
//  ,dOCVImageOperation
  ;

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvImageOperation]);
//  RegisterPropertyEditor(TypeInfo(TocvCustomImageOperations), TocvImageOperation, 'ImageOperation', TImageOperationPropertyEditor);
  RegisterComponents('OpenCV', [TocvCamera]);
  RegisterComponents('OpenCV', [TocvView]);
//  UnlistPublishedProperty(TocvCustomImageOperations, 'Tag');
//  UnlistPublishedProperty(TocvCustomImageOperations, 'Name');
//  RegisterClasses([TocvImageOperationNone,TocvImageOperationGrayScale,TovcImageOperationCanny,TovcImageOperationSmooth]);
end;

end.

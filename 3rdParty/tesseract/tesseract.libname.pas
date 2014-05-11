unit tesseract.libname;

interface

const

  TESS_MAJOR_VERSION = '3';
  TESS_MINOR_VERSION = '02';

  TESS_VERSION = TESS_MAJOR_VERSION + '.' + TESS_MINOR_VERSION;

  TESS_VERSION_DLL = TESS_MAJOR_VERSION + TESS_MINOR_VERSION;

//{$IFDEF DEBUG}
//  tesseract_classes_Dll = 'tesseract_classesd.dll';
//{$ELSE}
  tesseract_classes_Dll = 'tesseract_classes.dll';
//{$ENDIF}

implementation

end.

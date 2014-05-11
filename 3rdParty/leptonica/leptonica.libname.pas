unit leptonica.libname;

interface

const

  LIBLEPT_MAJOR_VERSION = '1';
  LIBLEPT_MINOR_VERSION = '68';

  LIBLEPT_VERSION = LIBLEPT_MAJOR_VERSION + '.' + LIBLEPT_MINOR_VERSION;

  LIBLEPT_VERSION_DLL = LIBLEPT_MAJOR_VERSION + LIBLEPT_MINOR_VERSION;

//{$IFDEF DEBUG}
//  leptonica_Dll = 'liblept' + LIBLEPT_VERSION_DLL + 'd.dll';
//{$ELSE}
  leptonica_Dll = 'liblept' + LIBLEPT_VERSION_DLL + '.dll';
//{$ENDIF}

implementation

end.

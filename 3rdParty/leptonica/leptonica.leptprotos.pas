unit leptonica.leptprotos;

interface

Uses
  leptonica.pix;

function pixRead(const filename: pAnsiChar): pPix; stdcall;
procedure pixDestroy(Var pix: pPix); stdcall;

implementation

Uses
  leptonica.libname;

function pixRead; external leptonica_Dll;
procedure pixDestroy; external leptonica_Dll;

end.

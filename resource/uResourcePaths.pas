unit uResourcePaths;

{$WRITEABLECONST ON}

interface

Const
  {
  Relative paths from
  <root>\bin\Win32
  <root>\bin\Win64
  to <root>\resource
  }
  cResourceMedia = '..\..\resource\media\';
  cResourceFaces = '..\..\resource\faces\';
  cResourceFaceDetect = '..\..\resource\facedetectxml\';
  cResourceHaar = '..\..\resource\facedetectxml\';
  cResourceHaar_newer = '..\..\resource\facedetectxml_newer\';
  cResourceResultDefault = '..\..\resource\result\';

function cResourceResult: AnsiString;

implementation

uses
  SysUtils;

function cResourceResult: AnsiString;
begin
  if DirectoryExists(cResourceResultDefault) then
    Result := cResourceResultDefault
  else
    Result := '';
end;

end.

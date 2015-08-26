unit uResourcePaths;

{$WRITEABLECONST ON}

interface

Const
  cResourceMedia = '..\..\resource\media\';
  cResourceFaceDetect = '..\..\resource\facedetectxml\';
  cResourceResultDefault = '..\..\resource\result\';

function cResourceResult: AnsiString;

implementation

uses
  System.SysUtils;

function cResourceResult: AnsiString;
begin
  if DirectoryExists(cResourceResultDefault) then
    Result := cResourceResultDefault
  else
    Result := '';
end;

end.

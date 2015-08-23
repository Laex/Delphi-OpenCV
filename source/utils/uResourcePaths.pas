unit uResourcePaths;

interface

Const
  cResourceMedia = '..\..\resource\media\';
  cResourceFaceDetect = '..\..\resource\facedetectxml\';

function cResourceResult: string;

implementation

uses
  System.SysUtils;

const
  ResourceResultDefault = '..\..\resource\result\';

function cResourceResult: string;
begin
  if DirectoryExists(ResourceResultDefault) then
    Result := ResourceResultDefault
  else
    Result := '';
end;

end.

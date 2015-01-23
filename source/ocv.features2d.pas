unit ocv.features2d;

interface

Uses
  WinApi.Windows;

// nonfree
Type
  TSURF = class
    // ------------------------------------------------
    class function Create: TSURF; overload;
    class function Create(hessianThreshold: double; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
      upright: BOOL = false): TSURF; overload;
    procedure Free; reintroduce;
  end;

  TSurfFeatureDetector = TSURF;
  TSurfDescriptorExtractor = TSURF;

implementation

uses
  ocv.lib;

function CreateSURF: TSURF; stdcall; external opencv_classes_lib;
function CreateSURFFromValue(hessianThreshold: double; nOctaves: Integer = 4; nOctaveLayers: Integer = 2; extended: BOOL = true;
  upright: BOOL = false): TSURF; stdcall; external opencv_classes_lib;
procedure ReleaseSURF(ex: TSURF); stdcall; external opencv_classes_lib;

{ TSURF }

class function TSURF.Create: TSURF;
begin
  Result := CreateSURF;
end;

class function TSURF.Create(hessianThreshold: double; nOctaves, nOctaveLayers: Integer; extended, upright: BOOL): TSURF;
begin
  Result := CreateSURFFromValue(hessianThreshold, nOctaves, nOctaveLayers, extended, upright);
end;

procedure TSURF.Free;
begin
  ReleaseSURF(Self);
end;

end.

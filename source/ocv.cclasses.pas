unit ocv.cclasses;

interface

uses
  ocv.core.types_c;

Type

  pCRect = ^TCRect;

  TCRect = record
    x, y, width, height: Integer;
  end;

  TCVectorRect = class
  public
    function size(): size_t; virtual; stdcall; abstract;
    procedure push_back(const Val: TCRect); virtual; stdcall; abstract;
    function at(i: Integer): pCRect; virtual; stdcall; abstract;
    function Vector(): Pointer; virtual; stdcall; abstract;
    // ---------------------
    class function Create: TCVectorRect;
    procedure Free; reintroduce;
  end;

  TCVectorInt = class
  public
    function size(): size_t; virtual; stdcall; abstract;
    procedure push_back(const Val: Integer); virtual; stdcall; abstract;
    function at(i: Integer): pInteger; virtual; stdcall; abstract;
    function Vector(): Pointer; virtual; stdcall; abstract;
    // ---------------------
    class function Create: TCVectorInt;
    procedure Free; reintroduce;
  end;

  TCVectorDouble = class
  public
    function size(): size_t; virtual; stdcall; abstract;
    procedure push_back(const Val: Double); virtual; stdcall; abstract;
    function at(i: Integer): pDouble; virtual; stdcall; abstract;
    function Vector(): Pointer; virtual; stdcall; abstract;
    // ---------------------
    class function Create: TCVectorDouble;
    procedure Free; reintroduce;
  end;

implementation

uses ocv.lib;

function CreateCVectorRect: TCVectorRect; stdcall; external opencv_classes_lib;
procedure ReleaseCVectorRect(ex: TCVectorRect); stdcall; external opencv_classes_lib;
function CreateCVectorInt: TCVectorInt; stdcall; external opencv_classes_lib;
procedure ReleaseCVectorInt(ex: TCVectorInt); stdcall; external opencv_classes_lib;
function CreateCVectorDouble: TCVectorDouble; stdcall; external opencv_classes_lib;
procedure ReleaseCVectorDouble(ex: TCVectorDouble); stdcall; external opencv_classes_lib;

{ TCVectorRect }

class function TCVectorRect.Create: TCVectorRect;
begin
  Result := CreateCVectorRect;
end;

procedure TCVectorRect.Free;
begin
  ReleaseCVectorRect(Self);
end;

{ TCVectorInt }

class function TCVectorInt.Create: TCVectorInt;
begin
  Result := CreateCVectorInt;
end;

procedure TCVectorInt.Free;
begin
  ReleaseCVectorInt(Self);
end;

{ TCVectorDouble }

class function TCVectorDouble.Create: TCVectorDouble;
begin
  Result := CreateCVectorDouble;
end;

procedure TCVectorDouble.Free;
begin
  ReleaseCVectorDouble(Self);
end;

end.

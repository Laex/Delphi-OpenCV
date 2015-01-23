(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)
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

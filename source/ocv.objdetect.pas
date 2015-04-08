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
unit ocv.objdetect;

{$I OpenCV.inc}

interface

Uses
//{$IFDEF MSWINDOWS}
//  Winapi.Windows,
//{$ENDIF MSWINDOWS}
  ocv.Mat,
  ocv.cclasses,
  ocv.core.types_c;

const
  CV_HAAR_DO_CANNY_PRUNING    = 1;
  CV_HAAR_SCALE_IMAGE         = 2;
  CV_HAAR_FIND_BIGGEST_OBJECT = 4;
  CV_HAAR_DO_ROUGH_SEARCH     = 8;

Type
  TccvCascadeClassifier = class
  public
    function empty(): bytebool; virtual; stdcall; abstract;
    function load(const FileName: PAnsiChar): bytebool; virtual; stdcall; abstract;
    procedure detectMultiScale(image: TccvMat; objects: TCVectorRect; scaleFactor: Double { = 1.1 };
      minNeighbors: integer { = 3 }; flags: integer { = 0 }; minSize: TCvSize { = CvSize() };
      maxSize: TCvSize { = CvSize()) } ); overload; virtual; stdcall; abstract;
    procedure detectMultiScaleLevel(image: TccvMat; objects: TCVectorRect; rejectLevels: TCVectorInt;
      levelWeights: TCVectorDouble; scaleFactor: Double { = 1.1 }; minNeighbors: integer { = 3 };
      flags: integer { = 0 }; minSize: TCvSize { = CvSize() }; maxSize: TCvSize { = CvSize()) };
      outputRejectLevels: bytebool { = false } ); overload; virtual; stdcall; abstract;
    function isOldFormatCascade(): bytebool; virtual; stdcall; abstract;
    // function getOriginalWindowSize(): TCvSize; virtual; stdcall; abstract;
    // function getFeatureType(): integer; virtual; stdcall; abstract;
    // function setImage(image: TccvMat): bytebool; virtual; stdcall; abstract;
    // --------------------------------------------
    class function Create: TccvCascadeClassifier; overload;
    class function Create(const FileName: PAnsiChar): TccvCascadeClassifier; overload;
    class function Create(const FileName: String): TccvCascadeClassifier; overload;
    procedure Free; reintroduce;
  end;

implementation

uses ocv.lib;

function CreateCascadeClassifier: TccvCascadeClassifier; stdcall; external opencv_classes_lib;
function CreateCascadeClassifierFromFile(const FileName: PAnsiChar): TccvCascadeClassifier; stdcall;
  external opencv_classes_lib;
procedure ReleaseCascadeClassifier(ex: TccvCascadeClassifier); stdcall; external opencv_classes_lib;

{ TccvCascadeClassifier }

class function TccvCascadeClassifier.Create: TccvCascadeClassifier;
begin
  Result := CreateCascadeClassifier;
end;

class function TccvCascadeClassifier.Create(const FileName: PAnsiChar): TccvCascadeClassifier;
begin
  Result := CreateCascadeClassifierFromFile(FileName);
end;

class function TccvCascadeClassifier.Create(const FileName: String): TccvCascadeClassifier;
begin
  Result := CreateCascadeClassifierFromFile(PAnsiChar(@AnsiString(FileName)[1]));
end;

procedure TccvCascadeClassifier.Free;
begin
  ReleaseCascadeClassifier(Self);
end;

end.

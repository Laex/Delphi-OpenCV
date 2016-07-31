(*
  *****************************************************************
  Delphi-OpenCV
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

unit ocv.cls.types;

{$I OpenCV.inc}

interface

Type
  TOpenCVClass = Pointer;

  IOCVCommon = interface
    function _InternalData: TOpenCVClass;
  end;

  TOCVCommon = class(TInterfacedObject, IOCVCommon)
  protected
    FData: TOpenCVClass;
  public
    constructor Create(const OpenCVClass: TOpenCVClass);
    function _InternalData: TOpenCVClass;
  end;



implementation

Uses ocv.utils;

{ TOCVCommon }

constructor TOCVCommon.Create(const OpenCVClass: TOpenCVClass);
begin
  FData := OpenCVClass;
end;

function TOCVCommon._InternalData: TOpenCVClass;
begin
  Result := FData;
end;

end.

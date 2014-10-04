// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

program clsMat;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.Core.types_c,
  ocv.core_c,
  ocv.mat;

procedure Print(const M: TccvMat);

Var
  matdata: PByte;

begin
  Writeln('M         = $', IntToHex(Integer(M), 8));
  With M do
  begin
    Writeln('elemSize  = ', elemSize);
    Writeln('elemSize1 = ', elemSize1);
    Writeln('type      = ', _type);
    Writeln('depth     = ', depth);
    Writeln('channels  = ', channels);
    Writeln('empty     = ', empty);
    Writeln('total     = ', total);
    Writeln('flags     = $', IntToHex(flags, 8));
    Writeln('dims      = ', dims);
    Writeln('rows      = ', rows);
    Writeln('cols      = ', cols);
    Writeln('data      = $', IntToHex(Integer(data), 8));
    matdata := data;
    if Assigned(matdata) then
      Writeln('($', IntToHex(Integer(matdata^), 2), ')')
    else
      Writeln;
  end;
end;

Var
  mat, cmat: TccvMat;

begin
  try
    Writeln('--------- Create empty MAT');
    mat := TccvMat.Create;
    Print(mat);
    mat.Free;
    Readln;
    Writeln('--------- Create MAT 2x2 CV_8UC1 - 1 byte, 1 channel');
    mat := TccvMat.Create(2, 2, CV_8UC1);
    Print(mat);
    mat.Free;
    Readln;
    Writeln('--------- Create MAT 4x2 CV_32FC2 - single (4-byte floating point), 2 channel');
    mat := TccvMat.Create(4, 2, CV_32FC2);
    Print(mat);
    mat.Free;
    Readln;
    Writeln('--------- Create 2x2 MAT');
    mat := TccvMat.Create(2, 2, CV_8UC1);
    Print(mat);
    mat.Free;
    Readln;
    Writeln('--------- Create 3x3 MAT');
    mat := TccvMat.Create(3, 3, CV_8UC1);
    Print(mat);
    mat.copyTo(cmat);
    mat.Free;
    Readln;
    Writeln('--------- Copy Create 3x3 MAT');
    Print(cmat);
    cmat.Free;
    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

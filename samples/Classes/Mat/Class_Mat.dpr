(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // laentir Valetov
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
  ******************************************************************* *)

// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Class_Mat;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  uLibName in '..\..\..\include\uLibName.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  core_c in '..\..\..\include\core\core_c.pas',
  Mat in '..\..\..\include\core\Mat.pas';

procedure Print(const M: IMat);
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
    Write('data      = $', IntToHex(Integer(data), 8));
    if Assigned(data) then
      Writeln('($', IntToHex(Integer(data^), 2), ')')
    else
      Writeln;
    Writeln('getMat()  = $', IntToHex(Integer(getMat), 8));
    Write('refcount  = $', IntToHex(Integer(refcount), 8));
    if Assigned(refcount) then
      Writeln('(', refcount^, ')')
    else
      Writeln;
  end;
end;

Var
  Mat: IMat;
  Mat1: IMat;
  pMat: Pointer;

begin
  try
    Writeln('--------- Create empty MAT');
    Mat := CreateMat;
    Print(Mat);
    Readln;
    Writeln('--------- Create MAT 2x2 CV_8UC1 - 1 byte, 1 channel');
    Mat := CreateMat(2, 2, CV_8UC1);
    Print(Mat);
    Readln;
    Writeln('--------- Create MAT 4x2 CV_32FC2 - single (4-byte floating point), 2 channel');
    Mat := CreateMat(4, 2, CV_32FC2);
    Print(Mat);
    Readln;
    Writeln('--------- Create 2x2 MAT');
    Mat := CreateMat(2, 2, CV_8UC1);
    Print(Mat);
    Writeln('--------- Create 3x3 MAT');
    Mat1 := CreateMat(3, 3, CV_8UC1);
    Print(Mat1);
    Readln;
    // WriteLn('--------- Swap Mat');
    // pMat:=Mat.getMat;
    // Mat.setMat(Mat1.getMat);
    // Mat1.setMat(pMat);
    // Print(Mat);
    // Print(Mat1);
    // Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

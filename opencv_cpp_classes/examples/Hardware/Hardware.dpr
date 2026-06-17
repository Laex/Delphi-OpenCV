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
program Hardware;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.core_c,
  ocv.cls.core;

begin
  try
    Writeln('getThreadNum:        ', getThreadNum);
    Writeln('getTickCount:        ', getTickCount);
    Writeln('getTickFrequency:    ', getTickFrequency:1:2);
    Writeln('getCPUTickCount:     ', getCPUTickCount);
    Writeln('checkHardwareSupport:');
    Writeln(' MMX      ', checkHardwareSupport(CV_CPU_MMX));
    Writeln(' SSE      ', checkHardwareSupport(CV_CPU_SSE));
    Writeln(' SSE 2    ', checkHardwareSupport(CV_CPU_SSE2));
    Writeln(' SSE 3    ', checkHardwareSupport(CV_CPU_SSE3));
    Writeln(' SSSE 3   ', checkHardwareSupport(CV_CPU_SSSE3));
    Writeln(' SSE 4.1  ', checkHardwareSupport(CV_CPU_SSE4_1));
    Writeln(' SSE 4.2  ', checkHardwareSupport(CV_CPU_SSE4_2));
    Writeln(' POPCOUNT ', checkHardwareSupport(CV_CPU_POPCNT));
    Writeln(' AVX      ', checkHardwareSupport(CV_CPU_AVX));
    Writeln(' AVX2     ', checkHardwareSupport(CV_CPU_AVX2));
    Writeln('getNumberOfCPUs:     ', getNumberOfCPUs);
    Writeln('useOptimized:        ', useOptimized);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

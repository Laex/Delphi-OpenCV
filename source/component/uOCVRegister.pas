// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// Mikhail Grigorev
// email:sleuthound@gmail.com
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
{$IFNDEF CLR}
{$I OpenCV.inc}
unit uOCVRegister;
{$ENDIF}

interface

procedure Register;

implementation

Uses
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, LResources,
{$ELSE}
{$IFDEF VER6P}DesignIntf, System.Classes, {$ELSE}DsgnIntf, Classes, {$ENDIF VER6P}
{$ENDIF FPC}
{$ENDIF}
  uOCVSource,
  uOCVView,
  uOCVImageOperation;

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvImageOperation, TocvCameraSource, TocvView, TocvFileSource, TocvIPCamSource]);
  RegisterClasses([TocvNoneOperation, TocvGrayScaleOperation, TovcCannyOperation, TovcSmoothOperation, TovcErodeOperation,
    TovcDilateOperation, TocvLaplaceOperation, TovcSobelOperation, TocvThresholdOperation, TocvAdaptiveThresholdOperation,
    TocvContoursOperation, TocvRotateOperation, TocvAbsDiff, TocvHaarCascade, TocvMatchTemplate, TocvMotionDetect,
    TovcCropOperation]);
end;

{$IFDEF FPC}

initialization

{$I OpenCV.lrs}
{$ENDIF FPC}

end.

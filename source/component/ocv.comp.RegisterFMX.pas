// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// Mikhail Grigorev
// email:sleuthhound@gmail.com
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
unit ocv.comp.RegisterFMX;
{$ENDIF}

interface

procedure Register;

implementation

uses
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, LResources,
{$ELSE}
{$IFDEF DELPHI6_UP}DesignIntf, System.Classes, {$ELSE}DsgnIntf, Classes, {$ENDIF}
{$ENDIF FPC}
{$ENDIF}
  ocv.comp.ViewFMX;

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvViewFMX]);
end;

{$IFDEF FPC}

initialization

{$I ocv.lrs}
{$ENDIF FPC}

end.

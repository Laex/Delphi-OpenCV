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
unit ocv.comp.RegisterVCL;
{$ENDIF}

interface

procedure Register;

implementation

{$R OpenCV.dcr}

uses
  Windows,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, LResources,
{$ELSE}
{$IFDEF VER6P}DesignIntf, System.Classes, {$ELSE}DsgnIntf, Classes, {$ENDIF VER6P}
{$ENDIF FPC}
{$ENDIF}
  ocv.comp.View, Graphics, ToolsAPI;

{$IFNDEF FPC}
{$IF DECLARED(IOTAAboutBoxServices)}
const
  OCVVersion = '2.4.9';

resourcestring
  resPackageName = 'Delphi OpenCV Components v' + OCVVersion;
  resAboutDescription = 'Delphi OpenCV Components';
  resAboutURL = 'Web: https://github.com/Laex/Delphi-OpenCV/';
  resAboutCopyright = 'Copyright (c) 2013-2014 Laentir Valetov and Mikhail Grigorev';
  resLicense = 'GNU General Public License (GPL) v3.0';
{$ENDIF}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('OpenCV', [TocvView]);
end;

{$IFNDEF FPC}
{$IF DECLARED(IOTAAboutBoxServices)}
var
  AboutBoxIndex: Integer = -1;

procedure RegisterAboutBox;
begin
  SplashScreenServices.AddPluginBitmap(resPackageName, LoadBitmap(HInstance, 'SPLASH'), False, resLicense);
  AboutBoxIndex := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(
    resPackageName,
    resAboutDescription + sLineBreak +
    resAboutCopyright + sLineBreak +
    resAboutURL,
    LoadBitmap(HInstance, 'ABOUT'), False, resLicense);
end;

procedure UnregisterAboutBox;
begin
  if AboutBoxIndex <> -1 then
    (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(AboutBoxIndex);
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFNDEF FPC}
{$IF DECLARED(IOTAAboutBoxServices)}
  RegisterAboutBox;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  {$I ocv.lrs}
{$ENDIF FPC}

{$IFNDEF FPC}
{$IF DECLARED(IOTAAboutBoxServices)}
finalization
  UnRegisterAboutBox;
{$ENDIF}
{$ENDIF}

end.

// --------------------------------- OpenCV license.txt ---------------------------
(* //    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //    By downloading, copying, installing or using the software you agree to this license.
  //    If you do not agree to this license, do not download, install,
  //    copy or use the software.
  //
  //
  //                             License Agreement
  //                  For Open Source Computer Vision Library
  //
  //   Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  //   Copyright (C) 2009, Willow Garage Inc., all rights reserved.
  //   Third party copyrights are property of their respective owners.
  //
  //   Redistribution and use in source and binary forms, with or without modification,
  //   are permitted provided that the following conditions are met:
  //
  //     * Redistribution's of source code must retain the above copyright notice,
  //       this list of conditions and the following disclaimer.
  //
  //     * Redistribution's in binary form must reproduce the above copyright notice,
  //       this list of conditions and the following disclaimer in the documentation
  //       and/or other materials provided with the distribution.
  //
  //     * The name of the copyright holders may not be used to endorse or promote products
  //       derived from this software without specific prior written permission.
  //
  //   This software is provided by the copyright holders and contributors "as is" and
  //   any express or implied warranties, including, but not limited to, the implied
  //   warranties of merchantability and fitness for a particular purpose are disclaimed.
  //   In no event shall the Intel Corporation or contributors be liable for any direct,
  //   indirect, incidental, special, exemplary, or consequential damages
  //   (including, but not limited to, procurement of substitute goods or services;
  //   loss of use, data, or profits; or business interruption) however caused
  //   and on any theory of liability, whether in contract, strict liability,
  //   or tort (including negligence or otherwise) arising in any way out of
  //   the use of this software, even if advised of the possibility of such damage. *)

(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  laentir Valetov
  //  email:laex@bk.ru
  //  **************************************************************************************************
  //  You may retrieve the latest version of this file at the GitHub,
  //  located at git://github.com/Laex/Delphi-OpenCV.git
  //  **************************************************************************************************
  //  License:
  //  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  //  you may not use this file except in compliance with the License. You may obtain a copy of the
  //  License at http://www.mozilla.org/MPL/
  //
  //  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  //  ANY KIND, either express or implied. See the License for the specific language governing rights
  //  and limitations under the License.
  //
  //  Alternatively, the contents of this file may be used under the terms of the
  //  GNU Lesser General Public License (the  "LGPL License"), in which case the
  //  provisions of the LGPL License are applicable instead of those above.
  //  If you wish to allow use of your version of this file only under the terms
  //  of the LGPL License and not to allow others to use your version of this file
  //  under the MPL, indicate your decision by deleting  the provisions above and
  //  replace  them with the notice and other provisions required by the LGPL
  //  License.  If you do not delete the provisions above, a recipient may use
  //  your version of this file under either the MPL or the LGPL License.
  //
  //  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  //  **************************************************************************************************
  //  Warning: Using Delphi XE3 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  **************************************************************************************************
  //  Original file:
  //  opencv\modules\legacy\src\bgfg_estimation.cpp
  //  ************************************************************************************************* *)

{$IFDEF DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$ELSE}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$ENDIF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$TYPEDADDRESS ON}
unit bgfg_estimation;

interface

Uses blobtrack, legacy, Core.types_c;

// Function cvCreateBGStatModel creates and returns initialized BG model.
// Parameters:
// first_frame   - frame from video sequence
// model_type ñ type of BG model (CV_BG_MODEL_MOG, CV_BG_MODEL_FGD,Ö)
// parameters  - (optional) if NULL the default parameters of the algorithm will be used
function cvCreateBGStatModel(first_frame: pIplImage; model_type: Integer; params: Pointer): pCvBGStatModel;

type
  // * FOREGROUND DETECTOR INTERFACE */
  TCvFGDetectorBase = class(TCvFGDetector)
  protected
    m_pFG: pCvBGStatModel;
    m_FGType: Integer;
    m_pFGParam: Pointer; // * Foreground parameters.
    m_ParamFGD: TCvFGDStatModelParams;
    m_ParamMOG: TCvGaussBGStatModelParams;
    m_SaveName: pCvChar;
    m_LoadName: pCvChar;
  public
    procedure SaveState(FileStorage: pCvFileStorage); override;
    procedure LoadState(FileStorage: pCvFileStorage; FileNode: pCvFileNode); override;
    constructor Create(_type: Integer; param: Pointer);
    destructor Destroy; override;
    procedure ParamUpdate; override;
    function GetMask(): pIplImage; override;
    // * Process current image: */
    procedure Process(pImg: pIplImage); override;
    // * Release foreground detector: */
    procedure Release(); override;
  end;

  // CV_EXPORTS void cvReleaseFGDetector(CvFGDetector** ppT );
procedure cvReleaseFGDetector(var ppT: TCvFGDetector);
// CV_EXPORTS CvFGDetector* cvCreateFGDetectorBase(int type, void *param);
function cvCreateFGDetectorBase(_type: Integer; param: Pointer): TCvFGDetector;

implementation

Uses System.SysUtils;

function cvCreateBGStatModel(first_frame: pIplImage; model_type: Integer; params: Pointer): pCvBGStatModel;
Var
  bg_model: pCvBGStatModel;
begin
  bg_model := nil;
  if (model_type = CV_BG_MODEL_FGD) or (model_type = CV_BG_MODEL_FGD_SIMPLE) then
    bg_model := cvCreateFGDStatModel(first_frame, { (CvFGDStatModelParams*) } params)
  else if (model_type = CV_BG_MODEL_MOG) then
    bg_model := cvCreateGaussianBGModel(first_frame, { (CvGaussBGStatModelParams*) } params);
  Result := bg_model;
end;

procedure cvReleaseFGDetector(var ppT: TCvFGDetector);
begin
  ppT.Release();
  FreeAndNil(ppT);
end;

function cvCreateFGDetectorBase(_type: Integer; param: Pointer): TCvFGDetector;
begin
  Result := TCvFGDetectorBase.Create(_type, param);
end;

{ TCvFGDetectorBase }

constructor TCvFGDetectorBase.Create(_type: Integer; param: Pointer);
Var
  P: PSingle;
begin
  m_pFG := nil;
  m_FGType := _type;
  m_pFGParam := param;
  if (m_FGType = CV_BG_MODEL_FGD) or (m_FGType = CV_BG_MODEL_FGD_SIMPLE) then
  begin
    if Assigned(m_pFGParam) then
    begin
      m_ParamFGD := pCvFGDStatModelParams(m_pFGParam)^;
    end
    else
    begin
      m_ParamFGD.Lc := CV_BGFG_FGD_LC;
      m_ParamFGD.N1c := CV_BGFG_FGD_N1C;
      m_ParamFGD.N2c := CV_BGFG_FGD_N2C;
      m_ParamFGD.Lcc := CV_BGFG_FGD_LCC;
      m_ParamFGD.N1cc := CV_BGFG_FGD_N1CC;
      m_ParamFGD.N2cc := CV_BGFG_FGD_N2CC;
      m_ParamFGD.delta := CV_BGFG_FGD_DELTA;
      m_ParamFGD.alpha1 := CV_BGFG_FGD_ALPHA_1;
      m_ParamFGD.alpha2 := CV_BGFG_FGD_ALPHA_2;
      m_ParamFGD.alpha3 := CV_BGFG_FGD_ALPHA_3;
      m_ParamFGD.T := CV_BGFG_FGD_T;
      m_ParamFGD.minArea := CV_BGFG_FGD_MINAREA;
      m_ParamFGD.is_obj_without_holes := 1;
      m_ParamFGD.perform_morphing := 1;
    end;
    AddParam('LC', @m_ParamFGD.Lc);
    AddParam('alpha1', @m_ParamFGD.alpha1);
    AddParam('alpha2', @m_ParamFGD.alpha2);
    AddParam('alpha3', @m_ParamFGD.alpha3);
    AddParam('N1c', @m_ParamFGD.N1c);
    AddParam('N2c', @m_ParamFGD.N2c);
    AddParam('N1cc', @m_ParamFGD.N1cc);
    AddParam('N2cc', @m_ParamFGD.N2cc);
    m_SaveName := nil;
    m_LoadName := nil;
    AddParam('SaveName', m_SaveName);
    AddParam('LoadName', m_LoadName);
    AddParam('ObjWithoutHoles', @m_ParamFGD.is_obj_without_holes);
    AddParam('Morphology', @m_ParamFGD.perform_morphing);

    SetModuleName('FGD');
  end
  else if (m_FGType = CV_BG_MODEL_MOG) then // 'MOG'=='Mixture Of Gaussians'
  begin
    if Assigned(m_pFGParam) then
    begin
      m_ParamMOG := pCvGaussBGStatModelParams(m_pFGParam)^;
    end
    else
    begin // These constants are all from cvaux/include/cvaux.h
      m_ParamMOG.win_size := CV_BGFG_MOG_WINDOW_SIZE;
      m_ParamMOG.bg_threshold := CV_BGFG_MOG_BACKGROUND_THRESHOLD;

      m_ParamMOG.std_threshold := CV_BGFG_MOG_STD_THRESHOLD;
      m_ParamMOG.weight_init := CV_BGFG_MOG_WEIGHT_INIT;

      m_ParamMOG.variance_init := CV_BGFG_MOG_SIGMA_INIT * CV_BGFG_MOG_SIGMA_INIT;
      m_ParamMOG.minArea := CV_BGFG_MOG_MINAREA;
      m_ParamMOG.n_gauss := CV_BGFG_MOG_NGAUSSIANS;
    end;
    AddParam('NG', @m_ParamMOG.n_gauss);

    SetModuleName('MOG');
  end;
end;

destructor TCvFGDetectorBase.Destroy;
begin
  if Assigned(m_pFG) then
    cvReleaseBGStatModel(m_pFG);
  inherited;
end;

function TCvFGDetectorBase.GetMask: pIplImage;
begin
  Result := iif(Assigned(m_pFG), m_pFG^.foreground, nil);
end;

procedure TCvFGDetectorBase.LoadState(FileStorage: pCvFileStorage; FileNode: pCvFileNode);
begin
  if (m_FGType = CV_BG_MODEL_FGD) or (m_FGType = CV_BG_MODEL_FGD_SIMPLE) then
    if Length(m_LoadName) > 0 then // * File name is not empty */
      // cvRestoreStatModel(m_LoadName, (CvFGDStatModel*)m_pFG);
end;

procedure TCvFGDetectorBase.ParamUpdate;
begin
  if Assigned(m_pFG) then
    cvReleaseBGStatModel(m_pFG);
end;

procedure TCvFGDetectorBase.Process(pImg: pIplImage);
Var
  param: Pointer;
begin
  if (m_pFG = nil) then
  begin
    param := m_pFGParam;
    if (m_FGType = CV_BG_MODEL_FGD) or (m_FGType = CV_BG_MODEL_FGD_SIMPLE) then
    begin
      param := @m_ParamFGD;
    end
    else if (m_FGType = CV_BG_MODEL_MOG) then
    begin
      param := @m_ParamMOG;
    end;
    m_pFG := cvCreateBGStatModel(pImg, m_FGType, param);
    LoadState(0, 0);
  end
  else
  begin
    cvUpdateBGStatModel(pImg, m_pFG);
  end;
end;

procedure TCvFGDetectorBase.Release;
begin
  SaveState(0);
  if Assigned(m_pFG) then
    cvReleaseBGStatModel(m_pFG);
end;

procedure TCvFGDetectorBase.SaveState(FileStorage: pCvFileStorage);
begin
  if (m_FGType = CV_BG_MODEL_FGD) or (m_FGType = CV_BG_MODEL_FGD_SIMPLE) then
    if Length(m_SaveName) > 0 then // * File name is not empty */
      // cvSaveStatModel(m_SaveName, (CvFGDStatModel*)m_pFG);
end;

end.

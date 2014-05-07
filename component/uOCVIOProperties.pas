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

unit uOCVIOProperties;

interface

Uses
  System.Classes,
  DesignEditors,
  DesignIntf;

Type
  TImageOperationProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TImagePreprocessingProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

implementation

Uses
  System.SysUtils,
  System.TypInfo,
  System.RTLConsts,
  uOCVImageOperation,
  uOCVTypes;

{TImageOperationProperty}

function TImageOperationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TImageOperationProperty.GetValue: string;
begin
  Result := GetRegisteredImageOperations.GetNameByClass(TocvImageOperation(GetOrdValue).ClassType);
end;

procedure TImageOperationProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  rIO: TRegisteredImageOperations;
begin
  rIO := GetRegisteredImageOperations;
  for I := 0 to rIO.Count - 1 do
    Proc(rIO[I]);
end;

procedure TImageOperationProperty.SetValue(const Value: string);
Var
  APropertiesClass: TocvImageOperationClass;
  I: Integer;
  AIntf: IocvEditorPropertiesContainer;
begin
  APropertiesClass := GetRegisteredImageOperations.FindByName(Value);
  if APropertiesClass = nil then
    APropertiesClass := TocvImageOperationClass(GetRegisteredImageOperations.Objects[0]);

  for I := 0 to PropCount - 1 do
    if Supports(GetComponent(I), IocvEditorPropertiesContainer, AIntf) then
      AIntf.SetPropertiesClass(APropertiesClass);

  Modified;
end;

{TImagePreprocessingProperty}

function TImagePreprocessingProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TImagePreprocessingProperty.GetValue: string;
begin
  Result := GetRegisteredImageOperations.GetNameByClass(TocvImageOperation(GetOrdValue).ClassType);
end;

procedure TImagePreprocessingProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('None');
  Proc('Threshold');
  Proc('AdaptiveThreshold');
end;

procedure TImagePreprocessingProperty.SetValue(const Value: string);
Var
  APropertiesClass: TocvImageOperationClass;
  I: Integer;
  AIntf: IocvEditorPropertiesContainer;
begin
  APropertiesClass := GetRegisteredImageOperations.FindByName(Value);
  if APropertiesClass = nil then
    APropertiesClass := TocvImageOperationClass(GetRegisteredImageOperations.Objects[0]);

  for I := 0 to PropCount - 1 do
    if Supports(GetComponent(I), IocvEditorPropertiesContainer, AIntf) then
      AIntf.SetPropertiesClass(APropertiesClass);

  Modified;
end;

initialization

RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvImageOperation, 'Operation', TImageOperationProperty);
RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvImageOperationCollectionItem, 'Operation',
  TImageOperationProperty);
RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvContoursOperation, 'Preprocessing', TImagePreprocessingProperty);

UnlistPublishedProperty(TocvImageOperation, 'OperationClassName');
UnlistPublishedProperty(TocvImageOperationCollectionItem, 'OperationClassName');
UnlistPublishedProperty(TocvContoursOperation, 'OperationClassName');

end.

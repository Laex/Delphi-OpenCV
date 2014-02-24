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

  TVideoSourceProperty = class(TInterfaceProperty)
  private
    FGetValuesStrProc: TGetStrProc;
  protected
    procedure ReceiveComponentNames(const S: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

implementation

Uses

  VCL.Dialogs,

  System.SysUtils,
  System.TypInfo,
  System.RTLConsts,
  uOCVImageOperation,
  uOCVSplitter,
  uOCVTypes;

{ TImageOperationProperty }

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
  i: Integer;
  rIO: TRegisteredImageOperations;
begin
  rIO := GetRegisteredImageOperations;
  for i := 0 to rIO.Count - 1 do
    Proc(rIO[i]);
end;

procedure TImageOperationProperty.SetValue(const Value: string);
Var
  APropertiesClass: TocvImageOperationClass;
  i: Integer;
  AIntf: IocvEditorPropertiesContainer;
begin
  APropertiesClass := GetRegisteredImageOperations.FindByClassName(Value);
  if APropertiesClass = nil then
    APropertiesClass := TocvImageOperationClass(GetRegisteredImageOperations.Objects[0]);
  for i := 0 to PropCount - 1 do
    if Supports(GetComponent(i), IocvEditorPropertiesContainer, AIntf) then
      AIntf.SetPropertiesClass(APropertiesClass);
  Modified;
end;

{ TVideoSourceProperty }

function TVideoSourceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable];
end;

function TVideoSourceProperty.GetValue: string;
Var
  AInterface: IInterface;
  ICR: IocvDataSource;
begin
  AInterface := GetIntfValue;
  if (AInterface <> nil) and Supports(AInterface, IocvDataSource, ICR) then
    Result := ICR.GetName
  else
    Result := '';
end;

procedure TVideoSourceProperty.GetValues(Proc: TGetStrProc);
begin
  FGetValuesStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TypeInfo(TComponent)), ReceiveComponentNames);
  finally
    FGetValuesStrProc := nil;
  end;
end;

procedure TVideoSourceProperty.ReceiveComponentNames(const S: string);
var
  Temp: TComponent;
  Intf: IInterface;
  i: Integer;
begin
  Temp := Designer.GetComponent(S);
  if Assigned(FGetValuesStrProc) and Assigned(Temp) then
  begin
    if Supports(TObject(Temp), GetTypeData(GetPropType)^.Guid, Intf) then
      FGetValuesStrProc(S);
    if not HasInstance(Temp) then
    if Temp is TocvSplitter then
      for i := 0 to (Temp as TocvSplitter).Channels.Count - 1 do
        FGetValuesStrProc(S + '[' + i.ToString + ']');
  end;
end;

procedure TVideoSourceProperty.SetValue(const Value: string);
var
  Intf: IInterface;
  Component: TComponent;
  CompName: string;
  n1, n2, ChanIndex: Integer;
  SpComp: TocvSplitter;
begin
  if Value = '' then
    Intf := nil
  else
  begin
    if Pos('[', Value) <> 0 then
    begin
      CompName := Copy(Value, 1, Pos('[', Value) - 1);
      n1 := Pos('[', Value);
      n2 := Pos(']', Value);
      ChanIndex := Copy(Value, n1 + 1, n2 - n1 - 1).ToInteger;
      SpComp := Designer.GetComponent(CompName) as TocvSplitter;
      if Assigned(SpComp) and (ChanIndex < SpComp.Channels.Count) then
        Intf := SpComp.Channels[ChanIndex]
      else
        raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
    end
    else
    begin
      Component := Designer.GetComponent(Value);
      if (Component = nil) or (not Supports(TObject(Component), GetTypeData(GetPropType)^.Guid, Intf)) then
        raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
    end;
  end;
  SetIntfValue(Intf);
end;

initialization

RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvImageOperation, 'Properties', TImageOperationProperty);
RegisterPropertyEditor(TypeInfo(string), TocvImageOperation, 'PropertiesClassName', nil);

RegisterPropertyEditor(TypeInfo(IocvDataSource), nil, 'VideoSource', TVideoSourceProperty);

end.

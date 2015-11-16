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

{$I OpenCV.inc}

{$IFNDEF CLR}
unit ocv.comp.IOProperties;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, LResources,
{$ELSE}
{$IFDEF HAS_UNITSCOPE}
  System.Classes,
{$ELSE ~HAS_UNITSCOPE}
  Classes,
{$ENDIF ~HAS_UNITSCOPE}
{$IFDEF DELPHI6_UP}DesignEditors, DesignIntf,{$ELSE}DsgnIntf,{$ENDIF}
{$ENDIF FPC}
{$ENDIF}
  ocv.comp.ImageOperation;

type
  /// Используется в TocvImageOperation
  TImageOperationProperty = class(TComponentProperty)
  private
    FInstance: TPersistent;
  protected
    function GetInstance: TPersistent; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Initialize; override;
    property Instance: TPersistent read GetInstance;
  end;

  /// Используется в TocvImageOperationCollectionItem
  TImageOperationCollectionItemProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  /// Используется в TocvContourOperation для Preprocessing
  TImageContourPrepProperty = class(TImageOperationCollectionItemProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// Используется в TocvMotionDetectOperation для Threshold
  TImageMotionDetectThresholdProperty = class(TImageOperationCollectionItemProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TocvIOPropertyChangeEvent = procedure(Sender: TObject; const PropName: string) of object;

  TocvCustomImageOperationProperty = class(TocvCustomImageOperation)
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnChangingProperty: TocvIOPropertyChangeEvent;
    FOnChangedProperty: TocvIOPropertyChangeEvent;
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure ChangedProperty(const PropName: string); virtual;
    procedure ChangingProperty(const PropName: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChangedProperty: TocvIOPropertyChangeEvent read FOnChangedProperty write FOnChangedProperty;
    property OnChangingProperty: TocvIOPropertyChangeEvent read FOnChangingProperty write FOnChangingProperty;
  end;

implementation

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.TypInfo,
  System.RTLConsts,
{$ELSE}
  SysUtils,
  TypInfo,
  RTLConsts,
{$ENDIF}
  ocv.comp.Types;

{TImageOperationProperty}

function TImageOperationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paSubProperties, paVolatileSubProperties];
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

{TImageContourPrepProperty}

procedure TImageContourPrepProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('None');
  Proc('Threshold');
  Proc('AdaptiveThreshold');
end;

{TocvCustomImageOperationProperty}

procedure TocvCustomImageOperationProperty.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TocvCustomImageOperationProperty.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TocvCustomImageOperationProperty.ChangedProperty(const PropName: string);
begin
  if Assigned(FOnChangedProperty) then
    FOnChangedProperty(Self, PropName);
end;

procedure TocvCustomImageOperationProperty.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TocvCustomImageOperationProperty.ChangingProperty(const PropName: string);
begin
  if Assigned(FOnChangingProperty) then
    FOnChangingProperty(Self, PropName);
end;

procedure TocvCustomImageOperationProperty.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TocvCustomImageOperationProperty.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

function TImageOperationProperty.GetInstance: TPersistent;
var
  LInstance: TPersistent;
  LPersistentPropertyName: string;
begin
  if not Assigned(FInstance) then
  begin
    LInstance := GetComponent(0);
    LPersistentPropertyName := GetName;
    if IsPublishedProp(LInstance, LPersistentPropertyName) then
    begin
      FInstance := TPersistent(GetObjectProp(LInstance, LPersistentPropertyName));
    end;
  end;
  Result := FInstance;
end;

procedure TImageOperationProperty.GetProperties(Proc: TGetPropProc);
begin
  inherited;
end;

procedure TImageOperationProperty.Initialize;
var
  LInstance: TPersistent;
  LPersistentPropertyName: string;
begin
  inherited Initialize;
  LInstance := Instance;
  LPersistentPropertyName := GetName;
  if LInstance is TComponent then
  begin
    if (TComponent(LInstance).Name = '') and (TComponent(LInstance).Name <> LPersistentPropertyName) then
    begin
      TComponent(LInstance).Name := LPersistentPropertyName;
    end;
  end
  else if LInstance is TocvCustomImageOperation then
  begin
    if (TocvCustomImageOperation(LInstance).Name = '') and (TocvCustomImageOperation(LInstance).Name <> LPersistentPropertyName)
    then
    begin
      TocvCustomImageOperation(LInstance).Name := LPersistentPropertyName;
    end;
  end;
end;

{TImageOperationCollectionItemProperty}

function TImageOperationCollectionItemProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TImageOperationCollectionItemProperty.GetValue: string;
begin
  Result := GetRegisteredImageOperations.GetNameByClass(TocvImageOperation(GetOrdValue).ClassType);
end;

procedure TImageOperationCollectionItemProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  rIO: TRegisteredImageOperations;
begin
  rIO := GetRegisteredImageOperations;
  for I := 0 to rIO.Count - 1 do
    Proc(rIO[I]);
end;

procedure TImageOperationCollectionItemProperty.SetValue(const Value: string);
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

{TImageMotionDetectThresholdProperty}

procedure TImageMotionDetectThresholdProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('Threshold');
  Proc('AdaptiveThreshold');
end;

initialization

RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvImageOperation, 'Operation', TImageOperationProperty);
RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvImageOperationCollectionItem, 'Operation',
  TImageOperationCollectionItemProperty);
RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvContoursOperation, 'Preprocessing', TImageContourPrepProperty);
RegisterPropertyEditor(TypeInfo(TocvCustomImageOperation), TocvMotionDetect, 'Threshold', TImageMotionDetectThresholdProperty);

UnlistPublishedProperty(TocvCustomImageOperation, 'Name');
UnlistPublishedProperty(TocvImageOperation, 'OperationClassName');
UnlistPublishedProperty(TocvImageOperationCollectionItem, 'OperationClassName');
UnlistPublishedProperty(TocvContoursOperation, 'OperationClassName');
UnlistPublishedProperty(TocvMotionDetect, 'OperationClassName');

end.

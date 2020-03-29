// ***************************************************************************
//
// SQLCollection
//
// Copyright (c) 2020 João Antônio Duarte
//
// https://github.com/joaoduarte19/sqlcollection
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit SQLCollection.Design.Register;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  VCLEditors,
  DesignEditors,
  DesignIntf,
  SQLCollection.Core;

type
  TSQLCollectionComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;

    class procedure ShowSQLCollectionEditor(ADesign: IDesigner; ASQLCollection: TSQLCollection);
  end;

  TSQLCollectionItemsEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  SQLCollection.Design.Editor;

procedure Register;
begin
  RegisterComponentEditor(TSQLCollection, TSQLCollectionComponentEditor);
  RegisterPropertyEditor(TypeInfo(TSQLCategories), nil, '', TSQLCollectionItemsEditor);
end;

{ TSQLCollectionComponentEditor }

procedure TSQLCollectionComponentEditor.ExecuteVerb(Index: integer);
begin
  ShowSQLCollectionEditor(Designer, TSQLCollection(Component));
end;

function TSQLCollectionComponentEditor.GetVerb(Index: integer): string;
begin
  case index of
    0:
      Result := 'Edit SQLCollection...';
  end;
end;

function TSQLCollectionComponentEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

class procedure TSQLCollectionComponentEditor.ShowSQLCollectionEditor(ADesign: IDesigner;
  ASQLCollection: TSQLCollection);
var
  LSQLColEditor: TSQLCollectionEditor;
begin
  for LSQLColEditor in GSQLCollectionEditorList do
  begin
    if LSQLColEditor.SQLCollection = ASQLCollection then
      try
        LSQLColEditor.Show;
        LSQLColEditor.BringToFront;
        if LSQLColEditor.WindowState <> wsNormal then
          LSQLColEditor.WindowState := wsNormal;
        LSQLColEditor.SetFocus;
        Exit;
      except
      end;
  end;

  { SQlCollection.Design.Editor }
  LSQLColEditor := TSQLCollectionEditor.Create(Application);
  try
    LSQLColEditor.SQLCollection := ASQLCollection;
    LSQLColEditor.Designer := ADesign;
    LSQLColEditor.Caption := ASQLCollection.Owner.Name + '.' + ASQLCollection.Name;
    LSQLColEditor.Show;
  except
    if Assigned(LSQLColEditor) then
      LSQLColEditor.Release;
  end;
end;

{ TSQLCollectionItemsEditor }

procedure TSQLCollectionItemsEditor.Edit;
begin
  inherited;
  TSQLCollectionComponentEditor.ShowSQLCollectionEditor(Designer, TSQLCollection(Self.GetComponent(0)));
end;

function TSQLCollectionItemsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TSQLCollectionItemsEditor.GetValue: string;
begin
  Result := 'Edit Items...';
end;

end.

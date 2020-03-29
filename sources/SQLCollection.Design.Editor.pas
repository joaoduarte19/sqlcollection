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

unit SQLCollection.Design.Editor;

interface

uses
  ToolsAPI,
  DesignIntf,
  DesignEditors,
  DesignWindows,
  SQLCollection.Core,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  System.Generics.Collections,
  System.Actions,
  Vcl.ActnList;

type
  TSQLCollectionEditor = class(TDesignWindow)
    tlbMain: TToolBar;
    statbar: TStatusBar;
    pnHeader: TPanel;
    pnFooter: TPanel;
    lstCategories: TListBox;
    splCatItems: TSplitter;
    lstItems: TListBox;
    actlst1: TActionList;
    procedure lstCategoriesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSQLCollection: TSQLCollection;
    function GetConfigFileName: string;
    procedure FillCategories;
  public
    procedure SelectItem(const ASQLItem: TSQLItem; const AFocusCtrl: Boolean = True);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;

    property SQLCollection: TSQLCollection read FSQLCollection write FSQLCollection;
  end;

var
  GSQLCollectionEditorList: TList<TSQLCollectionEditor>;

implementation

uses
  System.Win.Registry,
  System.IOUtils;

{$R *.dfm}


{ TSQLCollectionEditor }

procedure TSQLCollectionEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if (Self.Designer = ADesigner) then
  begin
    FSQLCollection := nil;
    Self.Release;
  end;

  inherited DesignerClosed(ADesigner, AGoingDormant);
end;

procedure TSQLCollectionEditor.FillCategories;
var
  I: Integer;
  LIndex: Integer;
  LSQLCategory: TSQLCategory;
begin
  if not Assigned(FSQLCollection) then
    Exit;

  LIndex := - 1;
  if lstCategories.Items.Count > 0 then
    LIndex := lstCategories.ItemIndex;

  lstCategories.Items.BeginUpdate;
  try
    lstCategories.Clear;
    for I := 0 to Pred(FSQLCollection.Items.Count) do
    begin
      LSQLCategory := FSQLCollection.Items.SQLCategory[I];
      lstCategories.Items.AddObject(LSQLCategory.Name, LSQLCategory);
    end;

    if lstCategories.Items.Count > 0 then
    begin
      if (LIndex >= 0) and (LIndex <= (lstCategories.Items.Count - 1)) then
        lstCategories.ItemIndex := LIndex
      else
        lstCategories.ItemIndex := 0;

      lstCategoriesClick(lstCategories);
    end;
  finally
    lstCategories.Items.EndUpdate;
  end;
end;

procedure TSQLCollectionEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LReg: TRegIniFile;
begin
  LReg := TRegIniFile.Create(GetConfigFileName);
  try
    SaveFormPos(LReg, 'EditorPos', Self);
    LReg.WriteInteger('EditorConfigs', 'CategoriesWidth', lstCategories.Width);
  finally
    LReg.Free;
  end;
end;

procedure TSQLCollectionEditor.FormShow(Sender: TObject);
var
  LReg: TRegIniFile;
begin
  LReg := TRegIniFile.Create(GetConfigFileName);
  try
    LoadFormPos(LReg, 'EditorPos', Self);
    lstCategories.Width := LReg.ReadInteger('EditorConfigs', 'CategoriesWidth', lstCategories.Width);
  finally
    LReg.Free;
  end;

  FillCategories;
end;

function TSQLCollectionEditor.GetConfigFileName: string;
begin
  Result := 'Software\SQLCollectionEditor';
end;

procedure TSQLCollectionEditor.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  inherited ItemDeleted(ADesigner, AItem);
  if AItem = FSQLCollection then
  begin
    FSQLCollection := nil;
    Self.Release;
  end;
end;

procedure TSQLCollectionEditor.lstCategoriesClick(Sender: TObject);
var
  I: Integer;
  LSelectedObject: TObject;
  LSQLCategory: TSQLCategory;
  LComponentList: IDesignerSelections;
begin
  if lstCategories.ItemIndex < 0 then
    Exit;

  LSelectedObject := nil;
  if lstItems.Items.Count > 0 then
    LSelectedObject := lstItems.Items.Objects[lstItems.ItemIndex];

  {$IFNDEF TestMode}
  LComponentList := TDesignerSelections.Create;
  {$ENDIF}
  LSQLCategory := lstCategories.Items.Objects[lstCategories.ItemIndex] as TSQLCategory;

  if LSQLCategory <> nil then
  begin
    {$IFNDEF TestMode}
    LComponentList.Add(LSQLCategory);
    {$ENDIF}
    lstItems.Clear;
    lstItems.Items.BeginUpdate;
    try
      for I := 0 to Pred(LSQLCategory.SQLItems.Count) do
        lstItems.Items.AddObject(LSQLCategory.SQLItems.SQLItem[I].Name, LSQLCategory.SQLItems.SQLItem[I]);

      if lstItems.Count > 0 then
        if (LSelectedObject <> nil) and (lstItems.Items.IndexOfObject(LSelectedObject) >= 0) then
          lstItems.ItemIndex := lstItems.Items.IndexOfObject(LSelectedObject)
        else
          lstItems.ItemIndex := 0;

      if Assigned(lstItems.OnClick) then
        lstItems.OnClick(lstItems);
    finally
      lstItems.Items.EndUpdate;
    end;
  end;

  {$IFNDEF TestMode}
  if LComponentList.Count = 0 then
    LComponentList.Add(FSQLCollection);
  Designer.SetSelections(LComponentList);
  {$ENDIF}
end;

procedure TSQLCollectionEditor.SelectItem(const ASQLItem: TSQLItem; const AFocusCtrl: Boolean);
begin
  if not (csDesigning in FSQLCollection.ComponentState) then
    Exit;

  if AFocusCtrl then
    FillCategories;

  lstCategories.ItemIndex := lstCategories.Items.IndexOf(ASQLItem.Category);
  if Assigned(lstCategories.OnClick) then
    lstCategories.OnClick(lstCategories);

  lstItems.ItemIndex := lstItems.Items.IndexOfObject(ASQLItem);
  lstItems.Selected[lstItems.ItemIndex] := True;

  if AFocusCtrl then
    lstItems.SetFocus;
end;

initialization

GSQLCollectionEditorList := TList<TSQLCollectionEditor>.Create;

finalization

if Assigned(GSQLCollectionEditorList) then
  FreeAndNIl(GSQLCollectionEditorList);

end.

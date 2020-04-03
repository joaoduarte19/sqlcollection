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
  Vcl.ActnList,
  Vcl.Buttons,
  SQLCollection.Design.SQLEditor,
  Vcl.Menus,
  SQLCollection.Design.JsonDataObjects;

type
  TSQLCollectionEditor = class(TDesignWindow)
    tlbMain: TToolBar;
    statbar: TStatusBar;
    pnHeader: TPanel;
    pnFooter: TPanel;
    actlst1: TActionList;
    cbxSearch: TComboBox;
    lbSearch: TLabel;
    btnSearch: TSpeedButton;
    actSQLCategoryAdd: TAction;
    actSQLCategoryRemove: TAction;
    actSQLItemAdd: TAction;
    actSQLItemRemove: TAction;
    actSearchItems: TAction;
    pnItems: TPanel;
    splCatItems: TSplitter;
    lstCategories: TListBox;
    lstItems: TListBox;
    splFooter: TSplitter;
    tvItems: TTreeView;
    pn1: TPanel;
    btnCloseSearchResults: TSpeedButton;
    pmSQLItems: TPopupMenu;
    mniAddSQLItem: TMenuItem;
    pmSQLCategories: TPopupMenu;
    actSQLItemCopyStructure: TAction;
    actSQLItemCopyLink: TAction;
    actSQLItemPaste: TAction;
    actSQLItemCut: TAction;
    actSQLItemSelectAll: TAction;
    actSQLItemRename: TAction;
    actSQLItemChangeCategory: TAction;
    mniSQLItemRemove: TMenuItem;
    mniSQLItemCopyLink: TMenuItem;
    mniSQLItemCopyStructure: TMenuItem;
    mniSQLItemPaste: TMenuItem;
    mniSQLItemCut: TMenuItem;
    mniSQLItemSelectAll: TMenuItem;
    mniSQLItemRename: TMenuItem;
    mniN1: TMenuItem;
    mniN2: TMenuItem;
    mniSQLItemChangeCategory: TMenuItem;
    mniN3: TMenuItem;
    mniAddCategory: TMenuItem;
    mniRemoveCategory: TMenuItem;
    actSQLItemEdit: TAction;
    mniSQLItemEdit: TMenuItem;
    mniN4: TMenuItem;
    procedure lstCategoriesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSearchItemsExecute(Sender: TObject);
    procedure btnCloseSearchResultsClick(Sender: TObject);
    procedure lstItemsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actSQLCategoryAddExecute(Sender: TObject);
    procedure actSQLItemAddExecute(Sender: TObject);
    procedure lstCategoriesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lstCategoriesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure actSQLCategoryRemoveExecute(Sender: TObject);
    procedure actSQLItemRemoveExecute(Sender: TObject);
    procedure actSQLItemRenameExecute(Sender: TObject);
    procedure actSQLItemChangeCategoryExecute(Sender: TObject);
    procedure actSQLItemSelectAllExecute(Sender: TObject);
    procedure actSQLItemCopyLinkExecute(Sender: TObject);
    procedure actSQLItemCopyStructureExecute(Sender: TObject);
    procedure actSQLItemCutExecute(Sender: TObject);
    procedure actSQLItemPasteExecute(Sender: TObject);
    procedure actSQLItemEditExecute(Sender: TObject);
  private
    FSQLCollection: TSQLCollection;
    FSQLEditor: TSQLEditor;
    function GetConfigFileName: string;
    procedure FillCategories;
    procedure SQLItemRemove;
    procedure SQLItemsToJsonArray(const ASQLItems: TList<TSQLItem>; AJsonArray: TJDOJsonArray);
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
  System.IOUtils,
  System.RegularExpressions,
  Vcl.Clipbrd;

{$R *.dfm}

{ TSQLCollectionEditor }

procedure TSQLCollectionEditor.actSQLCategoryAddExecute(Sender: TObject);
var
  LCategory: string;
begin
  if not InputQuery('Insert Category', 'Enter category: ', LCategory) then
    Exit;

  FSQLCollection.Items.Add(LCategory);
  FillCategories;
  if Assigned(Designer) then
    Designer.Modified;

  lstCategories.ItemIndex := lstCategories.Items.IndexOf(LCategory);
  if lstCategories.ItemIndex >= 0 then
  begin
    lstCategories.Selected[lstCategories.ItemIndex] := True;
    lstCategoriesClick(lstCategories);
  end;
end;

procedure TSQLCollectionEditor.actSQLItemAddExecute(Sender: TObject);
var
  LSQLCategory: TSQLCategory;
  I: Integer;
  LStr: string;
begin
  if lstCategories.ItemIndex >= 0 then
    LSQLCategory := lstCategories.Items.Objects[lstCategories.ItemIndex] as TSQLCategory
  else
  begin
    if not InputQuery('Insert SQLCategory', 'Enter SQLCategory name: ', LStr) then
      Exit;

    LSQLCategory := FSQLCollection.Items.Add(LStr);
    FillCategories;
  end;

  I := 0;
  repeat
    Inc(I);
    LStr := Format('SQL%d', [I]);
  until (LSQLCategory.SQLItems[LStr] = nil);

  if not InputQuery('Insert SQLItem', 'Enter SQLItem name', LStr) then
    Exit;

  if LSQLCategory.SQLItems[LStr] = nil then
  begin
    LSQLCategory.SQLItems.Add(LStr);
    FillCategories;
    if Assigned(Designer) then
      Designer.Modified;

  end
  else
    ShowMessage('An SQLItem with this name already exists.');
end;

procedure TSQLCollectionEditor.actSQLItemChangeCategoryExecute(Sender: TObject);
var
  LSQLItem: TSQLItem;
  LName: string;
  LSQLCategory: TSQLCategory;
  I: Integer;
begin
  if lstItems.ItemIndex < 0 then
    Exit;

  if not InputQuery('Change Category', 'Enter new category: ', LName) then
    Exit;

  LSQLCategory := FSQLCollection.Items[LName];
  if not Assigned(LSQLCategory) then
    FSQLCollection.Items.Add(LName);

  LSQLItem := nil;
  for I := Pred(lstItems.Count) to 0 do
  begin
    if lstItems.Selected[I] then
    begin
      LSQLItem := lstItems.Items.Objects[I] as TSQLItem;
      LSQLItem.Collection := LSQLCategory.SQLItems;
    end;
  end;

  FillCategories;

  if Assigned(LSQLItem) then
    try
      lstCategories.ItemIndex := lstCategories.Items.IndexOfObject(LSQLCategory);
      lstCategories.Selected[lstCategories.ItemIndex] := True;
      lstCategoriesClick(lstCategories);
      lstItems.ItemIndex := lstItems.Items.IndexOfObject(LSQLItem);
      lstItems.Selected[lstItems.ItemIndex] := True;
    except
    end;

  if Assigned(Designer) then
    Designer.Modified;
end;

procedure TSQLCollectionEditor.actSQLItemCopyLinkExecute(Sender: TObject);
var
  LSQLItem: TSQLItem;
begin
  if lstItems.ItemIndex < 0 then
    Exit;

  LSQLItem := lstItems.Items.Objects[lstItems.ItemIndex] as TSQLItem;

  Clipboard.Open;
  try
    Clipboard.AsText := Format('%s.%s.FindSQL(''%s'', ''%s'');',
      [FSQLCollection.Owner.Name, FSQLCollection.Name, LSQLItem.Name, LSQLItem.Category]);
  finally
    Clipboard.Close;
  end;
end;

procedure TSQLCollectionEditor.actSQLItemCopyStructureExecute(Sender: TObject);
var
  I: Integer;
  LSQLItem: TSQLItem;
  LSQLItemList: TList<TSQLItem>;
  LJsonArray: TJDOJsonArray;
begin
  LSQLItemList := TList<TSQLItem>.Create;
  try
    for I := 0 to Pred(lstItems.Count) do
      if lstItems.Selected[I] then
      begin
        LSQLItem := lstItems.Items.Objects[I] as TSQLItem;
        LSQLItemList.Add(LSQLItem);
      end;
    LJsonArray := TJDOJsonArray.Create;
    try
      SQLItemsToJsonArray(LSQLItemList, LJsonArray);

      Clipboard.Open;
      try
        Clipboard.AsText := LJsonArray.ToJSON(False);
      finally
        Clipboard.Close;
      end;
    finally
      FreeAndNil(LJsonArray);
    end;
  finally
    FreeAndNil(LSQLItemList);
  end;
end;

procedure TSQLCollectionEditor.actSQLItemCutExecute(Sender: TObject);
begin
  actSQLItemCopyStructure.Execute;
  SQLItemRemove;
end;

procedure TSQLCollectionEditor.actSQLItemEditExecute(Sender: TObject);
var
  I: Integer;
begin
  if lstItems.ItemIndex < 0 then
    Exit;

  { SQLCollection.Design.SQLEditor }
  if not Assigned(FSQLEditor) then
    FSQLEditor := TSQLEditor.Create(Self);

  for I := 0 to Pred(lstItems.Items.Count) do
    if lstItems.Selected[I] then
      FSQLEditor.OpenSQLEditor(TSQLItem(lstItems.Items.Objects[I]));

  if FSQLEditor.WindowState <> wsNormal then
    FSQLEditor.WindowState := wsNormal;

  FSQLEditor.Show;
end;

procedure TSQLCollectionEditor.actSQLItemPasteExecute(Sender: TObject);
var
  LClipBoardStr: string;
  LJsonArray: TJDOJsonArray;
  I: Integer;
  LJsonObject: TJDOJsonObject;
  LSQLItem: TSQLItem;
  LSQLCategory: TSQLCategory;
begin
  Clipboard.Open;
  try
    LClipBoardStr := Clipboard.AsText;
  finally
    Clipboard.Close;
  end;

  if Trim(LClipBoardStr) = '' then
    Exit;

  LSQLItem := nil;
  LSQLCategory := nil;

  LJsonArray := TJDOJsonArray.Create;
  try
    LJsonArray.FromJSON(LClipBoardStr);
    for I := 0 to Pred(LJsonArray.Count) do
    begin
      LJsonObject := LJsonArray.O[I];
      LSQLItem := FSQLCollection.FindSQLItem(LJsonObject.S['SQLItemName'], LJsonObject.S['SQLCategoryName']);
      if Assigned(LSQLItem) then
      begin
        LSQLItem.SQL.Text := LJsonObject.S['SQL'];
      end
      else
      begin
        LSQLCategory := FSQLCollection.Items[LJsonObject.S['SQLCategoryName']];
        if not Assigned(LSQLCategory) then
          LSQLCategory := FSQLCollection.Items.Add(LJsonObject.S['SQLCategoryName']);

        LSQLItem := LSQLCategory.SQLItems.Add(LJsonObject.S['SQLItemName']);
        LSQLItem.SQL.Text := LJsonObject.S['SQL'];
      end;
    end;

    FillCategories;

    if Assigned(LSQLCategory) and Assigned(LSQLItem) then
    begin
      lstCategories.ItemIndex := lstCategories.Items.IndexOfObject(LSQLCategory);
      lstCategories.Selected[lstCategories.ItemIndex] := True;
      lstCategoriesClick(lstCategories);
      lstItems.ItemIndex := lstItems.Items.IndexOfObject(LSQLItem);
      lstItems.Selected[lstItems.ItemIndex] := True;
    end;


    if Assigned(Designer) then
      Designer.Modified;


  finally
    FreeAndNil(LJsonArray);
  end;
end;

procedure TSQLCollectionEditor.actSQLCategoryRemoveExecute(Sender: TObject);
var
  LSQLCategory: TSQLCategory;
begin
  if lstCategories.ItemIndex < 0 then
    Exit;

  LSQLCategory := lstCategories.Items.Objects[lstCategories.ItemIndex] as TSQLCategory;

  if MessageDlg('Do you really want to delete this category?', mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes then
    Exit;

  FreeAndNil(LSQLCategory);
  FillCategories;
  if Assigned(Designer) then
    Designer.Modified;

end;

procedure TSQLCollectionEditor.actSQLItemRemoveExecute(Sender: TObject);
begin
  if lstItems.ItemIndex < 0 then
    Exit;

  if MessageDlg('Do you really want to delete this SQLItem?', mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes then
    Exit;

  SQLItemRemove;
end;

procedure TSQLCollectionEditor.actSQLItemRenameExecute(Sender: TObject);
var
  LSQLItem: TSQLItem;
  LName: string;
begin
  if lstItems.ItemIndex < 0 then
    Exit;

  LSQLItem := lstItems.Items.Objects[lstItems.ItemIndex] as TSQLItem;

  if not InputQuery('Rename SQLItem', 'Enter new name: ', LName) then
    Exit;

  if SameText(LName, LSQLItem.Name) then
    Exit;

  if Assigned((LSQLItem.Collection as TSQLItems)[LName]) then
  begin
    MessageDlg(Format('SQLItem %s already exists', [LName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  LSQLItem.Name := LName;
  FillCategories;
  if Assigned(Designer) then
    Designer.Modified;
end;

procedure TSQLCollectionEditor.actSQLItemSelectAllExecute(Sender: TObject);
begin
  lstItems.SelectAll;
end;

procedure TSQLCollectionEditor.actSearchItemsExecute(Sender: TObject);
var
  LSearchText: string;
  I: Integer;
  LMatches: TMatchCollection;
  LSQLItemName: string;
  LSQLCategoryName: string;
  LSQLItem: TSQLItem;
begin
  if cbxSearch.Text = '' then
    Exit;

  LSearchText := cbxSearch.Text;

  I := cbxSearch.Items.IndexOf(LSearchText);
  if I < 0 then
    cbxSearch.Items.Insert(0, LSearchText)
  else
    cbxSearch.Items.Move(I, 0);

  LMatches := TRegex.Matches(LSearchText, #39'([^'#39']*)'#39);
  if LMatches.Count = 2 then
  begin
    LSQLItemName := LMatches[0].Groups[0].Value;
    LSQLCategoryName := LMatches[1].Groups[0].Value;

    LSQLItem := FSQLCollection.FindSQLItem(LSQLItemName, LSQLCategoryName);

    if Assigned(LSQLItem) then
    begin
      lstCategories.ItemIndex := lstCategories.Items.IndexOf(LSQLCategoryName);
      lstCategoriesClick(lstCategories);
      lstItems.ItemIndex := lstItems.Items.IndexOfObject(LSQLItem);
      lstItems.Selected[lstItems.ItemIndex] := true;
      Exit;
    end;
  end;
end;

procedure TSQLCollectionEditor.btnCloseSearchResultsClick(Sender: TObject);
begin
  pnFooter.Height := 1;
end;

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
    LReg.WriteString('EditorConfigs', 'SearchText', cbxSearch.Items.Text);
  finally
    LReg.Free;
  end;
end;

procedure TSQLCollectionEditor.FormCreate(Sender: TObject);
begin
  if Assigned(GSQLCollectionEditorList) then
    GSQLCollectionEditorList.Add(Self);
end;

procedure TSQLCollectionEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(GSQLCollectionEditorList) then
    GSQLCollectionEditorList.Remove(Self);
end;

procedure TSQLCollectionEditor.FormShow(Sender: TObject);
var
  LReg: TRegIniFile;
begin
  LReg := TRegIniFile.Create(GetConfigFileName);
  try
    LoadFormPos(LReg, 'EditorPos', Self);
    lstCategories.Width := LReg.ReadInteger('EditorConfigs', 'CategoriesWidth', lstCategories.Width);
    cbxSearch.Items.Text := LReg.ReadString('EditorConfigs', 'SearchText', '');
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

  LComponentList := TDesignerSelections.Create;
  LSQLCategory := lstCategories.Items.Objects[lstCategories.ItemIndex] as TSQLCategory;

  if LSQLCategory <> nil then
  begin
    LComponentList.Add(LSQLCategory);
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

  if LComponentList.Count = 0 then
    LComponentList.Add(FSQLCollection);
  if Assigned(Designer) then
    Designer.SetSelections(LComponentList);
end;

procedure TSQLCollectionEditor.lstCategoriesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: Integer;
  LIndex: Integer;
  LSQLCategory: TSQLCategory;
  LAccept: Boolean;
begin
  LIndex := lstCategories.ItemAtPos(Point(X, Y), True);

  LAccept := (Source = lstItems) and
    (LIndex >= 0) and
    (LIndex < lstCategories.Items.Count) and
    (LIndex <> lstCategories.ItemIndex);

  if not LAccept then
    Exit;

  LSQLCategory := lstCategories.Items.Objects[LIndex] as TSQLCategory;

  for I := 0 to lstItems.Items.Count - 1 do
  begin
    if lstItems.Selected[I] then
    begin
      (lstItems.Items.Objects[I] as TSQLItem).Collection := LSQLCategory.SQLItems;
    end;
  end;

  FillCategories;

  lstCategories.ItemIndex := LIndex;
  lstCategoriesClick(lstCategories);
end;

procedure TSQLCollectionEditor.lstCategoriesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  LIndex: Integer;
begin
  LIndex := lstCategories.ItemAtPos(Point(X, Y), true);

  Accept := (Source = lstItems) and
    (LIndex >= 0) and
    (LIndex < lstCategories.Items.Count) and
    (LIndex <> lstCategories.ItemIndex);
end;

procedure TSQLCollectionEditor.lstItemsDblClick(Sender: TObject);
begin
  actSQLItemEdit.Execute;
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

procedure TSQLCollectionEditor.SQLItemRemove;
var
  LSQLItem: TSQLItem;
  I: Integer;
  LHasItemDeleted: Boolean;
begin
  LHasItemDeleted := False;
  for I := Pred(lstItems.Items.Count) downto 0 do
    if lstItems.Selected[I] then
    begin
      LSQLItem := TSQLItem(lstItems.Items.Objects[I]);
      lstItems.Items.Objects[I] := nil;
      FreeAndNil(LSQLItem);
      lstItems.Items.Delete(I);
      LHasItemDeleted := True;
    end;

  if (lstItems.Items.Count > 0) then
    LSQLItem := TSQLItem(lstItems.Items.Objects[Pred(lstItems.Items.Count)]);

  if LHasItemDeleted then
  begin
    if (lstItems.Items.Count > 0) then
      LSQLItem := TSQLItem(lstItems.Items.Objects[Pred(lstItems.Items.Count)]);

    FillCategories;
    if Assigned(Designer) then
      Designer.Modified;
  end;

end;

procedure TSQLCollectionEditor.SQLItemsToJsonArray(const ASQLItems: TList<TSQLItem>; AJsonArray: TJDOJsonArray);
var
  LSQLItem: TSQLItem;
  LJsonObject: TJDOJsonObject;
begin
  for LSQLItem in ASQLItems do
  begin
    LJsonObject := AJsonArray.AddObject;
    LJsonObject.S['SQLCategoryName'] := LSQLItem.Category;
    LJsonObject.S['SQLItemName'] := LSQLItem.Name;
    LJsonObject.S['SQL'] := LSQLItem.SQL.Text;
  end;
end;

initialization

GSQLCollectionEditorList := TList<TSQLCollectionEditor>.Create;

finalization

if Assigned(GSQLCollectionEditorList) then
  FreeAndNIl(GSQLCollectionEditorList);

end.

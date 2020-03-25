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

unit SQLCollection.Base;

interface

uses
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TSortableCollection = class(TOwnedCollection)
  private
    FRttiContext: TRttiContext;
    FCollectionItems: TList<TCollectionItem>;
    FDefaultComparer: IComparer<TCollectionItem>;
    function GetCollectionItems: TList<TCollectionItem>;
  protected
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    destructor Destroy; override;

    procedure Sort(const AComparer: IComparer<TCollectionItem> = nil);
    function BinarySearch(const AItem: TCollectionItem; out AIndex: Integer;
      const AComparer: IComparer<TCollectionItem> = nil): Boolean; overload;

    function BinarySearch(const AItem: string; out AIndex: Integer;
      const AComparer: IComparer<TCollectionItem> = nil): Boolean; overload;

    property CollectionItems: TList<TCollectionItem> read GetCollectionItems;
  end;

implementation

uses
  System.SysUtils;

type
  THelpCollectionItem = class(TCollectionItem)
  private
    FName: string;
  protected
    function GetDisplayName: string; override;
  public
    property Name: string read FName write FName;
  end;

{ TSortableCollection }

function TSortableCollection.BinarySearch(const AItem: string; out AIndex: Integer;
  const AComparer: IComparer<TCollectionItem>): Boolean;
var
  LCollectionItem: THelpCollectionItem;
begin
  LCollectionItem := THelpCollectionItem.Create(nil);
  try
    LCollectionItem.Name := AItem;
    Result := BinarySearch(LCollectionItem, AIndex, AComparer);
  finally
    LCollectionItem.Free;
  end;
end;

function TSortableCollection.BinarySearch(const AItem: TCollectionItem; out AIndex: Integer;
  const AComparer: IComparer<TCollectionItem>): Boolean;
var
  LComparer: IComparer<TCollectionItem>;
begin
  if Assigned(AComparer) then
    LComparer := AComparer
  else
    LComparer := FDefaultComparer;

  Result := GetCollectionItems.BinarySearch(AItem, AIndex, LComparer);
end;

constructor TSortableCollection.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FRttiContext := TRttiContext.Create;
  FCollectionItems := nil;

  FDefaultComparer := TDelegatedComparer<TCollectionItem>.Create(
    function(const Left, Right: TCollectionItem): Integer
    begin
      Result := CompareText(Left.DisplayName, Right.DisplayName);
    end);
end;

destructor TSortableCollection.Destroy;
begin
  FRttiContext.Free;
  inherited Destroy;
end;

function TSortableCollection.GetCollectionItems: TList<TCollectionItem>;
var
  LObjType: TRttiType;
  LField: TRttiField;
begin
  if not Assigned(FCollectionItems) then
  begin
    LObjType := FRttiContext.GetType(Self.ClassType);
    while Assigned(LObjType.BaseType) do
    begin
      LObjType := LObjType.BaseType;
      if LObjType.Handle = TypeInfo(TCollection) then
      begin
        LField := LObjType.GetField('FItems');
        if Assigned(LField) then
        begin
          FCollectionItems := LField.GetValue(Self).AsType<TList<TCollectionItem>>;
          Break;
        end;
      end;
    end;
  end;
  Result := FCollectionItems;
end;

procedure TSortableCollection.Sort(const AComparer: IComparer<TCollectionItem>);
var
  LComparer: IComparer<TCollectionItem>;
begin
  if Assigned(AComparer) then
    LComparer := AComparer
  else
    LComparer := FDefaultComparer;

  GetCollectionItems.Sort(LComparer);
end;

{ THelpCollectionItem }

function THelpCollectionItem.GetDisplayName: string;
begin
  Result := FName;

  if Result.IsEmpty then
    Result := inherited GetDisplayName;
end;

end.

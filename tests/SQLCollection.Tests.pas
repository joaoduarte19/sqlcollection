unit SQLCollection.Tests;

interface

uses
  DUnitX.TestFramework,
  SQLCollection.Core;

type

  [TestFixture]
  TSQLCollectionTest = class
  private
    FSQLCollection: TSQLCollection;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSQLCategories;

    [Test]
    procedure TestSQLItems;

    [Test]
    procedure TestSQLCollectionMethods;

  end;

implementation

uses
  System.Classes,
  System.SysUtils;

procedure TSQLCollectionTest.Setup;
begin
  FSQLCollection := TSQLCollection.Create(nil);

end;

procedure TSQLCollectionTest.TearDown;
begin
  FSQLCollection.Free;
end;

procedure TSQLCollectionTest.TestSQLCategories;
begin
  FSQLCollection.Items.Clear;
  FSQLCollection.Items.Add('Category 1');
  FSQLCollection.Items.Add('Category 3');
  FSQLCollection.Items.Add('Category 2');

  Assert.AreEqual(3, FSQLCollection.Items.Count);
  Assert.AreEqual('Category 1', FSQLCollection.Items.SQLCategory[0].Name);
  Assert.AreEqual('Category 2', FSQLCollection.Items.SQLCategory[1].Name);
  Assert.AreEqual('Category 3', FSQLCollection.Items.SQLCategory[2].Name);

  Assert.IsNotNull(FSQLCollection.Items['Category 1']);
  Assert.IsNotNull(FSQLCollection.Items['Category 2']);
  Assert.IsNotNull(FSQLCollection.Items['Category 3']);
  Assert.IsNull(FSQLCollection.Items['Category 4']);
end;

procedure TSQLCollectionTest.TestSQLCollectionMethods;
var
  I: Integer;
  LCategory: TSQLCategory;
  J: Integer;
begin
  FSQLCollection.Items.Clear;
  for I := 100 downto 1 do
  begin
    LCategory := FSQLCollection.Items.Add(Format('Category %d', [I]));
    for J := 100 downto 1 do
    begin
      LCategory.SQLItems.Add(Format('SQLItem %d', [J])).SQL.Text := Format('SELECT * FROM Item_%d', [J]);
    end;
  end;

  Assert.IsNotNull(FSQLCollection.FindSQLCategory('Category 100'), 'Category not found');
  Assert.IsNotNull(FSQLCollection.FindSQLItem('SQLItem 100', 'Category 100'), 'Item not found');

  // The trim is to remove the line break that is inserted in the TStringList that stores the SQL
  Assert.AreEqual(FSQLCollection.FindSQL('SQLItem 100', 'Category 10').Trim, 'SELECT * FROM Item_100');
end;

procedure TSQLCollectionTest.TestSQLItems;
var
  LCategory: TSQLCategory;
begin
  FSQLCollection.Items.Clear;
  LCategory := FSQLCollection.Items.Add('My Category');
  LCategory.SQLItems.Add('SQLItem1');
  LCategory.SQLItems.Add('SQLItem5');
  LCategory.SQLItems.Add('SQLItem3');
  LCategory.SQLItems.Add('SQLItem2');
  LCategory.SQLItems.Add('SQLItem9');
  LCategory.SQLItems.Add('SQLItem4');
  LCategory.SQLItems.Add('SQLItem6');
  LCategory.SQLItems.Add('SQLItem8');
  LCategory.SQLItems.Add('SQLItem7');

  Assert.AreEqual(9, LCategory.SQLItems.Count);
  Assert.AreEqual('SQLItem1', LCategory.SQLItems.SQLItem[0].Name);
  Assert.AreEqual('sqlitem2', LCategory.SQLItems.SQLItem[1].Name);
  Assert.AreEqual('sqlitem3', LCategory.SQLItems.SQLItem[2].Name);
  Assert.AreEqual('SQLItem4', LCategory.SQLItems.SQLItem[3].Name);
  Assert.AreEqual('SQLItem5', LCategory.SQLItems.SQLItem[4].Name);
  Assert.AreEqual('SQLItem6', LCategory.SQLItems.SQLItem[5].Name);
  Assert.AreEqual('SQLItem7', LCategory.SQLItems.SQLItem[6].Name);
  Assert.AreEqual('SQLItem8', LCategory.SQLItems.SQLItem[7].Name);
  Assert.AreEqual('SQLItem9', LCategory.SQLItems.SQLItem[8].Name);

  Assert.IsNotNull(LCategory.SQLItems['SQLItem1']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem2']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem3']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem4']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem5']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem6']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem7']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem8']);
  Assert.IsNotNull(LCategory.SQLItems['SQLItem9']);

  Assert.IsNull(LCategory.SQLItems['SQLItem10']);
end;

initialization

TDUnitX.RegisterTestFixture(TSQLCollectionTest);

end.

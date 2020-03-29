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

unit SQLCollection.Design.SQLEditor;

interface

uses
  DesignIntf,
  DesignEditors,
  DesignWindows,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  System.Actions,
  Vcl.ActnList,
  Vcl.Menus,
  System.ImageList,
  Vcl.ImgList,
  SynCompletionProposal,
  SynEditHighlighter,
  SynHighlighterSQL,
  SynEditPlugins,
  SynMacroRecorder,
  SynEditMiscClasses,
  SynEditSearch,
  SynEdit,
  Vcl.ComCtrls,
  SQLCollection.Core;

type
  TSQLItemInfo = class
  public
    FModified: Boolean;
    FSQL: TStrings;
    FSQLItem: TSQLItem;
    constructor Create;
    destructor Destroy; override;
  end;

  TSQLEditor = class(TDesignWindow)
    SynEditSearch1: TSynEditSearch;
    dlgReplace: TReplaceDialog;
    dlgFind: TFindDialog;
    SynMacroRecorder: TSynMacroRecorder;
    SynSQLSyn: TSynSQLSyn;
    SynCompletionProposal: TSynCompletionProposal;
    imglstCompletion: TImageList;
    pmEditor: TPopupMenu;
    mniSave: TMenuItem;
    mniSaveAll: TMenuItem;
    mniN2: TMenuItem;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniCopy1: TMenuItem;
    mniCopyToNavicat: TMenuItem;
    mniPaste: TMenuItem;
    mniPaste1: TMenuItem;
    mniPasteFromNavicat: TMenuItem;
    mniSelectAll: TMenuItem;
    mniUndo: TMenuItem;
    mniRedo: TMenuItem;
    mniN3: TMenuItem;
    mniFind: TMenuItem;
    mniReplace: TMenuItem;
    actlst1: TActionList;
    actSave: TAction;
    actSaveAll: TAction;
    actOpenQuery: TAction;
    actExecQuery: TAction;
    actCopyToNavicat: TAction;
    actPasteFromNavicat: TAction;
    actMacroPlay: TAction;
    actMacroPlayAll: TAction;
    actMacroStop: TAction;
    actMacroRecord: TAction;
    actSelectAll: TAction;
    actCopy: TAction;
    actCut: TAction;
    actPaste: TAction;
    actUndo: TAction;
    actRedo: TAction;
    actFind: TAction;
    actReplace: TAction;
    actCompletion: TAction;
    imglst1: TImageList;
    pnFooter: TPanel;
    pnMacros: TPanel;
    btnMacroStop: TSpeedButton;
    btnMacroRec: TSpeedButton;
    btnMacroPlay: TSpeedButton;
    btnMacroPlayAll: TSpeedButton;
    pnCaret: TPanel;
    pnInfo1: TPanel;
    pnInfo2: TPanel;
    pnHeader: TPanel;
    tbcSQLItems: TTabControl;
    edtSQLEdit: TSynEdit;
    procedure actMacroPlayExecute(Sender: TObject);
    procedure actMacroPlayAllExecute(Sender: TObject);
    procedure actMacroStopExecute(Sender: TObject);
    procedure actMacroRecordExecute(Sender: TObject);
    procedure edtSQLEditGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
    procedure edtSQLEditPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure edtSQLEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure SynMacroRecorderStateChange(Sender: TObject);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x,
      y: Integer; var CanExecute: Boolean);
    procedure tbcSQLItemsChange(Sender: TObject);
    procedure tbcSQLItemsChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actCopyToNavicatExecute(Sender: TObject);
    procedure actPasteFromNavicatExecute(Sender: TObject);
  private
    FTimerAutoClose: TTimer;
    FOpeningClosing: Boolean;
    function GetMacroRecording: Boolean;
    procedure AddSynCompletionReserverdWords;
    procedure AddSynCompletionFunctions;
    procedure AddSynCompletionDataTypes;
    procedure tmrAutoCloseTimer(Sender: TObject);
    function ContainsSQLItemInEditor(ASQLItem: TSQLItem; var ATabIndex: Integer): Boolean;
    procedure DeleteSelection;
    function ParseSQLParamsToNavicatParams(ASQL: string): string;
  public
    { Public declarations }
    procedure OpenSQLEditor(ASQLItem: TSQLItem);
  end;

/// <summary>
  /// For access to TSynEdit protected methods
  /// </summary>
  THackSynEdit = class(TCustomSynEdit);

var
  SQLEditor: TSQLEditor;

implementation

uses
  SynEditTypes,
  SQLCollection.Design.Editor,
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynUnicode,
  Vcl.Clipbrd,
  System.RegularExpressions;

{$R *.dfm}


procedure TSQLEditor.actCopyExecute(Sender: TObject);
begin
  if edtSQLEdit.SelAvail then
    edtSQLEdit.CopyToClipboard;
end;

procedure TSQLEditor.actCopyToNavicatExecute(Sender: TObject);
begin
  if edtSQLEdit.SelAvail then
    edtSQLEdit.DoCopyToClipboard(ParseSQLParamsToNavicatParams(edtSQLEdit.SelText));
end;

procedure TSQLEditor.actCutExecute(Sender: TObject);
begin
  if not edtSQLEdit.ReadOnly then
    edtSQLEdit.CutToClipboard;
end;

procedure TSQLEditor.actFindExecute(Sender: TObject);
begin
  dlgFind.Execute;
end;

procedure TSQLEditor.actMacroPlayAllExecute(Sender: TObject);
var
  LLinesCount: Integer;
begin
  LLinesCount := edtSQLEdit.Lines.Count;
  while (LLinesCount > 0) and (edtSQLEdit.CaretX < edtSQLEdit.Lines.Count - 1) and
    (Trim(edtSQLEdit.Lines[edtSQLEdit.CaretY]) <> '') do
  begin
    actMacroPlay.Execute;
    Dec(LLinesCount);
  end;
end;

procedure TSQLEditor.actMacroPlayExecute(Sender: TObject);
begin
  SynMacroRecorder.PlaybackMacro(edtSQLEdit);
end;

procedure TSQLEditor.actMacroRecordExecute(Sender: TObject);
begin
  SynMacroRecorder.RecordMacro(edtSQLEdit);
end;

procedure TSQLEditor.actMacroStopExecute(Sender: TObject);
begin
  SynMacroRecorder.Stop;
end;

procedure TSQLEditor.actPasteExecute(Sender: TObject);
begin
  if not edtSQLEdit.ReadOnly then
  begin
    if edtSQLEdit.SelAvail then
    begin
      DeleteSelection;
    end;
    edtSQLEdit.PasteFromClipboard;
  end;
end;

procedure TSQLEditor.actPasteFromNavicatExecute(Sender: TObject);
var
  LAddPasteEndMarker: Boolean;
  LStartOfBlock: TBufferCoord;
  LEndOfBlock: TBufferCoord;
  LPasteMode: TSynSelectionMode;
  LMem: HGLOBAL;
  LPByte: PByte;
  LInsText: string;
begin
  edtSQLEdit.BeginUpdate;
  try
    if not edtSQLEdit.CanPaste then
      Exit;

    if edtSQLEdit.SelAvail then
    begin
      DeleteSelection;
    end;

    edtSQLEdit.BeginUndoBlock;
    LAddPasteEndMarker := False;
    LPasteMode := edtSQLEdit.SelectionMode;
    try
      if Clipboard.HasFormat(SynEditClipboardFormat) then
      begin
        Clipboard.Open;
        try
          LMem := Clipboard.GetAsHandle(SynEditClipboardFormat);
          LPByte := GlobalLock(LMem);
          try
            if LPByte <> nil then
              LPasteMode := PSynSelectionMode(LPByte)^;
          finally
            GlobalUnlock(LMem);
          end
        finally
          Clipboard.Close;
        end;
      end;

      edtSQLEdit.UndoList.AddChange(crPasteBegin, edtSQLEdit.BlockBegin, edtSQLEdit.BlockEnd, '', smNormal);
      LAddPasteEndMarker := True;
      if edtSQLEdit.SelAvail then
      begin
        edtSQLEdit.UndoList.AddChange(crDelete, edtSQLEdit.BlockBegin, edtSQLEdit.BlockEnd, edtSQLEdit.SelText,
          edtSQLEdit.ActiveSelectionMode);
      end
      else
        edtSQLEdit.ActiveSelectionMode := edtSQLEdit.SelectionMode;

      if edtSQLEdit.SelAvail then
      begin
        LStartOfBlock := edtSQLEdit.BlockBegin;
        LEndOfBlock := edtSQLEdit.BlockEnd;
        edtSQLEdit.BlockBegin := LStartOfBlock;
        edtSQLEdit.BlockEnd := LEndOfBlock;

        // Pasting always occurs at column 0 when current selection is
        // smLine type
        if edtSQLEdit.ActiveSelectionMode = smLine then
          LStartOfBlock.Char := 1;
      end
      else
        LStartOfBlock := edtSQLEdit.CaretXY;

      LInsText := TRegEx.Replace(GetClipboardText, '(\[\$)([^\s]*)(\])', ':\2', [roIgnoreCase, roMultiLine]);
      LInsText := LInsText.Replace(#9, #32#32); { Tabs to Space }

      THackSynEdit(edtSQLEdit).SetSelTextPrimitiveEx(LPasteMode, PWideChar(LInsText), True);
      LEndOfBlock := edtSQLEdit.BlockEnd;
      if LPasteMode = smNormal then
        edtSQLEdit.UndoList.AddChange(crPaste, LStartOfBlock, LEndOfBlock, edtSQLEdit.SelText,
          LPasteMode)
      else if LPasteMode = smLine then
        if edtSQLEdit.CaretX = 1 then
          edtSQLEdit.UndoList.AddChange(crPaste, BufferCoord(1, LStartOfBlock.Line),
            BufferCoord(edtSQLEdit.CharsInWindow, LEndOfBlock.Line - 1), edtSQLEdit.SelText, smLine)
        else
          edtSQLEdit.UndoList.AddChange(crPaste, BufferCoord(1, LStartOfBlock.Line),
            LEndOfBlock, edtSQLEdit.SelText, smNormal);
    finally
      if LAddPasteEndMarker then
        edtSQLEdit.UndoList.AddChange(crPasteEnd, edtSQLEdit.BlockBegin, edtSQLEdit.BlockEnd, '', smNormal);
      edtSQLEdit.EndUndoBlock;
    end;
  finally
    edtSQLEdit.EndUpdate;
  end;
end;

procedure TSQLEditor.actRedoExecute(Sender: TObject);
begin
  edtSQLEdit.Redo;
end;

procedure TSQLEditor.actReplaceExecute(Sender: TObject);
begin
  dlgReplace.Execute;
end;

procedure TSQLEditor.actSaveAllExecute(Sender: TObject);
var
  I: Integer;
  LSQLItemInfo: TSQLItemInfo;
  LDesigner: IDesigner;
begin

  if tbcSQLItems.Tabs.Count <= 0 then
    Exit;

  tbcSQLItems.Tabs.BeginUpdate;
  try
    for I := 0 to Pred(tbcSQLItems.Tabs.Count) do
    begin
      LSQLItemInfo := tbcSQLItems.Tabs.Objects[I] as TSQLItemInfo;

      if not Assigned(LSQLItemInfo) then
        Continue;

      LSQLItemInfo.FSQLItem.SQL.Assign(LSQLItemInfo.FSQL);
      LSQLItemInfo.FModified := False;
    end;
  finally
    tbcSQLItems.Tabs.EndUpdate;
  end;

  LDesigner := (Owner as TSQLCollectionEditor).Designer;
  if LDesigner <> nil then
    LDesigner.Modified;
end;

procedure TSQLEditor.actSaveExecute(Sender: TObject);
var
  LSQLItemInfo: TSQLItemInfo;
  LDesigner: IDesigner;
begin
  if tbcSQLItems.TabIndex < 0 then
    Exit;

  LSQLItemInfo := tbcSQLItems.Tabs.Objects[tbcSQLItems.TabIndex] as TSQLItemInfo;
  if not Assigned(LSQLItemInfo) then
    Exit;

  LSQLItemInfo.FSQLItem.SQL.Assign(edtSQLEdit.Lines);
  LSQLItemInfo.FSQL.Assign(edtSQLEdit.Lines);
  LSQLItemInfo.FModified := False;

  edtSQLEdit.Modified := False;

  LDesigner := (Owner as TSQLCollectionEditor).Designer;
  if LDesigner <> nil then
    LDesigner.Modified;
end;

procedure TSQLEditor.actSelectAllExecute(Sender: TObject);
begin
  edtSQLEdit.SelectAll;
end;

procedure TSQLEditor.actUndoExecute(Sender: TObject);
begin
  edtSQLEdit.Undo;
end;

procedure TSQLEditor.AddSynCompletionDataTypes;
var
  LTypes: string;
  LDataTypes: TArray<string>;
  LDataType: string;
begin
  LTypes := SynSQLSyn.GetKeyWords(Ord(tkDatatype));
  if not LTypes.ToLower.Contains('uuid') then
    LTypes := LTypes + ',uuid';

  LDataTypes := LTypes.Split([',']);

  for LDataType in LDataTypes do
  begin
    SynCompletionProposal.InsertList.Add(LDataType);
    SynCompletionProposal.ItemList.Add(
      Format('\color{clNavy} type \column{}\color{$00D200D2}\style{+B}%s\style{-B}', [LDataType]));
  end;
end;

procedure TSQLEditor.AddSynCompletionFunctions;
var
  LFunctions: TArray<string>;
  LFunction: string;
begin
  LFunctions := SynSQLSyn.GetKeyWords(Ord(tkFunction)).Split([',']);
  for LFunction in LFunctions do
  begin
    SynCompletionProposal.InsertList.Add(LFunction);
    SynCompletionProposal.ItemList.Add(
      Format('\image{3}\color{clNavy} function \column{}\color{$00D200D2}\style{+B}%s\style{-B}', [LFunction]));
  end;
end;

procedure TSQLEditor.AddSynCompletionReserverdWords;
var
  LKeys: TArray<string>;
  LKey: string;
begin
  LKeys := SynSQLSyn.GetKeyWords(Ord(tkKey)).Split([',']);
  for LKey in LKeys do
  begin
    SynCompletionProposal.InsertList.Add(LKey);
    SynCompletionProposal.ItemList.Add(
      Format('\color{clNavy} \column{}\color{clBlue}\style{+B}%s\style{-B}', [LKey]));
  end;
end;

function TSQLEditor.ContainsSQLItemInEditor(ASQLItem: TSQLItem; var ATabIndex: Integer): Boolean;
var
  I: Integer;
  APair: TSQLItemInfo;
begin
  Result := False;
  ATabIndex := - 1;

  tbcSQLItems.Tabs.BeginUpdate;
  try
    for I := 0 to Pred(tbcSQLItems.Tabs.Count) do
    begin
      APair := tbcSQLItems.Tabs.Objects[I] as TSQLItemInfo;
      if APair.FSQLItem = ASQLItem then
      begin
        ATabIndex := I;
        Result := True;
        Break;
      end;
    end;
  finally
    tbcSQLItems.Tabs.EndUpdate;
  end;
end;

procedure TSQLEditor.DeleteSelection;
  function TrimTrailingSpaces(const S: string): string;
  var
    I: Integer;
  begin
    I := Length(S);
    while (I > 0) and ((S[I] = #32) or (S[I] = #9)) do
      Dec(I);
    Result := Copy(S, 1, I);
  end;

var
  LX: Integer;
  LTempString: string;
  LBB: TBufferCoord;
  LBE: TBufferCoord;
begin
  LBB := edtSQLEdit.BlockBegin;
  LBE := edtSQLEdit.BlockEnd;

  case edtSQLEdit.ActiveSelectionMode of
    smNormal:
      begin
        if edtSQLEdit.Lines.Count > 0 then
        begin
          LTempString := Copy(edtSQLEdit.Lines[LBB.Line - 1], 1, LBB.Char - 1) +
            Copy(edtSQLEdit.Lines[LBE.Line - 1], LBE.Char, MaxInt);
          TSynEditStringList(edtSQLEdit.Lines).DeleteLines(LBB.Line, LBE.Line - LBB.Line);
          if edtSQLEdit.Options >= [eoScrollPastEol, eoTrimTrailingSpaces] then
            LTempString := TrimTrailingSpaces(LTempString);
          edtSQLEdit.Lines[LBB.Line - 1] := LTempString;
        end;
        edtSQLEdit.CaretXY := LBB;
      end;
    smColumn:
      begin
            // swap LX if needed
        if LBB.Char > LBE.Char then
          SwapInt(Integer(LBB.Char), Integer(LBE.Char));

        for LX := LBB.Line - 1 to LBE.Line - 1 do
        begin
          LTempString := edtSQLEdit.Lines[LX];
          Delete(LTempString, LBB.Char, LBE.Char - LBB.Char);

          if eoTrimTrailingSpaces in edtSQLEdit.Options then
            edtSQLEdit.Lines[LX] := TrimTrailingSpaces(LTempString)
          else
            edtSQLEdit.Lines[LX] := LTempString;
        end;
          // Lines never get deleted completely, so keep caret at end.
        edtSQLEdit.CaretXY := BufferCoord(LBB.Char, LBE.Line);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
      end;
    smLine:
      begin
        if LBE.Line = edtSQLEdit.Lines.Count then
        begin
          edtSQLEdit.Lines[LBE.Line - 1] := '';
          for LX := LBE.Line - 2 downto LBB.Line - 1 do
            edtSQLEdit.Lines.Delete(LX);
        end
        else
        begin
          for LX := LBE.Line - 1 downto LBB.Line - 1 do
            edtSQLEdit.Lines.Delete(LX);
        end;
          // smLine deletion always resets to first column.
        edtSQLEdit.CaretXY := BufferCoord(1, LBB.Line);
      end;
  end;
end;

procedure TSQLEditor.dlgFindFind(Sender: TObject);
var
  LOptions: TSynSearchOptions;
  LDlg: TFindDialog;
  LSearch: string;
begin
  try
    if Sender = dlgReplace then
      LDlg := dlgReplace
    else
      LDlg := dlgFind;
    LSearch := LDlg.FindText;
    if Length(LSearch) = 0 then
      ShowMessage('Search text is empty!')
    else
    begin
      LOptions := [];
      if not (frDown in LDlg.Options) then
        Include(LOptions, ssoBackwards);
      if frMatchCase in LDlg.Options then
        Include(LOptions, ssoMatchCase);
      if frWholeWord in LDlg.Options then
        Include(LOptions, ssoWholeWord);
      if edtSQLEdit.SearchReplace(LSearch, '', LOptions) = 0 then
        ShowMessage(Format('Texto %s não encontrado', [LSearch]));
    end;
  except
  end;
end;

procedure TSQLEditor.dlgReplaceReplace(Sender: TObject);
var
  LOptions: TSynSearchOptions;
  LSearch: string;
begin
  try
    LSearch := dlgReplace.FindText;
    if Length(LSearch) = 0 then
      ShowMessage('Cannot replace empty text!')
    else
    begin
      LOptions := [ssoReplace];
      if frMatchCase in dlgReplace.Options then
        Include(LOptions, ssoMatchCase);
      if frWholeWord in dlgReplace.Options then
        Include(LOptions, ssoWholeWord);
      if frReplaceAll in dlgReplace.Options then
        Include(LOptions, ssoReplaceAll);
      if edtSQLEdit.SearchReplace(LSearch, dlgReplace.ReplaceText, LOptions) = 0 then
        ShowMessage(Format('Searched text %s  cannot be replaced!', [LSearch]));
    end;
  except
  end;
end;

procedure TSQLEditor.edtSQLEditGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
begin
  if (aLine = 1) or ((aLine mod 10) = 0) or (aLine = TCustomSynEdit(Sender).CaretY) then
    aText := IntToStr(aLine)
  else if ((aLine mod 5) = 0) then
    aText := '-'
  else
    aText := '.'
end;

procedure TSQLEditor.edtSQLEditPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
const
  BRACKET_FONT_COLOR = clBlack;
  BRACKET_BRUSH_COLOR = clAqua;
var
  LEditor: TSynEdit;
  LOpenChars: array of WideChar;
  LCloseChars: array of WideChar;
  LAttr: TSynHighlighterAttributes;

  function IsCharBracket(AChar: WideChar): Boolean;
  begin
    Result := CharInSet(AChar, ['{', '[', '(', '<', '}', ']', ')', '>']);
  end;

  function CharToPixels(LBufferCoord: TBufferCoord): TPoint;
  begin
    Result := LEditor.RowColumnToPixels(LEditor.BufferToDisplayPos(LBufferCoord));
  end;

  procedure SetCanvasStyle;
  begin
    LEditor.Canvas.Brush.Style := bsSolid;
    LEditor.Canvas.Font.Assign(LEditor.Font);
    LEditor.Canvas.Font.Style := LAttr.Style;
    if (TransientType = ttAfter) then
    begin
      LEditor.Canvas.Font.Color := BRACKET_FONT_COLOR;
      LEditor.Canvas.Brush.Color := BRACKET_BRUSH_COLOR;
    end
    else
    begin
      LEditor.Canvas.Font.Color := LAttr.Foreground;
      LEditor.Canvas.Brush.Color := LAttr.Background;
    end;

    if (LEditor.Canvas.Font.Color = clNone) then
      LEditor.Canvas.Font.Color := LEditor.Font.Color;
    if (LEditor.Canvas.Brush.Color = clNone) then
      LEditor.Canvas.Brush.Color := LEditor.Color;
  end;

var
  LBufferCoord: TBufferCoord;
  LPoint: TPoint;
  LDisplayCoord: TDisplayCoord;
  LStr: string;
  I: Integer;
  LArrayLen: Integer;
  LStart: Integer;
  LTmpCharA: WideChar;
  LTmpCharB: WideChar;
begin
  try
    LEditor := TSynEdit(Sender);
    LArrayLen := 3;

    SetLength(LOpenChars, LArrayLen);
    SetLength(LCloseChars, LArrayLen);

    for I := 0 to LArrayLen - 1 do
      case I of
        0:
          begin
            LOpenChars[I] := '(';
            LCloseChars[I] := ')';
          end;
        1:
          begin
            LOpenChars[I] := '{';
            LCloseChars[I] := '}';
          end;
        2:
          begin
            LOpenChars[I] := '[';
            LCloseChars[I] := ']';
          end;
        3:
          begin
            LOpenChars[I] := '<';
            LCloseChars[I] := '>';
          end;
      end;

    LBufferCoord := LEditor.CaretXY;
    LDisplayCoord := LEditor.DisplayXY;
    LStart := LEditor.SelStart;

    if (LStart > 0) and (LStart <= Length(LEditor.Text)) then
      LTmpCharA := LEditor.Text[LStart]
    else
      LTmpCharA := #0;

    if (LStart < Length(LEditor.Text)) then
      LTmpCharB := LEditor.Text[LStart + 1]
    else
      LTmpCharB := #0;

    if not IsCharBracket(LTmpCharA) and not IsCharBracket(LTmpCharB) then
      Exit;

    LStr := LTmpCharB;
    if not IsCharBracket(LTmpCharB) then
    begin
      LBufferCoord.Char := LBufferCoord.Char - 1;
      LStr := LTmpCharA;
    end;

    LEditor.GetHighlighterAttriAtRowCol(LBufferCoord, LStr, LAttr);

    if (LEditor.Highlighter.SymbolAttribute = LAttr) then
    begin
      for I := low(LOpenChars) to high(LOpenChars) do
      begin
        if (LStr = LOpenChars[I]) or (LStr = LCloseChars[I]) then
        begin
          LPoint := CharToPixels(LBufferCoord);
          SetCanvasStyle;
          LEditor.Canvas.TextOut(LPoint.x, LPoint.y, LStr);
          LBufferCoord := LEditor.GetMatchingBracketEx(LBufferCoord);

          if (LBufferCoord.Char > 0) and (LBufferCoord.Line > 0) then
          begin
            LPoint := CharToPixels(LBufferCoord);
            if LPoint.x > LEditor.Gutter.Width then
            begin
              SetCanvasStyle;
              if LStr = LOpenChars[I] then
                LEditor.Canvas.TextOut(LPoint.x, LPoint.y, LCloseChars[I])
              else
                LEditor.Canvas.TextOut(LPoint.x, LPoint.y, LOpenChars[I]);
            end;
          end;
        end;
      end;
      LEditor.Canvas.Brush.Style := bsSolid;
    end;
  except
  end;
end;

procedure TSQLEditor.edtSQLEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
const
  MODIFIED_STRS: array [Boolean] of string = ('', 'Modified');
  INSERT_MODE_STRS: array [Boolean] of string = ('Overwrite', 'Insert');
var
  LCaret: TBufferCoord;
  LSelLen: Integer;
  LLinSel: Integer;
begin
  try
    if Changes * [scAll, scCaretX, scCaretY] <> [] then
    begin
      LCaret := edtSQLEdit.CaretXY;
      LSelLen := edtSQLEdit.SelLength;

      if LSelLen > 0 then
        LLinSel := edtSQLEdit.BlockEnd.Line - edtSQLEdit.BlockBegin.Line + 1
      else
        LLinSel := 0;

      pnCaret.Caption := Format('%6d:%3d  Sel: %d | %d', [LCaret.Line, LCaret.Char, LSelLen, LLinSel]);
    end;

    if Changes * [scAll, scInsertMode, scReadOnly] <> [] then
    begin
      if edtSQLEdit.ReadOnly then
        pnInfo1.Caption := 'ReadOnly'
      else if GetMacroRecording then
        pnInfo1.Caption := 'Macro Recording'
      else
        pnInfo1.Caption := INSERT_MODE_STRS[edtSQLEdit.InsertMode];
    end;

    if Changes * [scAll, scModified] <> [] then
      pnInfo2.Caption := MODIFIED_STRS[edtSQLEdit.Modified];

    edtSQLEdit.InvalidateGutter;
  except
  end;
end;

procedure TSQLEditor.FormCreate(Sender: TObject);
begin
  FTimerAutoClose := TTimer.Create(Self);
  FTimerAutoClose.Enabled := False;
  FtimerAutoClose.Interval := 500;
  FTimerAutoClose.OnTimer := tmrAutoCloseTimer;

  FOpeningClosing := True;
  tbcSQLItems.Tabs.Clear;

end;

procedure TSQLEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTimerAutoClose);
end;

function TSQLEditor.GetMacroRecording: Boolean;
begin
  Result := (SynMacroRecorder.State = msRecording);
end;

procedure TSQLEditor.OpenSQLEditor(ASQLItem: TSQLItem);
var
  LTabIndex: Integer;
  LSQLItemInfo: TSQLItemInfo;
begin
  if ContainsSQLItemInEditor(ASQLItem, LTabIndex) then
  begin
    tbcSQLItems.TabIndex := LTabIndex;
    Exit;
  end;

  LSQLItemInfo := TSQLItemInfo.Create;
  LSQLItemInfo.FSQL.Assign(ASQLItem.SQL);
  LSQLItemInfo.FSQLItem := ASQLItem;

  LTabIndex := tbcSQLItems.Tabs.AddObject(Format('%s - %s', [ASQLItem.Name, ASQLItem.Category]), LSQLItemInfo);
  tbcSQLItems.TabIndex := LTabIndex;
  tbcSQLItemsChange(tbcSQLItems);
end;

function TSQLEditor.ParseSQLParamsToNavicatParams(ASQL: string): string;
  function NameDelimiter(CurChar: Char): Boolean;
  begin
    case CurChar of
      ' ', ',', ';', ')', #13, #10:
        Result := True;
    else
      Result := False;
    end;
  end;

var
  LLiteralChar: Char;
  LCurChar: Char;
  LCurPos: PChar;
  LStartPos: PChar;
  LBeginPos: PChar;
  LNameStart: PChar;
  LName: string;
begin
  Result := '';

  LStartPos := PChar(ASQL);
  LBeginPos := LStartPos;
  LCurPos := LStartPos;
  while True do
  begin
    // Fast forward
    while True do
    begin
      case LCurPos^ of
        #0, ':', '''', '"', '`':
          Break;
      end;
      Inc(LCurPos);
    end;

    case LCurPos^ of
      #0: // string end
        Break;
      '''', '"', '`': // literal
        begin
          LLiteralChar := LCurPos^;
          Inc(LCurPos);
          // skip literal, escaped literal chars must not be handled because they
          // end the string and start a new string immediately.
          while (LCurPos^ <> #0) and (LCurPos^ <> LLiteralChar) do
            Inc(LCurPos);
          if LCurPos^ = #0 then
            Break;
          Inc(LCurPos);
        end;
      ':': // parameter
        begin
          // 0123456789
          // a ::text

          Inc(LCurPos);
          if LCurPos^ = ':' then
          begin
            Inc(LCurPos); // skip escaped ":"
            Result := Result + ASQL.Substring(LStartPos - LBeginPos, LCurPos - LStartPos);
            LStartPos := LCurPos;
          end
          else
          begin
            Result := Result + ASQL.Substring(LStartPos - LBeginPos, LCurPos - LStartPos - 1);

            LLiteralChar := #0;
            case LCurPos^ of
              '''', '"', '`':
                begin
                  LLiteralChar := LCurPos^;
                  Inc(LCurPos);
                end;
            end;
            LNameStart := LCurPos;

            LCurChar := LCurPos^;
            while LCurChar <> #0 do
            begin
              if (LCurChar = LLiteralChar) or ((LLiteralChar = #0) and NameDelimiter(LCurChar)) then
                Break;
              Inc(LCurPos);
              LCurChar := LCurPos^;
            end;
            SetString(LName, LNameStart, LCurPos - LNameStart);

            // Replace Parameter
            Result := Result + '[$' + LName + ']';

            if LLiteralChar <> #0 then
              Inc(LCurPos);

            LStartPos := LCurPos;
          end;
        end;
    end;
  end;
  Result := Result + ASQL.Substring(LStartPos - LBeginPos, LCurPos - LStartPos);
end;

procedure TSQLEditor.SynCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string;
  var x, y: Integer; var CanExecute: Boolean);
begin
  SynCompletionProposal.ItemList.Clear;
  SynCompletionProposal.InsertList.Clear;
  AddSynCompletionReserverdWords;
  AddSynCompletionDataTypes;
  AddSynCompletionFunctions;
end;

procedure TSQLEditor.SynMacroRecorderStateChange(Sender: TObject);
begin
  actMacroPlay.Enabled := (SynMacroRecorder.EventCount > 0) and (SynMacroRecorder.State = msStopped);
  actMacroPlayAll.Enabled := (SynMacroRecorder.EventCount > 0) and (SynMacroRecorder.State = msStopped);
  actMacroRecord.Enabled := (SynMacroRecorder.State = msStopped);
  actMacroStop.Enabled := GetMacroRecording;
end;

procedure TSQLEditor.tbcSQLItemsChange(Sender: TObject);
var
  LPair: TSQLItemInfo;
begin
  try
    if (tbcSQLItems.TabIndex < 0) or (tbcSQLItems.Tabs.Count <= 0) then
      Exit;

    LPair := tbcSQLItems.Tabs.Objects[tbcSQLItems.TabIndex] as TSQLItemInfo;
    if Assigned(LPair) then
    begin
      edtSQLEdit.Lines.Assign(LPair.FSQL);
      edtSQLEdit.Modified := LPair.FModified;
    end;
  except
  end;
end;

procedure TSQLEditor.tbcSQLItemsChanging(Sender: TObject; var AllowChange: Boolean);
var
  LSQLItemInfo: TSQLItemInfo;
begin
  try
    if (tbcSQLItems.TabIndex < 0) or (tbcSQLItems.Tabs.Count <= 0) then
      Exit;

    LSQLItemInfo := tbcSQLItems.Tabs.Objects[tbcSQLItems.TabIndex] as TSQLItemInfo;
    if Assigned(LSQLItemInfo) then
    begin
      LSQLItemInfo.FSQL.Assign(edtSQLEdit.Lines);
      LSQLItemInfo.FModified := edtSQLEdit.Modified;
      edtSQLEdit.Lines.Clear;
    end;
  except
  end;
end;

procedure TSQLEditor.tmrAutoCloseTimer(Sender: TObject);
begin
  if (not FOpeningClosing) and (tbcSQLItems.Tabs.Count <= 0) then
  begin
    TTimer(Sender).Enabled := False;
    Close;
  end;
end;

{ TSQLItemInfo }

constructor TSQLItemInfo.Create;
begin
  inherited Create;
  FModified := False;
  FSQL := TStringList.Create;
  FSQLItem := nil;
end;

destructor TSQLItemInfo.Destroy;
begin
  FreeAndNil(FSQL);
  inherited Destroy;
end;

end.

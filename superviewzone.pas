unit SuperViewZone;

{$mode objfpc}{$H+}

interface

uses
  SuperViewZoneConfig,
  syncobjs,
  gmap,
  ghashmap,
  gvector,
  hashmapstr,
  glinkedlist,
  variants,
  Grids,
  VirtualTrees,
  VTUtils,
  Forms,
  ExtCtrls,
  Dialogs,//dbg!
  Graphics,
  //ArrayUtils,
  uArrayAsPHP,
  Classes, SysUtils;

type
  { TSuperViewItem }

  TSuperViewItem = class
  public
    Name: TSuperViewPath;
    Info: TSuperViewInfo;
    Node: PVirtualNode;
    Path: TSuperViewPath;
    GridIndex: integer;
    constructor Create(APath, AName: TSuperViewPath; AData: TSuperViewInfo);
    destructor Destroy(); override;
  end;

  TSuperViewEventType = (eventAdd, eventDel);

  TSuperViewEvent = record
    Path: TSuperViewPath;
    Info: TSuperViewInfo;
    Action: TSuperViewEventType;
    TimeTick: int64
  end;

  TSuperViewEventsList = specialize TLinkedList<TSuperViewEvent>;
  TVectorSuperViewEvent = specialize TVector<TSuperViewEvent>;
  //todo: change to TMap?
  TMapSuperView = specialize THashMap<TSuperViewPath, TSuperViewItem, THashFuncString>;

  //TMapPtrToIndex = specialize THashMap<pointer, integer, THashFuncPointer>;
  TVectorSuperViewItems = specialize TVector<TSuperViewItem>;
  //TMapSuperView = specialize TMap<TSuperViewPath, TSuperViewItem, THashFuncString>;

  //TTreeSuperView = specialize TTree<TSuperViewItem>;
  //TTreeSuperViewItem = specialize TTreeNode<TSuperViewItem>;

  { TSuperViewPresenter }
  TViewPresenterViewMode = (ViewModeTree, ViewModeCompactGrid);

  TPresenterCounter = record
    Count: integer;
    LostEvents: integer;
    TotalEvents: integer;
    DequeSize: integer;
  end;

  TSuperViewPresenter = class;

  TSuperViewEnumItemsProc = procedure (Sender: TSuperViewPresenter; Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer);
  TSuperViewEnumItemsProcClass = procedure (Item: TSuperViewItem; out Delete: boolean; out Stop: boolean; UserData: pointer) of object;

  {
  Все методы вызываются и работают в Main Thread.
  Кроме: ThreadAddEvent.
  вызываются из параллельного треда.

  }
  TSuperViewPresenter = class
  private
    // map of all paths - contein all items and all path slice
    Items: TMapSuperView;

    //todo: map of all items for thread 'add/del' optimization
    // ThreadCacheEnabled: boolean;
    // ThreadCacheNeedClear: boolean;
    // ? ThreadCacheNeedSync: boolean; ?
    // ThreadItemsCacheLock: TCriticalSection;
    // ThreadItemsCache: TMapSuperView;

    // Double Buffer
    ThreadListAddLock: TCriticalSection;
    ThreadListAddItems2: TVectorSuperViewEvent;
    ThreadListAddItems: TVectorSuperViewEvent;

    // grid remap array
    GridArray: TVectorSuperViewItems;

    CountersLock: TCriticalSection;
    Counters: TPresenterCounter;

    SyncInProcess: boolean;
    TransferInProcess: boolean;

    DropEnableTimer: boolean;
    TimerEnabled: boolean;
    TimerSync: TTimer;

    procedure SwapBufferBeetwenThread();

    procedure MainThreadProcessBuffer();

    procedure EventTimerProcessBuffer(Sender: TObject);

    // main thread. syncronized call. call from ext thread.
    procedure DoThreadSync();

    // main thread. syncronized call. call from ext thread.
    procedure DoEnableTimer();

    procedure EventGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);

    procedure EventGridResize(Sender: TObject);

    procedure EventVirtualTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);

    // delete item and update grid.
    // but not change tree!
    //todo: update tree? move to public?
    procedure DeleteItem(Item: TSuperViewItem);

  public
    ViewGrid: TCustomDrawGrid;
    ViewTree: TVirtualStringTree;
    ViewMode: TViewPresenterViewMode;

    OnChanged: TNotifyEvent;

    GridEmptyCell: integer;
    GridMaxEmptyCell: integer;

    GridWidth: integer;
    UpdateSpeed: integer; // interval of update from thread new info
    ThreadEventsMaxCount: integer;

    PathSplit: WideString;

    constructor Create();
    destructor Destroy(); override;

    procedure GridRepaintCell(Index: integer);

    function GridXYToIndex(x, y: integer): integer;
    function GridIndexToXY(index: integer): TPoint;

    function GridItemByIndex(index: integer): TSuperViewItem;

    procedure Setup(AViewTree: TVirtualStringTree; AViewGrid: TCustomDrawGrid;
      ATimer: TTimer);

    procedure SetViewMode(NewViewMode: TViewPresenterViewMode);

    procedure GridRecalc();

    procedure GridResizeAndCompact();

    function Count(): integer;

    procedure DrawCell(aCol, aRow: integer; aRect: TRect; aState: TGridDrawState;
      Node: TSuperViewItem); virtual;

    function Add(Path: TSuperViewPath; Info: TSuperViewInfo): TSuperViewItem; virtual;
    function Del(Path: TSuperViewPath; DeleteSubItems: boolean = False): boolean;
      virtual;
    procedure Update(Path: TSuperViewPath; Info: TSuperViewInfo); virtual;
    function Find(Path: TSuperViewPath): TSuperViewItem; virtual;

    function GetText(Node: TSuperViewItem; Col: integer): UTF8String; virtual;
    function GetTextCompactMode(Node: TSuperViewItem): UTF8String; virtual;

    procedure SetItemRange(RangeBegin, RangeEnd: integer; DelaultValue: variant;
      Clear: boolean); overload;
    procedure SetItemRange(RangeBegin, RangeEnd: integer); overload;

    procedure EnumSelected(Proc: TSuperViewEnumItemsProc; UserData: pointer);

    function SelectedPresent(): boolean;

    function SelectedItem(): TSuperViewItem;

    procedure Clear();

    function GetCounters(): TPresenterCounter;

    //todo: procedure SortItemsByName ...
    //todo: procedure SortItemsByProc ...

    // call from 'external' thread
    procedure ThreadAddEvent(Path: TSuperViewPath; Info: TSuperViewInfo;
      EventType: TSuperViewEventType);

    // call from 'external' thread
    procedure ThreadSync();
  end;

implementation

type
  TDrawGridHack = class(TCustomDrawGrid)
  end;

type
  ArrayOfWideString = array of WideString;

function GetPathCut(Path: ArrayOfWideString; PathSplit: WideString;
  Top: integer): TSuperViewPath;
var
  i: integer;
begin
  // create 'cuted path'
  Result := '';
  for i := Low(Path) to Top do
    Result := Result + Path[i] + PathSplit;
  SetLength(Result, Length(Result) - Length(PathSplit));
end;

function SuperViewEvent(Path: TSuperViewPath; Data: TSuperViewInfo;
  Action: TSuperViewEventType; TimeTick: int64): TSuperViewEvent;
begin
  Result.Path := Path;
  Result.Info := Data;
  Result.Action := Action;
  Result.TimeTick := TimeTick;
end;


{ TSuperViewItem }

constructor TSuperViewItem.Create(APath, AName: TSuperViewPath; AData: TSuperViewInfo);
begin
  Path := APath;
  Name := AName;
  Info := AData;
  self.Node := nil;
  self.GridIndex := -1;
end;

destructor TSuperViewItem.Destroy();
begin
  Info := Null;
  Name := '';
  Node := nil;
end;

procedure TSuperViewPresenter.SwapBufferBeetwenThread();
var
  ThreadListAddItems1: TVectorSuperViewEvent;
begin
  if SyncInProcess or TransferInProcess then
    exit;
  SyncInProcess := True;
  ThreadListAddLock.Enter;
  try
    Assert(ThreadListAddItems2.Size = 0, 'ThreadListAddItems2.Count <> 0');
    // swap buffer
    ThreadListAddItems1 := ThreadListAddItems;
    ThreadListAddItems := ThreadListAddItems2;
    ThreadListAddItems2 := ThreadListAddItems1;
    Counters.DequeSize := ThreadListAddItems2.Size;
  finally
    ThreadListAddLock.Leave;
    SyncInProcess := False;
  end;
end;

procedure TSuperViewPresenter.MainThreadProcessBuffer();
var
  j: integer;
  i: TSuperViewEvent;
  ic: integer;
  icount: integer;
begin
  if TransferInProcess then
    exit;
  TransferInProcess := True;
  try
    ic := 0;
    icount := ThreadListAddItems2.Size;
    icount := icount - 1;

    for j := 0 to icount do
    begin
      i := ThreadListAddItems2.Items[j];
      ic := ic + 1;
      if i.Action = eventAdd then
        Add(i.Path, i.Info)
      else if i.Action = eventDel then
        Del(i.Path);
      if (ic mod 50 = 9) and (icount - ic > 9) then
        Application.ProcessMessages();//todo: ???
    end;

    ThreadListAddItems2.Clear();

    if (ic > 0) and Assigned(OnChanged) then
      OnChanged(self);
  finally
    TransferInProcess := False;
  end;
end;

procedure TSuperViewPresenter.EventTimerProcessBuffer(Sender: TObject);
begin
  if (not TransferInProcess) then
  begin
    TimerSync.Enabled := False;
    TimerEnabled := False;//thread safe
    SwapBufferBeetwenThread();
    MainThreadProcessBuffer();
    Application.ProcessMessages();
  end;
end;

procedure TSuperViewPresenter.DoEnableTimer();
begin
  DropEnableTimer := False;//thread safe
  TimerEnabled := True;//thread safe
  TimerSync.Enabled := True;
end;

procedure TSuperViewPresenter.EventGridDrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  c, n: integer;
  item: TSuperViewItem;
begin
  n := (aRow * ViewGrid.ColCount) + aCol;
  c := GridArray.Size();
  if (c > 0) and (n < c) then
  begin
    item := GridArray.Items[n] as TSuperViewItem;
    DrawCell(aCol, aRow, aRect, aState, item);
  end;
end;

procedure TSuperViewPresenter.EventGridResize(Sender: TObject);
begin
  GridResizeAndCompact();
  GridRecalc();
end;

procedure TSuperViewPresenter.GridRepaintCell(Index: integer);
var
  aCol, aRow, c: integer;
  d: TDrawGridHack;
{  item: TSuperViewItem;
  aRect: TRect;
  ts1, ts2: TTextStyle;}
begin
  d := TDrawGridHack(ViewGrid);
  aRow := Index div ViewGrid.ColCount;
  aCol := Index mod ViewGrid.ColCount;
  c := Count();
  if (c > 0) and (Index < GridArray.Size) and (ViewGrid.IsCellVisible(aCol, aRow)) then
  begin
    {aRect := d.CellRect(aCol, aRow);
    aRect.Bottom:=aRect.Bottom - d.GridLineWidth;
    aRect.Right:=aRect.Right - d.GridLineWidth;
    //d.DrawFillRect(d.Canvas, aRect);
    ts1 := d.Canvas.TextStyle;
    ts2 := ts1;
    ts1.Opaque:=true;
    d.Canvas.TextStyle:=ts1;
    d.DrawCellText(aCol, aRow, aRect, d.GetGridDrawState(ACol, ARow), GetTextCompactMode(GridArray.Items[Index]));
    d.Canvas.TextStyle:=ts2;}

    //ViewGrid.DefaultDrawCell(aCol, aRow, ViewGrid.CellRect(aCol, aRow), []);
    //DrawCell(aCol, aRow, ViewGrid.CellRect(aCol, aRow), [], GridArray.Items[Index]);

    // give 'splash', but only for first item
    ViewGrid.InvalidateCell(aCol, aRow);

    //ViewGrid.Invalidate();
  end;
end;

procedure TSuperViewPresenter.EventVirtualTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if GetObjValid(Sender, Node) then
    CellText := GetText(GetObj(Sender, Node) as TSuperViewItem, Column)
  else
    CellText := 'ERROR!!!';
end;

procedure TSuperViewPresenter.DeleteItem(Item: TSuperViewItem);
begin
  Assert(Item <> nil, 'DeleteItem item=nil');
  if (ViewGrid <> nil) then
    if Item.GridIndex <> -1 then
    begin
      GridArray.Items[Item.GridIndex] := nil;
      Inc(GridEmptyCell);
      if ViewMode = ViewModeCompactGrid then
      begin
        if GridEmptyCell > GridMaxEmptyCell then
        begin
          GridResizeAndCompact();
          GridRecalc();
        end
        else
          GridRepaintCell(Item.GridIndex);
      end;
    end;
  Items.Delete(Item.Path);
  Item.Free;
end;

constructor TSuperViewPresenter.Create();
begin
  inherited;
  UpdateSpeed := 100;
  ThreadEventsMaxCount := 1023;
  PathSplit := '\';
  GridWidth := 100;
  GridArray := TVectorSuperViewItems.Create;
  GridMaxEmptyCell := 1000;
  ViewMode := ViewModeTree;
  Items := TMapSuperView.Create;
  ThreadListAddLock := TCriticalSection.Create;
  ThreadListAddItems := TVectorSuperViewEvent.Create;
  ThreadListAddItems2 := TVectorSuperViewEvent.Create;
  CountersLock := TCriticalSection.Create;
end;

destructor TSuperViewPresenter.Destroy();
var
  //i: TSuperViewItem;
  itr: TMapSuperView.TIterator;
begin
  // clear
  itr := Items.Iterator;
  if itr <> nil then
  begin
    repeat
      itr.GetMutable()^.Free;
    until not itr.Next;
    FreeAndNil(itr);
  end;

  //todo: free items before!!!
  FreeAndNil(GridArray);
  FreeAndNil(Items);
  FreeAndNil(ThreadListAddItems2);
  FreeAndNil(ThreadListAddItems);
  FreeAndNil(ThreadListAddLock);
  FreeAndNil(CountersLock);
  inherited Destroy();
end;

function TSuperViewPresenter.GridXYToIndex(x, y: integer): integer;
begin
  Result := (y * ViewGrid.ColCount) + x;
end;

function TSuperViewPresenter.GridIndexToXY(index: integer): TPoint;
begin
  Result.X := Index mod ViewGrid.ColCount;
  Result.Y := Index div ViewGrid.ColCount;
end;

function TSuperViewPresenter.GridItemByIndex(index: integer): TSuperViewItem;
begin
  if (index > 0) and (index < GridArray.Size()) then
  begin
    if GridArray.Items[index] <> nil then
      Result := GridArray.Items[index] as TSuperViewItem else
      Result := nil;
  end;
  //
end;

procedure TSuperViewPresenter.Setup(AViewTree: TVirtualStringTree;
  AViewGrid: TCustomDrawGrid; ATimer: TTimer);
begin
  if Assigned(AViewGrid) then
  begin
    ViewGrid := AViewGrid;
    ViewGrid.OnDrawCell := @EventGridDrawCell;
    ViewGrid.OnResize := @EventGridResize;
  end;
  //todo: add "if ass"
  ViewTree := AViewTree;
  ViewTree.NodeDataSize := sizeof(pointer);
  ViewTree.OnGetText := @EventVirtualTreeGetText;

  TimerSync := ATimer;
  TimerSync.Enabled := False;
  TimerSync.Interval:=UpdateSpeed;
  TimerEnabled := False;
  TimerSync.OnTimer := @EventTimerProcessBuffer;
end;

procedure TSuperViewPresenter.SetViewMode(NewViewMode: TViewPresenterViewMode);
begin
  if NewViewMode <> ViewMode then
  begin
    //todo: add "if ass"
    if NewViewMode = ViewModeTree then
    begin
      ViewMode := NewViewMode;
      ViewTree.Visible := True;
      if ViewGrid <> nil then
        ViewGrid.Visible := False;
    end
    else
    if (NewViewMode = ViewModeCompactGrid) and (ViewGrid <> nil) then
    begin
      GridResizeAndCompact();
      GridRecalc();
      ViewMode := NewViewMode;
      ViewGrid.Visible := True;
      ViewTree.Visible := False;
    end;
  end;
end;

procedure TSuperViewPresenter.GridRecalc();
var
  i: integer;
begin
  if ViewGrid <> nil then
  begin
    if ViewGrid.ColCount <> ViewGrid.ClientWidth div GridWidth then
    begin
      ViewGrid.ColCount := ViewGrid.ClientWidth div GridWidth;
      for i := 0 to ViewGrid.ColCount - 1 do
        ViewGrid.ColWidths[i] := GridWidth;
    end;
    if ViewGrid.RowCount <> (Count() div ViewGrid.ColCount) + 1 then
      ViewGrid.RowCount := (Count() div ViewGrid.ColCount) + 1;
  end;
end;

procedure TSuperViewPresenter.GridResizeAndCompact();
var
  NewArray: TVectorSuperViewItems;
  itemTest: TSuperViewItem;
  Changed: boolean;
  i, j, c, tt: integer;
begin
  NewArray := nil;
  Changed := False;
  c := GridArray.Size;
  c := c - 1;
  for i := 0 to c do
  begin
    if GridArray.Items[i] = nil then
    begin
      if not Changed then
      begin
        // create new array and copy all values (but skip "nil" cell)
        Changed := True;
        NewArray := TVectorSuperViewItems.Create;
        for j := 0 to i - 1 do
          NewArray.PushBack(GridArray.Items[j]);
        tt := i;
      end
      else
      ; // just skip
    end
    else
    begin
      if Changed then
        NewArray.PushBack(GridArray.Items[i]);
    end;
  end;

  // copy new array
  if Changed then
  begin
    FreeAndNil(GridArray);
    GridArray := NewArray;
    c := GridArray.Size;
    c := c - 1;
    for i := 0 to c do
    begin
      itemTest := GridArray.Items[i];
      itemTest.GridIndex := i;
    end;
    GridEmptyCell := 0;
  end;
end;

function TSuperViewPresenter.Count(): integer;
begin
  Result := Items.size;
end;

procedure TSuperViewPresenter.DrawCell(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState; Node: TSuperViewItem);
var
  d: TDrawGridHack;
begin
  d := TDrawGridHack(ViewGrid);
  if Node <> nil then
  begin
    d.DrawCellText(aCol, aRow, aRect, aState, GetTextCompactMode(Node));
  end;
end;

function TSuperViewPresenter.GetText(Node: TSuperViewItem; Col: integer): UTF8String;
begin
  if Node = nil then
    Result := '-'
  else
  begin
    if Col = -1 then
    begin
      Result := '';
      exit;
    end;

    if Col = 0 then
      Result := UTF8String(Node.Name)
    else
    begin
      if Node.Info = Null then
        Result := ''
      else if not VarIsArray(Node.Info) then
      begin
        if Col = 1 then
          Result := VarToStr(Node.Info);
      end
      else if VarIsArray(Node.Info) and (Col - 1 <= VarArrayHighBound(Node.Info, 1)) then
      begin
        Result := VarToStr(Node.Info[Col - 1]);
      end;
    end;
  end;
end;

function TSuperViewPresenter.GetTextCompactMode(Node: TSuperViewItem): UTF8String;
begin
  if Node = nil then
    Result := '-'
  else
  begin
    Result := UTF8String(Node.Name);

    if Node.Info <> Null then
    begin
      if not VarIsArray(Node.Info) then
        Result := Result + ': ' + VarToStr(Node.Info)
      else if VarIsArray(Node.Info) then
        Result := Result + ': ' + VarToStr(Node.Info[0]);
    end;
  end;
end;

procedure TSuperViewPresenter.SetItemRange(RangeBegin, RangeEnd: integer;
  DelaultValue: variant; Clear: boolean);
var
  i: integer;
begin
  ViewTree.BeginUpdate;
  if ViewGrid <> nil then
    ViewGrid.BeginUpdate;
  if Clear then
    self.Clear();
  for i := RangeBegin to RangeEnd do
  begin
    Add(TSuperViewPath(IntToStr(i)), DelaultValue);
  end;
  if ViewGrid <> nil then
    ViewGrid.EndUpdate(True);
  ViewTree.EndUpdate;
end;

procedure TSuperViewPresenter.SetItemRange(RangeBegin, RangeEnd: integer);
begin
  SetItemRange(RangeBegin, RangeEnd, Null, True);
end;

procedure TSuperViewPresenter.ThreadAddEvent(Path: TSuperViewPath;
  Info: TSuperViewInfo; EventType: TSuperViewEventType);
//todo: var NeedFastSync: boolean;
begin
  //NeedFastSync:=False;
  ThreadListAddLock.Enter;
  try
    //todo: inc overflow count
    if ThreadListAddItems.Size() < ThreadEventsMaxCount then
    begin
      ThreadListAddItems.PushBack(SuperViewEvent(Path, Info, EventType, GetTickCount64));
    end
    else
      InterLockedIncrement(Counters.LostEvents);

    if (TimerEnabled or DropEnableTimer) then
    // -
    else
    begin
      DropEnableTimer := True;
      TThread.Queue(nil, @DoEnableTimer);
    end;

    //NeedFastSync:=true;
  finally
    ThreadListAddLock.Leave;
  end;
  //if NeedFastSync then
  //  TThread.Synchronize(nil, @SwapBufferBeetwenThread);
end;

procedure TSuperViewPresenter.ThreadSync();
begin
  TThread.Synchronize(nil, @DoThreadSync);
end;

procedure TSuperViewPresenter.DoThreadSync();
begin
  if ThreadListAddItems.Size > 0 then
    if not TimerSync.Enabled then
    begin
      TimerSync.Enabled := True;
      DropEnableTimer := False;
      TimerEnabled := True;
    end;
end;

procedure TSuperViewPresenter.EnumSelected(Proc: TSuperViewEnumItemsProc; UserData: pointer);

  procedure EnumGrid(Proc: TSuperViewEnumItemsProc; UserData: pointer);
  var
    i, x, y, index: integer;
    item: TSuperViewItem;
    del, stop: boolean;
  begin
    stop:=false;
    for i := 0 to ViewGrid.SelectedRangeCount - 1 do
      for x := ViewGrid.SelectedRange[i].TopLeft.X to
        ViewGrid.SelectedRange[i].BottomRight.X do
        for y := ViewGrid.SelectedRange[i].TopLeft.Y to
          ViewGrid.SelectedRange[i].BottomRight.Y do
        begin
          del:=false;
          if stop then exit;
          index := GridXYToIndex(x, y);
          if (index < GridArray.Size) and Assigned(GridArray[GridXYToIndex(x, y)]) then
          begin
            item := GridArray[GridXYToIndex(x, y)];
            Proc(self, item, del, stop, UserData);
            if del then
              self.Del(item.Path);
            if stop then
              exit;
          end;
        end;
  end;

  procedure EnumTree(Proc: TSuperViewEnumItemsProc; UserData: pointer);
  var
    i: PVirtualNode;
    c: integer;
    del, stop: boolean;
  begin
    stop:=false;
    c := ViewTree.SelectedCount;
    while (c > 0) do
    begin
      Dec(c);
      for i in ViewTree.SelectedNodes() do
        if GetObjValid(ViewTree, i) then
        begin
          del:=false;
          Proc(self, (GetObj(ViewTree, i) as TSuperViewItem), del, stop, UserData);
          if del then begin
            self.Del((GetObj(ViewTree, i) as TSuperViewItem).Path, True);
            if stop then exit;
            // stop enumeration - enum is broken after delete items
            break;
          end;
          if stop then exit;
        end;
      if ViewTree.SelectedCount = 0 then
        break;
    end;

  end;

begin
  if ViewMode = ViewModeCompactGrid then
    EnumGrid(Proc, UserData)
  else if ViewMode = ViewModeTree then
    EnumTree(Proc, UserData);
end;

function TSuperViewPresenter.SelectedPresent(): boolean;
begin
  Result := False;
  if ViewMode = ViewModeCompactGrid then begin
    Result := GridItemByIndex(
      GridXYToIndex(ViewGrid.Selection.Left, ViewGrid.Selection.Top)) <> nil;
  end
  else if ViewMode = ViewModeTree then begin
    if (ViewTree.SelectedCount > 0) and
       (ViewTree.FocusedNode <> nil) and
       GetObjValid(ViewTree, ViewTree.FocusedNode) then
      Result := True;
  end;
end;

function TSuperViewPresenter.SelectedItem(): TSuperViewItem;
begin
  if SelectedPresent() then
  begin
    if ViewMode = ViewModeCompactGrid then
      Result := GridArray[GridXYToIndex(ViewGrid.Selection.Left, ViewGrid.Selection.Top)]
    else if ViewMode = ViewModeTree then begin
      Result := GetObj(ViewTree, ViewTree.FocusedNode) as TSuperViewItem;
    end
  end else
    raise ERangeError.Create('No selected item');
end;

procedure TSuperViewPresenter.Clear();
var
  itr: TMapSuperView.TIterator;
  i: TSuperViewItem;
begin
  // clear tree
  ViewTree.Clear();

  // clear items
  itr := Items.Iterator();
  try
    if itr <> nil then
      repeat
        i := itr.GetValue;
        i.Free;
      until not itr.Next;
  finally
    if itr <> nil then
      FreeAndNil(itr);
  end;
  FreeAndNil(Items);
  Items := TMapSuperView.Create;

  // clear grid
  GridEmptyCell := 0;
  GridArray.Clear();
  GridRecalc();
end;

function TSuperViewPresenter.GetCounters(): TPresenterCounter;
begin
  CountersLock.Enter;
  Result := Counters;
  CountersLock.Leave;
end;

function TSuperViewPresenter.Add(Path: TSuperViewPath; Info: TSuperViewInfo): TSuperViewItem;
  {procedure FillUserDataOnCreate(Tree: TVirtualStringTree; Node: PVirtualNode; Name: WideString; Level: integer; UserData: pointer);
  begin
    //Item:=TSuperViewItem.Create(Path, Info);
    //SetObj(Tree, Node, );
  end;}
var
  Item: TSuperViewItem;
  PathSlice: array of WideString;
  PathCut: TSuperViewPath;
  NodeTarget, NodeTmp: PVirtualNode;
  i: integer;
begin
  Result := nil;
  PathSlice := explode(PathSplit, WideString(Path));

  NodeTarget := ViewTree.RootNode;
  // iterate all slice of path
  for i := Low(PathSlice) to High(PathSlice) do
  begin
    // create 'cuted path'
    PathCut := GetPathCut(PathSlice, PathSplit, i);

    if not Items.contains(PathCut) then
    begin
      // this path not present - create

      // create empty path object
      if not Items.contains(PathCut) then
      begin
        if i <> High(PathSlice) then
        //todo: add fabric
          Item := TSuperViewItem.Create(PathCut, PathSlice[i], Null)
        else
        begin
          Item := TSuperViewItem.Create(PathCut, PathSlice[i], Info);
          Result := Item;
        end;
        Items.insert(PathCut, Item);
      end
      else
        Item := Items.Items[PathCut];

      NodeTmp := ViewTree.InsertNode(NodeTarget, amAddChildLast);
      NodeTarget := NodeTmp;
      SetObj(ViewTree, NodeTmp, Item);
      Item.Node := NodeTmp;

      // if add last item
      if i = High(PathSlice) then
      begin
        // update Grid
        if (ViewGrid <> nil) then
        begin
          Item.GridIndex := GridArray.Size;
          GridArray.PushBack(Item);
          if ViewMode = ViewModeCompactGrid then
          begin
            GridRecalc();
            // if empty cell too many - then compact
            if GridEmptyCell > GridMaxEmptyCell then
              GridResizeAndCompact();
          end;
        end;
      end;
    end
    else
    begin
      // this path already contain in "path map"

      // shift target node
      NodeTarget := Items.Items[PathCut].Node;

      // if this final item - then just update info
      if i = High(PathSlice) then
      begin
        Item := Items.Items[PathCut];
        if Item.Info <> Info then
        begin
          Item.Info := Info;

          if (ViewMode = ViewModeCompactGrid) then
            GridRepaintCell(Items.Items[PathCut].GridIndex)
          else if (ViewMode = ViewModeTree) then
          begin
            if Items.Items[PathCut].Node = nil then
              ShowError('add node=nil!', 0); //dbg!
            ViewTree.InvalidateNode(Items.Items[PathCut].Node);
          end;
        end;
      end;
    end;
  end;
end;

function TSuperViewPresenter.Del(Path: TSuperViewPath; DeleteSubItems: boolean): boolean;

  procedure TreeDeleteSubItems(Node: PVirtualNode);
  var
    i: PVirtualNode;
    Item: TSuperViewItem;
  begin
    // free items in nodes
    for i in ViewTree.ChildNodes(Node) do
    begin
      TreeDeleteSubItems(i);
      if GetObjValid(ViewTree, i) then
      begin
        Item := GetObj(ViewTree, i) as TSuperViewItem;
        DeleteItem(Item);
        SetObj(ViewTree, i, nil);
      end;
    end;
  end;

var
  //Item, TargetItem: TSuperViewItem;
  //PathCut: TSuperViewPath;
  PathSlice: array of WideString;
  PathNodes: array of PVirtualNode;
  i: integer;

begin
  Result := False;
  if Items.contains(Path) then
  begin
    PathSlice := explode(PathSplit, WideString(Path));

    // iterate all slice of path
    // for i:=Low(PathSlice) to High(PathSlice) do ...
    //PathCut := GetPathCut(PathSlice, PathSplit, i); ...

    //todo: remove VTV_FindByPath - use "path map"
    if VTV_FindByPath(ViewTree, ViewTree.RootNode, PathSlice, PathNodes) then
    begin
      if Assigned(OnChanged) then
        OnChanged(self);

      if DeleteSubItems then
        if PathNodes[High(PathNodes)]^.ChildCount > 0 then
        begin
          TreeDeleteSubItems(PathNodes[High(PathNodes)]);
          ViewTree.DeleteChildren(PathNodes[High(PathNodes)], True);
        end;

      for i := High(PathNodes) downto Low(PathNodes) do
      begin
        if PathNodes[i]^.ChildCount = 0 then
        begin
          Result := True;
          if GetObjValid(ViewTree, PathNodes[i]) then
            DeleteItem(GetObj(ViewTree, PathNodes[i]) as TSuperViewItem)
          else
            ShowError('GetObjValid(ViewTree, PathNodes[i])=nil', 0);
          ViewTree.DeleteNode(PathNodes[i]);
        end
        else
        begin
          // branch have items - stop
          break;
        end;
      end;
    end;
  end;
end;

procedure TSuperViewPresenter.Update(Path: TSuperViewPath; Info: TSuperViewInfo);
var
  Item: TSuperViewItem;
begin
  //todo: repaint
  if Items.contains(Path) then
  begin
    Item := Items.Items[Path];
    Item.Info := Info;
    if ViewMode = ViewModeCompactGrid then
      GridRepaintCell(Item.GridIndex);
  end;
end;

function TSuperViewPresenter.Find(Path: TSuperViewPath): TSuperViewItem;
begin
  if Items.contains(Path) then
    Result := Items.Items[Path]
  else
    Result := nil;
end;

end.

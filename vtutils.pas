unit VTUtils;
// version 1.1.0
// Common library!
// Tree View Utils:
//   VT version 5.5.3

{$MODE Delphi}

interface

uses
  SysUtils,
  VirtualTrees;

type
  ArrayOfTreeNode = array of PVirtualNode;
  ArrayOfBoolean = array of boolean;

//function AddNodeChild(tree: TVirtualStringTree; Parent: PVirtualNode): PVirtualNode;

function GetObj(T: TBaseVirtualTree; N: PVirtualNode): TObject; overload;

function GetObjValid(T: TBaseVirtualTree; N: PVirtualNode): boolean;

procedure SetObj(T: TBaseVirtualTree; N: PVirtualNode; O: TObject);

function VTV_GetPtrToMem(T: TBaseVirtualTree; N: PVirtualNode; var P: pointer): boolean;

type
  TGetNodeText = function(Tree: TVirtualStringTree; Node: PVirtualNode): WideString;
  TFilterNodeByText = function(Tree: TVirtualStringTree; Node: PVirtualNode;
    const FilterText: WideString): boolean;

// hide nodes if he not have text
// Result: true - in brach present visible nodes; false - branch is full hide
function VTV_FilterByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FilterText: WideString): boolean; overload;

function VTV_FilterByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FilterText: WideString; FilterProc: TFilterNodeByText): boolean; overload;

// find all nodes by text and return in Nodes array
procedure VTV_FindByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FindText: WideString; out Nodes: ArrayOfTreeNode;
  GetText: TGetNodeText = nil); overload;

procedure VTV_ShowAll(VTV: TBaseVirtualTree; RootNode: PVirtualNode);

// find one node where GetObj=ObjPtr
function VTV_FindByObj(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const ObjPtr: TObject): PVirtualNode;

// find nodes by path
function VTV_FindByPath(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FindPath: array of WideString; out Nodes: ArrayOfTreeNode;
  CaseSens: boolean = true): boolean;

// create tree struct by path
procedure VTV_CreatePath(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const CreatePath: array of WideString;
  out Nodes: ArrayOfTreeNode;
  out NewNodes: ArrayOfBoolean; CaseSens: boolean = true);

{
see: TStringEditLink

  TVTCustomEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TWinControl;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;
}

implementation

function VTV_FilterByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FilterText: WideString): boolean;
var
  CurrentNode: PVirtualNode;
  VisChild, Vis: boolean;
begin
  Result := False;
  CurrentNode := RootNode^.FirstChild;
  while (CurrentNode <> nil) do
  begin
    // VisChild=true if present visible childs
    VisChild := VTV_FilterByText(VTV, CurrentNode, FilterText); // <- RECURSION!
    // Filter! Vis=false - hide
    Vis := Pos(LowerCase(FilterText), LowerCase(VTV.Text[CurrentNode, 0])) <> 0;
    // if any node or any child is visible - result true
    Result := Result or Vis or VisChild;
    // current node is visible if current node not filtered or present visible child
    VTV.IsVisible[CurrentNode] := Vis or VisChild;
    // iteration
    CurrentNode := CurrentNode^.NextSibling;
  end;
  // constructor [ heX ]
end;

function VTV_FilterByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FilterText: WideString; FilterProc: TFilterNodeByText): boolean;
var
  CurrentNode: PVirtualNode;
  VisChild, Vis: boolean;
begin
  Result := False;
  CurrentNode := RootNode^.FirstChild;
  while (CurrentNode <> nil) do
  begin
    // VisChild=true if present visible childs
    VisChild := VTV_FilterByText(VTV, CurrentNode, FilterText, FilterProc);
    // <- RECURSION!
    // Filter! Vis=false - hide
    Vis := FilterProc(VTV, CurrentNode, FilterText);
    // if any node or any child is visible - result true
    Result := Result or Vis or VisChild;
    // current node is visible if current node not filtered or present visible child
    VTV.IsVisible[CurrentNode] := Vis or VisChild;
    // iteration
    CurrentNode := CurrentNode^.NextSibling;
  end;
  // constructor [ heX ]
end;

procedure VTV_FindByText(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FindText: WideString; out Nodes: ArrayOfTreeNode; GetText: TGetNodeText
  );
var
  CurrentNode: PVirtualNode;
  t: WideString;
begin
  CurrentNode := RootNode^.FirstChild;
  while (CurrentNode <> nil) do
  begin
    // Filter!
    if @GetText = nil then
      t := WideLowerCase(WideString(VTV.Text[CurrentNode, 0]))
    else
      t := GetText(VTV, CurrentNode);
    if FindText = t then
    begin
      // add finded item to array
      SetLength(Nodes, Length(Nodes) + 1);
      Nodes[Length(Nodes) - 1] := CurrentNode;
    end;
    VTV_FindByText(VTV, CurrentNode, FindText, Nodes, GetText); // <- RECURSION!
    CurrentNode := CurrentNode^.NextSibling;
  end;
  // constructor [ heX ]
end;

procedure VTV_ShowAll(VTV: TBaseVirtualTree; RootNode: PVirtualNode);
var
  CurrentNode: PVirtualNode;
begin
  CurrentNode := RootNode^.FirstChild;
  while (CurrentNode <> nil) do
  begin
    VTV.IsVisible[CurrentNode] := True;
    VTV_ShowAll(VTV, CurrentNode); // <- RECURSION!
    CurrentNode := CurrentNode^.NextSibling;
  end;
end;

function VTV_FindByObj(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const ObjPtr: TObject): PVirtualNode;
var
  CurrentNode: PVirtualNode;
begin
  Result := nil;
  CurrentNode := RootNode^.FirstChild;
  while (CurrentNode <> nil) do
  begin
    if GetObjValid(VTV, CurrentNode) and (GetObj(VTV, CurrentNode)=ObjPtr) then
    begin
      Result := CurrentNode;
      break;
    end;
    Result := VTV_FindByObj(VTV, CurrentNode, ObjPtr); // <- RECURSION!
    if Result <> nil then
      break;
    CurrentNode := CurrentNode^.NextSibling;
  end;
  // constructor [ heX ]
end;

function VTV_FindByPath(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const FindPath: array of WideString; out Nodes: ArrayOfTreeNode;
  CaseSens: boolean): boolean;
var
  CurrentNode: PVirtualNode;
  t: WideString;
  FindText: WideString;
  finded: boolean;
  i: integer;
begin
  Result:=false;
  SetLength(Nodes, Length(FindPath));
  for i:=Low(FindPath) to High(FindPath) do
    Nodes[i]:=nil;
  CurrentNode := RootNode^.FirstChild;
  for i:=Low(FindPath) to High(FindPath) do begin
    if CaseSens then
      FindText := FindPath[i] else
      FindText := WideLowerCase(FindPath[i]);
    finded := false;
    while (CurrentNode <> nil) do begin
      if CaseSens then
        t := WideString(VTV.Text[CurrentNode, 0]) else
        t := WideLowerCase(WideString(VTV.Text[CurrentNode, 0]));
      if FindText = t then begin
        // finded
        finded := true;
        Break;
      end;
      CurrentNode := CurrentNode^.NextSibling;
    end; // while
    if not finded then
      // not found - fail exit
      break
    else
      if i = High(FindPath) then begin
        // its last iteration - finded
        Nodes[i]:=CurrentNode;
        Result := true;
        break;
      end else
        if CurrentNode^.FirstChild = nil then begin
          // path is broken - fail exit
          break;
        end else begin
          // goto deeper
          Nodes[i]:=CurrentNode;
          CurrentNode:=CurrentNode^.FirstChild;
        end;
  end;

  // constructor [ heX ]
end;

procedure VTV_CreatePath(VTV: TVirtualStringTree; RootNode: PVirtualNode;
  const CreatePath: array of WideString; out Nodes: ArrayOfTreeNode; out
  NewNodes: ArrayOfBoolean; CaseSens: boolean);
var
  CurrentNode, LevelNode: PVirtualNode;
  t: WideString;
  FindText: WideString;
  stop: boolean;
  i, StartCreateLevel: integer;
begin
  StartCreateLevel := 0;

  SetLength(Nodes, Length(CreatePath));
  SetLength(NewNodes, Length(CreatePath));
  // find cycle
  LevelNode := RootNode;
  for i:=Low(CreatePath) to High(CreatePath) do begin
    NewNodes[i] := false;
    if CaseSens then
      FindText := CreatePath[i] else
      FindText := WideLowerCase(CreatePath[i]);
    stop := false;
    CurrentNode := LevelNode^.FirstChild;
    while (CurrentNode <> nil) do begin
      // Filter!
      if CaseSens then
        t := WideString(VTV.Text[CurrentNode, 0]) else
        t := WideLowerCase(WideString(VTV.Text[CurrentNode, 0]));
      if FindText = t then begin
        Nodes[i] := CurrentNode;
        // finded! can goto deeper?
        if CurrentNode^.FirstChild <> nil then begin
          LevelNode:=CurrentNode;
          break;
        end else begin
          // stop 'for'
          stop := true;
          StartCreateLevel := i+1;
          break;
        end;
      end;
      CurrentNode := CurrentNode^.NextSibling;
    end; // while
    // if not found:
    if CurrentNode=nil then begin
      stop:=true;
      StartCreateLevel := i;
      CurrentNode := LevelNode;
    end;
    if stop then break;
  end;

  // creation cycle
  if stop then
    for i:=StartCreateLevel to High(CreatePath) do begin
      NewNodes[i] := true;
      CurrentNode := VTV.InsertNode(CurrentNode, amAddChildLast, nil);
      Nodes[i] := CurrentNode;
    end;
  // constructor [ heX ]
end;

{
function AddNodeChild(tree: TVirtualStringTree; Parent: PVirtualNode): PVirtualNode;
var
  o: TParseItem;
begin
  o := TParseItem.Create(stsNone);
  Result := tree.AddChild(Parent, pointer(o));
end;
}

function GetObj(T: TBaseVirtualTree; N: PVirtualNode): TObject;
var
  p: PPointer;
begin
  if not GetObjValid(T, N) then
  begin
    //ShowMessage('GetObj - node invalid!');
    raise EObjectCheck.Create('GetObj - node invalid!');
  end;
  p := T.GetNodeData(N);
  Result := (TObject(p^)) as TObject;
end;

function GetObjValid(T: TBaseVirtualTree; N: PVirtualNode): boolean;
var
  p: PPointer;
begin
  Result := False;
  if (T <> nil) and (N <> nil) then
  begin
    p := T.GetNodeData(N);
    if p <> nil then
    begin
      if TObject(p^) is TObject then
        Result := True;
    end;
  end;
end;

procedure SetObj(T: TBaseVirtualTree; N: PVirtualNode; O: TObject);
var
  pp: PPointer;
begin
  pp := T.GetNodeData(N);
  pp^ := pointer(O);
end;

function VTV_GetPtrToMem(T: TBaseVirtualTree; N: PVirtualNode; var P: pointer
  ): boolean;
begin
  P := T.GetNodeData(N);
  Result := P <> nil;
end;



end.

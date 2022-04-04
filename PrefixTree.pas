unit PrefixTree;

{$mode delphi}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { TTreeNode }

  TTreeNode = class
    abstract
  protected
    SubNodes: TList<TTreeNode>;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(constref Node: TTreeNode): TTreeNode;

    function CheckName(const DomainName: string; const Left, Right: integer): boolean;
      virtual; abstract;
  end;

  { TFixedNode }

  TFixedNode = class sealed (TTreeNode)
  public
    constructor Create(const Name: string);

    function CheckName(const DomainName: string; const Left, Right: integer): boolean;
      override;
  end;

  { TDNSTree }

  TDNSTree = class
    sealed
  private
    RootNode: TFixedNode;
  public
    constructor Create;
    destructor Destroy; override;

    function CheckName(const DomainName: string): boolean;
  end;

implementation

{ TFixedNode }

constructor TFixedNode.Create(const Name: string);
begin
  inherited Create;
end;

function TFixedNode.CheckName(const DomainName: string;
  const Left, Right: integer): boolean;
begin
  Result := False;
end;

{ TTreeNode }

constructor TTreeNode.Create;
begin
  SubNodes := TList<TTreeNode>.Create;
end;

destructor TTreeNode.Destroy;
var
  i: integer;
begin
  for i := 0 to SubNodes.Count - 1 do
    SubNodes[i].Free;

  FreeAndNil(SubNodes);

  inherited Destroy;
end;

function TTreeNode.Add(constref Node: TTreeNode): TTreeNode;
begin
  SubNodes.Add(Node);
  Result := Node;
end;

{ TDNSTree }

constructor TDNSTree.Create;
begin
  RootNode := TFixedNode.Create('[root]');
end;

destructor TDNSTree.Destroy;
begin
  FreeAndNil(RootNode);

  inherited Destroy;
end;

function TDNSTree.CheckName(const DomainName: string): boolean;
var
  NameList: string;
  i: integer;
  FirstName, LastName: PChar;
begin
  if DomainName = '' then
  begin
    Result := False;
    Exit;
  end;

  // make local copy
  NameList := Copy(DomainName);

  // "split" into reversed list of null-terminates strings
  for i := 1 to Length(NameList) do
    if NameList[i] = '.' then
    begin
      NameList[i] := #0;
      LastName := @NameList[i + 1];
    end;
  FirstName := @NameList[1];

  Result := RootNode.CheckName(NameList, FirstName, LastName);

  SetLength(NameList, 0);
end;

end.

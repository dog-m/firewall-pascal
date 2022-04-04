unit StateMachine;

{$mode delphi}

interface

type
  TTransitionAction = procedure of object;

  { TStateMachine }

  TStateMachine = class abstract
  protected
    NextAction: TTransitionAction;

    procedure SetNextAction(const Action: TTransitionAction);
    function GetTerminated: boolean;
  public
    constructor Create; virtual;

    procedure DoStep;
    procedure Reset; virtual; abstract;
    procedure ForcedTermination; virtual;

    property Terminated: boolean read GetTerminated;
  end;

const
  NO_ACTION: TTransitionAction = nil;

implementation

{ TStateMachine }

procedure TStateMachine.SetNextAction(const Action: TTransitionAction);
begin
  NextAction := Action;
end;

procedure TStateMachine.DoStep;
var
  action: TTransitionAction;
begin
  if Assigned(NextAction) then
  begin
    // reset the next action
    action := NextAction;
    NextAction := NO_ACTION;

    // do the work
    action();
  end;
end;

procedure TStateMachine.ForcedTermination;
begin
  NextAction := NO_ACTION;
end;

function TStateMachine.GetTerminated: boolean;
begin
  Result := not Assigned(NextAction);
end;

constructor TStateMachine.Create;
begin
  NextAction := NO_ACTION;
end;

end.

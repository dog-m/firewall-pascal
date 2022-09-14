unit StateMachine;

{$mode delphi}

interface

type
  TTransitionAction = procedure of object;

  { TStateMachine }

  TStateMachine = class abstract
  private
    NextAction: TTransitionAction;
    Cancelled: boolean;

    function GetFinished: boolean;
  protected
    procedure CancellHandler; virtual;
    procedure SetNextAction(const Action: TTransitionAction);
  public
    constructor Create; virtual;

    procedure DoStep;
    procedure Reset; virtual;
    procedure Cancell; virtual;

    property IsFinished: boolean read GetFinished;
    property IsCancelled: boolean read Cancelled;
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

    // handle canceling
    if Cancelled then
    begin
      // call the handler
      CancellHandler;

      // overwrite further actions
      NextAction := NO_ACTION;
    end;
  end;
end;

procedure TStateMachine.Reset;
begin
  NextAction := NO_ACTION;
  Cancelled := False;
end;

procedure TStateMachine.Cancell;
begin
  Cancelled := True;
end;

function TStateMachine.GetFinished: boolean;
begin
  Result := not Assigned(NextAction);
end;

procedure TStateMachine.CancellHandler;
begin
end;

constructor TStateMachine.Create;
begin
  NextAction := NO_ACTION;
  Cancelled := False;
end;

end.

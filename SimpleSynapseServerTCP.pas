unit SimpleSynapseServerTCP;

{$mode delphi}

interface

uses
  Classes, SysUtils, blcksock, JobHTTP;

type
  TSimpleServerHTTP = class;

  { TListeningThread }

  TListeningThread = class(TThread)
  protected
    Server: TSimpleServerHTTP;
  public
    constructor Create(const AServer: TSimpleServerHTTP);

    procedure Execute; override;
  end;

  { TSimpleServerHTTP }

  TSimpleServerHTTP = class
  protected
    Listening: boolean;
    ListenerSocket: TTCPBlockSocket;
    Listener: TListeningThread;
    RequestJobManager: TJobManagerHTTP;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure StartListening(const Address, Port: string); virtual;
    procedure CheckForNewConnections; virtual;
  public
    property JobManager: TJobManagerHTTP read RequestJobManager;
  end;

const
  BANDWIDTH_UNLIMITED = JobHTTP.BANDWIDTH_UNLIMITED;

implementation

{ TListeningThread }

constructor TListeningThread.Create(const AServer: TSimpleServerHTTP);
begin
  inherited Create(False);
  Priority := tpLowest;
  Server := AServer;
end;

procedure TListeningThread.Execute;
begin
  while not Terminated do
  begin
    Server.CheckForNewConnections;

    Sleep(1000 div 22);
  end;
end;

{ TSimpleServerHTTP }

constructor TSimpleServerHTTP.Create;
begin
  Listening := False;
  RequestJobManager := TJobManagerHTTP.Create(TJobHTTP);

  ListenerSocket := TTCPBlockSocket.Create;
  Listener := TListeningThread.Create(Self);
end;

destructor TSimpleServerHTTP.Destroy;
begin
  Listening := False;
  Listener.Terminate;
  Listener.WaitFor;
  FreeAndNil(Listener);

  ListenerSocket.CloseSocket;

  FreeAndNil(ListenerSocket);
  FreeAndNil(RequestJobManager);

  inherited Destroy;
end;

procedure TSimpleServerHTTP.StartListening(const Address, Port: string);
begin
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(True, 10);

  ListenerSocket.Bind(Address, Port);
  if ListenerSocket.LastError <> 0 then
    raise Exception.Create(ListenerSocket.LastErrorDesc);

  ListenerSocket.Listen;
  if ListenerSocket.LastError <> 0 then
    raise Exception.Create(ListenerSocket.LastErrorDesc);

  ListenerSocket.EnableReuse(True);
  Listening := True;
end;

procedure TSimpleServerHTTP.CheckForNewConnections;
var
  NewConnection: TSocket;
begin
  if not Listening then Exit;

  if ListenerSocket.CanRead(10) then
  begin
    NewConnection := ListenerSocket.Accept;
    RequestJobManager.AddJob(NewConnection);
  end;
end;

end.

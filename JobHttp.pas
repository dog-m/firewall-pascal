unit JobHTTP;

{$mode delphi}

interface

uses
  JobSystem, blcksock, SysUtils, Classes;

type
  TSocket = PtrUInt;

  THostNameChecker = function(const Host: string): boolean of object;

  TStatisticsAllowedCounter = procedure(const ClientIP: string;
    const Host: string) of object;

  TStatisticsBlockedCounter = procedure(const ClientIP: string;
    const Host: string) of object;

  TStatisticsTransferedCounter = procedure(const C2S: integer;
    const S2C: integer) of object;

  { TJobHTTP }

  TJobHTTP = class(TJob<TSocket>)
  protected
    Client: TTCPBlockSocket;
    Server: TTCPBlockSocket;

    procedure HTTP_Init;
    procedure HTTP_Check;
    procedure HTTP_Connect;
    procedure HTTP_Serve;
    procedure HTTP_Disconnect;

    procedure SetJobData(const Data: TSocket); override;
  protected
    RequestCmd: ansistring;
    IsSecure: boolean;
    TargetHost: string;
    TargetPort: string;

    procedure ParseHostAndPort(const Address: string; const DefaultPort: string);
    function ForwardPacket(const Src: TBlockSocket; const Dst: TBlockSocket): integer;

    function CheckHostName: boolean;
    procedure AllowConnection;
    procedure BlockConnection;
  protected
    HostNameChecker: THostNameChecker;
    Handler_StatAllowed: TStatisticsAllowedCounter;
    Handler_StatBlocked: TStatisticsBlockedCounter;
    Handler_StatTransfered: TStatisticsTransferedCounter;
    BandwidthMax: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
  public
    procedure SetRequestChecker(const Handler: THostNameChecker);
    procedure SetHandler_StatAllowed(const Handler: TStatisticsAllowedCounter);
    procedure SetHandler_StatBlocked(const Handler: TStatisticsBlockedCounter);
    procedure SetHandler_StatTransfered(const Handler: TStatisticsTransferedCounter);

    procedure SetBandwidthMax(const Value: integer);
  end;

  { TJobManagerHTTP }

  TJobManagerHTTP = class(TJobManager<TSocket>)
  public
    procedure SetRequestChecker(const Handler: THostNameChecker);
    procedure SetHandler_StatAllowed(const Handler: TStatisticsAllowedCounter);
    procedure SetHandler_StatBlocked(const Handler: TStatisticsBlockedCounter);
    procedure SetHandler_StatTransfered(const Handler: TStatisticsTransferedCounter);

    procedure SetBandwidthMax(const Value: integer);
  end;

const
  BANDWIDTH_UNLIMITED = 0;

implementation

const
  INVALID_SOCKET = TSocket(not (0));

  HTTP_METHODS_ALLOWED = 'CONNECT-GET-POST-PUT-DELETE-HEAD-OPTIONS-TRACE-PATCH';
  HTTP_PROXY_METHOD = 'CONNECT';

  DEFAULT_PORT_str_HTTP = '80';
  DEFAULT_PORT_str_HTTPS = '443';

  HTTP_REPLY_CONNECTED = 'HTTP/1.1 200 Connection established';
  HTTP_REPLY_FORBIDDEN = 'HTTP/1.1 403 Forbidden';
  HTTP_REPLY_INTERNAL_ERROR = 'HTTP/1.1 500 Internal Error';
  HTTP_REPLY_SERVICE_UNAVAILABLE = 'HTTP/1.1 503 Service Unavailable';

  HTTP_CONNECTION_TIMEOUT = 5250; // ms
  HTTP_READ_TIMEOUT = 4; // ms

{ TJobManagerHTTP }

procedure TJobManagerHTTP.SetRequestChecker(const Handler: THostNameChecker);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).SetRequestChecker(Handler);
end;

procedure TJobManagerHTTP.SetHandler_StatAllowed(
  const Handler: TStatisticsAllowedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).SetHandler_StatAllowed(Handler);
end;

procedure TJobManagerHTTP.SetHandler_StatBlocked(
  const Handler: TStatisticsBlockedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).SetHandler_StatBlocked(Handler);
end;

procedure TJobManagerHTTP.SetHandler_StatTransfered(
  const Handler: TStatisticsTransferedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).SetHandler_StatTransfered(Handler);
end;

procedure TJobManagerHTTP.SetBandwidthMax(const Value: integer);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).SetBandwidthMax(Value);
end;

{ TJobHTTP }

procedure TJobHTTP.HTTP_Init;
const
  INDEX_METHOD = 0;
  INDEX_ADDRESS = 1;
var
  Args: TStringList;
begin
  Args := TStringList.Create;

  if Client.CanReadEx(HTTP_READ_TIMEOUT) and (Client.LastError = 0) then
  begin
    RequestCmd := Client.RecvString(0);

    Args.Delimiter := #32;
    Args.DelimitedText := RequestCmd;
  end;

  // is it actually a valid request?
  if (Args.Count > 1) and (Pos(Args[INDEX_METHOD], HTTP_METHODS_ALLOWED) <> 0) then
  begin
    // parse the request
    IsSecure := (Args[INDEX_METHOD] = HTTP_PROXY_METHOD);
    if IsSecure then
      ParseHostAndPort(Args[INDEX_ADDRESS], DEFAULT_PORT_str_HTTPS)
    else
      ParseHostAndPort(Args[INDEX_ADDRESS], DEFAULT_PORT_str_HTTP);

    // special case
    if TargetHost = '' then
      SetNextAction(HTTP_Disconnect)
    else
      SetNextAction(HTTP_Check);
  end
  else
    SetNextAction(HTTP_Disconnect);

  Args.Clear;
  FreeAndNil(Args);
end;

procedure TJobHTTP.HTTP_Check;
begin
  if CheckHostName then
  begin
    AllowConnection;
    SetNextAction(HTTP_Connect);
  end
  else
  begin
    BlockConnection;
    SetNextAction(HTTP_Disconnect);
  end;
end;

procedure TJobHTTP.HTTP_Connect;
var
  Headers: TStringList;
  HeaderLine: ansistring;
  i: integer;
begin
  Headers := TStringList.Create;
  try
    // capture headers
    while Client.CanReadEx(HTTP_READ_TIMEOUT) do
    begin
      HeaderLine := Client.RecvString(0);

      if HeaderLine.IsEmpty or (Client.LastError <> 0) then
        Break;

      Headers.Add(HeaderLine);
    end;

    if Client.LastError <> 0 then
    begin
      Client.SendString(HTTP_REPLY_INTERNAL_ERROR + CRLF + CRLF +
        '<h1>Error 500: Invalid request header format</h1>');

      SetNextAction(HTTP_Disconnect);
      Exit;
    end;

    TargetHost := Server.ResolveName(TargetHost);
    Server.Connect(TargetHost, TargetPort);

    if Server.LastError <> 0 then
    begin
      Client.SendString(HTTP_REPLY_SERVICE_UNAVAILABLE + CRLF +
        CRLF + '<h1>Error 503: ' + Server.LastErrorDesc + '</h1>');

      SetNextAction(HTTP_Disconnect);
      Exit;
    end;

    if IsSecure then
    begin
      // proxying httpS request
      Client.SendString(HTTP_REPLY_CONNECTED + CRLF + CRLF);
    end
    else
    begin
      // proxying httP request
      Server.SendString(RequestCmd + CRLF);

      for i := 0 to Pred(Headers.Count) do
        Server.SendString(Headers[i] + CRLF);

      Server.SendString(CRLF);
    end;

    // serving as usual
    SetNextAction(HTTP_Serve);
  finally
    SetLength(HeaderLine, 0);

    Headers.Clear;
    FreeAndNil(Headers);
  end;
end;

procedure TJobHTTP.HTTP_Serve;
var
  dForward, dBackward: integer;
begin
  // forward: A -> B
  dForward := ForwardPacket(Client, Server);

  // backward: A <- B
  dBackward := ForwardPacket(Server, Client);

  // register the amount of data being passed around
  if Assigned(Handler_StatTransfered) then
    Handler_StatTransfered(dForward, dBackward);

  // state transition
  if (Client.LastError = 0) and (Server.LastError = 0) then
    SetNextAction(HTTP_Serve)
  else
    SetNextAction(HTTP_Disconnect);
end;

procedure TJobHTTP.HTTP_Disconnect;
begin
  Server.CloseSocket;
  Client.CloseSocket;

  SetLength(RequestCmd, 0);
  SetLength(TargetHost, 0);
  SetLength(TargetPort, 0);
end;

procedure TJobHTTP.SetJobData(const Data: TSocket);
begin
  Client.Socket := Data;
end;

procedure TJobHTTP.ParseHostAndPort(const Address: string; const DefaultPort: string);
var
  i: integer;
  Target: string;
begin
  Target := Address;
  // FORMAT:  protocol://host:port/document
  // OR:      host:port
  try
    // remove optional protocol prefix
    i := Pos('://', Target);
    if i <> 0 then
      Target := Copy(Target, i + 3, Length(Target));

    // remove optional document
    i := Pos('/', Target);
    if i <> 0 then
      Target := Copy(Target, 1, i - 1);

    // does port specified?
    i := Pos(':', Target);
    if i <> 0 then
    begin
      TargetHost := Copy(Target, 1, i - 1);
      TargetPort := Copy(Target, i + 1, Length(Target));
    end
    else
    begin
      TargetHost := Target;
      TargetPort := DefaultPort;
    end;
  finally
    SetLength(Target, 0);
  end;
end;

function TJobHTTP.ForwardPacket(const Src: TBlockSocket;
  const Dst: TBlockSocket): integer;
var
  Buff: ansistring;
begin
  try
    // sync the constraints
    if Dst.MaxSendBandwidth <> BandwidthMax then
      Dst.MaxBandwidth := BandwidthMax;

    // pass the data
    if Src.CanReadEx(HTTP_READ_TIMEOUT) and (Dst.LastError = 0) then
    begin
      // receive chunk of data
      Buff := Src.RecvPacket(0);
      Result := Buff.Length;

      // is there anything?
      if (Result = 0) or (Src.LastError <> 0) then
        Exit;

      // send it to other side
      Dst.SendString(Buff);
    end
    else
      Result := 0;
  finally
    SetLength(Buff, 0);
  end;
end;

function TJobHTTP.CheckHostName: boolean;
begin
  if Assigned(HostNameChecker) then
    Result := HostNameChecker(TargetHost)
  else
    Result := True;
end;

procedure TJobHTTP.AllowConnection;
begin
  if Assigned(Handler_StatAllowed) then
    Handler_StatAllowed(Client.GetRemoteSinIP, TargetHost);
end;

procedure TJobHTTP.BlockConnection;
begin
  Client.SendString(HTTP_REPLY_FORBIDDEN + CRLF + CRLF + '<h1>Forbidden</h1>' + CRLF);

  if Assigned(Handler_StatBlocked) then
    Handler_StatBlocked(Client.GetRemoteSinIP, TargetHost);
end;

constructor TJobHTTP.Create;
begin
  inherited Create;

  Client := TTCPBlockSocket.Create;
  Server := TTCPBlockSocket.Create;

  Server.ConnectionTimeout := HTTP_CONNECTION_TIMEOUT;

  BandwidthMax := 0;
end;

destructor TJobHTTP.Destroy;
begin
  HTTP_Disconnect;

  FreeAndNil(Server);
  FreeAndNil(Client);

  inherited Destroy;
end;

procedure TJobHTTP.Reset;
begin
  Client.ResetLastError;
  Server.ResetLastError;
  Client.Socket := INVALID_SOCKET;

  SetNextAction(HTTP_Init);
end;

procedure TJobHTTP.SetRequestChecker(const Handler: THostNameChecker);
begin
  HostNameChecker := Handler;
end;

procedure TJobHTTP.SetHandler_StatAllowed(const Handler: TStatisticsAllowedCounter);
begin
  Handler_StatAllowed := Handler;
end;

procedure TJobHTTP.SetHandler_StatBlocked(const Handler: TStatisticsBlockedCounter);
begin
  Handler_StatBlocked := Handler;
end;

procedure TJobHTTP.SetHandler_StatTransfered(
  const Handler: TStatisticsTransferedCounter);
begin
  Handler_StatTransfered := Handler;
end;

procedure TJobHTTP.SetBandwidthMax(const Value: integer);
begin
  BandwidthMax := Value;
end;

end.

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
    procedure HTTP_Proxy_Config;
    procedure HTTP_Disconnect;

    procedure SetJobData(const Data: TSocket); override;
  protected
    RequestCmd: ansistring;
    IsSecure: boolean;
    TargetHost: string;
    TargetPort: string;
    TargetDocument: string;

    procedure ParseHostAndPort(const Address: string);
    function ForwardPacket(const Src: TBlockSocket; const Dst: TBlockSocket): integer;

    function CheckHostName: boolean;
    procedure AllowConnection;
    procedure BlockConnection;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
  public
    HostNameChecker: THostNameChecker;
    Handler_StatAllowed: TStatisticsAllowedCounter;
    Handler_StatBlocked: TStatisticsBlockedCounter;
    Handler_StatTransfered: TStatisticsTransferedCounter;
    BandwidthMax: integer;

    property Host: string read TargetHost;
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
  TCP_LINGER_TIME = 175; // ms

implementation

const
  INVALID_SOCKET = TSocket(not (0));

  HTTP_METHODS_ALLOWED = 'CONNECT-GET-POST-PUT-DELETE-HEAD-OPTIONS-TRACE-PATCH';
  HTTP_METHOD_PROXY = 'CONNECT';

  HTTP_PORT_DEFAULT_str = '80';
  HTTP_PORT_SECURE_str = '443';

  HTTP_REPLY_OK = 'HTTP/1.1 200 OK';
  HTTP_REPLY_CONNECTED = 'HTTP/1.1 200 Connection established';
  HTTP_REPLY_FORBIDDEN = 'HTTP/1.1 403 Forbidden';
  HTTP_REPLY_INTERNAL_ERROR = 'HTTP/1.1 500 Internal Error';
  HTTP_REPLY_SERVICE_UNAVAILABLE = 'HTTP/1.1 503 Service Unavailable';

  HTTP_HEADER_PROXY_CONFIG = 'application/x-ns-proxy-autoconfig';

  HTTP_CONNECTION_TIMEOUT = 5250; // ms
  HTTP_READ_TIMEOUT = 4; // ms

  HTTP_HOST_LOCAL = 'localhost';

// rudimentary PAC-file handling

var
  pacFileContents: string = '';

function GetPacContent: string;
begin
  if pacFileContents.IsEmpty then
  begin
    with TStringList.Create do
    begin
      if FileExists('./proxy.pac') then
        LoadFromFile('./proxy.pac');

      pacFileContents := Text;

      Free;
    end;
  end;

  Result := pacFileContents;
end;

{ TJobManagerHTTP }

procedure TJobManagerHTTP.SetRequestChecker(const Handler: THostNameChecker);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).HostNameChecker := Handler;
end;

procedure TJobManagerHTTP.SetHandler_StatAllowed(
  const Handler: TStatisticsAllowedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).Handler_StatAllowed := Handler;
end;

procedure TJobManagerHTTP.SetHandler_StatBlocked(
  const Handler: TStatisticsBlockedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).Handler_StatBlocked := Handler;
end;

procedure TJobManagerHTTP.SetHandler_StatTransfered(
  const Handler: TStatisticsTransferedCounter);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).Handler_StatTransfered := Handler;
end;

procedure TJobManagerHTTP.SetBandwidthMax(const Value: integer);
var
  job: JobType;
begin
  for job in JobPool do
    TJobHTTP(job).BandwidthMax := Value;
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
    IsSecure := (Args[INDEX_METHOD] = HTTP_METHOD_PROXY);
    ParseHostAndPort(Args[INDEX_ADDRESS]);

    if (TargetDocument = '/proxy.pac') or (TargetDocument = '/wpad.dat') then
      // handle automatic-proxy-configuration requests
      SetNextAction(HTTP_Proxy_Config)
    else if (TargetHost = HTTP_HOST_LOCAL) and (TargetPort = '15000') then
      // deny cyclic requests
      SetNextAction(HTTP_Disconnect)
    else
      // continue as usial
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

procedure TJobHTTP.HTTP_Proxy_Config;
begin
  Client.SendString(HTTP_REPLY_OK + CRLF + HTTP_HEADER_PROXY_CONFIG +
    CRLF + CRLF + GetPacContent);

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
  Client.SetLinger(True, TCP_LINGER_TIME);
end;

procedure TJobHTTP.ParseHostAndPort(const Address: string);
var
  i: integer;
  Target: string;
begin
  Target := Address;
  // FORMAT:  protocol://host:port/document
  // OR:      host:port
  // OR:      /document
  try
    // remove optional protocol prefix
    i := Pos('://', Target);
    if i <> 0 then
      Target := Copy(Target, i + 3, MaxInt);

    // remove optional document
    i := Pos('/', Target);
    if i <> 0 then
    begin
      TargetDocument := Copy(Target, i, MaxInt);
      Target := Copy(Target, 1, i - 1);
    end
    else
      TargetDocument := '/';

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
      TargetPort := ifThen<string>(IsSecure, HTTP_PORT_SECURE_str,
        HTTP_PORT_DEFAULT_str);

      // handle requests to local resources
      if Length(TargetHost) = 0 then TargetHost := HTTP_HOST_LOCAL;
    end;

    // normalize
    TargetHost := LowerCase(TargetHost);
    TargetPort := LowerCase(TargetPort);
    TargetDocument := LowerCase(TargetDocument);
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

end.

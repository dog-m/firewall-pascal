// WARNING: DISABLE Windows service "SharedAccess" before enabling DNS filter !!!
unit Unit1;

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Forms, ComCtrls, StdCtrls,
  syncobjs, ExtCtrls, Menus, Spin, IdUDPServer, IdUDPClient,
  SimpleSynapseServerTCP, TAGraph, TASeries, TAIntervalSources, IdSocketHandle,
  IdGlobal;

type

  { TAddressChecker }

  TAddressChecker = class(TForm)
    AnyTCPBox1: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnReloadLists: TButton;
    BtnResetCharts: TButton;
    TrafficLimiterBox: TCheckBox;
    Label2: TLabel;
    TrafficLimiter: TSpinEdit;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    DNSServer: TIdUDPServer;
    DNSClient: TIdUDPClient;
    Log: TListView;
    Savelogwithtimestamp1: TMenuItem;
    StatisticsTimer: TTimer;
    Charts: TPageControl;
    TabTraffic: TTabSheet;
    TabPackets: TTabSheet;
    ChartTraffic: TChart;
    AreaHTTP_received: TAreaSeries;
    AreaHTTP_sent: TAreaSeries;
    ChartRequests: TChart;
    AreaDNS_blocked: TAreaSeries;
    AreaDNS_allowed: TAreaSeries;
    AreaHTTP_blocked: TAreaSeries;
    AreaHTTP_allowed: TAreaSeries;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    AllowList: TListBox;
    TabSheet3: TTabSheet;
    CheckDNSBox: TCheckBox;
    AnyTCPBox: TCheckBox;
    BlockList: TListBox;
    AllowlistEnabledChk: TCheckBox;
    DHCPServer: TIdUDPServer;
    BlocklistEnabledChk: TCheckBox;
    LogMenu: TPopupMenu;
    CopyLogRecordBtn: TMenuItem;
    N1: TMenuItem;
    ClearLogBtn: TMenuItem;
    procedure BtnReloadListsClick(Sender: TObject);
    procedure BtnResetChartsClick(Sender: TObject);
    procedure ChartRequestsAxisList0MarkToText(var AText: string; AMark: double);
    procedure ChartTrafficAxisList0MarkToText(var AText: string; AMark: double);
    procedure DNSServerUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure CheckDNSBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearLogBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure HTTPServerTimerTimer(Sender: TObject);
    procedure Savelogwithtimestamp1Click(Sender: TObject);
    procedure StatisticsTimerTimer(Sender: TObject);
    procedure DHCPServerUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure CopyLogRecordBtnClick(Sender: TObject);
    procedure TrafficLimiterBoxChange(Sender: TObject);
    procedure TrafficLimiterChange(Sender: TObject);
  private
    LogLock: TCriticalSection;
    ServerHTTP: TSimpleServerHTTP;

    procedure LogRequest(const Peer, Protocol, Host: string);
    procedure LoadAccessListTo(const FileName: string; const TargetList: TStrings);

    procedure HTTPServerStatisticsTCPHandler(const C2S: integer; const S2C: integer);
    function HTTPServerHostNameChecker(const Host: string): boolean;
    procedure HTTPServerConnectionAllowedHandler(const ClientIP: string;
      const HostName: string);
    procedure HTTPServerConnectionBlockedHandler(const ClientIP: string;
      const HostName: string);
  public
    class function DNSPacketExtractHostName(const Data: TIdBytes): string;
    procedure DNSPacketForward(const OriginalPacket: TIdBytes;
      const Binding: TIdSocketHandle);
    class procedure DNSPacketBlock(OriginalPacket: TIdBytes;
      const Binding: TIdSocketHandle);

    function CheckHostName(const Host: string): boolean;
  end;

var
  AddressChecker: TAddressChecker;

implementation

{$R *.lfm}

uses
  Clipbrd;

type
  TStatistics = packed record
    DNS: packed record
      allowed,
      blocked: longint;
      end;

    HTTP: packed record
      allowed,
      blocked,
      received,
      sent: longint;
      end;
  end;

const
  PLOT_SECOND: double = 1 / 60 / 60 / 24;
  PLOT_X_WINDOW_SIZE_TICKS: integer = 5 * 60; // 5 minutes

function PLOT_X_WINDOW_SIZE_TIME: double;
begin
  Result := PLOT_SECOND * PLOT_X_WINDOW_SIZE_TICKS;
end;

const
  DEFAULT_PORT_DNS = 53;
  DEFAULT_PORT_HTTP = 80;
  DEFAULT_PORT_HTTPS = 443;

  WORKER_POOL_SIZE = 96;

var
  Statistics: TStatistics;


procedure SwapBytes(var A: byte; var B: byte);
begin
  A := A xor B;
  B := B xor A;
  A := A xor B;
end;

procedure FixWordOrder(X: pbyte; const Size: longint);
var
  i: integer;
begin
  for i := 0 to Size - 1 do
  begin
    if i and 1 = 0 then // Even
      SwapBytes(X^, pbyte(nativeuint(X) + 1)^);

    Inc(X);
  end;
end;


const
  PROTOCOL_DNS = 'DNS';
  PROTOCOL_HTTP = 'HTTP';
  PROTOCOL_UNKNOWN = 'UNKNOWN';

procedure TAddressChecker.LogRequest(const Peer, Protocol, Host: string);
begin
  LogLock.Acquire;
  try
    with Log.Items.Add do
    begin
      Caption := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      with SubItems do
      begin
        Append(Peer);
        Append(Protocol);
        Append(Host);
      end;
    end;
  finally
    LogLock.Release;
  end;
end;

function TAddressChecker.CheckHostName(const Host: string): boolean;
var
  i: integer;
begin
  Result := True;

  if BlocklistEnabledChk.Checked then
  begin
    for i := 0 to BlockList.Items.Count - 1 do
      if Host.EndsWith(BlockList.Items[i], True) then
      begin
        Result := False;
        Break;
      end;
  end;

  if AllowlistEnabledChk.Checked then
  begin
    Result := False;

    for i := 0 to AllowList.Items.Count - 1 do
      if Host.EndsWith(AllowList.Items[i], True) then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

class procedure TAddressChecker.DNSPacketBlock(OriginalPacket: TIdBytes;
  const Binding: TIdSocketHandle);
begin
  InterlockedIncrement(Statistics.DNS.blocked);

  // append answer "not supported"
  OriginalPacket[2] := $81;
  OriginalPacket[3] := $83;

  Binding.Send(OriginalPacket);

  SetLength(OriginalPacket, 0);
end;

// https://en.wikipedia.org/wiki/Domain_Name_System
class function TAddressChecker.DNSPacketExtractHostName(const Data: TIdBytes): string;
type
  TDNSHeader = packed record
    TransactionID: word;
    Flags: word;
    Questions: word;
    AnswerRRs: word;
    AuthorityRRs: word;
    AdditionalRRs: word;
  end;

  TTypeAndClass = packed record
    QType: word;
    QClass: word;
  end;

var
  Packet: TMemoryStream;
  Header: TDNSHeader;
  TypeAndClass: TTypeAndClass;

  WordLen, NameLen: byte;
  RequestedNameTmp: array[1..255] of char;
  NamePtr: PChar;
begin
  Result := '';
  Packet := TMemoryStream.Create;
  WriteTIdBytesToStream(Packet, Data);
  Packet.Position := 0;

  // -----------------------

  Packet.ReadBuffer(Header, SizeOf(Header));
  FixWordOrder(@Header, SizeOf(Header));

  with Header do
    if AnswerRRs + AuthorityRRs + AdditionalRRs <> 0 then
      Exit;

  // -----------------------

  FillChar(RequestedNameTmp, 255, #0);
  NamePtr := @RequestedNameTmp;
  NameLen := 0;

  while True do
  begin
    Packet.ReadBuffer(WordLen, 1);

    if WordLen = 0 then
      Break
    else
      Inc(NameLen, WordLen + 1);

    Packet.Read(NamePtr^, WordLen);
    Inc(NamePtr, WordLen);

    NamePtr^ := '.';
    Inc(NamePtr);
  end;

  Result := Copy(RequestedNameTmp, 0, NameLen - 1);

  // -----------------------

  Packet.ReadBuffer(TypeAndClass, SizeOf(TypeAndClass));
  FixWordOrder(@TypeAndClass, SizeOf(TypeAndClass));

  // -----------------------

  FreeAndNil(Packet);
end;

procedure TAddressChecker.DNSPacketForward(const OriginalPacket: TIdBytes;
  const Binding: TIdSocketHandle);
var
  NewSize: integer;
  RepliedPacket: TIdBytes;
begin
  InterlockedIncrement(Statistics.DNS.allowed);

  // send request to google and save a reply
  DNSClient.SendBuffer(OriginalPacket);

  // grab the result
  SetLength(RepliedPacket, DNSClient.BufferSize);
  NewSize := DNSClient.ReceiveBuffer(RepliedPacket);
  SetLength(RepliedPacket, NewSize);

  // packet is now ready to send back
  Binding.Send(RepliedPacket);

  SetLength(RepliedPacket, 0);
end;

procedure TAddressChecker.DNSServerUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  HostName: string;
begin
  HostName := DNSPacketExtractHostName(AData);

  if CheckHostName(HostName) then
  begin
    LogRequest(ABinding.PeerIP, PROTOCOL_DNS, HostName);
    DNSPacketForward(AData, ABinding);
  end
  else
    DNSPacketBlock(AData, ABinding);
end;

procedure TAddressChecker.ChartTrafficAxisList0MarkToText(var AText: string;
  AMark: double);
const
  KB = 1000;
  MB = 1000 * KB;
  GB = 1000 * MB;
  FORMAT = '#,##0.# ';
begin
  if AMark < 0 then
  begin
    AText := '';
    Exit;
  end;

  if AMark >= GB then
    AText := FormatFloat(FORMAT + 'Gb', AMark / GB)
  else if AMark >= MB then
    AText := FormatFloat(FORMAT + 'Mb', AMark / MB)
  else if AMark >= KB then
    AText := FormatFloat(FORMAT + 'Kb', AMark / KB)
  else
    AText := FormatFloat(FORMAT + 'b', AMark);
end;

procedure TAddressChecker.ChartRequestsAxisList0MarkToText(var AText: string;
  AMark: double);
begin
  AText := FormatFloat('#,##0.#', AMark);
end;

procedure TAddressChecker.BtnReloadListsClick(Sender: TObject);
const
  ALLOWLIST_FILENAME = './list_allow.txt';
  BLOCKLIST_FILENAME = './list_block.txt';
begin
  LoadAccessListTo(ALLOWLIST_FILENAME, AllowList.Items);
  LoadAccessListTo(BLOCKLIST_FILENAME, BlockList.Items);
end;

procedure TAddressChecker.BtnResetChartsClick(Sender: TObject);
var
  timeBaseline: double;

  procedure FlattenChart(const Series: TAreaSeries);
  var
    i: integer;
    timestamp: double;
  begin
    Series.BeginUpdate;

    timestamp := timeBaseline - PLOT_SECOND * PLOT_X_WINDOW_SIZE_TICKS;
    for i := 0 to Pred(Series.Count) do
    begin
      Series.XValue[i] := timestamp;
      Series.YValue[i] := 0;

      // next tick
      timestamp := timestamp + PLOT_SECOND;
    end;

    Series.EndUpdate;
  end;

begin
  timeBaseline := Now;

  FlattenChart(AreaDNS_allowed);
  FlattenChart(AreaDNS_blocked);

  FlattenChart(AreaHTTP_allowed);
  FlattenChart(AreaHTTP_blocked);

  FlattenChart(AreaHTTP_received);
  FlattenChart(AreaHTTP_sent);
end;

procedure TAddressChecker.CheckDNSBoxClick(Sender: TObject);
begin
  DNSServer.Active := CheckDNSBox.Checked;
  DHCPServer.Active := DNSServer.Active;
end;

procedure TAddressChecker.HTTPServerStatisticsTCPHandler(const C2S: integer;
  const S2C: integer);
begin
  InterlockedExchangeAdd(Statistics.HTTP.sent, C2S * 8);
  InterlockedExchangeAdd(Statistics.HTTP.received, S2C * 8);
end;

function TAddressChecker.HTTPServerHostNameChecker(const Host: string): boolean;
begin
  Result := CheckHostName(Host);
end;

procedure TAddressChecker.HTTPServerConnectionAllowedHandler(const ClientIP: string;
  const HostName: string);
begin
  LogRequest(ClientIP, PROTOCOL_HTTP, HostName);
  InterlockedIncrement(Statistics.HTTP.allowed);
end;

procedure TAddressChecker.HTTPServerConnectionBlockedHandler(
  const ClientIP: string; const HostName: string);
begin
  InterlockedIncrement(Statistics.HTTP.blocked);
end;

procedure TAddressChecker.FormCreate(Sender: TObject);
var
  i: integer;
  timestamp: double;
begin
  LogLock := TCriticalSection.Create;

  Charts.ActivePage := TabTraffic;

  // init charts
  timestamp := Now - PLOT_X_WINDOW_SIZE_TIME;
  for i := -PLOT_X_WINDOW_SIZE_TICKS to 0 do
  begin
    AreaDNS_allowed.AddXY(timestamp, 0);
    AreaDNS_blocked.AddXY(timestamp, 0);

    AreaHTTP_allowed.AddXY(timestamp, 0);
    AreaHTTP_blocked.AddXY(timestamp, 0);

    AreaHTTP_received.AddXY(timestamp, 0);
    AreaHTTP_sent.AddXY(timestamp, 0);

    // next tick
    timestamp := timestamp + PLOT_SECOND;
  end;

  BtnReloadLists.Click;

  // start up the show!
  ServerHTTP := TSimpleServerHTTP.Create;
  with ServerHTTP.JobManager do
  begin
    // setup handlers
    SetRequestChecker(HTTPServerHostNameChecker);
    SetHandler_StatAllowed(HTTPServerConnectionAllowedHandler);
    SetHandler_StatBlocked(HTTPServerConnectionBlockedHandler);
    SetHandler_StatTransfered(HTTPServerStatisticsTCPHandler);
  end;
  ServerHTTP.StartListening('0.0.0.0', '15000');
end;

procedure TAddressChecker.LoadAccessListTo(const FileName: string;
  const TargetList: TStrings);
const
  ACCESSLIST_COMMENT_START = '!';
var
  i: integer;
  line: string;
begin
  if FileExists(FileName) then
    with TStringList.Create do
    begin
      LoadFromFile(FileName);

      TargetList.Clear;
      for i := 0 to Count - 1 do
      begin
        line := Trim(Strings[i]);

        if (line = '') or (line[1] = ACCESSLIST_COMMENT_START) then
          Continue; // comments

        TargetList.Append(line);
      end;

      Free;
    end;
end;

procedure TAddressChecker.Savelogwithtimestamp1Click(Sender: TObject);
const
  DIR_LOGS = './logs';
var
  i, j: integer;
  Item: TListItem;
  Line: string;
begin
  if SysUtils.ForceDirectories('./logs') then
    with TStringList.Create do
    begin
      for i := 0 to Log.Items.Count - 1 do
      begin
        Item := Log.Items[i];

        Line := Item.Caption;
        for j := 0 to Item.SubItems.Count - 1 do
          Line := Line + #9 + Item.SubItems[j];

        Append(Line);
      end;

      SaveToFile(DIR_LOGS + '/Log_' + FormatDateTime('yyyy.mm.dd_hh.nn.ss',
        Now) + '.log');

      Free;
    end;
end;

procedure TAddressChecker.ClearLogBtnClick(Sender: TObject);
var
  i: integer;
begin
  Log.Selected := nil;

  Log.BeginUpdate;
  try
    for i := 0 to Log.Items.Count - 1 do
      Log.Items[i].SubItems.Clear;

    Log.Clear;
  finally
    Log.EndUpdate;
  end;
end;

procedure TAddressChecker.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DNSServer.Active := False;
  DHCPServer.Active := False;

  FreeAndNil(ServerHTTP);
  FreeAndNil(LogLock);
end;

procedure TAddressChecker.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F12 then
  begin
    AllowlistEnabledChk.Checked := not AllowlistEnabledChk.Checked;
    Key := VK_NONAME;
  end;
end;

procedure TAddressChecker.HTTPServerTimerTimer(Sender: TObject);
begin
end;

procedure TAddressChecker.StatisticsTimerTimer(Sender: TObject);
var
  newTimeTick, lastTimeTick: double;

  procedure RenderAndReset(const Chart: TAreaSeries; var Value: longint);
  begin
    with Chart do
    begin
      Delete(0);
      AddXY(newTimeTick, Value);
    end;
    InterlockedExchange(Value, 0);
  end;

begin
  newTimeTick := Now;

  // time warping to keep plot in right time-window
  lastTimeTick := AreaHTTP_received.MaxXValue;
  if newTimeTick - lastTimeTick > PLOT_X_WINDOW_SIZE_TIME then
    BtnResetCharts.Click;

  RenderAndReset(AreaDNS_allowed, Statistics.DNS.allowed);
  RenderAndReset(AreaDNS_blocked, Statistics.DNS.blocked);

  RenderAndReset(AreaHTTP_allowed, Statistics.HTTP.allowed);
  RenderAndReset(AreaHTTP_blocked, Statistics.HTTP.blocked);

  RenderAndReset(AreaHTTP_received, Statistics.HTTP.received);
  RenderAndReset(AreaHTTP_sent, Statistics.HTTP.sent);
end;

// non-functional
procedure TAddressChecker.DHCPServerUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  Request: string absolute AData;
begin
  LogRequest(Format('%s:%d', [ABinding.PeerIP, ABinding.PeerPort]),
    'DHCP', IntToStr(Length(Request)));
end;

procedure TAddressChecker.CopyLogRecordBtnClick(Sender: TObject);
begin
  with Log do
  begin
    if Selected = nil then
      Exit;

    Clipboard.AsText := Selected.Caption + #13#10 + Selected.SubItems.Text;
  end;
end;

procedure TAddressChecker.TrafficLimiterBoxChange(Sender: TObject);
begin
  // apply constraints
  TrafficLimiterChange(nil);

  // reset constraint if it is not enabled
  if not TrafficLimiterBox.Checked then
    ServerHTTP.JobManager.SetBandwidthMax(BANDWIDTH_UNLIMITED);
end;

procedure TAddressChecker.TrafficLimiterChange(Sender: TObject);
begin
  if TrafficLimiterBox.Checked then
    ServerHTTP.JobManager.SetBandwidthMax(TrafficLimiter.Value);
end;

end.

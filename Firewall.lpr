// icon: https://icons8.com/icon/zvzyoB3xqw1m/firewall
program Firewall;

{$mode delphi}

uses
  Forms, Interfaces, {XPMan,}
  Unit1 in 'Unit1.pas' {AddressChecker};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAddressChecker, AddressChecker);
  Application.Run;
end.

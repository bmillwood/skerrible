{ pkgs ? import <nixpkgs> {} }:

let
  testHost = "skerrible.test";
in

pkgs.testers.nixosTest {
  name = "skerrible-module";

  containers.machine = { config, lib, pkgs, ... }: {
    imports = [ ./config.nix ];

    services.skerrible = {
      enable = true;
      virtualHost = testHost;
    };

    networking.hosts."127.0.0.1" = [ testHost ];
  };

  testScript = ''
    machine.wait_for_unit("nginx.service")
    machine.wait_for_unit("skerrible.service")
    machine.wait_for_open_port(80)
    machine.wait_for_open_port(4170)

    machine.succeed("curl --fail --silent --show-error http://${testHost}/ -o /dev/null")
  '';
}

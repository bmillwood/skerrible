{ config, lib, pkgs, ... }:
let
  cfg = config.services.skerrible;
  skerrible = pkgs.callPackage ./. {};
  webClient = pkgs.callPackage ./web-client {};
  inherit (lib) mkIf mkOption types;
in
{
  options = {
    services.skerrible = {
      enable = lib.mkEnableOption "skerrible";

      virtualHost = mkOption {
        type = types.str;
      };

      location = mkOption {
        type = types.str;
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    users.users.skerrible = {
      isNormalUser = true;
    };
    systemd.services.skerrible = {
      description = "skerrible server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "skerrible";
        ExecStart = "${skerrible}/bin/skerrible-server ${webClient}";
      };
    };
    services.nginx.virtualHosts.${cfg.virtualHost}.locations.${cfg.location} = {
      proxyPass = "http://localhost:4170/";
      proxyWebsockets = true;
    };
  };
}

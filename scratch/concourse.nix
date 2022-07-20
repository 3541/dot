# This is a snippet of configuration for Concourse CI. It is incomplete in that the build does not
# build/copy required resources for the Concourse service. For now, it looks like this is overkill.
# Keeping it around just in case.
        environment.systemPackages = [
          (pkgs.writeShellScriptBin "concourse-generate-keys" ''
            set -e

            DIR="/etc/concourse/keys"
            mkdir -p "$DIR"
            if [ -f "$DIR/session_signing_key" ]; then
              echo "Keys already exist."
              exit
            fi

            touch "$DIR/authorized_worker_keys"
            for key in session_signing_key tsa_host_key; do
              ssh-keygen -t rsa -b 4096 -m PEM -f "$DIR/$key" -N "" -C "concourse@opportunity"
              chmod 600 "$DIR/$key"
            done

            chown -R concourse:concourse /etc/concourse
            chmod 700 /etc/concourse
            chmod 700 "$DIR"
          '')

          (pkgs.writeShellScriptBin "concourse-generate-worker-keys" ''
            set -e

            DIR="/etc/concourse/keys"
            mkdir -p "$DIR/workers"
            chmod 700 "$DIR/workers"

            for host in "$@"; do
              key="$DIR/workers/''${host}_key"
              ssh-keygen -t rsa -b 4096 -m PEM -f "$key" -N "" -C "concourse@$host"
              cat "$key.pub" >> "$DIR/authorized_worker_keys"
              chmod 600 "$key"
            done
          '')
        ];

        systemd.services.concourse = {
          requires = [ "postgresql.service" ];
          after = [ "postgresql.service" ];
          wantedBy = [ "multi-user.target" ];

          script = ''
            ${
              pkgs.fly.overrideAttrs (prev: {
                pname = "concourse";
                subPackages = [ "cmd/concourse" ];

                postInstall = "";
              })
            }/bin/concourse web
          '';

          serviceConfig = {
            User = "concourse";
            Group = "concourse";
          };

          environment = {
            CONCOURSE_ADD_LOCAL_USER = "alex:pw";
            CONCOURSE_MAIN_TEAM_LOCAL_USER = "alex";
            CONCOURSE_POSTGRES_SOCKET = "/var/run/postgresql";
            CONCOURSE_SESSION_SIGNING_KEY =
              "/etc/concourse/keys/session_signing_key";
            CONCOURSE_TSA_HOST_KEY = "/etc/concourse/keys/tsa_host_key";
            CONCOURSE_TSA_AUTHORIZED_KEYS =
              "/etc/concourse/keys/authorized_worker_keys";
          };
        };

        users = {
          groups.concourse = { };

          users.concourse = {
            isSystemUser = true;
            group = "concourse";
          };
        };

        services.postgresql = {
          enable = true;
          ensureDatabases = [ "atc" ];

          ensureUsers = [{
            name = "concourse";
            ensurePermissions."DATABASE \"atc\"" = "ALL PRIVILEGES";
          }];
        };

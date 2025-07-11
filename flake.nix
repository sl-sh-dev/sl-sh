{
  description = "Slosh - A Rust-based shell with Lisp VM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" ];
        };
      in
      {
        packages = {
          default = self.packages.${system}.slosh;

          slosh = pkgs.rustPlatform.buildRustPackage {
            pname = "slosh";
            version = "0.11.1";

            src = ./.;

            cargoLock = {
              lockFile = ./Cargo.lock;
              outputHashes = {
                "sl-console-0.10.1" = "sha256-vsmqQFaVKGB4/pa0flHTcj4aCxIdRgtw336+nHGOWhM=";
                "sl-liner-0.7.0" = "sha256-+cHY4UlVmL2BbPkh17QFI/pzOs+xBQm+s1nCRU4T5YI=";
              };
            };

            nativeBuildInputs = with pkgs; [
              rustToolchain
              pkg-config
            ];

            buildInputs = with pkgs; [
              openssl
            ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              pkgs.darwin.apple_sdk.frameworks.Security
              pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
            ];

            cargoBuildFlags = [ "-p" "slosh" ];

            # Skip tests that require filesystem access in sandbox
            checkPhase = ''
              export HOME=$TMPDIR
              cargo test --workspace
            '';

            # Copy lisp files to the output
            postInstall = ''
              mkdir -p $out/share/slosh
              cp -r lisp $out/share/slosh/
            '';

            meta = with pkgs.lib; {
              description = "A register-based virtual machine shell with Lisp syntax";
              homepage = "https://github.com/sl-sh-dev/slosh";
              license = licenses.mit;
              maintainers = [];
              mainProgram = "slosh";
            };
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustToolchain
            pkg-config
            openssl
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
          ];

          shellHook = ''
            echo "Slosh development environment"
            echo "Run 'cargo run --bin slosh' to build the project and run the shell in debug mode."
          '';
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.slosh}/bin/slosh";
        };
      });
}

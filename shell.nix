{ pkgs }:
let
  rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
in
pkgs.mkShell {
  shellHooks = ''
  '';

  buildInputs =
    builtins.attrValues {
      inherit rust;
      inherit (pkgs)
        rust-analyzer

        autoPatchelfHook
        pkg-config
      ;

    };
}

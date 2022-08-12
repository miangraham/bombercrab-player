{
  description = "";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-22.05"; };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      system = "x86_64-linux";
      overlays = [ rust-overlay.overlays.default ];
      pkgs = import nixpkgs { inherit overlays system; };
    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };
    };
}

{

  description = "Flake for neil";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        devShells =
          {
            default =
              pkgs.mkShell {
                packages = with pkgs;[
                  clojure
                  babashka
                  clj-kondo
                  graalvm17-ce
                ];
              };
          };
        packages =
          {
            default = pkgs.neil.overrideAttrs (_: {
              src = ./.;
              version = "SNAPSHOT";
            });
          };
      });
}

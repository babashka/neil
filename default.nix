{ pkgs ?  import (fetchTarball "https://channels.nixos.org/nixos-21.11/nixexprs.tar.xz"){}}:
pkgs.mkShell{
  nativeBuildInputs = with pkgs; [
    clojure
    babashka
    clj-kondo
    graalvm17-ce # for java
  ];
}

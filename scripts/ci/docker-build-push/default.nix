let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ../../.. { inherit config system; }
, pkgs ? iohkPkgs.pkgs
, hostPkgs ? import <nixpkgs> { inherit config system; }
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = with iohkPkgs.dockerImages; [
    demoWallet
  ];

in
  writeScript "docker-build-push" (''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    username="$(docker info | sed '/Username:/!d;s/.* //')"
    tag="v${iohkPkgs.cardano-sl-node.version}"

  '' + concatMapStringsSep "\n" (image: ''
    echo "Loading ${image}"
    tagged="$username/${image.imageName}:$tag"
    docker load -i "${image}"
    docker tag "${image.imageName}:latest" "$tagged"
    docker push "$tagged"
  '') images)

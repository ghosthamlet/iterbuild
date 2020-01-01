{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
}:
let
  kaggle = (import ./requirements.nix { pkgs=pkgs; }).packages.kaggle;
in
  pkgs.haskell-nix.stackProject {
    src = (pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; }).outPath;
    modules = [{
      # Not sure pkgconfig is intended for this, but what should I do, it works and the attribute does not have documentation.
      # I found it after reading the source code for a considerable amount of time.
      packages.iterbuild.components.library.pkgconfig = [[pkgs.which pkgs.git kaggle]];
      packages.iterbuild.components.library.postUnpack = ''
      substituteInPlace iterbuild/src/Capabilities.hs --replace "\"git\"" "\"$(which git)"\"
      substituteInPlace iterbuild/src/Capabilities.hs --replace "\"kaggle\"" "\"$(which kaggle)"\"
      '';
      packages.iterbuild.dontPatchELF = false; # this does not remove unnecessary deps, dont feel like debugging it now
  }];
}
# For some reason, this returns a set of haskell packages (analogous to the one at
# nixpkgs.haskellPackages), which has iterbuild and all its dependencies available, the dependencies 
# being packages in 'lts-x.y'. This is why many of the functions like 'extend' which are available
# in the regular nixpkgs repo cannot be called - though note that the function 'ghcWithPackages' was manually added.
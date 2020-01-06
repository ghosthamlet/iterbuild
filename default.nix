{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball {
    url=https://github.com/input-output-hk/haskell.nix/archive/28b23cc7de5a1a89160e83b030fe9b3ce8.tar.gz;
    sha256="04z49bc7gh4c5f9mvj31zv8kjvbq3c9wihl88rzlxlpwsqsivc1w";
  })),
  kaggle ? (import ./requirements.nix { pkgs=pkgs; }).packages.kaggle
}:
pkgs.haskell-nix.stackProject {
  src = (pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; }).outPath;
  modules = [{
    # Not sure pkgconfig is intended for this, but what should I do, it works and the attribute does not have documentation.
    # I found it after reading the source code for a considerable amount of time.
    packages.iterbuild.components.library.pkgconfig = [[pkgs.which pkgs.git pkgs.unzip kaggle]];
    # TODO: When I try to inspect the iterbuild components, I get the error that postUnpack is both null and not null, lol
    packages.iterbuild.components.library.postUnpack = ''
    substituteInPlace iterbuild/src/Capabilities.hs --replace "\"git\"" "\"$(which git)"\"
    substituteInPlace iterbuild/src/Capabilities.hs --replace "\"kaggle\"" "\"$(which kaggle)"\"
    substituteInPlace iterbuild/src/Capabilities.hs --replace "\"unzip\"" "\"$(which unzip)"\"
    # cat iterbuild/src/Capabilities.hs
    '';
    packages.iterbuild.dontPatchELF = false; # this does not remove unnecessary deps, dont feel like chasing unnecessary deps now
}];
}
# For some reason, this returns a set of haskell packages (analogous to the one at
# nixpkgs.haskellPackages), which has iterbuild and all its dependencies available, the dependencies 
# being packages in 'lts-x.y'. This is why many of the functions like 'extend' which are available
# in the regular nixpkgs repo cannot be called - though note that the function 'ghcWithPackages' was manually added.
# iterbuild

Iterate quickly by using Haskell where it excells.

The idea is using Python as an imperative data manipulation language and moving all the higher level workflow code to Haskell. More concretely, you specify where your python functions lie, and then you can compose and apply those python objects with each other, by letting iterbuild generate the corresponding boilerplate code. Notice that this is strictly weaker than having a foreign function interface, since you never actually run python code, you only generate it.

## Development

It gets annoying that the nix function `fetchTarball` clears its cache often. To mitigate this, you can run `nix-prefectch-url --unpack [url for currently used haskell.nix commit, see default.nix]`, this will store the `haskell.nix` source in the nix store.

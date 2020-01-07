
# Iterbuild

An embedded DSL for specifying machine learning experiments fast.

**WARNING:** Iterbuild is in early development and thus missing many functions making it ergonomic to use in actual machine learning projects.

## Installation

### With nix

Iterbuild is a nix package, so first you need to install nix. For the multi-user nix installation, you will want to run
```
sh <(curl https://nixos.org/nix/install) --daemon
```
and follow the on-screen promts. For more details, see [the nix manual](https://nixos.org/nix/manual/#sect-multi-user-installation).

```
{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball {
    url=https://github.com/input-output-hk/haskell.nix/archive/28b23cc7de5a1a89160e83b030fe9b3ce8.tar.gz;
    sha256="04z49bc7gh4c5f9mvj31zv8kjvbq3c9wihl88rzlxlpwsqsivc1w";
  }))
}:
let
  ibpkgs = (import (builtins.fetchTarball https://gitlab.com/adrianmarti/iterbuild/repository/archive.tar.gz)) pkgs;
  haskellEnv = ibpkgs.ghcWithPackages (pkgs: [pkgs.iterbuild]); # you can add more haskell packages here
in
  with pkgs; stdenv.mkDerivation {
  name = "your-project";

  src = null;

  buildInputs = [
    ibpkgs.iterbuild.components.exes.iterbuild # the iterbuild CLI
    haskellEnv
  ];

  shellHook = ''
    # This is a temporary hack for fixing https://github.com/NixOS/nixpkgs/issues/64603. Otherwise haskell throws a runtime error when printing
    # utf8 characters. Thought this should really be fixed inside the iterbuild package.
    export LANG=C.UTF-8
  '';
}

```

## Who you should be to benefit from iterbuild

You are a machine learning practitioner who needs to find a good model on a specific dataset. I am not talking about production code or needing to quickly write a logistic regression, but rather you should be someone who spends a considerable amount of time on tweaking your data processing pipeline and choosing which models to use. The perfect example for this is a [kaggle competition](www.kaggle.com/competitions).

## Motivating Problems

The sad truth about your kind of work is that thinking long and hard will only bring you so far. Knowing with certainty which techniques will work and which will not work in advance is not something achievable by humans. You deal with this by running as many experiments as possible.

In fact, I say this is the single most important requirement for your machine learning toolchain. All other things you value, like expressivity, reproducibility and reusability serve that goal, though the latter 2 do so in a more long-term fashion.

You probably use python or R for your project since those languages have such a rich ecosystem of machine learning packages and the dynamic typing helps you write very expressive code. For exploratory data analysis you undoubtably use jupyter notebooks and for your data processing you either do it directly in a notebook or you use a script file in order to make your code more reusable. Also, if you want to at least be able to theoretically reproduce old experiments, you are using git.

This kind of machine learning toolchain has many problems, I will highlight 2 here:

### 1. The unreasonable cost of pipeline parametrization

Consider the following very typical situation: You wrote your data processing pipeline and specified your model and you now realize the whole thing performs badly. You want to replace the model with a different model, so you edit the code and it performs better.

Why is this so unreasonable, it's not that much code? Because I would argue that most the code you are writing for your project is of this nature, you always want to just change a little thing and you mostly do that by editing the code. But editing the code is not ideal.
* You can't decide to use 2 different models in 2 parts of the codebase
* Changing a hardcoded model may break existing code

So what you want is some kind of parameter for specifying the model, but doing that requires writing more code, which slows you down.

Iterbuild will provide a mechanism which alleviates this problem.

### 2. Git is not modular

Consider machine learning code that first preprocesses the data and then augments it before feeding it to a model. For the most part, the preprocessing and the augmentation evolve independently, so if you notice a new version of your preprocessing hurts performance, you may want to replace it with an old one. With regular git this may be hard to do, because you would have to basically have to identify the faulty commit and then rebase it out of existence(and if you want to preserve history you will also do some stuff with branching. It gets even more nasty if your commit does also contain other changes you don't want to undo). This is just insane and any sane person would resort to copy pasting code around while viewing older commits. Needless to say, this is still stupid, and it ilustrates the rift between the mental model of version control ml users have, and how git handles it. *We need a tool that lets us naturally express the concept of rolling back a part of our code*.

## A different workflow model

The idea is using Python as an imperative data manipulation language and moving all the higher level workflow code to Haskell. More concretely, you specify where your python functions lie, and then you can compose and apply those python objects with each other, by letting iterbuild generate the corresponding boilerplate code. Notice that this is strictly weaker than having a foreign function interface, since you never actually run python code, you only generate it. I think that when doing machine learning, using a language that is not *Python* or *R* will slow you down too much, since the machine learning ecosystems of those languges make them extremely expressive.

"Bringing the power of haskell's abstractions to reuse machine learning code"
"influenced by nix"
"pure functional build system" -- Ressources are variables, not filenames. Because filenames are irrelevant and error-prone.
code example
### Parametrization by default

Remark: Free applicatives.
code examples here
### A modular version of version control

code examples

In general, one can make the case that composition code should be written in Haskell....

## Development

It gets annoying that the nix function `fetchTarball` clears its cache often. To mitigate this, you can run `nix-prefectch-url --unpack [url to correct haskell.nix commit, the same commit as the one specified in default.nix]`, this will store the `haskell.nix` source in the nix store and it will get used every time you run `fetchTarball`.

## Future plans
### Desired features
### The long term view

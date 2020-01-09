
# Iterbuild

An embedded Haskell DSL for specifying machine learning experiments fast.

Using iterbuild has the following benefits:
* The python ecosystem as a base
* The full power of Haskell for specifying your experiments
* A library for defining your experiments, featuring:
  * Reproducibility
  * A modular version of version control
  * Saving models and intermediate results

while minimizing code overhead over just naively running experiments.

In short, it does this by placing itself as a kind of haskell-based build system that generates python code.

**WARNING:** Iterbuild is in early development and thus the library is quite incomplete and subject to significant change. Currently the code examples may appear to be a bit verbose, especially for a machine learning experiment specification DSL, but I am confident that Haskell's overloading with type classes may help iterbuild provide a very expressive interface that actually lets us achieve that sought after speed.

## Installation with nix

Iterbuild is a nix package, so first you need to install nix. For the multi-user nix installation, you will want to run
```
sh <(curl https://nixos.org/nix/install) --daemon
```
and follow the on-screen prompts. For more details, see [the nix manual](https://nixos.org/nix/manual/#sect-multi-user-installation). Now if you want to use iterbuild from a local project, you can copy the following script into a file called `default.nix`.

```
{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball {
    url=https://github.com/input-output-hk/haskell.nix/archive/28b23cc7de5a1a89160e83b030fe9b3ce8.tar.gz;
    sha256="04z49bc7gh4c5f9mvj31zv8kjvbq3c9wihl88rzlxlpwsqsivc1w";
  }))
}:
let
  ibpkgs = import (builtins.fetchTarball https://gitlab.com/adrianmarti/iterbuild/repository/archive.tar.gz) { pkgs=pkgs; };
  # or this if you want to build from a local copy:
  # ibpkgs = import ./path/to/your/local/iterbuild/clone { pkgs=pkgs; };
  haskellEnv = ibpkgs.ghcWithPackages (pkgs: [pkgs.iterbuild]); # you can add more haskell packages here
in
  with pkgs; stdenv.mkDerivation {
  name = "your-project";

  src = null;

  buildInputs = [
    ibpkgs.iterbuild.components.exes.iterbuild # the iterbuild CLI
    haskellEnv
    # you can add packages from nixpkgs here
  ];

  shellHook = ''
    # This is a temporary hack for fixing https://github.com/NixOS/nixpkgs/issues/64603. Otherwise haskell throws a runtime error when printing
    # utf8 characters. Thought this should really be fixed inside the iterbuild package.
    export LANG=C.UTF-8
  '';
}
```

Now every time you run `nix-shell --pure` in the directory containing this file, you will be put into a bash shell that has the `iterbuild` command line utility and a `ghc` which has *iterbuild* and its dependencies already installed.

## Who you should be to benefit from iterbuild

You are a machine learning practitioner who needs to find a good model on a specific data set. I am not talking about production code or needing to quickly write a logistic regression, but rather you should be someone who spends a considerable amount of time on tweaking your data processing pipeline and choosing which models to use. The perfect example for this is a [kaggle competition](www.kaggle.com/competitions).

## Motivating Problems

The sad truth about your kind of work is that thinking long and hard will only bring you so far. Knowing with certainty which techniques will work and which will not work in advance is not something achievable by humans. You deal with this by running as many experiments as possible.

In fact, I'd say this is the single most important requirement for your machine learning tool chain. All other things you value, like expressivity, reproducibility and reusability serve that goal, though the latter two do so in a more long-term fashion.

You probably use python or R for your project since those languages have such a rich ecosystem of machine learning packages and the dynamic typing helps you write very expressive code. For exploratory data analysis you 
undoubtedly use jupyter notebooks and for your data processing you either do it directly in a notebook or you use a script file in order to make your code more reusable. Also, if you want to at least be able to theoretically reproduce old experiments, you are using git.

This kind of machine learning tool chain has many problems, I will highlight two here:

### 1. The unreasonable cost of pipeline parametrization

Consider the following very typical situation: You wrote your data processing pipeline and specified your model and you now realize the whole thing performs badly. You want to replace the model with a different model, so you edit the code and it performs better.

Why is this so unreasonable, it's not that much code? Because I would argue that most the code you are writing for your project is of this nature, you always want to just change a little thing and you mostly do that by editing the code. But editing the code is not ideal.
* You can't decide to use two different models in two parts of the codebase
* Changing a hard coded model may break existing code

So what you want is some kind of parameter for specifying the model, but doing that requires writing more code and also changing existing code which called the changed portion of the code. This is just unreasonable to do every time you want to change a little thing.

Iterbuild will provide a mechanism which lets you write code in a regular fashion and then still lets you replace a variable used in that code with a new one, but without needing to touch existing code.

### 2. Git is not modular

Consider machine learning code that first preprocesses the data and then augments it before feeding it to a model. For the most part, the preprocessing and the augmentation evolve independently, so if you notice a new version of your preprocessing hurts performance, you may want to replace it with an old one, but while keeping the newer data augmentation code. With regular git this may be hard to do, because you would have to basically have to identify the faulty commit and then rebase it out of existence and reapply the changes you want from it(If you don't want to throw away the new preprocessing code, you will need to deal with branches). This is just insane and any sane person would resort to copy pasting code around while viewing older commits. Needless to say, this is still stupid, and it illustrates the rift between the mental model of version control ml users have, and how git handles it. *We need a tool that lets us naturally express the concept of rolling back a part of our code*.

This monolithic way of doing version control also is bad for reproducibility. I have very often experienced that changing the code for a model will cause all the code depending on that code to not work in the way it was originally run. For example, if you had an analysis analyzing some very particular aspects of the model's performance, as soon as you update the model, you will need to run that analysis again, but all the commentary you wrote will be meaningless since the results are different. So you need to always also update and run all the code depending on the changed code, although it may require nontrivial manual labour, in the case of an analysis. Or nontrivial machine labour, in case you wrote a model depending on the modified code. This is why people rely on the  solution of writing models to disk and calling them from there. Though this manual versioning process makes git meaningless and we really don't want to go back to the days of no automatic version control. So we need to make git more modular to allow for upgrading only part of the code(And link trained models to their source code, but that's another issue).

## A different workflow model

The idea is using Python as an imperative data manipulation language and moving all the higher level workflow code to Haskell. More concretely, you specify where your python functions lie, and then you can compose and apply those python objects with each other by letting iterbuild generate the corresponding boilerplate code. Notice that this is strictly weaker than having a foreign function interface, since you never actually run python code, you only generate it. I think that when doing machine learning, using a language that is not *Python* or *R* will slow you down too much, since the machine learning ecosystems of those languages make them extremely expressive, so I have decided that it is an uncompromisable building block to be based on python.

For example, the following script imports two python objects, and applies one to the other. It adds a python module to the path `../target` which has python code doing the required thing. (If you want this to run, you will want a `.git/` folder with the corresponding commit in the current directory and have space in the parent directory so iterbuild can create a few folders.)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text.Lazy as LT
import Protolude
import Path
import Capabilities
import Python

commit :: BaseMonad (Path Rel Dir)
commit = fetchCommit . pure $ "6ab59593aa59023c193d343253b4932582b75a0e"

yourString :: BaseMonad PyObject
yourString = (`PyObject` "yourstring") . PyModule . (</> [relfile|yourdata.py|]) <$> commit

yourFunction :: BaseMonad PyObject
yourFunction = (`PyObject` "yourfunction") . PyModule . (</> [relfile|yourfunctions.py|]) <$> commit

main :: IO ()
main = void . withDefaults $ apply yourFunction (pure <$> yourString) (pure [])
```

(I would like to remind you of the warning at the beginning of the README.) Note that the code above would have worked for any applicative with a few class implementations, but I still don't have a class for reasonably lowering that back into an IO monad.

Now what is the point of this whole endeavour of manipulating python code inside Haskell? There are two reasons:

* I think that Haskell functors like applicatives or monads do a very good job at hiding a lot of complexity behind familiar syntax. In Haskell it is easy to implement complicated abstractions that change the way that execution is handled. For example, parallelism and caching can be handled without the end user seeing more than the fact that his values are inside a functor. In general I think that Haskell is a perfect language for implementing very concise libraries that may do very complex things, while at the same time being type-safe.
* We have now created a new kind of layer for writing code. This layer can manipulate code that is in the git repository and thus resembles what a build system does. If we compare *iterbuild* to a build system though, we will notice that its interface is a functional program. I think this is the cleanest way of implementing a build system, since it allows you to [use a familiar interface](http://www.lihaoyi.com/post/BuildToolsasPureFunctionalPrograms.html) without having to invent your own. For example, we will not use filenames to refer to resources, but instead we can just use variables inside the functional program to refer to things. Of course you will sometimes still need to refer to external files(like the git commit hash or the source of those python files in the code above), but once all your external resources have been referenced with a variable, you will not need to see explicit paths again. Instead those intermediate steps will be done in content addressable storage and the references to the CAS will be passed around by functions without you looking at them. This has the benefit of relieving the programmer of the pain of always thinking about where on the filesystem to store things.

Note that although the use of code generation is generally discouraged, I would say that in this case it is a good idea, because the rules for generating code are very simple. One only needs to specify how to apply python functions, and maybe also how to apply them while caching results. So the cost of maintaining this code-generating code is quite low. To show the power gained from a new layer for specifying how to build experiments in Haskell, I will illustrate how it solves the problems mentioned above.

### Parametrization by default

It turns out that there is a construction in Haskell that lets you do parameterization by default. This construction is [the free applicative](http://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html). To explain why this is useful, lets expand the recursive definition of a free applicative functor. So let *F* be a functor, the free applicative functor over *F* at a type *b*, is a type which can be any function of the form

```
a1 -> a2 -> ... -> an -> b
```
(for any natural number *n* and any types *a1*, *a2*, ... , *an*) together with values *F a1*, *F a2*, ..., *F an*. So if we let *F* be the identity functor, we essentially get a way to compute *b* with a certain function and a fixed set of arguments for that function. This means that we can just replace the default arguments with other default arguments(we actually need a *Typeable* constraint on the *ai* for that). The good thing about this, is that this is an applicative, so we can start building our expressions happily with applicative syntax and not worry about this parameterization going on behind the scenes until we have a machine learning experiment we like and run. When we decide to change something, we will not edit the code, but just define a new experiment where we replace a function or an argument with another.

Now we need some way to refer to such dependencies of values inside a functor. There is probably a good interface doing this with minimal explicit variable-name annotations. But for now I have just used the free applicative construction on a functor which lets you optionally attach a variable name to the value, so later you can use that variable name to replace the value. Here is a code example:

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Extended default rules is quite nice because it uses the typeable constraint
-- required by 'label' and 'also', and casts everything down to a default like String or
-- Int. Of course, then I can't use Text, but it saves a lot of type annotations.
{-# LANGUAGE ExtendedDefaultRules #-}
import Protolude
import Expr

finalString :: Expr [Char]
finalString = do
        -- 1) introduce all named variables
        (+) <- label "operator" (+)
        a <- label "a" 3
        b <- label "b" 3
        resultName <- label "resultName" "sum"

        -- 2) optionally introduce other operations and compose them
        return $ "The " <> resultName <> " of " <> show a <> " and " <> show b <> " is " <> show (a + b)

-- compute the expression
out = collapse finalString

replacer :: Typeable a => Labeled a -> Labeled a
replacer = mapKey "a" (+1) -- add one to a
    `also` replaceKey "resultName" "product"
    `also` replaceKey "operator" (*)
    `also` identity

replacedExpr = replaceE replacer finalString
out' = collapse replacedExpr

main = putStrLn out >> putStrLn out'
```
This prints:
```
The sum of 3 and 3 is 6
The product of 4 and 3 is 12
```

In practice, one probably does not want to use applicative do notation, since I found it to sometimes require a monad instance for no apparent reason, though that might just be me not understanding how exactly the desugaring works. Still, the goal here is to write what looks like regular Haskell code. For example one could give reasonable default names to python objects imported from the outside world(like just using the function's name) and similarly for commits. Like this, most our objects will already have type `F a` for a type `a`. Moreover the functions that iterbuild exposes, all have type signatures like `F a -> F b -> F c -> F d`. The point is that we can arrange it such that most of our code is just regular function application or composition, and not explicit variable namings or some fancy applicative operations. I think this is an uncompromisable requirement for making *iterbuild* ergonomic and easy to use.

### A modular version of version control

With the current setup, notice that we have a new style of programming. We have one(or more) mostly append-only haskell files to specify experiments and they always refer to python functions by git commit. So now there is nothing stopping us from creating many independent git branches with different components of the data science pipeline and then referring to them from our haskell files. Note that the haskell files themselves would sit inside a dedicated branch, so they are also version controlled. This may seem a bit slow to work with, but git worktrees should make this acceptable. Maybe one might want a few command aliases making creating empty git branches easier.

---

This git example shows what I really like about this haskell-based *build system* layer. It lets you talk explicitly about your workflow. Here is another example: I am planing to add to iterbuild capabilities to run the python code and store the model results inside the CAS. Like this, a user that likes to run his code on the *google cloud platform*, can just write the code that does this and then tell iterbuild to use that behind the scenes. All this without touching the existing experiment code. Iterbuild does not aim to give you everything you would ever possibly need for your machine learning experiments(because it is a near-impossible task to fit everyone's workflow), but it rather aims to provide a sane default workflow with the capability to completely change it. (Currently this repo's effects are implemented with the [mtl](https://hackage.haskell.org/package/mtl), but I want to change the effect system to [freer monads](https://hackage.haskell.org/package/freer-simple) so the user can add his own effects or handle existing effects differently easily).

## Development

It gets annoying that the nix function `fetchTarball` clears its cache often. To mitigate this, you can run `nix-prefectch-url --unpack [url to correct haskell.nix commit, the same commit as the one specified in default.nix]`, this will store the `haskell.nix` source in the nix store and it will get used every time you run `fetchTarball`.

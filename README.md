Intro
=====

A set of modules used to teach Haskell development. Can be installed
with either `stack` or `cabal` by following instructions below.

Slides are available [here](http://mschristiansen.github.io/intro).


Instructions for Stack
----------------------

Install `stack` by following these
[instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install)

1. Clone this repository: `git clone git://github.com/mschristiansen/intro`
2. Install GHC: `stack setup`
3. Compile and run: `stack exec intro`


Instructions for Using Cabal
----------------------------

Prerequisite: working installation of git, GHC and Cabal. If not
installed, try the package manager that comes with your distribution
eg. for Ubuntu or Debian `apt-get install git ghc cabal`.

1. Clone this repository: `git clone git://github.com/mschristiansen/intro`
2. Create a sandbox for project dependencies: `cabal sandbox init`
3. Install project dependencies: `cabal install --dependencies-only --enable-tests`
4. Compile and run the application: `cabal run`

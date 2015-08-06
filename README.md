Intro
=====

A set of modules used to teach Haskell development.


Instructions for Use
--------------------
Prerequisite: working installation of git, GHC and Cabal. If not installed, try the package manager that comes with your distribution eg. for Ubuntu or Debian `apt-get install git ghc cabal`.

1. Clone this repository.
```
git clone git://github.com/mschristiansen/intro
```

2. Create a sandbox for project dependencies.
```
cabal sandbox init
```

3. Install project dependencies.
```
cabal install --dependencies-only --enable-tests
```

4. Compile and run the application.
```
cabal run
```

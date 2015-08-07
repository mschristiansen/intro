% Introduction to Haskell
% Mikkel Christiansen
% House4Hack, August 2015


## Objectives

To teach enough for you to start a project in Haskell.


## Agenda

- Get Started with the Project
- Cover the essential Haskell Tools
- Teach the basic syntax and elements of the Haskell language
- Show resources for further study


## Approach

- Ask questions any time
- Test code and look up documentation as we go
- Break and fix the project
- Let ghc be your teacher!


# Get Started with the Project

## The Intro Project

A small web service.

- Displays html pages
- Has JSON API
- In-memory store for (x,y) points
- Can do some aggregation on points
- Lots of room for improvements!


## Install Tools

Get compiler: ghc
Get build manager: cabal (cabal-install)

On Debian/Ubuntu it can be done with:
`apt-get install git ghc cabal-install`


## Download Project

Clone repository and tell `cabal` to start fetching dependencies.

```
$ git clone git@github.com:mschristiansen/intro.git
$ cd intro
$ cabal sandbox init
$ cabal install --dependencies-only --enable-tests
```

## Project Structure




# Haskell Tools

## Using Cabal

Cabal is a build manager that handles library dependencies.

Our project is defined in `intro.cabal`.


## intro.cabal

## Using GHCi

You can start an interactive prompt (aka. REPL, READ-EVAL-PRINT-LOOP).
It has tab completion and can be used to evaluate expressions and
find types.

```
$ ghci

```

In a project you can do it like this:

```
$ cabal repl

```

## GHCi Commands



# Syntax

## Basic Syntax

```
> "Hello World"
> print "Hello World!"
> length "Hello World"
> let hello = "Hello World" in print hello
> 1 + 3 * 3
> (1 + 3) * 3
> 1 + 3 $ * 4
> (+) 1 3
> (+ 1) 3

```

## List Syntax

```
> [1, 3, 4, 5]
> [1..10]
> let integers = [1..]
> take 10 integers
> let double x = x * x
> take 10 $ map double integers
> 

```

## Pattern Matching

```
> length :: [a] -> Integer
> length []     =  0
> length (x:xs) =  1 + length xs
```

## Conditionals

```
> if null [] then "nothing" else "something"
```

## Types

```
> data User = User Int String
> data User = User { userAge :: Int, userName :: "mikkel" }
```

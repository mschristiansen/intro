% Introduction to Haskell
% Mikkel Christiansen
% House4Hack, August 2015


## Objectives

To teach enough for you to start a project in Haskell.


## Agenda

- Get Started with the Project
- Cover the essential Haskell Tools
- Teach the basic syntax and elements of the Haskell language
- Point to learning resources


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

Version control: git (optional)

Get compiler: ghc

Get build manager: cabal || cabal-install

On Debian/Ubuntu it can be done with:
```
sudo apt-get install git ghc cabal-install
```


## Download Project

Clone repository and tell `cabal` to start fetching dependencies.

```
$ git clone git@github.com:mschristiansen/intro.git
$ cd intro
$ cabal sandbox init
$ cabal install --dependencies-only --enable-tests
$ cabal test
$ cabal run
```


# Haskell Tools

## Using Cabal

Cabal is a build manager that handles library dependencies.

Dependencies are fetched from [Hackage](http://hackage.haskell.org).

Use Hackage to browse reference documentation and find libraries.

Our project is defined in `intro.cabal`.

Let's have a look...

## Using GHCi

GHC interactive. A Haskell REPL.

```
$ ghci
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
Prelude> _
```

In a project you can import all the project dependencies if you do it
like this:

```
$ cabal repl

```

The [Prelude](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html)?


## GHCi Commands

GHCi will try to evaluate expressions not starting with colon.

```
> 2 + 2                     -- evaluates to 4
> let square x = x * x      -- define that two expressions are equivalent
> square 2                  -- square 2 = 2 * 2
> :?                        -- display help
> :t 'a'                    -- to get type information about character 'a'
> :l Intro                  -- to load module Intro
> :m +Data.List             -- import module Data.List
> :set -XOverloadedStrings  -- enable language extension
> :q                        -- to quit
```


# Syntax

## Basic Syntax

```
> "Hello World"
> print "Hello World!"
> length "Hello World"
> let hello = "Hello World" in print hello
> 1 + 3 * 3                               -- infix notation
> (1 + 3) * 3                             -- precedence
> 1 + 3 $ * 4                             -- '$' instead of parenthesis
> (+) 1 3                                 -- prefix an infix fn
> (+ 1) 3                                 -- partial application
> let { x = a + b where a = 1; b=2 } in x -- semicolon and brackets
> let add a b = a + b                     -- define prefix addition
> let add1 a b = (+) a b                  -- f(x,y) = g(x,y) <=> f = g
> let add2 = (+)
> add 3 4
> 3 `add` 4                               -- infix a prefix fn
> let inc = \i -> i + 1                   -- lambda fn
> let triple = \x y z -> (x,y,z)          ---lambda with multiple args
```

## List Syntax

Lists are always homogeneous.

Variables are camelCase.

```
> [1, 3, 4, 5] == (1:3:4:[5])                  -- True
> [2,4..10]                                    -- Even numbers from 2 to 10
> let smallCaps = ['a'..'z']                   -- Characters
> head smallCaps : tail smallCaps == smallCaps -- True
> let integers = [1..]                         -- infinite lists
> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
> let square x = x * x
> take 5 $ drop 100 $ map square $ filter (not . even) integers
> [ y : show x | x <- [1..5], y <- smallCaps, even x ]
> foldl' (+) 0 [2,4..10]
> foldl' (++) "" ["Hello", " ", "World"]
> concat ["Hello", " ", "World"]
> concatMap show [10..20]
```

## Pattern Matching

Multi-line definitions are cumbersome in ghci.

Type in `Scratch.hs` instead and reload.


```
> len :: [a] -> Integer              -- our own recursive length function.
> len []     =  0
> len (x:xs) =  1 + length xs
>
> interl [] [] = []
> interl (x:xs) (y:ys) = x : y : interl xs ys
>
> isA "A" = True
> isA _   = False
>
```

## Conditionals

```
> if null [] then "nothing" else "something"  -- predicate (null [] == True)
>
> null xs | xs == []  = True  -- using a guard in fn definition
> null xs | otherwise = False -- could have pattern matched directly on []

> null = cond xs of
>         [] -> True
>         _  -> False
```

## Types

Always starts with a capital letter.

Type has a constructor typically named same as type.

```
> type Years = Int

> newtype Name = Name { unName :: String }

> data User = User Years Name
> data User2 = User2 { userAge :: Years, userName :: Name } -- record

> let mikkel = User 36 (Name "Mikkel")
> let mikkel2 = User2 36 (Name "Mikkel")
> let mikkel3 = User2 { userAge = 36, userName = (Name "Mikkel")}

> data colors = Red | Blue | Green                       -- a sum type
> data Request = GET String | PUT String | POST String deriving (Show, Eq)

> instance Show User where
>   show (User age name) = unName name ++ " is " ++ show age ++ " old."

```

Type classes is for sharing an interface between types.

Can generally be derived. Most common Show, Read, Eq, Ord, Bounded.

## Maybe and Either

In the Prelude in modules: Data.Maybe and Data.Either.

Often used with fmap, (<$>), and (*) from Control.Applicative.

```
> data Maybe a = Nothing | Just a
>
> (+1) <$> Just 3                -- Just 4
> fmap (+1) Just 3               -- Just 4
> (+)  <$> Just 4 <*> Just 5     -- Just 9
> (++) <$> Just 3 <*> Nothing    -- Nothing
>
> data Either a b = Left a | Right b
>
> (++) <$> Right "Hello " <*> Right "World"  -- Right "Hello World"
> (+1) <$> Left 4                            -- Left 4
> (+1) <$> Right 4                           -- Right 5
> (+)  <$> Left "Not a number" <*> Right 3   -- Left "Not a number"

```

## Side Effects

Side effects only happens in the IO Monad. Once impure you can never
be pure again.

Think of it as a special type that you wrap and unwrap.

```
> Int -> Int               -- Pure: Always same output for same input
> Int -> IO Int            -- Impure: Hard to test or reason about.

> main :: IO ()
> main = do                -- Do syntax for sequencing side effects.
>  print "Hello World"     -- Always from top to bottom.
>  print "Good Morning!"
>  return ()               -- Return 'exits' the Monad.

> main2 = print "Hello World"

> main3 :: IO Int
> main3 = return 4
```

# Return to the Intro Project

## The Modules

- Types.hs -- Definition of Project Types
- Pages.hs -- Some html pages
- Store.hs -- A basic data store based on STM and Data.IntMap
- Intro.hs -- Route definitions


## Create an Address Book

- Create an "addressbook" route in Intro.hs
- Create a static page for route in Pages.hs
- Create a Person type in Types.hs
- Create some static entries in Store.hs (don't use STM yet.)
- List the entries on the addressbook page.
- Store the entries in a (Map Name Person)
- Create routes for "adressbook/:name"
- Add ability to add, remove, search, addresses.


# Learning Resources

## Learn You a Haskell for Great Good!

Basic introduction. The place where most people start.

Book can be read for free online or bought in hardcopy.

[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)


## Real World Haskell

Practical examples of fairly large applications in Haskell.

Book can be read for free online or bought in hardcopy.

[Real World Haskell](http://book.realworldhaskell.org/)


## Lambda Luminaries Meetup

Active functional programming community that meet once a
month. Generally at [house4hack](http://www.house4hack.co.za/).

[Lambda Luminaries](http://www.meetup.com/lambda-luminaries/)


## Reddit

Very active community on Reddit.

[Haskell Subreddit](https://www.reddit.com/r/haskell)

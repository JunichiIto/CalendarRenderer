# CalendarRenderer

Sample Haskell implementation of https://github.com/JunichiIto/calendar_renderer

## Setup Haskell and HSpec

````
$ brew install ghc
$ brew install haskell-platform
$ cabal update
$ cabal install hspec
````

## Usage

````
$ ghci CalendarRenderer
> putStr $ renderCalendar 2013 4
      April 2013     
 Su Mo Tu We Th Fr Sa
     1  2  3  4  5  6
  7  8  9 10 11 12 13
 14 15 16 17 18 19 20
 21 22 23 24 25 26 27
 28 29 30
> :quit
````

## How to test

````
$ runghc CalendarRendererSpec.hs
````

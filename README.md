# IMOKURI.github.io

This is my personal website.  
Sources of my website are in "source" branch.  

[![Build Status](https://travis-ci.org/IMOKURI/IMOKURI.github.io.svg?branch=source)](https://travis-ci.org/IMOKURI/IMOKURI.github.io)


### Build

Using stack and GHC 7.10.* (lts-3.7)

~~~
stack build
stack exec site build
stack exec site watch
~~~

Using cabal and GHC 7.8.* (lts-2.17)

~~~
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
cabal run build
cabal run watch
~~~


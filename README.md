yahoo-portfolio-manager
=======================

Command line portfolio monitoring utility written in Haskell

Persists position information in local sqlite db and pulls latest quotes from yahoo 

Installation
============

```sh
$ sudo apt-get install zlib1g-dev libsqlite3-dev
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

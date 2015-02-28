# Yahoo Portfolio Manager

Command line portfolio monitoring utility written in Haskell
* Persists position information in local sqlite db
* Pulls latest quotes from yahoo 

## Installation

Use cabal to build the source:
```sh
$ sudo apt-get install zlib1g-dev libsqlite3-dev
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```
Documentation can be generated using haddock via the `DocGen.sh` executable.

## License

"Yahoo Portfolio Manager" is released under the MIT License. See `LICENSE`.

## Author

Lhoghu | [@lhoghu](http://twitter.com/lhoghu) | http://github.com/lhoghu

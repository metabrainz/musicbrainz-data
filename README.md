# `musicbrainz-data`

Data access layer for the NES version of the MusicBrainz database.

# Installing

`musicbrainz-data` is packaged using Cabal, and requires GHC 7.4 (or higher). We
recommend installing the 2012.2.0.0 version of the Haskell Platform, as this
provides a lot of dependencies this project uses; however, with an up to
cabal-install and the aforementioned GHC version satisfied you should also be
able to build this project.

Once you have those requirements, simply run:

`cabal install`

# Working on `musicbrainz-data`.

## Dependencies

To install just the dependencies needed to run `musicbrainz-data`, run:

`cabal install --only-dependencies`.

## Building

To build, simply run `cabal build`.

## Running tests

We use Cabal to run tests. To build tests while you develop, enable test
building when you configure musicbrainz-data:

`cabal configure --enable-tests`

Every time you run `cabal build` you will also build tests. You can run tests by
running:

`cabal test`

Check the output of `cabal test --help` for various things that can also be done
while you run tests.

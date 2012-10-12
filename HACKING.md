# Working on `musicbrainz-data`.

## Running tests

We use Cabal to run tests. To build tests while you develop, enable test
building when you configure musicbrainz-data:

`cabal configure --enable-tests`

Every time you run `cabal build` you will also build tests. You can run tests by
running:

`cabal test`

Check the output of `cabal test --help` for various things that can also be done
while you run tests.

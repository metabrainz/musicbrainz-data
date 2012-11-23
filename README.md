# `musicbrainz-data`

Data access layer for the NES version of the MusicBrainz database.

# Installing

`musicbrainz-data` is packaged using Cabal, and requires GHC 7.4 (or
higher). We recommend installing the 2012.2.0.0 version of the Haskell
Platform, as this provides a lot of dependencies this project uses;
however, with an up to cabal-install and the aforementioned GHC
version satisfied you should also be able to build this project.

You also need access to a postgresql database server with the cube and
uuid-ossp extensions, on Ubuntu 12.04 and 12.10 these extensions are
in the postgresql-contrib-9.1 package.

Once you have those requirements, simply run:

`cabal install`

# Working on `musicbrainz-data`.

## Dependencies

To install just the dependencies needed to run `musicbrainz-data`, run:

`cabal install --only-dependencies`.

If you plan to run tests, you will need to run:

`cabal install --enable-tests --only-dependencies`.

## Building

To build, simply run `cabal build`.

## Running tests

To run tests you first need to set up the test database, run the
following commands (obviously the createuser isn't needed if you
already have a musicbrainz user, and the dropdb isn't needed if you
don't have an existing musicbrainz_nes database):

    createuser -U postgres musicbrainz --no-createdb --no-superuser --no-createrole;
    dropdb     -U postgres musicbrainz_nes
    createdb   -U postgres musicbrainz_nes --owner=musicbrainz
    psql       -U postgres musicbrainz_nes

That last command will start the postgresql prompt, on that prompt run
the following commands:

    CREATE EXTENSION "uuid-ossp"
    CREATE EXTENSION cube
    \i test/data/test-schema.sql
    \q

Your database is now ready to run the tests.

We use Cabal to run tests. To build tests while you develop, enable test
building when you configure musicbrainz-data:

`cabal configure --enable-tests`

Every time you run `cabal build` you will also build tests. You can run tests by
running:

`cabal test`

Check the output of `cabal test --help` for various things that can also be done
while you run tests.

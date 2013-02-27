#!/bin/bash
export PATH=/usr/local/postgres/bin:$PATH
cabal update

if ! grep -q $(md5sum test/data/test-schema.sql) .schema-version
then
  psql -U postgres -c "DROP DATABASE musicbrainz_nes"
  psql -U postgres -c "CREATE DATABASE musicbrainz_nes"
  psql -U postgres musicbrainz_nes < /usr/local/postgres/share/contrib/cube.sql
  psql -U postgres musicbrainz_nes < /usr/local/postgres/share/contrib/uuid-ossp.sql
  psql -U postgres musicbrainz_nes < /usr/local/postgres/share/contrib/musicbrainz_collate.sql
  psql -U postgres musicbrainz_nes < test/data/test-schema.sql
fi
md5sum test/data/test-schema.sql > .schema-version

cabal clean
cabal install --enable-tests --only-dependencies --force-reinstalls
cabal configure --enable-tests
cabal build

cabal test --test-option='--jxml=junit.xml' --test-option '-j5'

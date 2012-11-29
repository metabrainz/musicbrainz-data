module MusicBrainz.Data
    ( -- * Fetching entities
      FindLatest(..)
    , ResolveReference(..)
    , ViewRevision(..)

      -- * Working with trees
    , ViewAliases(..)
    , ViewAnnotation(..)
    , ViewIPICodes(..)
    , ViewTree(..)

      -- * Editing data
    , Create(..)
    , Merge
    , merge
    , Update(..)

      -- * Working with revisions
    , revisionParents
   ) where

import MusicBrainz.Data.Artist ()
import MusicBrainz.Data.ArtistType ()
import MusicBrainz.Data.Country ()
import MusicBrainz.Data.Editor ()
import MusicBrainz.Data.Gender ()
import MusicBrainz.Data.Label ()
import MusicBrainz.Data.Recording ()
import MusicBrainz.Data.Release ()
import MusicBrainz.Data.ReleaseGroup ()
import MusicBrainz.Data.ReleaseGroupType ()

import MusicBrainz.Data.Alias
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.IPI
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Revision
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update

import MusicBrainz.Edit
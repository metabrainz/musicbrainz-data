{-| Definitions of all types used within MusicBrainz. -}
module MusicBrainz.Types
    ( -- * MusicBrainz entities
      -- ** Core entities
      -- | Core entities hold MusicBrainz IDs, have versioning, etc.

      -- *** Artists
      Artist(..)
    , ArtistType(..)

      -- *** Labels
    , Label(..)
    , LabelType(..)

      -- *** Recordings
    , Recording(..)

      -- *** Releases
    , Release(..)
    , ReleasePackaging(..)
    , ReleaseStatus(..)

      -- *** Release Groups
    , ReleaseGroup(..)
    , ReleaseGroupType(..), Primary, Secondary

      -- *** Works
    , Work(..)
    , WorkType(..)

      -- ** Non-core entities
      -- | These are entities that are used by core-entities, but are not
      -- particularly interesting on their own.
    , Alias(..)
    , AliasType(..)
    , ArtistCredit
    , ArtistCreditName(..)
    , Country(..)
    , Editor(..)
    , Gender(..)
    , IPI(..)
    , Language(..)
    , Script(..)

      -- ** Relationships
    , LinkedRelationship(..)
    , Relationship(..)
    , RelationshipType(..)
    , RelationshipAttribute(..)

      -- * Entity attributes
    , MBID(..), mbid
    , PartialDate(..)
    , emptyDate, isEmpty

      -- * Versioning
    , CoreEntity, coreRef, coreRevision, coreData
    , Revision
    , Tree(..)
    , treeData

      -- ** Edit system mechanics
    , Edit(..)
    , EditNote(..)
    , VoteScore(..), Vote(voteVote, voteEditor, voteSuperceded)
    , EditStatus(..)

      -- * Entity/reference handling
    , Entity, entityRef, entityData
    , Ref, Referenceable, dereference, RefSpec
    ) where

import MusicBrainz.Types.Internal

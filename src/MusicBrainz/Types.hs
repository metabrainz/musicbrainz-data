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

      -- *** URLs
    , Url(..)

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
    , CdToc(..)
    , Country(..)
    , Editor(..)
    , Gender(..)
    , IPI, ipi
    , ISRC, isrc
    , ISWC, iswc
    , Language(..)
    , Medium(..)
    , MediumFormat(..)
    , PUID(..), puid
    , ReleaseLabel(..)
    , Script(..)
    , Track(..)

      -- ** Relationships
    , LinkedRelationship(..)
    , Relationship(..)
    , RelationshipType(..)
    , RelationshipAttribute(..)
    , RelationshipTarget(..)

      -- * Entity attributes
    , MBID(..), mbid
    , PartialDate(..)
    , emptyDate, isEmpty

      -- * Versioning
    , CoreEntity, coreRef, coreRevision, coreData
    , Revision(revisionCreatedAt)
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

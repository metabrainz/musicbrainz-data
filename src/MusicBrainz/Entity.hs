{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module MusicBrainz.Entity where

import Control.Applicative
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

import MusicBrainz.Ref (Ref)
import {-# SOURCE #-} MusicBrainz.Revision (Revision)

--------------------------------------------------------------------------------
{-| An 'Entity' is something that has been loaded from the database. It cotains
both data about itself (in @entityData@), and also a reference to itself (in
@entityRef@) so that other data/entities can refer to it. -}
data Entity a = Entity { entityRef :: !(Ref a)
                       , entityData :: !a
                       }

deriving instance (Eq a, Show a) => Eq (Entity a)
deriving instance (Eq a, Show a) => Show (Entity a)

instance (FromField (Ref a), FromRow a) => FromRow (Entity a) where
  fromRow = Entity     -- Entity reference
                   <$> field
                       -- Delegetate to the actual entity to parse its data.
                   <*> fromRow


--------------------------------------------------------------------------------
{-| Represents a view of a versioned MusicBrainz \'core\' entity at a specific
point in time (a specific 'Revision'). -}
data CoreEntity a = CoreEntity
    { coreRef :: !(Ref a)
    , coreRevision :: !(Ref (Revision a))
    , coreData :: !a
    }

deriving instance (Eq a, Show a) => Eq (CoreEntity a)
deriving instance (Eq a, Show a) => Show (CoreEntity a)

instance (FromField (Ref a), FromRow a) => FromRow (CoreEntity a) where
  fromRow = CoreEntity     -- Core entity's MBID
                       <$> field
                           -- The revision reference
                       <*> field
                           -- Delegetate to the actual entity to parse its data.
                       <*> fromRow

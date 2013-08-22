{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Ref where

import Control.Lens

--------------------------------------------------------------------------------
{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data Ref a = Referenceable a => Ref !(RefSpec a)

deriving instance Eq (Ref a)
deriving instance Ord (Ref a)
deriving instance Show (Ref a)

{-| The family of types which can be referenced via a primary key. -}
class (Eq (RefSpec a), Ord (RefSpec a), Show (RefSpec a)) => Referenceable a where
  {-| The exact type of all attributes that make up a reference. For example,
  a PostgreSQL @SERIAL@ field would be 'Int', while a compound key might be
  @(@'Int'@, @'Int'). -}
  type RefSpec a :: *

--------------------------------------------------------------------------------
{-| Unpack a reference into its individual attributes. -}
dereference :: Referenceable a => Ref a -> RefSpec a
dereference  = view (from reference)


--------------------------------------------------------------------------------
{-| An 'Iso'morphism to move between a set of attributes and a reference, and
back again. -}
reference :: Referenceable a => Iso' (RefSpec a) (Ref a)
reference = iso Ref (\(Ref r) -> r)

{-| Various 'Lens'es to easily traverse MusicBrainz data types. -}
module MusicBrainz.Lens where

import Control.Lens
import Data.Text

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Provide a single lens to view all relationships inside a 'Tree'. -}
class TreeRelationships a where
  {-| A 'Lens' into all relationships for any 'Tree'. -}
  relationships :: SimpleLens (Tree a) (Set.Set LinkedRelationship)


instance TreeRelationships Artist where
  relationships = lens getter setter
    where
      getter = artistRelationships
      setter tree new = tree { artistRelationships = new }


--------------------------------------------------------------------------------
{-| Provide a single lens to view all aliases inside a 'Tree'. -}
class TreeAliases a where
  {-| A 'Lens' into all aliases for any 'Tree'. -}
  aliases :: SimpleLens (Tree a) (Set.Set Alias)


instance TreeAliases Artist where
  aliases = lens getter setter
    where
      getter = artistAliases
      setter tree new = tree { artistAliases = new }

instance TreeAliases Label where
  aliases = lens getter setter
    where
      getter = labelAliases
      setter tree new = tree { labelAliases = new }

instance TreeAliases Work where
  aliases = lens getter setter
    where
      getter = workAliases
      setter tree new = tree { workAliases = new }


--------------------------------------------------------------------------------
{-| Provide a single lens to view the annotation inside a 'Tree'. -}
class TreeAnnotation a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  annotation :: SimpleLens (Tree a) Text


instance TreeAnnotation Artist where
  annotation = lens getter setter
    where
      getter = artistAnnotation
      setter tree new = tree { artistAnnotation = new }

instance TreeAnnotation Label where
  annotation = lens getter setter
    where
      getter = labelAnnotation
      setter tree new = tree { labelAnnotation = new }

instance TreeAnnotation Recording where
  annotation = lens getter setter
    where
      getter = recordingAnnotation
      setter tree new = tree { recordingAnnotation = new }

instance TreeAnnotation Release where
  annotation = lens getter setter
    where
      getter = releaseAnnotation
      setter tree new = tree { releaseAnnotation = new }

instance TreeAnnotation ReleaseGroup where
  annotation = lens getter setter
    where
      getter = releaseGroupAnnotation
      setter tree new = tree { releaseGroupAnnotation = new }

instance TreeAnnotation Work where
  annotation = lens getter setter
    where
      getter = workAnnotation
      setter tree new = tree { workAnnotation = new }

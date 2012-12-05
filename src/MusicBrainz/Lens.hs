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
  relationships f artist = f (artistRelationships artist) <&> \b -> artist { artistRelationships = b }

--------------------------------------------------------------------------------
{-| Provide a single lens to view all aliases inside a 'Tree'. -}
class TreeAliases a where
  {-| A 'Lens' into all aliases for any 'Tree'. -}
  aliases :: SimpleLens (Tree a) (Set.Set Alias)

instance TreeAliases Artist where
  aliases f artist = f (artistAliases artist) <&> \b -> artist { artistAliases = b }

instance TreeAliases Label where
  aliases f label = f (labelAliases label) <&> \b -> label { labelAliases = b }

instance TreeAliases Work where
  aliases f work = f (workAliases work) <&> \b -> work { workAliases = b }


--------------------------------------------------------------------------------
{-| Provide a single lens to view the annotation inside a 'Tree'. -}
class TreeAnnotation a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  annotation :: SimpleLens (Tree a) Text

instance TreeAnnotation Artist where
  annotation f artist = f (artistAnnotation artist) <&> \b -> artist { artistAnnotation = b }

instance TreeAnnotation Label where
  annotation f label = f (labelAnnotation label) <&> \b -> label { labelAnnotation = b }

instance TreeAnnotation Recording where
  annotation f recording = f (recordingAnnotation recording) <&> \b -> recording { recordingAnnotation = b }

instance TreeAnnotation Release where
  annotation f release = f (releaseAnnotation release) <&> \b -> release { releaseAnnotation = b }

instance TreeAnnotation ReleaseGroup where
  annotation f releaseGroup = f (releaseGroupAnnotation releaseGroup) <&> \b -> releaseGroup { releaseGroupAnnotation = b }

instance TreeAnnotation Work where
  annotation f work = f (workAnnotation work) <&> \b -> work { workAnnotation = b }


--------------------------------------------------------------------------------
{-| Provide a single lens to view the IPI codes inside a 'Tree'. -}
class TreeIPICodes a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  ipiCodes :: SimpleLens (Tree a) (Set.Set IPI)

instance TreeIPICodes Artist where
  ipiCodes f artist = f (artistIpiCodes artist) <&> \b -> artist { artistIpiCodes = b }

instance TreeIPICodes Label where
  ipiCodes f label = f (labelIpiCodes label) <&> \b -> label { labelIpiCodes = b }

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MusicBrainz.Data.AliasType.Tests
import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Data.ArtistCredit.Tests
import qualified MusicBrainz.Data.ArtistType.Tests
import qualified MusicBrainz.Data.Country.Tests
import qualified MusicBrainz.Data.Edit.Tests
import qualified MusicBrainz.Data.Editor.Tests
import qualified MusicBrainz.Data.Gender.Tests
import qualified MusicBrainz.Data.Label.Tests
import qualified MusicBrainz.Data.LabelType.Tests
import qualified MusicBrainz.Data.Language.Tests
import qualified MusicBrainz.Data.MediumFormat.Tests
import qualified MusicBrainz.Data.Recording.Tests
import qualified MusicBrainz.Data.Relationship.Tests
import qualified MusicBrainz.Data.Release.Tests
import qualified MusicBrainz.Data.ReleaseGroup.Tests
import qualified MusicBrainz.Data.ReleaseGroupType.Tests
import qualified MusicBrainz.Data.ReleasePackaging.Tests
import qualified MusicBrainz.Data.ReleaseStatus.Tests
import qualified MusicBrainz.Data.Script.Tests
import qualified MusicBrainz.Data.Url.Tests
import qualified MusicBrainz.Data.Work.Tests
import qualified MusicBrainz.Data.WorkType.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

import Test.MusicBrainz (testGroup, testRunner)

main :: IO ()
main = testRunner
  [ testGroup "MusicBrainz.Data.AliasType"
      MusicBrainz.Data.AliasType.Tests.tests
  , testGroup "MusicBrainz.Data.Artist"
      MusicBrainz.Data.Artist.Tests.tests
  , testGroup "MusicBrainz.Data.ArtistCredit"
      MusicBrainz.Data.ArtistCredit.Tests.tests
  , testGroup "MusicBrainz.Data.ArtistType"
      MusicBrainz.Data.ArtistType.Tests.tests
  , testGroup "MusicBrainz.Data.Country"
      MusicBrainz.Data.Country.Tests.tests
  , testGroup "MusicBrainz.Data.Edit"
      MusicBrainz.Data.Edit.Tests.tests
  , testGroup "MusicBrainz.Data.Editor"
      MusicBrainz.Data.Editor.Tests.tests
  , testGroup "MusicBrainz.Data.Gender"
      MusicBrainz.Data.Gender.Tests.tests
  , testGroup "MusicBrainz.Data.Label"
      MusicBrainz.Data.Label.Tests.tests
  , testGroup "MusicBrainz.Data.LabelType"
      MusicBrainz.Data.LabelType.Tests.tests
  , testGroup "MusicBrainz.Data.Language"
      MusicBrainz.Data.Language.Tests.tests
  , testGroup "MusicBrainz.Data.MediumFormat"
      MusicBrainz.Data.MediumFormat.Tests.tests
  , testGroup "MusicBrainz.Data.Recording"
      MusicBrainz.Data.Recording.Tests.tests
  , testGroup "MusicBrainz.Data.Relationship"
      MusicBrainz.Data.Relationship.Tests.tests
  , testGroup "MusicBrainz.Data.Release"
      MusicBrainz.Data.Release.Tests.tests
  , testGroup "MusicBrainz.Data.ReleaseGroup"
      MusicBrainz.Data.ReleaseGroup.Tests.tests
  , testGroup "MusicBrainz.Data.ReleaseGroupType"
      MusicBrainz.Data.ReleaseGroupType.Tests.tests
  , testGroup "MusicBrainz.Data.ReleasePackaging"
      MusicBrainz.Data.ReleasePackaging.Tests.tests
  , testGroup "MusicBrainz.Data.ReleaseStatus"
      MusicBrainz.Data.ReleaseStatus.Tests.tests
  , testGroup "MusicBrainz.Data.Script"
      MusicBrainz.Data.Script.Tests.tests
  , testGroup "MusicBrainz.Data.Url"
      MusicBrainz.Data.Url.Tests.tests
  , testGroup "MusicBrainz.Data.Work"
      MusicBrainz.Data.Work.Tests.tests
  , testGroup "MusicBrainz.Data.WorkType"
      MusicBrainz.Data.WorkType.Tests.tests
  , testGroup "MusicBrainz.Schema"
      MusicBrainz.Schema.Tests.tests
  , testGroup "MusicBrainz.Types"
      MusicBrainz.Types.Tests.tests
  ]

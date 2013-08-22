{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MusicBrainz.AliasType.Tests
import qualified MusicBrainz.Artist.Tests
import qualified MusicBrainz.ArtistCredit.Tests
import qualified MusicBrainz.ArtistType.Tests
import qualified MusicBrainz.Country.Tests
import qualified MusicBrainz.Edit.Tests
import qualified MusicBrainz.Editor.Tests
import qualified MusicBrainz.Gender.Tests
import qualified MusicBrainz.Label.Tests
import qualified MusicBrainz.LabelType.Tests
import qualified MusicBrainz.Language.Tests
import qualified MusicBrainz.MediumFormat.Tests
import qualified MusicBrainz.Recording.Tests
import qualified MusicBrainz.Relationship.Tests
import qualified MusicBrainz.Release.Tests
import qualified MusicBrainz.ReleaseGroup.Tests
import qualified MusicBrainz.ReleaseGroupType.Tests
import qualified MusicBrainz.ReleasePackaging.Tests
import qualified MusicBrainz.ReleaseStatus.Tests
import qualified MusicBrainz.Script.Tests
import qualified MusicBrainz.Url.Tests
import qualified MusicBrainz.Work.Tests
import qualified MusicBrainz.WorkType.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

import Test.MusicBrainz (testGroup, testRunner)

main :: IO ()
main = testRunner
  [ testGroup "MusicBrainz.AliasType"
      MusicBrainz.AliasType.Tests.tests
  , testGroup "MusicBrainz.Artist"
      MusicBrainz.Artist.Tests.tests
  , testGroup "MusicBrainz.ArtistCredit"
      MusicBrainz.ArtistCredit.Tests.tests
  , testGroup "MusicBrainz.ArtistType"
      MusicBrainz.ArtistType.Tests.tests
  , testGroup "MusicBrainz.Country"
      MusicBrainz.Country.Tests.tests
  , testGroup "MusicBrainz.Edit"
      MusicBrainz.Edit.Tests.tests
  , testGroup "MusicBrainz.Editor"
      MusicBrainz.Editor.Tests.tests
  , testGroup "MusicBrainz.Gender"
      MusicBrainz.Gender.Tests.tests
  , testGroup "MusicBrainz.Label"
      MusicBrainz.Label.Tests.tests
  , testGroup "MusicBrainz.LabelType"
      MusicBrainz.LabelType.Tests.tests
  , testGroup "MusicBrainz.Language"
      MusicBrainz.Language.Tests.tests
  , testGroup "MusicBrainz.MediumFormat"
      MusicBrainz.MediumFormat.Tests.tests
  , testGroup "MusicBrainz.Recording"
      MusicBrainz.Recording.Tests.tests
  , testGroup "MusicBrainz.Relationship"
      MusicBrainz.Relationship.Tests.tests
  , testGroup "MusicBrainz.Release"
      MusicBrainz.Release.Tests.tests
  , testGroup "MusicBrainz.ReleaseGroup"
      MusicBrainz.ReleaseGroup.Tests.tests
  , testGroup "MusicBrainz.ReleaseGroupType"
      MusicBrainz.ReleaseGroupType.Tests.tests
  , testGroup "MusicBrainz.ReleasePackaging"
      MusicBrainz.ReleasePackaging.Tests.tests
  , testGroup "MusicBrainz.ReleaseStatus"
      MusicBrainz.ReleaseStatus.Tests.tests
  , testGroup "MusicBrainz.Script"
      MusicBrainz.Script.Tests.tests
  , testGroup "MusicBrainz.Url"
      MusicBrainz.Url.Tests.tests
  , testGroup "MusicBrainz.Work"
      MusicBrainz.Work.Tests.tests
  , testGroup "MusicBrainz.WorkType"
      MusicBrainz.WorkType.Tests.tests
  , testGroup "MusicBrainz.Schema"
      MusicBrainz.Schema.Tests.tests
  , testGroup "MusicBrainz.Types"
      MusicBrainz.Types.Tests.tests
  ]

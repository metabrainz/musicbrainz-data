{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Editor.Tests where

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor

import Test.MusicBrainz

tests :: [Test]
tests = [ testRegister ]

testRegister :: Test
testRegister = testCase "Register and lookup an editor" $ mbTest $ do
  registered <- register Editor { editorName = "ocharles" }

  Just editorRef <- resolveReference (dereference $ entityRef registered)
  liftIO $ editorRef @?= entityRef registered

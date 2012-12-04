{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Editor.Tests where

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor

import Test.MusicBrainz

tests :: [Test]
tests = [ testRegister
        , testResolveUnknown
        ]

--------------------------------------------------------------------------------
testRegister :: Test
testRegister = testCase "Register and lookup an editor" $ mbTest $ do
  registered <- register ocharles

  Just editorRef <- resolveReference (dereference $ entityRef registered)
  liftIO $ editorRef @?= entityRef registered

  found <- findEditorByName (editorName ocharles)
  liftIO $ found @?= Just registered

  where
    ocharles = Editor { editorName = "ocharles" }

--------------------------------------------------------------------------------
testResolveUnknown :: Test
testResolveUnknown = testCase "Resolve a reference that doesn't exist" $ mbTest $ do
  ref <- resolveReference (-1)
  liftIO $ (ref:: Maybe (Ref Editor)) @?= Nothing

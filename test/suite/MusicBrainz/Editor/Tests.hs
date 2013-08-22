{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Editor.Tests where

import Test.MusicBrainz

import MusicBrainz.Class.ResolveReference
import MusicBrainz.Entity
import MusicBrainz.Editor
import MusicBrainz.Ref

tests :: [Test]
tests = [ testRegister
        , testResolveUnknown
        ]

--------------------------------------------------------------------------------
testRegister :: Test
testRegister = testCase "Register and lookup an editor" $ do
  registered <- register ocharles

  Just editorRef <- resolveReference (dereference $ entityRef registered)
  liftIO $ editorRef @?= entityRef registered

  found <- findEditorByName (editorName ocharles)
  liftIO $ found @?= Just registered

  where
    ocharles = Editor { editorName = "ocharles", editorPassword = "foo" }

--------------------------------------------------------------------------------
testResolveUnknown :: Test
testResolveUnknown = testCase "Resolve a reference that doesn't exist" $ do
  ref <- resolveReference (-1)
  liftIO $ (ref:: Maybe (Ref Editor)) @?= Nothing

fetchEndPoints source r to = go to
  where
    go ToArtist = endPoint ArtistRelationship "artist"
    endPoint constructor table = do
      rels <- query [sql|
        SELECT target_id, relationship_id
        FROM artist_revision
        JOIN artist_tree USING (artist_tree_id)
        JOIN l_artist_artist ON (source_id = artist_tree_id)
        WHERE revision_id = ?
      |] (Only r)
      return $ map constructPartialRel rels
    where
      constructPartialRel (targetId, relationshipId) =
        (constructor targetId, relationshipId)

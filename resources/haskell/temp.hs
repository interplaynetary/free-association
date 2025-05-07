-- Core share calculation function that handles all depths
providerShares :: Player -> TreeRef -> Int -> ShareMap
providerShares player providerRef depth
  | depth <= 1 = initialShares
  | otherwise = normalizeShares finalShares
  where
    initialShares =
      case getTreeByRef player providerRef of
        Nothing -> Map.empty
        Just z ->
          let contributors = nodeContributors $ zipperCurrent z
              mutualValues = [(refTreeId ref, mutualFulfillment player providerRef ref) | ref <- Set.toList contributors]
              total = sum $ map snd mutualValues
          in if total == 0
             then Map.empty
             else Map.fromList [(id, val / total) | (id, val) <- mutualValues]

    (finalShares, _) = foldl' (processDepth player) (initialShares, Set.empty) [2 .. depth]

    normalizeShares shares =
      let totalShare = sum $ Map.elems shares
      in if totalShare > 0
         then Map.map (/ totalShare) shares
         else shares 
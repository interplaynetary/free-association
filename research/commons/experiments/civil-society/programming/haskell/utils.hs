
-------------------------
-- Zipper Navigation --
-------------------------

-- Core navigation
enterChild :: String -> TreeZipper -> Maybe TreeZipper
enterChild name z@(TreeZipper current ctx) =
  case Map.lookup name (nodeChildren current) of
    Just child ->
      Just $
        TreeZipper
          { zipperCurrent = child,
            zipperContext = Just $ Ctx current (Map.delete name $ nodeChildren current) (maybe [] (\c -> [c]) ctx)
          }
    Nothing -> Nothing

exitToParent :: TreeZipper -> Maybe TreeZipper
exitToParent (TreeZipper current (Just (Ctx parent siblings ancestors))) =
  Just $
    TreeZipper
      { zipperCurrent = parent {nodeChildren = Map.insert (nodeId current) current siblings},
        zipperContext = case ancestors of
          [] -> Nothing
          (a : as) -> Just $ a {ctxAncestors = as}
      }
exitToParent _ = Nothing

-- The -: operator for more readable navigation chains
(-:) :: a -> (a -> b) -> b
x -: f = f x

-- Enhanced navigation functions
enterSibling :: String -> TreeZipper -> Maybe TreeZipper
enterSibling name z = exitToParent z >>= enterChild name

goToRoot :: TreeZipper -> TreeZipper
goToRoot z = case exitToParent z of
  Nothing -> z
  Just parent -> goToRoot parent

-- Modify the current node safely
modifyNode :: (Node -> Node) -> TreeZipper -> TreeZipper
modifyNode f z@(TreeZipper current ctx) = z {zipperCurrent = f current}

-- Get all siblings of the current node
getSiblings :: TreeZipper -> [String]
getSiblings z = case exitToParent z of
  Nothing -> []
  Just parent -> Map.keys $ nodeChildren $ zipperCurrent parent

-- Safe navigation with breadcrumbs
type NavigationPath = [String]

followPath :: NavigationPath -> TreeZipper -> Maybe TreeZipper
followPath [] z = Just z
followPath (x : xs) z = enterChild x z >>= followPath xs

-- Get the current path from root
getCurrentPath :: TreeZipper -> NavigationPath
getCurrentPath z = reverse $ getPath z []
  where
    getPath z acc = case exitToParent z of
      Nothing -> acc
      Just parent -> getPath parent (nodeId (zipperCurrent z) : acc)


------------------------------
-- Mutual Fulfillment Utils --
------------------------------

-- Get all contributors that mutually fulfill with the current node
getMutualContributors :: ContributorIndex -> TreeZipper -> [(String, Float)]
getMutualContributors ci z =
  [ (nodeId $ zipperCurrent contributor, mutualFulfillment ci z contributor)
    | contributor <- validContributors
  ]
  where
    contributors = nodeContributors $ zipperCurrent z
    validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]

-- Find the path to the highest mutual fulfillment node
findHighestMutualPath :: ContributorIndex -> TreeZipper -> TreeZipper -> Maybe NavigationPath
findHighestMutualPath ci root target =
  let allPaths = map getCurrentPath $ getAllDescendantsCached root
      pathScores = [(path, scorePath path) | path <- allPaths]
      scorePath path = case followPath path root of
        Nothing -> 0
        Just node -> mutualFulfillment ci node target
   in fmap fst $ maximumByMay (\a b -> compare (snd a) (snd b)) pathScores

-- Helper for safe maximum with empty lists
maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp xs = Just $ maximumBy cmp xs

-- Get all nodes with mutual fulfillment above a threshold
getHighMutualNodes :: ContributorIndex -> TreeZipper -> Float -> [TreeZipper]
getHighMutualNodes ci z threshold =
  filter (\node -> mutualFulfillment ci z node > threshold) (getAllDescendantsCached z)

-- Calculate mutual fulfillment between all pairs in a forest
calculateForestMutualFulfillment :: ContributorIndex -> Forest -> [(String, String, Float)]
calculateForestMutualFulfillment ci forest =
  [ (id1, id2, mutualFulfillment ci z1 z2)
    | (id1, z1) <- Map.toList forest,
      (id2, z2) <- Map.toList forest,
      id1 < id2 -- Only calculate each pair once
  ]
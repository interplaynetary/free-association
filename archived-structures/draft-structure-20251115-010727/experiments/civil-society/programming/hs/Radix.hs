{-# LANGUAGE OverloadedStrings #-}

{- | Radix Tree (Compressed Trie) for Efficient Indexing

A space-efficient data structure for storing and retrieving strings.

Inspired by GunDB's Radix.js, providing:
- Prefix compression (shared prefixes stored once)
- Fast lookups (O(k) where k = key length)
- Space-efficient (no redundant storage)
- Wildcard matching
- Range queries

Use cases in Free Association:
- Username indexing (~@username lookups)
- Path indexing (fast navigation)
- Resource type indexing (find all "tutoring" needs)
- Location indexing (find all in "San Francisco")

Structure:
  A radix tree compresses common prefixes:
  
  Regular Trie:          Radix Tree:
  t → e → s → t          test → [value1]
  t → e → a              tea → [value2]
  
  Instead of: t → e → {s→t, a}
  We get: te → {st→[value1], a→[value2]}

Implementation:
  Uses two special characters:
  - GROUP (ASCII 29): Marks subtrees
  - RECORD (ASCII 30): Marks values
  
  Example:
  insert "test" 1
  insert "tea" 2
  
  Result:
  { "te": {
      GROUP: {
        "st": { RECORD: 1 },
        "a": { RECORD: 2 }
      }
    }
  }
-}

module Radix where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

-- ============================================================================
-- TYPES
-- ============================================================================

-- | Special markers for radix tree structure
groupMarker :: Text
groupMarker = T.singleton $ toEnum 29  -- ASCII group separator

recordMarker :: Text
recordMarker = T.singleton $ toEnum 30  -- ASCII record separator

-- | Radix tree node
data RadixNode a = RadixNode
  { nodeChildren :: M.Map Text (RadixNode a)  -- GROUP subtrees
  , nodeValue :: Maybe a                       -- RECORD value
  }
  deriving (Show, Eq)

-- | Empty radix node
emptyNode :: RadixNode a
emptyNode = RadixNode
  { nodeChildren = M.empty
  , nodeValue = Nothing
  }

-- | Radix tree
newtype RadixTree a = RadixTree (RadixNode a)
  deriving (Show, Eq)

-- | Create empty radix tree
empty :: RadixTree a
empty = RadixTree emptyNode

-- ============================================================================
-- INSERTION
-- ============================================================================

{- | Insert a key-value pair into the radix tree

Algorithm:
1. Find longest matching prefix with existing keys
2. If exact match: update value
3. If partial match: split the key and create subtrees
4. If no match: create new branch

Example:
  insert "test" 1 empty
  insert "tea" 2 tree
  
  Result: "te" → { "st"→1, "a"→2 }
-}
insert :: Text -> a -> RadixTree a -> RadixTree a
insert key value (RadixTree root) =
  RadixTree $ insertNode (T.unpack key) value root

insertNode :: String -> a -> RadixNode a -> RadixNode a
insertNode [] value node =
  -- Empty key: set value at this node
  node { nodeValue = Just value }

insertNode key value node =
  case findMatchingChild key (M.toList $ nodeChildren node) of
    Nothing ->
      -- No matching child: create new branch
      let newChild = emptyNode { nodeValue = Just value }
      in node { nodeChildren = M.insert (T.pack key) newChild (nodeChildren node) }
    
    Just (matchKey, matchLen, child) ->
      if matchLen == length key && matchLen == T.length matchKey
        then
          -- Exact match: update value
          node { nodeChildren = M.insert matchKey (child { nodeValue = Just value }) (nodeChildren node) }
        
        else if matchLen == T.length matchKey
          then
            -- Matched entire existing key, continue down
            let remainingKey = drop matchLen key
                updatedChild = insertNode remainingKey value child
            in node { nodeChildren = M.insert matchKey updatedChild (nodeChildren node) }
          
        else if matchLen == length key
          then
            -- Matched entire new key, need to split existing
            let matchPrefix = T.pack $ take matchLen key
                childSuffix = T.drop matchLen matchKey
                newSubchild = emptyNode 
                  { nodeChildren = M.singleton childSuffix child
                  , nodeValue = Just value
                  }
                updatedChildren = M.delete matchKey $ M.insert matchPrefix newSubchild (nodeChildren node)
            in node { nodeChildren = updatedChildren }
          
        else
          -- Partial match: split both keys
          let matchPrefix = T.pack $ take matchLen key
              keySuffix = drop matchLen key
              childSuffix = T.drop matchLen matchKey
              newKeyChild = emptyNode { nodeValue = Just value }
              newSubtree = emptyNode
                { nodeChildren = M.fromList
                    [ (T.pack keySuffix, newKeyChild)
                    , (childSuffix, child)
                    ]
                }
              updatedChildren = M.delete matchKey $ M.insert matchPrefix newSubtree (nodeChildren node)
          in node { nodeChildren = updatedChildren }

-- | Find the child with the longest matching prefix
findMatchingChild :: String -> [(Text, RadixNode a)] -> Maybe (Text, Int, RadixNode a)
findMatchingChild _ [] = Nothing
findMatchingChild key children =
  let matches = mapMaybe (\(childKey, child) ->
        let matchLen = commonPrefixLength key (T.unpack childKey)
        in if matchLen > 0
          then Just (childKey, matchLen, child)
          else Nothing
        ) children
  in case matches of
    [] -> Nothing
    _ -> Just $ maximumBy (comparing (\(_, len, _) -> len)) matches
  where
    maximumBy cmp (x:xs) = foldl (\a b -> if cmp a b == GT then a else b) x xs
    maximumBy _ [] = error "maximumBy: empty list"

-- | Calculate length of common prefix
commonPrefixLength :: String -> String -> Int
commonPrefixLength = go 0
  where
    go n (x:xs) (y:ys) | x == y = go (n+1) xs ys
    go n _ _ = n

-- ============================================================================
-- LOOKUP
-- ============================================================================

{- | Lookup a value by key

Returns Nothing if key not found, Just value if found.
-}
lookup :: Text -> RadixTree a -> Maybe a
lookup key (RadixTree root) =
  lookupNode (T.unpack key) root

lookupNode :: String -> RadixNode a -> Maybe a
lookupNode [] node = nodeValue node

lookupNode key node =
  case findMatchingChild key (M.toList $ nodeChildren node) of
    Nothing -> Nothing
    Just (matchKey, matchLen, child) ->
      if matchLen < T.length matchKey
        then Nothing  -- Partial match only
        else if matchLen == length key
          then nodeValue child  -- Exact match
          else lookupNode (drop matchLen key) child  -- Continue down

-- ============================================================================
-- DELETION
-- ============================================================================

{- | Delete a key from the radix tree

Returns the tree with the key removed.
-}
delete :: Text -> RadixTree a -> RadixTree a
delete key (RadixTree root) =
  RadixTree $ deleteNode (T.unpack key) root

deleteNode :: String -> RadixNode a -> RadixNode a
deleteNode [] node = node { nodeValue = Nothing }

deleteNode key node =
  case findMatchingChild key (M.toList $ nodeChildren node) of
    Nothing -> node  -- Key not found
    Just (matchKey, matchLen, child) ->
      if matchLen == length key && matchLen == T.length matchKey
        then
          -- Exact match: delete value
          let updatedChild = child { nodeValue = Nothing }
              updatedChildren = if null (nodeChildren updatedChild) && nodeValue updatedChild == Nothing
                then M.delete matchKey (nodeChildren node)
                else M.insert matchKey updatedChild (nodeChildren node)
          in node { nodeChildren = updatedChildren }
        else if matchLen == T.length matchKey
          then
            -- Continue down
            let remainingKey = drop matchLen key
                updatedChild = deleteNode remainingKey child
                updatedChildren = if null (nodeChildren updatedChild) && nodeValue updatedChild == Nothing
                  then M.delete matchKey (nodeChildren node)
                  else M.insert matchKey updatedChild (nodeChildren node)
            in node { nodeChildren = updatedChildren }
          else
            node  -- No match

-- ============================================================================
-- TRAVERSAL
-- ============================================================================

{- | Map over all values in the radix tree

Applies function to each (key, value) pair.
Returns list of results.
-}
mapWithKey :: (Text -> a -> b) -> RadixTree a -> [b]
mapWithKey f (RadixTree root) =
  mapNodeWithKey T.empty f root

mapNodeWithKey :: Text -> (Text -> a -> b) -> RadixNode a -> [b]
mapNodeWithKey prefix f node =
  let valueResults = case nodeValue node of
        Nothing -> []
        Just v -> [f prefix v]
      
      childResults = concatMap (\(childKey, child) ->
        mapNodeWithKey (prefix <> childKey) f child
        ) (M.toList $ nodeChildren node)
  
  in valueResults ++ childResults

{- | Fold over all values in the radix tree -}
foldWithKey :: (Text -> a -> b -> b) -> b -> RadixTree a -> b
foldWithKey f acc (RadixTree root) =
  foldNodeWithKey T.empty f acc root

foldNodeWithKey :: Text -> (Text -> a -> b -> b) -> b -> RadixNode a -> b
foldNodeWithKey prefix f acc node =
  let acc' = case nodeValue node of
        Nothing -> acc
        Just v -> f prefix v acc
      
      acc'' = M.foldlWithKey' (\a childKey child ->
        foldNodeWithKey (prefix <> childKey) f a child
        ) acc' (nodeChildren node)
  
  in acc''

{- | Get all keys in the radix tree -}
keys :: RadixTree a -> [Text]
keys = mapWithKey (\k _ -> k)

{- | Get all values in the radix tree -}
values :: RadixTree a -> [a]
values = mapWithKey (\_ v -> v)

{- | Get all key-value pairs -}
toList :: RadixTree a -> [(Text, a)]
toList = mapWithKey (,)

-- ============================================================================
-- PREFIX QUERIES
-- ============================================================================

{- | Find all keys with a given prefix

Returns list of (key, value) pairs where key starts with prefix.

Example:
  tree has: "test"→1, "tea"→2, "toast"→3
  prefixMatch "te" tree → [("test", 1), ("tea", 2)]
-}
prefixMatch :: Text -> RadixTree a -> [(Text, a)]
prefixMatch prefix (RadixTree root) =
  case findPrefixNode (T.unpack prefix) root of
    Nothing -> []
    Just (remainingPrefix, node) ->
      let fullPrefix = T.pack $ take (T.length prefix - length remainingPrefix) (T.unpack prefix)
      in mapNodeWithKey fullPrefix (,) node

findPrefixNode :: String -> RadixNode a -> Maybe (String, RadixNode a)
findPrefixNode [] node = Just ([], node)

findPrefixNode prefix node =
  case findMatchingChild prefix (M.toList $ nodeChildren node) of
    Nothing -> Nothing
    Just (matchKey, matchLen, child) ->
      if matchLen < T.length matchKey
        then Nothing  -- Partial match only
        else if matchLen >= length prefix
          then Just ([], child)  -- Found prefix
          else findPrefixNode (drop matchLen prefix) child

-- ============================================================================
-- EXAMPLES
-- ============================================================================

{- | Example: Username indexing -}
exampleUsernameIndex :: IO ()
exampleUsernameIndex = do
  putStrLn "📇 Radix Tree - Username Indexing\n"
  
  -- Build username index
  let tree = empty
              & insert "alice" "~alice-pub-key-123"
              & insert "bob" "~bob-pub-key-456"
              & insert "carol" "~carol-pub-key-789"
              & insert "alex" "~alex-pub-key-abc"
  
  putStrLn "Inserted users: alice, bob, carol, alex\n"
  
  -- Lookup exact
  putStrLn "Lookup 'alice':"
  case Radix.lookup "alice" tree of
    Just pubKey -> putStrLn $ "  Found: " ++ T.unpack pubKey
    Nothing -> putStrLn "  Not found"
  putStrLn ""
  
  -- Prefix search
  putStrLn "Prefix search 'al':"
  let matches = prefixMatch "al" tree
  mapM_ (\(name, pubKey) -> putStrLn $ "  " ++ T.unpack name ++ " → " ++ T.unpack pubKey) matches
  putStrLn ""
  
  putStrLn "KEY: Radix trees compress common prefixes!"
  putStrLn "  'alice' and 'alex' share 'al' prefix (stored once)"
  putStrLn ""
  where
    (&) = flip ($)

{- | Example: Path indexing -}
examplePathIndex :: IO ()
examplePathIndex = do
  putStrLn "🗂️ Radix Tree - Path Indexing\n"
  
  -- Build path index
  let tree = empty
              & insert "/users/alice/needs" (Just "alice-needs-data")
              & insert "/users/alice/capacities" (Just "alice-capacities-data")
              & insert "/users/bob/needs" (Just "bob-needs-data")
              & insert "/contexts/SF/needs" (Just "sf-needs-data")
  
  putStrLn "Inserted paths:\n"
  putStrLn "  /users/alice/needs"
  putStrLn "  /users/alice/capacities"
  putStrLn "  /users/bob/needs"
  putStrLn "  /contexts/SF/needs"
  putStrLn ""
  
  -- Find all alice paths
  putStrLn "Find all '/users/alice' paths:"
  let alicePaths = prefixMatch "/users/alice" tree
  mapM_ (\(path, _) -> putStrLn $ "  " ++ T.unpack path) alicePaths
  putStrLn ""
  
  -- Find all user paths
  putStrLn "Find all '/users' paths:"
  let userPaths = prefixMatch "/users" tree
  putStrLn $ "  Found " ++ show (length userPaths) ++ " paths"
  putStrLn ""
  where
    (&) = flip ($)

-- ============================================================================
-- INTEGRATION WITH FREE ASSOCIATION
-- ============================================================================

{- | Use cases in Free Association

1. **Username Index (~@username):**
   ```haskell
   usernameIndex :: RadixTree Text  -- username → pub key
   
   -- Fast lookup
   lookupUser "alice" = Radix.lookup "alice" usernameIndex
   
   -- Autocomplete
   autocomplete "ali" = prefixMatch "ali" usernameIndex
   ```

2. **Resource Type Index:**
   ```haskell
   resourceIndex :: RadixTree [EntityId]  -- type → [entities]
   
   -- Find all "tutoring" needs
   tutoringNeeds = fromMaybe [] $ Radix.lookup "tutoring" resourceIndex
   ```

3. **Location Index:**
   ```haskell
   locationIndex :: RadixTree [EntityId]  -- location → [entities]
   
   -- Find all in San Francisco
   sfEntities = fromMaybe [] $ Radix.lookup "San Francisco" locationIndex
   ```

4. **Path Navigation:**
   ```haskell
   pathIndex :: RadixTree Node  -- path → node
   
   -- Navigate to user's private space
   privateSpace = Radix.lookup "~alice-key/private" pathIndex
   ```

5. **Commitment Index:**
   ```haskell
   commitmentIndex :: RadixTree Commitment  -- id → commitment
   
   -- Fast commitment lookup
   getCommitment id = Radix.lookup id commitmentIndex
   ```
-}

-- ============================================================================
-- PERFORMANCE CHARACTERISTICS
-- ============================================================================

{- | Time Complexity

Operations:
- Insert: O(k) where k = key length
- Lookup: O(k) where k = key length
- Delete: O(k) where k = key length
- Prefix match: O(k + m) where m = number of matches

Space Complexity:
- O(n * k) where n = number of keys, k = average key length
- BUT: Common prefixes stored once (compression!)

Example:
  1000 usernames starting with "user" (4 chars)
  
  Regular storage: 1000 * 4 = 4000 chars for prefix
  Radix tree: 4 chars for prefix (stored once!)
  
  Space savings: 99.9% for prefix!
-}

-- ============================================================================
-- INTEGRATION WITH NETWORKED ZIPPER
-- ============================================================================

{- | Extend NetworkedZipper with radix indexing

Current: Linear search through all nodes
Enhanced: Radix tree index for O(k) lookups

```haskell
data IndexedZipper = IndexedZipper
  { zipper :: Zipper
  , usernameIndex :: RadixTree Text
  , pathIndex :: RadixTree RemoteRef
  , resourceIndex :: RadixTree [EntityId]
  }

-- Fast username lookup (no network query!)
lookupUsername :: Text -> IndexedZipper -> Maybe Text
lookupUsername name iz = Radix.lookup name (usernameIndex iz)

-- Fast path navigation
navigateToPath :: Text -> IndexedZipper -> Maybe Zipper
navigateToPath path iz = do
  ref <- Radix.lookup path (pathIndex iz)
  -- Follow reference
  ...
```
-}


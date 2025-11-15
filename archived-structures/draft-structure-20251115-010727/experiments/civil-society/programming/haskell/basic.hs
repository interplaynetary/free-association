-- Free Association: A Typed Lambda Calculus Formalization

-- Improved type definitions with newtypes for better type safety
newtype PersonId = PersonId String deriving (Eq, Ord, Show)
newtype RecognitionPoints = RecognitionPoints Int deriving (Eq, Ord, Show)

-- Unwrap RecognitionPoints to get the Int value
getPoints :: RecognitionPoints -> Int
getPoints (RecognitionPoints p) = p

-- Type for proportion/percentage values between 0.0 and 1.0
newtype Proportion = Proportion Float deriving (Eq, Ord, Show)

-- Unwrap Proportion to get the Float value
getProportion :: Proportion -> Float
getProportion (Proportion p) = p

-- Custom error type for more informative error handling
data RecognitionError = 
    PersonNotFound PersonId 
  | NoRecognition PersonId PersonId
  | ZeroTotalRecognition PersonId
  | NoMutualRecognitions PersonId
  | PathNotFound PersonId PersonId
  | OtherError String
  deriving (Show)

-- Type for network relationships
-- Each person maps to a list of others they recognize and by how many points
type Recognitions = [(PersonId, RecognitionPoints)]
type Network = [(PersonId, Recognitions)]

-- Type for representing mutual recognition relationships
data MutualRecognition = MutualRecognition {
    person1 :: PersonId,
    person2 :: PersonId,
    mutualValue :: Proportion
} deriving (Show)

-- Calculate one's total recognition points
totalRecognition :: Recognitions -> RecognitionPoints
totalRecognition recognitions = RecognitionPoints $ sum $ map (getPoints . snd) recognitions

-- Calculate share of total recognition
shareOfTotalRecognition :: PersonId -> Recognitions -> Either RecognitionError Proportion
shareOfTotalRecognition person recognitions =
    let total = getPoints $ totalRecognition recognitions
        personRec = case lookup person recognitions of
                       Just rec -> getPoints rec
                       Nothing -> 0
    in if total == 0 
       then Left $ ZeroTotalRecognition (PersonId "the recognizer") 
       else Right $ Proportion (fromIntegral personRec / fromIntegral total)

-- Find a person's recognition of another
recognitionOf :: PersonId -> PersonId -> Network -> Either RecognitionError RecognitionPoints
recognitionOf recognizer recognized network =
    case lookup recognizer network of
        Nothing -> Left $ PersonNotFound recognizer
        Just recognitions -> 
            case lookup recognized recognitions of
                Nothing -> Left $ NoRecognition recognizer recognized
                Just points -> Right points

-- Calculate mutual recognition between two people (minimum of their respective recognition shares)
mutualRecognition :: PersonId -> PersonId -> Network -> Either RecognitionError MutualRecognition
mutualRecognition p1 p2 network = do
    p1RecognizesP2 <- recognitionOf p1 p2 network
    p2RecognizesP1 <- recognitionOf p2 p1 network
    
    p1Recs <- case lookup p1 network of
                 Nothing -> Left $ PersonNotFound p1
                 Just recs -> Right recs
    
    p2Recs <- case lookup p2 network of
                 Nothing -> Left $ PersonNotFound p2
                 Just recs -> Right recs
    
    let p1Total = totalRecognition p1Recs
        p2Total = totalRecognition p2Recs
    
    if getPoints p1Total == 0 
    then Left $ ZeroTotalRecognition p1
    else if getPoints p2Total == 0 
         then Left $ ZeroTotalRecognition p2
         else let p1ShareToP2 = fromIntegral (getPoints p1RecognizesP2) / fromIntegral (getPoints p1Total)
                  p2ShareToP1 = fromIntegral (getPoints p2RecognizesP1) / fromIntegral (getPoints p2Total)
                  mutual = min p1ShareToP2 p2ShareToP1
              in Right $ MutualRecognition p1 p2 (Proportion mutual)

-- Calculate a person's share in another's surplus distribution
surplusShare :: PersonId -> PersonId -> Network -> Either RecognitionError Proportion
surplusShare recipient provider network = do
    mr <- mutualRecognition recipient provider network
    
    providerRecs <- case lookup provider network of
                       Nothing -> Left $ PersonNotFound provider
                       Just recs -> Right recs
    
    -- Get all people the provider recognizes (except themselves)
    let otherPersons = filter (\(p, _) -> p /= provider) providerRecs
    
    -- Calculate all mutual recognitions the provider has
    mutuals <- traverse (\(p, _) -> mutualRecognition provider p network) otherPersons
    
    let totalMutualValues = sum $ map (getProportion . mutualValue) mutuals
    
    if totalMutualValues == 0
    then Left $ NoMutualRecognitions provider
    else Right $ Proportion (getProportion (mutualValue mr) / totalMutualValues)

-- Calculate transitive recognition through a network path
transitiveRecognition :: [PersonId] -> Network -> Either RecognitionError Proportion
transitiveRecognition [] _ = Left $ OtherError "Empty path provided"
transitiveRecognition [_] _ = Right $ Proportion 1.0
transitiveRecognition (p1:p2:ps) network = do
    direct <- surplusShare p1 p2 network
    rest <- transitiveRecognition (p2:ps) network
    Right $ Proportion (getProportion direct * getProportion rest)

-- Simulate the effect of false recognition on self-actualization
falseRecognitionImpact :: Proportion -> Proportion -> Proportion
falseRecognitionImpact falseRecognitionShare realContributorBaseShare =
    let falseShare = getProportion falseRecognitionShare
        realBase = getProportion realContributorBaseShare
        
        -- Total recognition share is fixed at 1.0, so false recognition displaces real recognition
        realRecognitionRemaining = 1.0 - falseShare
        
        -- This causes a proportional decrease in mutual recognition with real contributors
        mutualWithRealContributors = min realRecognitionRemaining realBase
        
        -- Which results in decreased surplus from real contributors
        surplusFromRealContributors = if realBase == 0 
                                      then 0.0 
                                      else mutualWithRealContributors / realBase
        
        -- Leading to decreased social-material basis for self-actualization
        selfActualizationCapacity = surplusFromRealContributors * realBase
    in Proportion selfActualizationCapacity

-- Create a PersonId
person :: String -> PersonId
person = PersonId

-- Create RecognitionPoints
points :: Int -> RecognitionPoints
points = RecognitionPoints

-- Create a Proportion
proportion :: Float -> Proportion
proportion = Proportion

-- Example network with 6 people to demonstrate six degrees of separation
exampleNetwork :: Network
exampleNetwork = [
    (person "Alice", [(person "Bob", points 50), (person "Charlie", points 30), (person "David", points 20)]),
    (person "Bob", [(person "Alice", points 40), (person "Erin", points 60)]),
    (person "Charlie", [(person "Alice", points 70), (person "David", points 30)]),
    (person "David", [(person "Alice", points 10), (person "Charlie", points 40), (person "Frank", points 50)]),
    (person "Erin", [(person "Bob", points 30), (person "Frank", points 70)]),
    (person "Frank", [(person "David", points 60), (person "Erin", points 40)])
]

-- Demonstrate how mutual recognition connects a network
mapNetworkConnections :: PersonId -> Network -> [(PersonId, Either RecognitionError Proportion)]
mapNetworkConnections personId network =
    let allPeople = map fst network
        connections = map (\p -> (p, mutualRecognition personId p network >>= \mr -> 
                                     Right (mutualValue mr))) 
                          (filter (/= personId) allPeople)
    in connections

-- Function to check if a person exists in the network
personExists :: PersonId -> Network -> Bool
personExists personId network = any (\(p, _) -> p == personId) network

-- Function to find all paths between two people up to a certain degree of separation
findPaths :: PersonId -> PersonId -> Int -> Network -> Either RecognitionError [[PersonId]]
findPaths start end maxDepth network = 
    if not (personExists start network) 
    then Left $ PersonNotFound start
    else if not (personExists end network)
         then Left $ PersonNotFound end
         else let paths = findPathsHelper start end maxDepth network []
              in if null paths 
                 then Left $ PathNotFound start end
                 else Right paths

findPathsHelper :: PersonId -> PersonId -> Int -> Network -> [PersonId] -> [[PersonId]]
findPathsHelper current end depth network visited
    | current == end = [reverse (current:visited)]
    | depth <= 0 = []
    | otherwise = 
        case lookup current network of
            Nothing -> []
            Just recognitions ->
                let neighbors = map fst recognitions
                    validNeighbors = filter (\p -> p `notElem` (current:visited)) neighbors
                in concatMap (\next -> findPathsHelper next end (depth-1) network (current:visited)) validNeighbors

-- Calculate strength of connection through all possible paths
connectionStrength :: PersonId -> PersonId -> Int -> Network -> Either RecognitionError Proportion
connectionStrength start end maxDepth network = do
    paths <- findPaths start end maxDepth network
    
    pathStrengths <- traverse (\path -> transitiveRecognition path network) paths
    
    Right $ Proportion (sum $ map getProportion pathStrengths)

-- Helper function to safely print Either values
printEither :: Show a => Show b => Either a b -> IO ()
printEither (Left err) = putStrLn $ "Error: " ++ show err
printEither (Right val) = print val

-- Main function to demonstrate the principles of free association
main :: IO ()
main = do
    putStrLn "Free Association Demonstration"
    putStrLn "-----------------------------"
    
    -- Calculate and display mutual recognitions
    putStrLn "Mutual Recognitions:"
    let alice_bob = mutualRecognition (person "Alice") (person "Bob") exampleNetwork
    printEither alice_bob
    
    -- Show surplus distribution
    putStrLn "\nSurplus Distribution:"
    let alice_share_of_bob = surplusShare (person "Alice") (person "Bob") exampleNetwork
    putStrLn $ "Alice's share of Bob's surplus: "
    printEither alice_share_of_bob
    
    -- Demonstrate six degrees of connection
    putStrLn "\nSix Degrees of Connection:"
    let paths = findPaths (person "Alice") (person "Frank") 6 exampleNetwork
    putStrLn $ "Paths from Alice to Frank: "
    printEither paths
    
    -- Show the impact of false recognition
    putStrLn "\nImpact of False Recognition on Self-Actualization:"
    putStrLn "False Recognition | Remaining Capacity for Self-Actualization"
    putStrLn "--------------------------------------------------------"
    mapM_ (\fr -> putStrLn $ show fr ++ " | " ++ 
                show (falseRecognitionImpact (proportion fr) (proportion 0.7))) 
          [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
    
    -- Show network growth and access to surplus
    putStrLn "\nNetwork Growth and Access to Surplus:"
    let aliceConnections = mapNetworkConnections (person "Alice") exampleNetwork
    mapM_ (\(p, mr) -> do
              putStr $ "Alice's connection to " ++ show p ++ ": "
              printEither mr) 
          aliceConnections
    
    -- Demonstrate share of total recognition
    putStrLn "\nShare of Total Recognition:"
    case lookup (person "Alice") exampleNetwork of
        Nothing -> putStrLn "Error: Alice not found in network"
        Just aliceRecs -> do
            let bobShare = shareOfTotalRecognition (person "Bob") aliceRecs
            putStr "Bob's share of Alice's total recognition: "
            printEither bobShare

-- This formal model demonstrates the key properties of free association:
-- 1. Mutual recognition is based on reciprocal contribution to self-actualization
-- 2. Surplus flows according to mutual recognition
-- 3. Network connections allow transitive access to resources
-- 4. False recognition naturally diminishes connections to real contributors
-- 5. The mathematics ensure that free association self-corrects toward social-material truth
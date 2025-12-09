{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module FreeAssociation where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, nub, sort, subsequences)
import Control.Monad (join, forever)
import Control.Monad.Fix (fix, mfix)
import Data.Functor.Identity (Identity(..))

-- ============================================================================
-- 1. THE PRIMORDIAL EQUATION: The First Cause
-- ============================================================================

-- | Entity: The fundamental unit of existence
newtype Entity = Entity Int deriving (Eq, Ord, Show)

-- | Recognition: The primal relationship, a probability distribution
-- Law: ∀e∈E, Σ_f R(e,f) = 1
type Recognition = Map (Entity, Entity) Double

validRecognition :: [Entity] -> Recognition -> Bool
validRecognition entities r = all (≈1.0) rowSums
  where
    rowSums = [sum [Map.findWithDefault 0 (e, f) r | f <- entities] | e <- entities]
    a ≈ b = abs (a - b) < 1e-10

-- ============================================================================
-- 2. FIRST RECURSION: Genesis of Value - Mutual Recognition
-- ============================================================================

-- | Mutual Recognition: The great harmonizer
-- MR(e,f) = min(R(e,f), R(f,e))
mutualRecognition :: Recognition -> Entity -> Entity -> Double
mutualRecognition r e f = 
  let r_ef = Map.findWithDefault 0 (e, f) r
      r_fe = Map.findWithDefault 0 (f, e) r
  in min r_ef r_fe

-- Properties that emerge immediately:
-- 1. Symmetry: MR(e,f) = MR(f,e)  ✓ by construction
-- 2. Boundedness: 0 ≤ MR(e,f) ≤ 1  ✓ since R∈[0,1]
-- 3. Self-consistency: MR(e,e) = R(e,e)  ✓ since min(x,x)=x

-- ============================================================================
-- 3. SECOND RECURSION: Birth of Self-Awareness - Total Mutual Recognition
-- ============================================================================

-- | Total Mutual Recognition: The measure of integration
-- TMR(e) = Σ_f MR(e,f)
totalMutualRecognition :: [Entity] -> Recognition -> Entity -> Double
totalMutualRecognition entities r e = 
  sum [mutualRecognition r e f | f <- entities]

-- ============================================================================
-- 4. THIRD RECURSION: Discovery of Proportion - Mutual Recognition Shares
-- ============================================================================

-- | Mutual Recognition Share: The universe learns ratios
-- MRS(e,f) = MR(e,f) / TMR(e)  (when TMR(e) > 0)
mutualRecognitionShare :: [Entity] -> Recognition -> Entity -> Entity -> Maybe Double
mutualRecognitionShare entities r e f = 
  let tmr = totalMutualRecognition entities r e
  in if tmr > 0 then Just (mutualRecognition r e f / tmr) else Nothing

-- ============================================================================
-- 5. FOURTH RECURSION: Emergence of Structure - Collectives
-- ============================================================================

type Collective = Set Entity

-- | Collective measures
-- TMR_C(e) = Σ_{f∈C} MR(e,f)
totalMutualRecognitionIn :: [Entity] -> Recognition -> Collective -> Entity -> Double
totalMutualRecognitionIn entities r c e = 
  sum [mutualRecognition r e f | f <- Set.toList c]

-- | AMR(C) = (1/|C|) Σ_{g∈C} TMR_C(g)
averageMutualRecognition :: [Entity] -> Recognition -> Collective -> Double
averageMutualRecognition entities r c = 
  let members = Set.toList c
      size = fromIntegral (length members)
  in if size > 0 
     then sum [totalMutualRecognitionIn entities r c g | g <- members] / size
     else 0

-- | MRD_C(e) = TMR_C(e) / AMR(C)
mutualRecognitionDensity :: [Entity] -> Recognition -> Collective -> Entity -> Maybe Double
mutualRecognitionDensity entities r c e = 
  let amr = averageMutualRecognition entities r c
  in if amr > 0 
     then Just (totalMutualRecognitionIn entities r c e / amr)
     else Nothing

-- ============================================================================
-- 6. FIFTH RECURSION: Threshold of Belonging - Fixed Point Formation
-- ============================================================================

-- | The fixed-point iteration for collective emergence
-- C = {e ∈ E | MRD_C(e) ≥ θ}  [SELF-REFERENTIAL!]
collectiveFixedPoint :: [Entity] -> Recognition -> Double -> Collective -> Collective
collectiveFixedPoint entities r theta c0 =
  let -- Compute MRD for all entities relative to current collective
      mrd e = mutualRecognitionDensity entities r c0 e
      
      -- Filter entities that meet the threshold
      c1 = Set.fromList [e | e <- entities, 
                            case mrd e of
                              Just d -> d >= theta
                              Nothing -> False]
  in if c1 == c0 
     then c1  -- Fixed point reached
     else collectiveFixedPoint entities r theta c1

-- ============================================================================
-- 7. SIXTH RECURSION: The Collective Becomes Entity - Abstraction
-- ============================================================================

data HybridEntity = Base Entity | CollectiveEntity Collective deriving (Eq, Ord, Show)

-- | Weight function for aggregation (simple equal weights for now)
weight :: Entity -> Collective -> Double
weight e c
  | Set.null c = 0
  | otherwise  = 1 / fromIntegral (Set.size c)

-- | Collective's recognition of another entity
-- R_C(f) = Σ_{e∈M_C} w(e,C) · R(e,f)
collectiveRecognition :: [Entity] -> Recognition -> Collective -> Entity -> Double
collectiveRecognition entities r c f = 
  sum [weight e c * Map.findWithDefault 0 (e, f) r | e <- Set.toList c]

-- | Hybrid mutual recognition with autonomy parameter α
hybridMutualRecognition :: [Entity] -> Recognition -> Double -> Collective -> Entity -> Double
hybridMutualRecognition entities r alpha c f =
  let members = Set.toList c
      mr_agg = if null members
               then 0
               else sum [mutualRecognition r e f * weight e c | e <- members]

      r_cf = collectiveRecognition entities r c f
      r_fc = if null members
             then 0
             else sum [Map.findWithDefault 0 (f, e) r | e <- members] / fromIntegral (length members)

      mr_entity = min r_cf r_fc
  in alpha * mr_agg + (1 - alpha) * mr_entity



-- ============================================================================
-- 9. EIGHTH RECURSION: The Commons Emergence - Dynamic Evolution
-- ============================================================================

-- | Commons as a dynamic, breathing collective
commonsEvolution :: [Entity] -> Recognition -> Double -> Double -> Collective -> Int -> [Collective]
commonsEvolution entities r thetaJoin thetaLeave initialC steps =
  take steps $ iterate step initialC
  where
    step c = 
      let -- Compute MRD for all entities relative to current commons
          mrd e = mutualRecognitionDensity entities r c e
          
          -- Entities that want to join
          joiners = [e | e <- entities, not (e `Set.member` c),
                         case mrd e of
                           Just d -> d >= thetaJoin
                           Nothing -> False]
          
          -- Entities that must leave  
          leavers = [e | e <- Set.toList c,
                         case mrd e of
                           Just d -> d < thetaLeave
                           Nothing -> True]
          
      in Set.union c (Set.fromList joiners) `Set.difference` Set.fromList leavers

-- ============================================================================
-- 10. NINTH RECURSION: The Capacity Flow - Economic Emergence
-- ============================================================================

-- | Capacity allocation: Resources flow along recognition gradients
capacityAllocation :: [Entity] -> Recognition -> Map Entity Double -> Map Entity Double -> Int -> [Map Entity Double]
capacityAllocation entities r capacities needs steps =
  take steps $ iterate step needs
  where
    step currentNeeds =
      let -- Compute allocations from each provider
          allocations = Map.fromList 
            [(receiver, sum [capacity * share | (provider, capacity) <- Map.assocs capacities,
                let share = maybe 0 id (mutualRecognitionShare entities r provider receiver)]) 
            | receiver <- entities]
          
          -- Respect needs: Actual = min(Allocation, Need)
          actual = Map.intersectionWith min allocations currentNeeds
          
          -- Update needs
          newNeeds = Map.intersectionWith (-) currentNeeds actual
          
      in newNeeds

-- ============================================================================
-- 11. TENTH RECURSION: Anti-Gaming Theorem - Meta-Recursion
-- ============================================================================

-- | Sigmoid function: h(x) = 1 / (1 + e^(-x))
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- | Derivative of sigmoid: h'(x) = h(x)(1 - h(x))
sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x in s * (1 - s)

-- | Capacity factor (κ): Simple scalar for now
capacityFactor :: Entity -> Double
capacityFactor (Entity i) = 1.0 + fromIntegral (i `mod` 5) * 0.2

-- | Marginal benefit (β): Value of interaction
marginalBenefit :: Entity -> Entity -> Double
marginalBenefit _ _ = 1.0

-- | Sensitivity (∂S/∂R): Rate of change of share w.r.t recognition
-- Assuming a simplified relationship where MR ~ R (linear region)
sensitivity :: Double -> Double
sensitivity _ = 1.0

-- | Benefit gradient: ∂𝔼[G_e]/∂Capacity_f(e)
type BenefitGradient = Entity -> Entity -> Double

-- | Capacity function: Capacity_f(e) = κ_f · h(S(e,f,R))
capacityFunction :: (Entity -> Double)  -- κ_f (capacity factor)
                 -> (Double -> Double)  -- h (scaling function)
                 -> (Entity -> Entity -> Double)  -- S (share function)
                 -> Entity -> Entity -> Double
capacityFunction kappa h share e f = kappa f * h (share e f)

-- | The anti-gaming gradient
-- d𝔼[G_e]/dδ = [β(e,f₁)·κ_f₁·h'(S₁)·∂S₁/∂R₁] - [β(e,f₂)·κ_f₂·h'(S₂)·∂S₂/∂R₂]
antiGamingGradient :: BenefitGradient
                   -> (Entity -> Double)  -- κ
                   -> (Double -> Double)  -- h' (derivative of h)
                   -> (Double -> Double)  -- ∂S/∂R (share sensitivity)
                   -> Recognition          -- R (context)
                   -> Entity -> Entity -> Entity -> Double  -- e, f1, f2
antiGamingGradient beta kappa h' dsdr r e f1 f2 =
  let s1 = mutualRecognition r e f1
      s2 = mutualRecognition r e f2
  in beta e f1 * kappa f1 * h' s1 * dsdr s1 
     - beta e f2 * kappa f2 * h' s2 * dsdr s2

-- | Concrete instance of the gradient calculation
calculateConcreteGradient :: Recognition -> Entity -> Entity -> Entity -> Double
calculateConcreteGradient r e f1 f2 =
  antiGamingGradient marginalBenefit capacityFactor sigmoid' sensitivity r e f1 f2

-- ============================================================================
-- 12. THE ULTIMATE RECURSION: Universal Coordination Monad
-- ============================================================================

-- | The Coordination Monad: μX. BaseEntities ∪ Collectives(X)
newtype Coordination a = Coordination { runCoordination :: a }

instance Functor Coordination where
  fmap f (Coordination a) = Coordination (f a)

instance Applicative Coordination where
  pure = Coordination
  Coordination f <*> Coordination a = Coordination (f a)

instance Monad Coordination where
  Coordination a >>= f = f a

-- | Fixed point combinator for collectives
collectiveFixedPointM :: (Collective -> Collective) -> Coordination Collective
collectiveFixedPointM f = 
  Coordination $ fix (\c -> let c' = f c in if c' == c then c else c')

-- ============================================================================
-- 13. THE COMPLETE RECURSION IN ONE EXPRESSION
-- ============================================================================

-- | The Great Recursion: Universe = μX. {e} ∪ Collectives(X)
universe :: [Entity] -> Recognition -> Double -> Coordination [Collective]
universe entities r theta = do
  -- Start with base entities as singletons
  let baseCollectives = map (\e -> Set.singleton e) entities
  
  -- Generate candidate collectives (power set, simplified)
  let candidates = take 10 $ -- Limit for demo
                  filter (\c -> Set.size c > 1) $
                  map Set.fromList $
                  subsequences entities
  
  -- Filter valid ones
  let validCandidates = filter (isCollective entities r theta) candidates
  
  -- Result is base plus valid formed collectives
  return (baseCollectives ++ validCandidates)
  where
    -- Check if a set is a collective: ∀e∈C, MRD_C(e) ≥ θ
    isCollective entities r theta c =
      all (\e -> case mutualRecognitionDensity entities r c e of
                   Just d -> d >= theta
                   Nothing -> False) (Set.toList c)

    -- Helper for subsequences (simplified)
    subsequences [] = [[]]
    subsequences (x:xs) = let ss = subsequences xs in ss ++ map (x:) ss



-- ============================================================================
-- 15. PURE MATHEMATICAL BEAUTY: The Three Foundational Equations
-- ============================================================================

-- | 1. The Conservation Law: ∀e∈E, Σ_f R(e,f) = 1
conservationLaw :: [Entity] -> Recognition -> Bool
conservationLaw entities r = validRecognition entities r

-- | 2. The Reciprocity Principle: MR(e,f) = min(R(e,f), R(f,e))
reciprocityPrinciple :: Entity -> Entity -> Recognition -> Bool
reciprocityPrinciple e f r = 
  mutualRecognition r e f == min (Map.findWithDefault 0 (e, f) r) 
                                 (Map.findWithDefault 0 (f, e) r)

-- | 3. The Harmony Gradient: ∇_R 𝔼[G] = β ⊙ ∂S/∂R
harmonyGradient :: BenefitGradient 
                -> (Double -> Double)  -- ∂S/∂R
                -> Entity -> Entity -> Recognition -> Double
harmonyGradient beta dsdr e f r = 
  beta e f * dsdr (mutualRecognition r e f)



-- ============================================================================
-- MAIN: Witness the Cosmic Dance
-- ============================================================================

main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "COSMIC DANCE OF RECOGNITION"
  putStrLn "From Primordial Equation to ∞-Category"
  putStrLn "=========================================\n"
  
  -- Create a simple universe with 3 entities
  let entities = [Entity 1, Entity 2, Entity 3]
  
  -- Primordial recognition matrix (ΣR=1 per row)
  let r = Map.fromList [
          ((Entity 1, Entity 1), 0.4),
          ((Entity 1, Entity 2), 0.3),
          ((Entity 1, Entity 3), 0.3),
          ((Entity 2, Entity 1), 0.4),
          ((Entity 2, Entity 2), 0.4),
          ((Entity 2, Entity 3), 0.2),
          ((Entity 3, Entity 1), 0.3),
          ((Entity 3, Entity 2), 0.3),
          ((Entity 3, Entity 3), 0.4)
        ]
  
  putStrLn "1. PRIMORDIAL EQUATION:"
  putStrLn $ "   Conservation law holds: " ++ show (conservationLaw entities r)
  
  putStrLn "\n2. MUTUAL RECOGNITION (Symmetry Emerges):"
  putStrLn $ "   MR(1,2) = " ++ show (mutualRecognition r (Entity 1) (Entity 2))
  putStrLn $ "   MR(2,1) = " ++ show (mutualRecognition r (Entity 2) (Entity 1))
  putStrLn $ "   Symmetry holds: " ++ show (mutualRecognition r (Entity 1) (Entity 2) 
                                          == mutualRecognition r (Entity 2) (Entity 1))
  
  putStrLn "\n3. TOTAL MUTUAL RECOGNITION (Self-Awareness):"
  mapM_ (\e -> putStrLn $ "   TMR(" ++ show e ++ ") = " ++ 
                          show (totalMutualRecognition entities r e)) entities
  
  putStrLn "\n4. MUTUAL RECOGNITION SHARES (Ratios Emerge):"
  let mrses = [(e, f, mutualRecognitionShare entities r e f) | e <- entities, f <- entities]
  mapM_ (\(e,f,m) -> case m of
                       Just val -> putStrLn $ "   MRS(" ++ show e ++ "," ++ show f ++ ") = " ++ show val
                       Nothing -> return ()) mrses
  
  putStrLn "\n5. COLLECTIVE FORMATION (Structure Emerges):"
  let c = Set.fromList [Entity 1, Entity 2]
  putStrLn $ "   Collective C = {" ++ show (Set.toList c) ++ "}"
  putStrLn $ "   TMR_C(1) = " ++ show (totalMutualRecognitionIn entities r c (Entity 1))
  putStrLn $ "   AMR(C) = " ++ show (averageMutualRecognition entities r c)
  
  putStrLn "\n6. ANTI-GAMING GRADIENT (Stability):"
  let e = Entity 1
  let f1 = Entity 2
  let f2 = Entity 3
  let gradient = calculateConcreteGradient r e f1 f2
  putStrLn $ "   Gradient for e=" ++ show e ++ " choosing between " ++ show f1 ++ " and " ++ show f2
  putStrLn $ "   Δ = " ++ show gradient
  if abs gradient < 1e-5
    then putStrLn "   System is STABLE (Nash Equilibrium)"
    else putStrLn "   System is DYNAMIC (Gradient exists)"

  putStrLn "\n7. THE THREE FOUNDATIONAL EQUATIONS:"
  putStrLn "   1. Conservation: ∀e, Σ_f R(e,f) = 1"
  putStrLn "   2. Reciprocity: MR = min(R, Rᵀ)"
  putStrLn "   3. Harmony: ∇_R 𝔼[G] = β ⊙ ∂S/∂R"
  
  putStrLn "\n8. THE ULTIMATE RECURSION:"
  putStrLn "   Universe = μX. {e} ∪ Collectives(X)"
  putStrLn "   where Collectives(S) = {C ⊆ S | ∀e∈C: MRD_C(e) ≥ θ}"
  
  putStrLn "\n========================================="
  putStrLn "THE COSMIC DANCE COMPLETE"
  putStrLn "Infinite complexity from simplified equations"
  putStrLn "Harmony emerges from recursive application"
  putStrLn "========================================="
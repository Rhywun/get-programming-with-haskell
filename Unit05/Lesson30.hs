module Lesson30 where

import qualified Data.Map as Map

--
--
-- The limitations of Applicative and Functor
--
--
-- Combining two Map lookups
--
type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep")
    , (2, "KINGinYELLOW")
    , (3, "dagon1997")
    , (4, "rcarter1919")
    , (5, "xCTHULHUx")
    , (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000)
    , ("KINGinYELLOW", 15000)
    , ("dagon1997", 300)
    , ("rcarter1919", 12)
    , ("xCTHULHUx", 50000)
    , ("yogSOThoth", 150000)
    ]

-- Goal:
{-
creditsFromId :: GamerId -> Maybe PlayerCredits
crediteFromId = undefined
-}

-- We need to hook up these two functions:
--
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- Wrapper:
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- QC3001
creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
-- Returns a nested Maybe

-- cont. p. 377

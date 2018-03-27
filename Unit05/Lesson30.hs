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
-}
-- We need to hook up these two functions:
--
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- A function to connect these would look like this, concrete & generally:
{-
                 Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
Applicative f => f     a        -> (a        -> f     b)             -> f     b
-}
-- One way is to make a wrapper around the 2nd function:
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- This works but is clumsy.
--
--
-- QC3001
{-
creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
-}
-- Returns a nested Maybe
--
--
-- A not-so-trivial echo IO action
--
-- We want this:
{-
  echo :: IO ()
-}
--
-- Need to combine these:
{-
getLine :: IO String
putStrLn :: String -> IO ()
-}
-- ==> IO String -> (String -> IO ()) -> IO ()
--  or Applicative f => f a -> (a -> f b) -> f b
--
-- QC302 - Try the same tactic we used with Maybe
{-
altPutStrLn :: IO String -> IO ()
altPutStrLn = undefined -- ???
-}
-- There is no way to get a value out of the IO context
--
--
-- The bind operator - >>=
--
--
-- Signature:
{-
  (>>=) :: Monad m => m a -> (a -> m b) -> m b
-}
-- Now we can solve the Maybe problem without the need for wrappers
--
-- E.g. creditsFromId' 1 == Just 2000
creditsFromId' :: GamerId -> Maybe PlayerCredits
creditsFromId' id = lookupUserName id >>= lookupCredits

--
-- And we can solve the IO problem which was impossible before
--
echo :: IO ()
echo = getLine >>= putStrLn

--
--
-- QC3003
--
readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

qc3 :: IO ()
qc3 = readInt >>= printDouble

--
--
-- The Monad type class
--
--
{-
  class Functor f where
    fmap   :: (a -> b) -> f a -> f b              -- or <$>

  class Functor f => Applicative f where
    <*>    :: f (a -> b) -> f a -> f b
    pure   :: a -> f a

  class Applicative m => Monad m where
    >>=    :: m a -> (a -> m b) -> m b
    >>     :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a
-}
-- Use >> when you want to discard the value produced by the first function
echoVerbose :: IO ()
echoVerbose = putStrLn "Usage: echo [string]" >> getLine >>= putStrLn

--
--  Using Monad to build a Hello <Name> program
--
askForName :: IO ()
askForName = putStr "Name? "

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- How to combine these?
--
-- 1. askForName >> getLine :: IO String
-- 2. (\name -> return (nameStatement name)) :: IO String
--                Using a lambda expression with return transforms this
--                type from (String -> String) into (String -> IO String)
-- 3. askForName >> getLine >>= (\name -> return (nameStatement name))
--
--
-- QC3004
-- (\x -> return ((+2) x))
--
-- 4. Finally, pipe everything to putStrLn
--    and apply hlint a few times to get something considerably shorter
--
helloName :: IO ()
helloName = (nameStatement <$> (askForName >> getLine)) >>= putStrLn

--
--
-- Q3001
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f x = x >>= (return . f)

-- Q3002
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp = undefined
-- LOLPASS

-- Q3003
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

module Lesson30 where

import qualified Data.Map                      as Map

--
-- The limitations of Applicative and Functor
--

-- Combining two Map lookups

type UserName = String

type GamerID = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerID UserName
userNameDB = Map.fromList
  [ (1, "nYarlathoTep")
  , (2, "KINGinYELLOW")
  , (3, "dagon1997")
  , (4, "rcarter1919")
  , (5, "xCTHULHUx")
  , (6, "yogSOThoth")
  ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList
  [ ("nYarlathoTep", 2000)
  , ("KINGinYELLOW", 15000)
  , ("dagon1997"   , 300)
  , ("rcarter1919" , 12)
  , ("xCTHULHUx"   , 50000)
  , ("yogSOThoth"  , 150000)
  ]

-- Goal:
{-
creditsFromId :: GamerId -> Maybe PlayerCredits
-}

-- We need to hook up these two functions:

lookupUserName :: GamerID -> Maybe UserName
lookupUserName gamerID = Map.lookup gamerID userNameDB

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

creditsFromID :: GamerID -> Maybe PlayerCredits
creditsFromID gamerID = altLookupCredits (lookupUserName gamerID)

-- This works but is clumsy.

-- QC1

creditsFromIDStrange :: GamerID -> Maybe (Maybe PlayerCredits)
creditsFromIDStrange gamerID = pure lookupCredits <*> lookupUserName gamerID
-- It returns a nested Maybe.

-- A not-so-trivial echo IO action

-- We want this:
{-
  echo :: IO ()
-}

-- Need to combine these:
{-
getLine :: IO String
putStrLn :: String -> IO ()
-}

-- ==>                  IO String -> (String -> IO ()) -> IO ()
--  or Applicative f => f  a      -> (a      -> f  b)  -> f  b

-- QC2 - Try the same tactic we used with Maybe

{-
altPutStrLn :: IO String -> IO ()
altPutStrLn = undefined -- ???
-}
-- There is no way to get a value out of the IO context.

--
-- The bind operator: >>=
--

-- Signature:
{-
  (>>=) :: Monad m => m a -> (a -> m b) -> m b
-}

-- Now we can solve the Maybe problem without the need for wrappers

{-
creditsFromID' 1 -- Just 2000
-}
creditsFromID' :: GamerID -> Maybe PlayerCredits
creditsFromID' gamerID = lookupUserName gamerID >>= lookupCredits

-- And we can chain together another level of indirection

type WillCoID = Int

gamerIDDB :: Map.Map WillCoID GamerID
gamerIDDB =
  Map.fromList [(1001, 1), (1002, 2), (1003, 3), (1004, 4), (1005, 5), (1006, 6)]

lookupGamerID :: WillCoID -> Maybe GamerID
lookupGamerID willCoID = Map.lookup willCoID gamerIDDB

{-
creditsFromWillCoID  1001 -- Just 2000
creditsFromWillCoID  100  -- Nothing
-}
creditsFromWillCoID :: WillCoID -> Maybe PlayerCredits
creditsFromWillCoID willCoID =
  lookupGamerID willCoID >>= lookupUserName >>= lookupCredits

-- And we can solve the IO problem which was impossible before

echo :: IO ()
echo = getLine >>= putStrLn

-- QC3

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

{-
qc3 ⏎ 3 -- 6
-}
qc3 :: IO ()
qc3 = readInt >>= printDouble

--
-- The Monad type class
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
echoVerbose = putStrLn "Enter a string and we'll echo it!" >> getLine >>= putStrLn

--  Using Monad to build a Hello <Name> program

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

-- QC4
-- (\x -> return ((+2) x))

-- 4. Finally, pipe everything to putStrLn
--    and apply hlint a few times to get something considerably shorter
--    and not painful to look at

helloName :: IO ()
-- helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn
helloName = (nameStatement <$> (askForName >> getLine)) >>= putStrLn

-- Q1

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f x = x >>= (return . f)

-- Q2

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = f >>= (\f -> x >>= (return . f))
-- Lolcheat

-- Q3

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just x) f = f x

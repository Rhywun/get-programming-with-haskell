module Lesson31 where

import qualified Data.Map as Map

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
-- Remember, hlint cleaned it to this soup:
-- helloName = (nameStatement <$> (askForName >> getLine)) >>= putStrLn
helloName =
  askForName >> -- becomes single-line statement
  getLine >>=
  (\name -- becomes <-
    -> return (nameStatement name)) >>=
  putStrLn

--
--
-- Do-notation revisited
--
--
helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

--
--
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

mainSugared :: IO ()
mainSugared = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

mainDesugared :: IO ()
mainDesugared =
  getLine >>=
  (\name --
    ->
     (\statement --
       -> putStrLn statement)
       (helloPerson name))

--
-- Sometimes the bind function is as clear as do
--
echo :: IO ()
echo = getLine >>= putStrLn

-- QC3101
echoDo :: IO ()
echoDo = do
  line <- getLine
  putStrLn line

--
--
-- Using do-notation to reuse the same code in different contexts
--
--
data Grade
  = F
  | D
  | C
  | B
  | A
  deriving (Eq, Ord, Enum, Show, Read)

data Degree
  = HS
  | BA
  | MS
  | PhD
  deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId         :: Int
  , candidateCodeReview :: Grade
  , candidateCultureFit :: Grade
  , candidateEducation  :: Degree
  } deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = candidateCodeReview candidate > B
    passedCultureFit = candidateCultureFit candidate > C
    educationMin = candidateEducation candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

--
-- QC3102
-- viable me == False
me = Candidate 1 A B BA

--
-- IO context
--
readInt :: IO Int
readInt = getLine >>= (return . read) -- or: read <$> getLine... um... OK

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStr "Id? "
  id <- readInt
  putStr "Code review? "
  codeReview <- readGrade
  putStr "Culture fit? "
  cultureFit <- readGrade
  putStr "Education? "
  education <- readDegree
  return (Candidate id codeReview cultureFit education)

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

--
-- QC3103
readGrade' :: IO Grade
readGrade' = do
  grade <- getLine
  return (read grade)

--
-- Maybe context
--
candidate1 = Candidate 1 A A BA

candidate2 = Candidate 2 C A PhD

candidate3 = Candidate 3 A B MS

candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

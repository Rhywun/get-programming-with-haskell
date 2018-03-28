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
readInt = getLine >>= (return . read) -- or: read <$> getLine... ... um... OK

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
assessCandidateMaybe id = do
  candidate <- Map.lookup id candidateDB
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

-- QC3104
qc4 :: Maybe String -> String
qc4 (Just decision) = decision
qc4 Nothing         = "Error: id not found"

--
-- List context
--
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

-- QC3105
-- Yes, it returns the empty list.
--
--
-- Putting it all together and writing a monadic (i.e. universal) function
-- Note that `candidates` here can be one of:
--   1. a single candidate entered interactively (IO Candidate)
--   2. a single candidate looked up in a Map (Maybe Candidate)
--   3. a list of candidates ([Candidate])
--   4. (some other Monad)
--
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

--
--
-- Q3101
-- Cheated (duh) and even fixed typo in the book
--
type Size = Double

area :: Size -> Double
area size = pi * (size / 2) ^ 2

type Cost = Double

type Pizza = (Size, Cost)

costPerSqIn :: Pizza -> Double
costPerSqIn (size, cost) = cost / area size

cheaperPizza :: Pizza -> Pizza -> Pizza
cheaperPizza p1 p2 =
  case compare (costPerSqIn p1) (costPerSqIn p2) of
    LT -> p1
    _  -> p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++
  show size ++ "\" pizza is cheaper at " ++ show cpsi ++ " per sq. in."
  where
    cpsi = costPerSqIn (size, cost)

ioMain :: IO ()
ioMain =
  putStrLn "What is the size of pizza 1" >> getLine >>=
  (\size1 ->
     putStrLn "What is the cost of pizza 1" >> getLine >>=
     (\cost1 ->
        putStrLn "What is the size of pizza 2" >> getLine >>=
        (\size2 ->
           putStrLn "What is the cost of pizza 2" >> getLine >>=
           (\cost2 ->
              (\pizza1 ->
                 (\pizza2 ->
                    (\betterPizza -> putStrLn (describePizza betterPizza))
                      (cheaperPizza pizza1 pizza2))
                   (read size2, read cost2))
                (read size1, read cost1)))))

--
-- Q3102
--
listMain :: [String]
listMain = do
  size1 <- [10, 12, 17]
  cost1 <- [12.0, 15.0, 20.0]
  size2 <- [10, 11, 18]
  cost2 <- [13.0, 14.0, 21.0]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = cheaperPizza pizza1 pizza2
  return (describePizza betterPizza)

--
-- Q3103
--
monadMain :: Monad m => m Size -> m Cost -> m Size -> m Cost -> m String
monadMain size1 cost1 size2 cost2 = do
  size1' <- size1
  cost1' <- cost1
  size2' <- size2
  cost2' <- cost2
  let pizza1 = (size1', cost1')
  let pizza2 = (size2', cost2')
  let betterPizza = cheaperPizza pizza1 pizza2
  return (describePizza betterPizza)

q3 = monadMain [10, 12, 17] [12.0, 15.0, 20.0] [10, 11, 18] [13.0, 14.0, 21.0]

module Lesson33 where

import           Control.Applicative
import           Control.Monad

--
-- Getting started
--

data Name = Name
  { firstName :: String
  , lastName  :: String
  }

instance Show Name where
  show (Name fn ln) = mconcat [fn, " ", ln]

data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId   :: Int
  , gradeLevel  :: GradeLevel
  , studentName :: Name
  } deriving (Show)

students :: [Student]
students =
  [ Student 1 Senior   (Name "Audre" "Lorde")
  , Student 2 Junior   (Name "Leslie" "Silko")
  , Student 3 Freshman (Name "Judith" "Butler")
  , Student 4 Senior   (Name "Guy" "Debord")
  , Student 5 Sophmore (Name "Jean" "Baudrillard")
  , Student 6 Junior   (Name "Julia" "Kristeva")
  ]

--
-- SELECT
--

-- Notice the signature is the same as for `fmap` except specialized to Monad

{-
_select gradeLevel students
_select (firstName . studentName) students
_select (\x -> (studentName x, gradeLevel x)) students
-}
_select :: Monad m => (a -> b) -> m a -> m b
_select f xs = f <$> xs

--
-- WHERE
--

{-
_where (\x -> gradeLevel x == Senior) students
_where (startsWith 'J' . firstName . studentName) students
_where (startsWith 'J' . firstName) (_select studentName students)
-}
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where p xs = do
  x <- xs
  guard (p x)
  return x

-- Or:
_where' p xs = xs >>= (\x -> guard (p x) >> return x)

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

-- == [Judith Butler,Jean Baudrillard,Julia Kristeva]
js = _where (startsWith 'J' . firstName) (_select studentName students)

--
-- JOIN
--

data Teacher = Teacher
  { teacherId   :: Int
  , teacherName :: Name
  } deriving (Show)

teachers =
  [Teacher 100 (Name "Simone" "De Beauvior"), Teacher 200 (Name "Susan" "Sontag")]

data Course = Course
  { courseId        :: Int
  , courseTitle     :: String
  , courseTeacherId :: Int
  } deriving (Show)

courses = [Course 101 "French" 100, Course 201 "English" 200]

{-
_join teachers courses teacherId courseTeacher
-}
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard (prop1 (fst dpairs) == prop2 (snd dpairs))
  return dpairs

-- No idea what to do here to desugar:
{-
_join' data1 data2 prop1 prop2 =
  data1 >>= (\d1 -> data2 >>= (\d2 -> let dpairs = (d1, d2)) >>
    guard (prop1 (fst dpairs) == prop2 (snd dpairs)) >> return dpairs)
-}

--
-- Building your HINQ interface and example queries
--

-- How to pleasantly combine these?

joinData = _join teachers courses teacherId courseTeacherId

whereResult = _where ((== "English") . courseTitle . snd) joinData

selectResult = _select (teacherName . fst) whereResult -- == [Susan Sontag]

-- Here's one way:
_hinq selectQuery joinQuery whereQuery = (selectQuery . whereQuery) joinQuery

finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId courseTeacherId)
                    (_where ((== "English") . courseTitle . snd))

-- What if we don't need a WHERE clause?
teacherFirstName = _hinq (_select firstName) finalResult (_where (const True))
  -- We can do better

--
-- Making a HINQ type for your queries
--

-- First, note the change to monoidal type signatures above
-- on _select, _where, and _join

data HINQ m a b
  = HINQ (m a -> m b) -- _select
         (m a) -- _join or data
         (m a -> m a) -- _where
  | HINQ_ (m a -> m b) -- _select
          (m a) -- _join or data

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause       ) = _hinq sClause jClause (_where (const True))

--
-- Running your HINQ queries
--

-- E.g. runHINQ query1 == [Susan Sontag]
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId courseTeacherId)
              (_where ((== "English") . courseTitle . snd))

-- E.g. runHINQ query2 == [Simone De Beauvior,Susan Sontag]
query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

-- HINQ with Maybe types

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

-- E.g. runHINQ maybeQuery1 == Just Simone De Beauvior
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId courseTeacherId)
                   (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

-- E.g. runHINQ maybeQuery2 == Nothing
maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId courseTeacherId)
                   (_where ((== "French") . courseTitle . snd))

-- Enough!

module Lesson33 where

import           Control.Monad

--
--
-- Getting started
--
--
data Name = Name
  { firstName :: String
  , lastName  :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId   :: Int
  , gradeLevel  :: GradeLevel
  , studentName :: Name
  } deriving (Show)

students =
  [ Student 1 Senior (Name "Audre" "Lorde")
  , Student 2 Junior (Name "Leslie" "Silko")
  , Student 3 Freshman (Name "Judith" "Butler")
  , Student 4 Senior (Name "Guy" "Debord")
  , Student 5 Sophmore (Name "Jean" "Baudrillard")
  , Student 6 Junior (Name "Julia" "Kristeva")
  ]

--
--
-- SELECT
--
--
-- _select (firstName . studentName) students
--   == ["Audre","Leslie","Judith","Guy","Jean","Julia"]
-- _select' (\x -> (studentName x, gradeLevel x)) students
--   == [(Audre Lorde,Senior),(Leslie Silko,Junior),(Judith Butler,Freshman),
--       (Guy Debord,Senior),(Jean Baudrillard,Sophmore),(Julia Kristeva,Junior)]
_select :: (a -> b) -> [a] -> [b]
_select f xs = do
  x <- xs
  return (f x)

-- Or:
_select' f xs = f <$> xs

--
--
-- WHERE
--
--
-- _where (startsWith 'J' . firstName) students
_where :: (a -> Bool) -> [a] -> [a]
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
--
-- JOIN
--
--
data Teacher = Teacher
  { teacherId   :: Int
  , teacherName :: Name
  } deriving (Show)

teachers =
  [ Teacher 100 (Name "Simone" "De Beauvior")
  , Teacher 200 (Name "Susan" "Sontag")
  ]

data Course = Course
  { courseId    :: Int
  , courseTitle :: String
  , teacher     :: Int
  } deriving (Show)

courses = [Course 101 "French" 100, Course 201 "English" 200]

-- _join teachers courses teacherId teacher ==
--    [(Teacher {teacherId = 100, teacherName = Simone De Beauvior},Course {courseId = 101,
--      courseTitle = "French", teacher = 100}),(Teacher {teacherId = 200, teacherName =
--      Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher= 200})]
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard (prop1 (fst dpairs) == prop2 (snd dpairs))
  return dpairs

-- cont. p. 419

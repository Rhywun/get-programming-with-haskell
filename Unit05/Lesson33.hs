module Lesson33 where

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
-- SELECT and WHERE
--
--
-- _select (firstName . studentName) students
--   == ["Audre","Leslie","Judith","Guy","Jean","Julia"]
-- _select' (\x -> (studentName x, gradeLevel x)) students
--   == [(Audre Lorde,Senior),(Leslie Silko,Junior),(Judith Butler,Freshman),
--       (Guy Debord,Senior),(Jean Baudrillard,Sophmore),(Julia Kristeva,Junior)]
_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
  val <- vals
  return (prop val)

_select' prop vals = prop <$> vals

-- cont. p. 416

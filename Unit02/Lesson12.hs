module Lesson12 where

-- Consider this

-- Is there a better way to express this?
anAlbum :: (String, String, Int, [String])
anAlbum = ("New Order", "Movement", 1981, ["Dreams Never End", "Truth", "Senses", "etc"])

--
-- Using type synonyms
--

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName = fst

lastName :: PatientName -> LastName
lastName = snd

-- QC1

{-
patientInfo' ("John", "Doe") 42 200 -- "Doe, John (42yrs. 200in.)"
-}
patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' (fname, lname) age height = name ++ " " ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

--
-- Creating new types
--

data Sex = Male | Female deriving (Show)

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg deriving (Show)

data ABOType = A | B | AB | O deriving (Show)

data BloodType = BloodType ABOType RhType deriving (Show)

-- Can the first blood type donate to the second?
canDonateTo' :: BloodType -> BloodType -> Bool
canDonateTo' (BloodType O _) _                = True
canDonateTo' _               (BloodType AB _) = True
canDonateTo' (BloodType A _) (BloodType A  _) = True
canDonateTo' (BloodType B _) (BloodType B  _) = True
canDonateTo' _               _                = False

type MiddleName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName

-- Jumping ahead...
instance Show Name where
  show (Name f l)             = f ++ " " ++ l
  show (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 :: Name
name1 = Name "Jerome" "Salinger"

name2 :: Name
name2 = NameWithMiddle "Jerome" "David" "Salinger"

--
-- Using record syntax
--

type Weight = Int

data PatientV1 =
  PatientV1 Name
            Sex
            Age -- in years
            Height -- in inches
            Weight -- in pounds
            BloodType
  deriving (Show)

johnDoe :: PatientV1
johnDoe = PatientV1 (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- QC2

janeSmith :: PatientV1
janeSmith =
  PatientV1 (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 55 130 (BloodType O Neg)

-- Let's use a record type instead

data Patient = Patient
  { patientName      :: Name
  , patientSex       :: Sex
  , patientAge       :: Age -- in years
  , patientHeight    :: Height -- in inches
  , patientWeight    :: Weight -- in pounds
  , patientBloodType :: BloodType
  }

jackieSmith :: Patient
jackieSmith = Patient
  { patientName      = Name "Jackie" "Smith"
  , patientAge       = 43
  , patientSex       = Female
  , patientHeight    = 62
  , patientWeight    = 115
  , patientBloodType = BloodType O Neg
  }

-- QC3

qc3 :: Name
qc3 = patientName jackieSmith

-- Record update:
jackieSmithUpdated :: Patient
jackieSmithUpdated = jackieSmith { patientAge = 44 }

-- Q1

-- E.g. canDonateTo jackieSmith jackieSmithUpdated == True
canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = canDonateTo' (patientBloodType p1) (patientBloodType p2)

-- Q2

-- E.g. putStrLn $ patientSummary jackieSmith -->
-- **************
-- Patient Name: Jackie Smith
-- Sex: Female
-- Age: 43
-- Height: 62
-- Weight: 115
-- Blood Type: BloodType O Neg
-- **************
patientSummary :: Patient -> String
patientSummary p =
  "**************"
    ++ "\nPatient Name: "
    ++ show (patientName p)
    ++ "\nSex: "
    ++ show (patientSex p)
    ++ "\nAge: "
    ++ show (patientAge p)
    ++ "\nHeight: "
    ++ show (patientHeight p)
    ++ "\nWeight: "
    ++ show (patientWeight p)
    ++ "\nBlood Type: "
    ++ show (patientBloodType p)
    ++ "\n**************"

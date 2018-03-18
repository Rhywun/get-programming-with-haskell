module Lesson12 where

--
-- Using type synonyms
--
--
type FirstName = String

type LastName = String

type Age = Int

type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName patient = fst patient

-- QC1201
-- E.g. patientInfo' ("John", "Doe") 42 200 == "Doe, John (42yrs. 200in.)"
patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' (fname, lname) age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

--
--
-- Creating new types
--
--
data Sex
  = Male
  | Female
  deriving (Show)

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType
  = Pos
  | Neg
  deriving (Show)

data ABOType
  = A
  | B
  | AB
  | O
  deriving (Show)

data BloodType =
  BloodType ABOType
            RhType
  deriving (Show)

-- Can the first blood type donate to the second?
canDonateTo' :: BloodType -> BloodType -> Bool
canDonateTo' (BloodType O _) _               = True
canDonateTo' _ (BloodType AB _)              = True
canDonateTo' (BloodType A _) (BloodType A _) = True
canDonateTo' (BloodType B _) (BloodType B _) = True
canDonateTo' _ _                             = False

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

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

--
--
-- Using record syntax
--
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

johnDoe = PatientV1 (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- QC1202
janeSmith =
  PatientV1
    (NameWithMiddle "Jane" "Elizabeth" "Smith")
    Female
    25
    55
    130
    (BloodType O Neg)

-- Let's use a record type instead
--
data Patient = Patient
  { name      :: Name
  , sex       :: Sex
  , age       :: Age -- in years
  , height    :: Height -- in inches
  , weight    :: Weight -- in pounds
  , bloodType :: BloodType
  }

jackieSmith :: Patient
jackieSmith =
  Patient
  { name = Name "Jackie" "Smith"
  , age = 43
  , sex = Female
  , height = 62
  , weight = 115
  , bloodType = BloodType O Neg
  }

-- QC1203
qc1203 = name jackieSmith

-- Record update:
jackieSmithUpdated = jackieSmith {age = 44}

-- Q1201
-- E.g. canDonateTo jackieSmith jackieSmithUpdated == True
canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = canDonateTo' (bloodType p1) (bloodType p2)

-- Q1202
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
  "**************" ++
  "\nPatient Name: " ++
  show (name p) ++
  "\nSex: " ++
  show (sex p) ++
  "\nAge: " ++
  show (age p) ++
  "\nHeight: " ++
  show (height p) ++
  "\nWeight: " ++
  show (weight p) ++
  "\nBlood Type: " ++ show (bloodType p) ++ "\n**************"

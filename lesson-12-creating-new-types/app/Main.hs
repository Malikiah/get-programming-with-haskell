module Main where

import Lib
-- Creating Type Synonyms
type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type Weight = Int
type MiddleName = String
data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName
-- Creating custom type signatures with type synonyms.
type PatientName = (FirstName,LastName)

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

-- Creating your own data types.
-- Type constructor = Sex && data contructors = Male | Female ; Male or Female
-- data constructors create a concrete instance of the type.
data Sex = Male | Female

-- data constructor record syntax for getting data without creating getters
data Patient = Patient { name :: Name
                        , sex :: Sex
                        , age :: Age
                        , height :: Height
                        , weight :: Weight
                        , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 63 89 333 (BloodType AB Pos)

-- QC 12.2
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 33 54 120 (BloodType O Pos)

-- setting data using record syntax
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , sex = Female
                      , age = 23
                      , height = 53
                      , weight = 287
                      , bloodType = BloodType A Neg }

jackieSmithUpdate = jackieSmith { age = 44 }

-- QC 12.1
patientInfo :: PatientName -> Age -> Height -> String
-- Potentially reducing overhead by only having one variable for the patientsname.
patientInfo (fname,lname) age height = name ++ " " ++ ageHeight
    -- Then calling that custom tuple type signature
    where name = lname ++ "," ++ fname
          ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ "in.)"

-- QC 12.3
-- GHCI> showName (name jackieSmith)
-- "Jackie Smith"

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType= A | B | AB | O
-- The Type and Data Constructor have the same name in this scenario because it makes sense to have the same name not because they have to.
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRH :: RhType -> String
showRH Pos = "+"
showRH Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRH rh
showABOType (BloodType abo rh ) = abo

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

-- Q 12.1

canPatientDonateTo patientA patientB = canDonateTo (bloodType patientA) (bloodType patientB) 

-- Q 12.2

patientSummary patient =  "*******" ++ "\n" ++
                          "PatientName: " ++ showName (name patient) ++ "\n" ++
                          "Sex: " ++ show (sexInitial (sex patient)) ++ "\n" ++
                          "Age: " ++ show (age patient) ++ "\n" ++
                          "Height: " ++ show (height patient) ++ "\n" ++
                          "Weight: " ++ show (weight patient) ++ "\n" ++
                          "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
                          "*******"


showPatientSummary :: Patient -> IO ()
showPatientSummary patient = putStrLn psp
    where psp = patientSummary patient

main :: IO ()
main = print "hello"

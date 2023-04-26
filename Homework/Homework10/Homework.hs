{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

class Container c where
  isEmpty :: c a -> Bool
  contains:: (Eq a) =>  c a -> a -> Bool
  replace :: c a -> b -> c b

data Box a =  EmptyBox | Box a

instance Container Box where
  -- isEmpty :: c a -> Bool
  isEmpty EmptyBox = True
  isEmpty _ = False
  
  -- contains:: c a -> b -> Bool
  contains EmptyBox _ = False   
  contains (Box a) b = a == b

  -- replace :: c a-> b -> c b
  replace EmptyBox a = Box a
  replace (Box a) b = Box b 



data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

instance Container (MailedBox t d) where
  -- isEmpty :: c a -> BoolMailedBox
  isEmpty (EmptyMailBox t d)  = True
  isEmpty _ = False 

  -- contains:: c a -> b -> Bool
  contains (EmptyMailBox t d) _  = False
  contains (MailBoxTo t d a) b = a == b 

  -- replace :: c a-> b -> c b
  replace (EmptyMailBox t d) b  = MailBoxTo t d b
  replace (MailBoxTo t d a) b = (MailBoxTo t d b) 




-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show,Eq,Ord)

data Experience = Programming | Managing | Leading deriving (Show,Eq,Ord)

type Address = String 

data Salary = USD Double | EUR Double deriving (Show,Eq,Ord)

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show,Eq)

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- -- Team memeber experience in years
newtype Exp = Exp Double

-- -- Team memeber data
type TeamMember = (String, Exp)

-- -- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- -- -- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0














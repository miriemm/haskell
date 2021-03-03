-----------------------
-- Miriem Omer
-- 21.12.2020
-----------------------

module Main where

import Data.Char (isLower,isUpper,isDigit)
import Data.Maybe

-- Exercise 5.2.1

-- list with all characters : digits, lowercase letters and uppercase letters
list = ['0'.. '9'] ++ ['a'..'z'] ++ ['A'..'Z']

-- the replicateList function will create a list of length 8 (first argument)
-- and items having the value of the list created above (second argument)
replicateList :: [String]
replicateList = replicate 8 list

-- the password function will generate all 8 character passwords
-- the sequenceA applied on replicateList will generate the passwords
passwords :: [String]
passwords = sequenceA replicateList


data User = User {email :: Email, password :: Password} deriving (Show, Eq)

data Password = Password String deriving (Show, Eq)

data Email = Email {username :: String, domain:: String} deriving (Show, Eq)

-- Exercise 5.2.2

-- function to calculate the number of lower characters in a string
nrOfLower :: String -> Int
nrOfLower string = length $ filter (isLower) string

-- function to calculate the number of upper characters in a string
nrOfUpper :: String -> Int
nrOfUpper string = length $ filter (isUpper) string

-- function to calculate the number of digits in a string
nrOfDigits :: String -> Int
nrOfDigits string = length $ filter (isDigit) string

-- function that takes a string
-- and returns a valid password wrapped in Just
-- or Nothing if the string is invalid password
validatePassword :: String -> Maybe Password
validatePassword pass =
    -- mandatory conditions (for valid password):
    -- at least one lowercase character (nrOfLower pass >= 1)
    -- at least one uppercase character (nrOfUpper pass >= 1)
    -- at least one digit (nrOfDigits >= 1)
    -- the password should contain at least 8 characters (length pass >= 8)
   if (nrOfLower pass >= 1) && (nrOfUpper pass >= 1) && (nrOfDigits pass >= 1) && (length pass >= 8) then Just (Password pass)
   else Nothing -- invalid password

--Exercise 5.2.3

-- function that takes a String as argument
-- counts the number of character '@' in that string
countCh :: String -> Int
countCh string = length $ filter (== '@') string

-- split function
-- takes as argument the character (representing the delimiter)
-- and a string
-- returns a list of strings (what is before the delimiter and what is after)
split :: Char -> String -> [String]
split c s =
   case break (==c) s of
      (ls, "") -> [ls]
      (ls, x:rs) -> ls : split c rs

-- in our exercise, we split a string according to '@'
-- it will return a list with 2 elements:
-- first: the username
-- second: the domain


-- function to get what is before the delimiter
-- this will represent the username
isUsername :: String -> String
isUsername string =
       let
         isUsernameHelper :: [String] -> String
         isUsernameHelper s =
          case s of
          -- if the list if empty, we return empty string
          [] -> ""
          --if it is not empty, we get the first element from the list of strings
          -- (what is before the delimiter)
          x:xs -> x
       in
       -- I've used the split function created above
       -- the delimiter is character '@'
       isUsernameHelper (split '@' string)

-- function to get what is after the delimiter
-- this will represent the domain
isDomain :: String -> String
isDomain string =
   let
     isDomainHelper :: [String] -> String
     isDomainHelper s =
       case s of
       -- if the list is empty, we return an empty string
        [] -> ""
        -- if it is not empty, we get the second element from the list of strings
        -- (what is after the delimiter)
        x:xs -> head xs
      in
      -- split is done according to '@'
     isDomainHelper (split '@' string)


-- function to check the domain
checkDomain :: String -> Bool
checkDomain string =
   let
     checkDomainHelper :: [String] -> Bool
     checkDomainHelper s =
       -- if the first element of the list (the hostname) is non-empty
       -- and the last element is the "com"
       -- then, the domain is correct
       if  (take 1 s) /= [""] && last s == "com" then True
       -- else, it is not
       else False
    in
       -- I've used the split function
       -- the delimiter in this case is '.'
       -- and second argument is the domain
     checkDomainHelper (split '.' $ isDomain string)


-- function that takes a string
-- and returns a valid email address wrapped in Just if valid
-- or Nothing if invalid
validateEmail :: String -> Maybe Email
validateEmail email =
     -- mandatory conditions:
     -- the username is at least 3 characters long : length (isUsername email) >= 3
     -- contains exactly ony '@' : countCh email == 1
     -- and the conditions for domain : checkDomain
     if length (isUsername email) >= 3 && countCh email == 1 && checkDomain email then Just (Email {username = isUsername email, domain = isDomain email})
     else Nothing


-- Exercise 5.2.4

-- function that validates the user
-- I've used fromJust to extract the element out of the Just
validateUser :: String -> String -> Maybe User
validateUser email password =
        -- if the email is not valid or the password is not valid, it returns Nothing
       if validateEmail email == Nothing || validatePassword password == Nothing then Nothing
       -- otherwise, it returns the User with email and password
       else Just User{email = fromJust (validateEmail email), password = fromJust (validatePassword password)}


main :: IO ()
main = do
  putStrLn "Enter your email: "
  email <- getLine
  putStrLn "Enter your password: "
  password <- getLine
  let
    valid = validateUser email password
  case valid of
       Nothing -> putStrLn "INVALID"
       Just valid -> putStrLn "VALID"
-----------------------
-- Miriem Omer
-- 29.11.2020
-----------------------

import Data.List (sortBy,intercalate)
import Data.Ord (comparing, Down(..))
import Data.Monoid((<>))


-- strWords function: splits a string into a list of words
-- first, I need to find the first word
-- I used takeWhile and dropWhile for this
-- dropWhile -> it the string starts with a delimiter character, I need to trim those first
-- after I have the first word, I also need the rest of the string starting after the first word
-- for this, I use splitAt
strWords :: String -> [String]
strWords "" = []
strWords str =
  let word = takeWhile (/=' ') $ dropWhile (==' ') str
      (_, rest) = splitAt (length word) str
  in word : strWords (dropWhile (==' ') rest)


-- function to check if a character is a vowel
-- I also added the CAPS LOCK vowels
isVowel :: Char -> Bool
isVowel char = elem char ['a','A','e','E','i','I','o','O','u','U']

-- helper function
-- converts a word to pig latin
piglatinizeHelper :: String -> String
piglatinizeHelper word =  -- if the first letter is a vowel, it adds "hay" at the end
                          if (isVowel $ head word) then word ++ "-" ++ "hay"
                          -- if it is a consonant, the first letter is moved to the end
                          -- and we add "ay"
                          else (tail word) ++ "-" ++ [(head word)] ++ "ay"

-- function to convert a list of words to a list of piglatinized words
-- it will return a list of words converted to pig latin
piglatinizeList :: [String] -> [String]
piglatinizeList list =
       case list of
       -- if the list is empty, it returns an empty list
       [] -> []
       -- if it is not empty, we use the function created above (piglatinizeHelper)
       -- converts the first element of the list into pig latin
       -- and recall the 'piglatinizeList' function for the rest of the list
       x:xs -> piglatinizeHelper x: piglatinizeList xs

-- and the final function
-- converts a string to pig latin
-- we need to convert each word of the string
piglatinize :: String -> String
piglatinize "" = ""
piglatinize string =
      let
       -- if the list is empty, returns empty list
       function list = if list == [] then ""
       -- if it is not empty, we concatenate the transformed words(by placing spaces between them)
       -- into a string
                        else intercalate " " list

       -- strWords string: breaks my string into a list of words
       -- piglatinizeList $ strWords string: converts to pig latin every word of this list
       in function $ (piglatinizeList $ strWords string)


-- iter function
iter :: (a -> a) -> a -> [a]
iter f x = x : map f (iter f x)


-- function that finds 2 successive and equal elements from a list
-- stopping condition: when 2 successive numbers are equal
identical (ac:an:as)
   | ac == an = an -- if the nrs are equal, we stop
   | otherwise = identical (an:as) -- if they are not equal, we continue


-- apprPi function: used to approximate minus pi
-- using iter and nextPi
apprPi :: Double
apprPi =
  let
    -- function to generate the next approximation of pi
    nextPi :: Double -> Double
    -- formula:
    nextPi xn = xn + (2 * cos (xn / 2)) / (2 * sin (xn / 2) - 1)

  in
    -- using identical, I go through the list of nextPi (returned by iter, starting from x0=0)
    -- and stop when we find 2 equal elements
    identical (iter nextPi 0)


-- Implement:

-- the update function for option 1

update :: (Eq k) => (v -> v) -> v -> k -> [(k, v)] -> [(k, v)]
update _ _ _ _ = error "Implement this function"

-- OR

-- the uniques, countOccurrences and countWords functions for option 2

-- uniques function to obtain the unique elements from a list
uniques :: (Eq a) => [a] -> [a]
uniques [] = [] -- if the list is empty, it returns an empty list
uniques (x:xs) =  x : (uniques (filter (/= x ) xs)) -- I used filter and took the elements that are not equal to x

-- count occurrences function
-- counts how many times a given element occurs in a list
countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences x list = (length . filter (==x)) list  -- first, I filter the list and then calculate the length of the resulting list

-- function to remove an element from a list
-- I will use this below, to remove an element after counting its number of occurrences
remove element list = filter (\e -> e/=element) list


-- helper function, I will use it in the countWords function
-- takes as argument a list of strings
-- returns a list of tuples
countHelper :: [String] -> [(String, Int)]
countHelper list =
      case list of
        [] -> [] -- if the list is empty, it returns an empty list
        x:xs ->  -- I zipped the list of unique elements with their number of occurrences
                 -- after counting the number of occurrences for an element, I remove it from the rest of the list
                 zip (uniques $ list) [countOccurrences x list] ++ countHelper (remove x list)

-- countWords function
-- takes as argument a string and returns a list of tuples
countWords :: String -> [(String, Int)]
countWords string = countHelper $ strWords string -- strWords: breaks a string into a list of words. I used countHelper on this list


-- Implement topWords using the functions implemented above.

-- topWords function: returns the top 'n' words from the string 'str' by the nr of occurrences
-- the order is based on the Int (count of words) first
-- and then ties broken by the lexicographically ordering for words
topWords :: Int -> String -> [(String, Int)]
topWords n str =
        -- if the number n is greater than the length of the list of tuples
        -- obtained with countWords str
        -- then, we sort the list in the following way:
        -- I combined two cals to 'comparing'
        -- 'Down' (for reversing the order of the second element - the Integers)
        -- and first element in lexicographic order (for multiple words with the same nr of occurrences)
        if n > length (countWords str) then sortBy (comparing  (Down . snd) <> comparing fst) $ countWords str
        -- if the number n is smaller than the length
        -- then we take the first n elements from the list(also sorted by the same rule)
        else take n  (sortBy (comparing  (Down . snd) <> comparing fst) $ (countWords str))



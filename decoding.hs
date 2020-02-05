--; Anne Benedicte Abildgaard Ejsing aejsin16@student.aau.dk
module Decoding where
  --; This function is called from main and is the entrypoint for decoding.
  -- It takes an encoded string and a table mapping characters and their 
  -- encoding value. First the entry in the table that matches x characters
  -- from the beginning of the encoded string is found. A new list is made
  -- excluding those x characters. Then the value of the entry is appended
  -- to the decoded string and the rest of the string is decoded.
  decode :: String -> [(String, Char)] -> String
  decode [] table = []
  decode (input) table =  let
        entry = findentry input table
        rest = newlist input (fst entry) 
      in
        (snd entry) : decode rest table

  --; Finds the entry in the association list that matches x characters of the
  -- string that is being decoded. 
  findentry :: [Char] -> [(String, Char)] -> (String, Char)
  findentry input ((k,v):rest)
    | ismatch input k = (k,v)
    | otherwise = findentry input rest

  --; Asserts if the second input list is contained in the first from the beginning.
  ismatch :: [Char] -> [Char] -> Bool
  ismatch _ [] = True
  ismatch [] _ = False
  ismatch (x:xs) (y:ys) 
      | x == y = ismatch xs ys
      | otherwise = False

  --; Removes elements of one list from the other
  newlist :: [Char] -> [Char] -> [Char]
  newlist input [] = input
  newlist (input:rest) (remove:rest1) = newlist rest rest1

--; Anne Benedicte Abildgaard Ejsing aejsin16@student.aau.dk
module Encoding where
  --; Datatype representing the treestructure used for huffman coding.
  data MyTree =
    Node {weight:: Integer, left :: MyTree, right :: MyTree} | 
    Leaf {weight :: Integer, symbol :: Char} 
    deriving (Show)
  
  --; This function is called from main, and performs the preprocessing of 
  -- the encoding. That is; computing the frequencies of each character,
  -- sorting these, building the tree, and creating a table from the 
  -- encoding tree. 
  preprocess :: String -> [(String, Char)]
  preprocess input = let
      freq = frequencies input
      sortedFreq = sortfrequencies freq
      leafNodes = map (\(c,i) -> Leaf i c) sortedFreq
      tree = buildtree leafNodes
    in
      maketable tree
  
  --; Encodes the string using the encoding table.
  encode :: String -> [(String, Char)] -> String
  encode [] table = []
  encode (input:rest) table =  case (find table input) of
      Nothing -> ("Error")
      Just result -> result ++ encode rest table
  
  --; Finds a char in the table and returns the corresponding encoding value
  find :: [(String, Char)] -> Char -> Maybe String
  find table s = case filter(\(x,t) -> t == s) table of
      [] -> Nothing
      [(x,_)] -> Just x 
  
  --; Builds the encoding tree. If there are more than two elements left in the
  -- list of trees a new node is made with the two trees as children. A new list
  -- of trees is then made, which excludes the two trees just used, but includes
  -- the tree just created.
  buildtree :: [MyTree] -> MyTree
  buildtree (first:second:rest) = buildtree newTreeList
    where
      newNode = Node (weight first + weight second) first second
      newTreeList = insertnode rest newNode
  buildtree (first:rest) = first


  --; Inserts a tree in a list of trees, such that it is inserted in respect to
  --; its weight.
  insertnode:: [MyTree] -> MyTree -> [MyTree]
  insertnode [] node = [node]
  insertnode (first:rest) node
    | weight first < weight node = [first] ++ insertnode rest node
    | otherwise = [node] ++ (first:rest)
  
  --; Determines the frequency of each character in the string, and returns 
  -- this in an association list of characters mapped to their frequency. The 
  -- first case of the gaurd checks if the current character is already present 
  -- in assoc, if it increments the counter for that character. If it is not
  -- present it creates an entry in assoc with that character and the count of
  -- one.
  frequencies :: String -> [(Char,Integer)]
  frequencies [] = []
  frequencies (first:rest) 
      | elementof first (map fst assoc) = map(\(sym,count) -> if sym == first  then (sym, count+1)  else (sym, count)) assoc
      | otherwise = assoc ++ [(first,1)]
    where
      assoc = frequencies rest
  
  --; Sorts the frequency association list by called foldl with the accumulation-
  -- function being insertfrequency, the start value being [] and the input being
  -- the association list.
  sortfrequencies :: [(Char,Integer)] -> [(Char,Integer)]
  sortfrequencies assoc = foldl insertfrequency [] assoc
  
  --; Inserts a tuple into the association list in a sorted order, such that
  -- the character with the lowest frequency is first.
  insertfrequency:: [(Char,Integer)] -> (Char,Integer) -> [(Char,Integer)]
  insertfrequency [] elem = [elem]
  insertfrequency (first:rest) (symbol, counter) 
    | snd first < counter = [first] ++ insertfrequency rest (symbol,counter)
    | otherwise = [(symbol,counter)] ++ (first:rest)
  
  --; Returns a true if the given element is in the given list, else returns
  -- false. Uses foldr with the 'or' function to assert wether the elemnt is
  -- equal to any element in the list.
  elementof :: (Eq a) => a -> [a] -> Bool
  elementof element lst = foldr (\ x -> (||)  (x == element)) False lst
  
  --; Creates the table representing the tree of the string to encode. This is
  -- done by appending 0 to the path if we go left and 1 if we go right. When 
  -- reaching a leaf node [([], symbol)] is returned.
  maketable :: MyTree -> [(String, Char)]
  maketable (Leaf weight symbol) = [([], symbol)]
  maketable (Node weight left right) = left1 ++ right1
    where
      left1 = map(\(path, symbol) -> ("0" ++ path, symbol)) (maketable left)
      right1 = map(\(path, symbol) -> ("1" ++ path, symbol)) (maketable right)

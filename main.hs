--; Anne Benedicte Abildgaard Ejsing aejsin16@student.aau.dk
import Encoding
import Decoding

--; Entrypoint of program. This function encodes an 
-- input from the file "unencoded.txt", and it then proceedes
-- to decode it again using the table created when encoding.
main :: IO ()
main = do
  input <- readFile "unencoded.txt"
  let table = Encoding.preprocess input
  let encoded = Encoding.encode input table
  let decoded = Decoding.decode encoded table
  print("input: ", input)
  print ("table:", table)
  print ("encoded:", encoded)
  print ("decoded:", decoded)
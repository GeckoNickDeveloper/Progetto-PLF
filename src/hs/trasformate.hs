import Data.List
import Data.Complex


main :: IO()
main = do
  let val_dct = dct [1.0, 3.7, 10.9, -2.6, 0, 0, 0.5, 14, 13.9]
  let val_dft = dft [(7.0 :+ 8.0), (1.4 :+ 0.0), (0.0 :+ (-3.0)), (0.0 :+ 0.0)]

  print (val_dct)
  print (idct val_dct)

  {-print (stringify_complex_list (val_dft))
  print (stringify_complex_list (idft val_dft))-}

  






{- DCT -}
dct :: [Double] -> [Double]
dct [] = []
dct xs = generate_dct xs (length xs) 0

generate_dct :: [Double] -> Int -> Int -> [Double]
generate_dct [] _ _ = []
generate_dct xs size k
  | k == size = []
  | otherwise = (sum_terms_dct xs size k 0) : (generate_dct xs size (k + 1))

sum_terms_dct :: [Double] -> Int -> Int -> Int -> Double
sum_terms_dct [] _ _ _ = 0.0
sum_terms_dct (x:xs) size k n =
  2 * x * cos (pi *  fromIntegral ((2 * n + 1) * k) / fromIntegral (2 * size)) + sum_terms_dct xs size k (n + 1)



{- IDCT -}
idct :: [Double] -> [Double]
idct [] = []
idct xs = generate_idct xs (length xs) 0

generate_idct :: [Double] -> Int -> Int -> [Double]
generate_idct [] _ _ = []
generate_idct (x:xs) size n
  | n == size = []
  | otherwise = ((x + sum_terms_idct xs size n 1) / fromIntegral (2 * size)) : (generate_idct (x:xs) size (n + 1))

sum_terms_idct :: [Double] -> Int -> Int -> Int -> Double
sum_terms_idct [] _ _ _ = 0.0
sum_terms_idct (x:xs) size n k =
  2 * x * cos (pi *  fromIntegral ((2 * n + 1) * k) / fromIntegral (2 * size)) + sum_terms_idct xs size n (k + 1)








{- DFT -}
dft :: [Complex Double] -> [Complex Double]
dft [] = []
dft xs = generate_dft xs (length xs) 0


generate_dft :: [Complex Double] -> Int -> Int -> [Complex Double]
generate_dft [] _ _ = []
generate_dft xs size k
  | k == size = []
  | otherwise = (sum_terms_dft xs size k 0) : (generate_dft xs size (k + 1))


sum_terms_dft :: [Complex Double] -> Int -> Int -> Int -> Complex Double
sum_terms_dft [] _ _ _ = 0.0
sum_terms_dft (x:xs) size k n =
  let
    theta = - ((2.0 * pi) / (fromIntegral size)) * (fromIntegral n) * (fromIntegral k)
  in
    (x * (cos theta :+ sin theta)) + sum_terms_dft xs size k (n + 1)









{- IDFT -}
idft :: [Complex Double] -> [Complex Double]
idft [] = []
idft xs = generate_idft xs (length xs) 0


generate_idft :: [Complex Double] -> Int -> Int -> [Complex Double]
generate_idft [] _ _ = []
generate_idft xs size n
  | n == size = []
  | otherwise = (sum_terms_idft xs size n 0 / (fromIntegral size)) : (generate_idft xs size (n + 1))


sum_terms_idft :: [Complex Double] -> Int -> Int -> Int -> Complex Double
sum_terms_idft [] _ _ _ = 0.0
sum_terms_idft (x:xs) size n k =
  let
    theta = ((2.0 * pi) / (fromIntegral size)) * (fromIntegral n) * (fromIntegral k)
  in
    (x * (cos theta :+ sin theta)) + sum_terms_idft xs size n (k + 1)











stringify_complex_list :: [Complex Double] -> [String]
stringify_complex_list [] = []
stringify_complex_list (x:xs) = (show x) : (stringify_complex_list xs)
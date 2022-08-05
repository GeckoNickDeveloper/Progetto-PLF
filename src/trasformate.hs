import Data.List
{- import Data.Complex -}


main :: IO()
main = do
	let val = dct [1.0, 3.7, 10.9, -2.6]
	print (val)

	print (idct val)








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


























{-
idct :: [Double] -> [Double]
idct [] = []
-}

{-
dft :: [Complex] -> [Complex]
dft [] = []

idft :: [Complex] -> [Complex]
idft [] = []
-}
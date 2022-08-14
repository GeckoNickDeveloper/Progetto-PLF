{- IMPORTAZIONE DELLE LIBRERIE -}
{- Libreria necessaria per utilizzare la funzione 'length' -}
import Data.List
{- Libreria utilizzata per la gestione dei numeri complessi -}
import Data.Complex
{- Libreria necessaria per:
    - utilizzare il tipo 'Either';
    - utilizzare la funzione 'readEither' -}
import Text.Read



{- MAIN -}
{- Il programma accetta in input una lista di numeri reali, calcolandone e stampandone i risultati:
     - la trasformata discreta del coseno di tipo 2 (DCT-II);
     - relativa trasformata inversa (IDCT-II).
   
   Viene poi richiesta in input una lista di numeri complessi, calcolandone e stampandone i risultati:
     - la trasformata discreta di Fourier (DFT);
     - relativa trasformata inversa. -}
main :: IO()
main = do
  realList <- acquire_real_list
  let val_dct = dct realList
  
  putStrLn "DCT:"
  putStrLn $ show val_dct

  putStrLn "IDCT:"
  putStrLn $ show (idct val_dct)

  putStrLn "\n\n"

  complexList <- acquire_complex_list
  
  let val_dft = dft complexList
  
  putStrLn "DFT:"
  putStrLn $ show val_dft

  putStrLn "IDFT:"
  print (stringify_complex_list (idft val_dft))



{- DCT -}
{- Funzione per il calcolo della DCT (versione semplificata) -}
dct :: [Double] -> [Double]
dct [] = []
dct xs = generate_dct xs (length xs) 0

{- Genera la trasformata -}
generate_dct :: [Double] -> Int -> Int -> [Double]
generate_dct [] _ _ = []
generate_dct xs size k
  | k == size = []
  | otherwise = (sum_terms_dct xs size k 0) : (generate_dct xs size (k + 1))

{- Effettua il calcolo del k-esimo elemento della trasformata -}
sum_terms_dct :: [Double] -> Int -> Int -> Int -> Double
sum_terms_dct [] _ _ _ = 0.0
sum_terms_dct (x:xs) size k n =
  2 * x * cos (pi *  fromIntegral ((2 * n + 1) * k) / fromIntegral (2 * size)) + sum_terms_dct xs size k (n + 1)



{- IDCT -}
{- Funzione per il calcolo della IDCT (versione semplificata) -}
idct :: [Double] -> [Double]
idct [] = []
idct xs = generate_idct xs (length xs) 0

{- Genera l'anti-trasformata (serie originale) -}
generate_idct :: [Double] -> Int -> Int -> [Double]
generate_idct [] _ _ = []
generate_idct (x:xs) size n
  | n == size = []
  | otherwise = ((x + sum_terms_idct xs size n 1) / fromIntegral (2 * size)) : (generate_idct (x:xs) size (n + 1))

{- Effettua il calcolo della sommatoria per k-esimo elemento dell'anti-trasformata -}
sum_terms_idct :: [Double] -> Int -> Int -> Int -> Double
sum_terms_idct [] _ _ _ = 0.0
sum_terms_idct (x:xs) size n k =
  2 * x * cos (pi *  fromIntegral ((2 * n + 1) * k) / fromIntegral (2 * size)) + sum_terms_idct xs size n (k + 1)



{- DFT -}
{- Funzione per il calcolo della DFT (versione semplificata) -}
dft :: [Complex Double] -> [Complex Double]
dft [] = []
dft xs = generate_dft xs (length xs) 0

{- Genera la trasformata -}
generate_dft :: [Complex Double] -> Int -> Int -> [Complex Double]
generate_dft [] _ _ = []
generate_dft xs size k
  | k == size = []
  | otherwise = (sum_terms_dft xs size k 0) : (generate_dft xs size (k + 1))

{- Effettua il calcolo del k-esimo elemento della trasformata -}
sum_terms_dft :: [Complex Double] -> Int -> Int -> Int -> Complex Double
sum_terms_dft [] _ _ _ = 0.0
sum_terms_dft (x:xs) size k n =
  let
    theta = - ((2.0 * pi) / (fromIntegral size)) * (fromIntegral n) * (fromIntegral k)
  in
    (x * (cos theta :+ sin theta)) + sum_terms_dft xs size k (n + 1)



{- IDFT -}
{- Funzione per il calcolo della IDFT (versione semplificata) -}
idft :: [Complex Double] -> [Complex Double]
idft [] = []
idft xs = generate_idft xs (length xs) 0

{- Genera l'anti-trasformata (serie originale) -}
generate_idft :: [Complex Double] -> Int -> Int -> [Complex Double]
generate_idft [] _ _ = []
generate_idft xs size n
  | n == size = []
  | otherwise = (sum_terms_idft xs size n 0 / (fromIntegral size)) : (generate_idft xs size (n + 1))

{- Calcolo della sommatoria per il k-esimo elemento -}
sum_terms_idft :: [Complex Double] -> Int -> Int -> Int -> Complex Double
sum_terms_idft [] _ _ _ = 0.0
sum_terms_idft (x:xs) size n k =
  let
    theta = ((2.0 * pi) / (fromIntegral size)) * (fromIntegral n) * (fromIntegral k)
  in
    (x * (cos theta :+ sin theta)) + sum_terms_idft xs size n (k + 1)



{- FUNZIONI AUSILIARIE -}
{- Converte una lista di numeri complessi in lista di stringhe -}
stringify_complex_list :: [Complex Double] -> [String]
stringify_complex_list [] = []
stringify_complex_list (x:xs) = (show x) : (stringify_complex_list xs)


{- Acquisisce una lista di numeri reali da tastiera.
   La funzione non termina fino a quando non verrà inserita una lista valida. -}
acquire_real_list :: IO [Double]
acquire_real_list = do
  putStrLn "Inserisci una lista di numeri reali"
  line <- getLine
  
  case readEither line :: Either String [Double] of
    Left err -> do
      putStrLn "Errore. \n"
      acquire_real_list
    Right value -> return (value)


{- Acquisisce una lista di numeri complessi da tastiera.
   La funzione non termina fino a quando non verrà inserita una lista valida. -}
acquire_complex_list :: IO [Complex Double]
acquire_complex_list = do
  putStrLn "Inserisci una lista di numeri complessi"
  line <- getLine
  
  case readEither line :: Either String [Complex Double] of
    Left err -> do
      putStrLn "Errore. \n"
      acquire_complex_list
    Right value -> return (value)
-- Prática 04 de Haskell
-- Nome: 

faixaIdoso :: Int -> String
faixaIdoso num
  | 60 <= num && num <= 64 = "IDO64"
  | 65 <= num && num <= 69 = "IDO69"
  | 70 <= num && num <= 74 = "IDO74"
  | 75 <= num && num <= 79 = "IDO79"
  | 80 <= num              = "IDO80"
  | otherwise              = "ND"


classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos list = [(fst tuple, snd tuple, faixaIdoso (snd tuple)) | tuple <- list]


classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' list = map (\tuple -> (fst tuple, snd tuple, faixaIdoso (snd tuple))) list


strColor :: (Int,Int,Int) -> String
strColor (r, g, b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b  ++ ")"


genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs num (cx,cy) raio = [(x,cy,raio) | x <- [cx, cx+12..cx+(12*num)]]


genReds :: Int -> [(Int,Int,Int)]
genReds num = [(x,0,0) | x <- [80, 90..(10*num + 70)]]
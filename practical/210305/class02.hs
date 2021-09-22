module Class02 where

import Data.Complex

-- Complex Float
-- 2 :+ (-1) represents 2-i  

-- function from Problem Set 1
-- Transpose 
transp :: [[Complex Float]] -> [[Complex Float]]
transp [] = []
transp ([] : _ ) = []
transp row = ((map head row) : transp (map tail row))

-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- (transp n)] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n] 

--------------------------------------------------------------------
--1
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

sh0 :: [[Complex Float]]  
sh0 = undefined

sh1 :: [[Complex Float]]
sh1 = undefined 
--2 a)
--00
s00 ::[[Complex Float]]
s00 = undefined 
--11
s11 :: [[Complex Float]]
s11 = undefined

--2 b)
s010 :: [[Complex Float]]
s010 = undefined

--3 a)
-- Hint use function conjugate
-- >x = 2 :+ (-1)
-- >conjugate x
-- 2:+ 1
norm :: [[Complex Float]] -> Complex Float
norm v = undefined

--3 b)
normalise :: [[Complex Float]] -> [[Complex Float]]
normalise v = undefined

--4 a)
--hint: recall cis 
--cis x = cos x + i sin x = e^{ix}
u3 :: (Float, Float, Float) -> [[Complex Float]]
u3 (t, p, l) = undefined

--4 b)

au3 = undefined
--

bu3 = undefined
--

--5 a)
systa :: [[Complex Float]]
systa = undefined
--5 b)
systb ::[[Complex Float]]
systb = undefined
--6
cnot_ :: Int -> [[Complex Float]]
cnot_ n = undefined

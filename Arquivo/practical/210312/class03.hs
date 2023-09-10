module Class03 where

import Data.Complex
import System.Random
import Data.List

-- function from Problem Set 1
-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- (transpose n)] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n] 

-- functions from Problem Set 2
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

had ::[[Complex Float]]
had = [[h,h],[h,-h]]
   where 
      h = 1/sqrt(2)

idgate :: [[Complex Float]]
idgate = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

cnot :: [[Complex Float]]
cnot = [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]]

s00 :: [[Complex Float]]
s00 = tensor s0 s0

bell ::[[Complex Float]]
bell = gate(gate cnot(tensor had idgate)) s00
----------------------------------------------------------
--1a
amplitude_acc :: [[Complex Float]] -> [Float]
amplitude_acc v = undefined

--1b
meas_acc :: [Float] -> Float -> [Float]
meas_acc l = undefined

--1c
state_to_char :: [Float] -> [Char]
state_to_char l = undefined

--1d
meas ::[[Complex Float]] -> IO [Char]
meas v = undefined

--2a
shots :: [[Complex Float]] -> Int -> IO [[Char]]
shots v n = undefined

--2b
freqs ::[[Complex Float]] -> Int -> IO [([Char], Int)]
freqs v n = undefined

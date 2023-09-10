-- Problem Set 2

-- GHCi:
-- :cd <directory>
-- :l <filename>, or :r to reload

import Data.Complex
-- Complex Float representation : 2-i => 2 :+ (-1)


-- Functions

-- Transpose of a matrix

transpose :: [[Complex Float]] -> [[Complex Float]]
transpose ([]:_) = []
transpose x = (map head x : transpose(map tail x))


-- Matrix multiplication

multiply_acc :: [Complex Float] -> [Complex Float] -> Complex Float
multiply_acc (x:xs) (y:ys) = x*y + (multiply_acc xs ys)
multiply_acc _ [] = 0
multiply_acc [] _ = 0


gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate x y = [[ multiply_acc a b | b <- (transpose y)] | a <- x ]

-- Alternate naming for operator
p = gate


-- Tensor product

tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor x [] = x
tensor [] y = y
tensor x y = [[ a*b | a <- rx, b <- ry ] | rx <- x, ry <- y ]

-- Alternate naming for operator
t = tensor


-- Functions for printing matrices and states

disp_real :: [[Complex Float]] -> IO()
disp_real x = mapM_ print [map (realPart) xrow | xrow <- x]

disp_imag :: [[Complex Float]] -> IO()
disp_imag x = mapM_ print [map (imagPart) xrow | xrow <- x]

disp_round :: [[Complex Float]] -> IO()
disp_round x = mapM_ print [map (round . realPart) xrow | xrow <- x]


-- Single-qubit basis states

q0 :: [[Complex Float]]
q0 = [[1],[0]]

q1 :: [[Complex Float]]
q1 = [[0],[1]]


-- Basic quantum gates

-- Identity
iden :: [[Complex Float]]
iden = [[1,0],[0,1]]

-- Pauli X
px :: [[Complex Float]]
px = [[0,1],[1,0]]

-- Pauli Y
py :: [[Complex Float]]
py = [[0, 0:+(-1)],[0:+1, 0]]

-- Pauli Z
pz :: [[Complex Float]]
pz = [[1,0],[0,-1]]

-- Hadamard
had :: [[Complex Float]]
had = [[x,x],[x,-x]]
 where x = 1/sqrt(2)
 
-- CNOT
cnot :: [[Complex Float]]
cnot = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

-- Swap
swap :: [[Complex Float]]
swap = [[1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]




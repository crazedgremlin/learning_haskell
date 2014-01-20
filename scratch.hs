module Scratch (quicksort) where

{-
    Quicksort in Haskell.

    It is incredibly impressive to me that quicksort can be so naturally
    implemented when expressed in a functional language.

    Unfortunately, it loses some points for not being an in-place sort.
-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort smaller) ++ [p] ++ (quicksort bigger)
    where smaller = filter (<= p) xs
          bigger = filter (> p) xs 

main = print $ quicksort [1,2,5,3,1,56,3,2]

{-
        DIY Operators

Haskell allows the user to define infix operators by hand.

Operator names are built from 
    ! # $ % & * + . / < = > ? \ ^ | : - ~
-}

x ^^^ y = (x*y)^(x+y)


{-
        Practicing working with Int
-}
threeAvg :: Int -> Int -> Int -> Float
threeAvg x y z = fromIntegral(x+y+z) / 3.0

numGreaterThanAvg :: Int -> Int -> Int -> Int
numGreaterThanAvg x y z = length bigUns
                  where avg = threeAvg x y z                    -- Float
                        floats = map (fromIntegral) [x,y,z]     -- [Float]
                        bigUns = filter (> avg) floats

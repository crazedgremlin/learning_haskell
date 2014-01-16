import Data.List

-- 11) Modified run-length encoding
{-
    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Elem a = Single a | Multiple Int a
    deriving (Show)

-- function #10 copied-and-pasted --------------------------
pack [] = []
pack (a:b) = [same] ++ pack diff
    where (same,diff) = span (==a) (a:b)
runlength xs = map (\a -> (length a,head a)) (pack xs)
-------------------------------------------------------------
encodeModified list = parse (runlength list)
    where
        parse [] = []
        parse ((count,elem):tuples)
          | count == 1      =       (Single elem) : parse tuples
          | otherwise       =       (Multiple count elem) : parse tuples

{- Problem 2

P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

decodeModified [] = []
decodeModified ((Single elem):others) = [elem] ++ decodeModified others
decodeModified ((Multiple n elem):others) = [elem | _ <- [1..n]] ++ decodeModified others


{- Problem 3
Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

-}
{-
encodeDirect list = encode list
  where
    encode [] = []
-}


{-
14) Duplicate elements in list
-}

dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

{-
15) replicate elements n times
-}
repli [] n = []
repli (x:xs) n = [x | _<-[1..n]] ++ repli xs n

{-
16) Drop every nth element from list
-}
dropEvery list n = [list !! (i-1) | i<-[1..length list], i `mod` n /= 0]
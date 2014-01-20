import Data.List
import PartOne (runlength)

-- 11) Modified run-length encoding
{-
    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified list = parse (runlength list)
    where
        parse :: [(Int,a)] -> [ListItem a]
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
decodeModified :: [ListItem a] -> [a]
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
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

{-
15) replicate elements n times
-}
repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = [x | _<-[1..n]] ++ repli xs n

{-
16) Drop every nth element from list
-}
dropEvery :: [a] -> Int -> [a]
dropEvery list n = [list !! (i-1) | i<-[1..length list], i `mod` n /= 0]

{-
17) split without predefined predicates

Cheating answer:
split list n = (take n list, drop n list)

Let's just define take and drop by hand so we're not cheating.
-}
split :: [a] -> Int -> ([a],[a])
split list n = (take' n list, drop' n list)
      where
        -- take the first m elements from the list
        take' _ [] = []
        take' 0 _ = []
        take' m (x:xs) = [x] ++ take' (m-1) xs

        -- take the last m elements from the list
        drop' _ [] = []
        drop' 0 xs = xs
        drop' m (x:xs) = drop' (m-1) xs


{-
18) extract a slice from a list

slice [1,2,3,4,5,6,7] 3 6

-}
slice :: [a] -> Int -> Int -> [a]
slice list from to = take (to-from+1) (drop (from-1) list)

{-
19) Rotate a list N places to the left
-}
rotate :: [a] -> Int -> [a]
rotate list 0 = list
rotate (x:xs) n = rotate (xs ++ [x]) (n-1)

{-
20) Remove Kth element from list
-}
removeAt :: Int -> [a] -> (a,[a])
removeAt k list = (removed, newList)
         where (left,right) = splitAt k list
               newList = (init left) ++ right
               removed = last left
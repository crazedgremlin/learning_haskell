-- Haskell 99 Questions
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- 1) get last element from list 
myLast [a] = a
myLast list = myLast( tail list)

-- 2) get the second-to-last element
myButLast [a,_] = a
myButLast (_:t) = myButLast t 

-- 3) find kth element of a list
elementAt list ind = list !! (ind - 1)

-- 4) find the number of elements of a list
myLength [] = 0
myLength (_:t) = 1 + myLength t

-- 5) reverse a list
myReverse [] = []
myReverse (a:b) = myReverse(b) ++ [a] 

-- 6) find out whether a list is a palindrome
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (a:b) = (a == myLast b) && (isPalindrome $ init b)

-- 7) flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x 

-- 8) eleminate consecutive duplicates of list elements
compress [a] = [a]
compress (a:b)
    | a == head b    = compress b
    | otherwise      = [a] ++ compress b 

-- 9) pack consecutive duplicates into sublists
{-
    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
                 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}
pack [] = []
pack (a:b) = [same] ++ pack diff
    where (same,diff) = span (==a) (a:b)

-- 10) run-length encoding of a list
{-
    encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    Example usage of span
    Prelude> span (=="a") ["a","a","b","c"]
    (["a","a"],["b","c"])

-}
runlength [] = []
runlength (a:b) = [(length same, a)] ++ runlength diff
    where (same,diff) = span (==a) (a:b)

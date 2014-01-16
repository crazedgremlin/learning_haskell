
This is an example of a literate Haskell file.  In these files, comments are
entered normally and code must be separated from comments by a newline and a >
character.

> main = print $ [(x, test x) | x<-[1..100]]

So far, we have not yet given a definition of test. This is not a problem; we
will do it now!

The test function will determine if the given number is prime. Given n, it will
return False if any number between 2 and n-1 divides n evenly. Otherwise, we
can deduce that the number is in fact prime.

> test n
>     | any (==0) [n `mod` x | x<-[2..(n-1)]]   =   False
>     | otherwise                                 =   True

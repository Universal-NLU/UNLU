module Help where

import Data.List
import Data.Char
import qualified Data.Text as T

-- returns the "holes" in a (presumably sorted) array
holes :: (Enum a, Eq a) => [a] -> [a]
holes [] = []
holes ary = [(head ary)..(last ary)] \\ ary
  
gaps :: (Enum a, Eq a) => [a] -> Int
gaps a = countgaps (holes a) 0
  where countgaps [] c = c
        countgaps (x:[]) c = c + 1
        countgaps (x:y:ys) c | y == (succ x) = countgaps (y:ys) c
                             | otherwise     = countgaps (y:ys) (c+1)                  

formatHandle :: String -> String
formatHandle handleName =
  let filename = T.dropAround (\x -> x `elem` "\": }") (snd (T.breakOn (T.pack ": ") (T.pack handleName)))
  in T.unpack filename
                             
-- returns the continuous domains in a sorted array with no duplicates
contDomains :: (Eq a, Num a) => [a] -> [[a]]
contDomains ary = addUp ary [] [] 
    where addUp [] currentAry doneArys = (reverse ((reverse currentAry):doneArys))
          addUp (x:xs) [] [] = addUp xs [x] []
          addUp (x:xs) (c:currentAry) doneArys =
              if x == c + 1
              then addUp xs (x:c:currentAry) doneArys
              else addUp xs [x] ((reverse (c:currentAry)):doneArys)

tuplify :: Show a => [a] -> (a, a)
tuplify (x:y:[]) = (x,y)
tuplify ary = error ("Wrong number of arguments in " ++ show ary)

triplify :: [a] -> (a,a,a)
triplify (x:y:z:[]) = (x,y,z)

maximumWithDefaultZero :: (Num a, Ord a) => [a] -> a
maximumWithDefaultZero [] = 0
maximumWithDefaultZero x = maximum x

fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, x, _) = x

trd3 :: (a,b,c) -> c
trd3 (_, _, x) = x

returnNext :: Char -> Char
returnNext c = chr (ord c + 1)

sec :: [a] -> a
sec (x:y:z) = y

flatten ((a,b),c) = (a,b,c)

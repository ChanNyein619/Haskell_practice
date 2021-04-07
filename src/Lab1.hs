module Lab1
    ( sumOfEvenNum
    , sumOfEvenNum2
    , sumOfEvenNum3
    , sumOfEvenNum4
    ) where

-- Recursions
sumOfEvenNum :: Integral a => a -> a
sumOfEvenNum nTerms = count 0 1 0
  where
    count prev cur result
      | cur > nTerms = result
      | otherwise = count cur (prev + cur) $ if even cur then result + cur else result

sumOfEvenNum2 :: Integral t => t -> t
sumOfEvenNum2 nTerms = count 0 1
  where
    count prev cur
      | cur > nTerms = if even cur then cur else 0
      | otherwise = (if even cur then cur else 0) + count cur (prev + cur)

-- generation of data list using filter, takeWhile and infinity list
fibonacci :: [Integer]
fibonacci = count 0 1
  where
    count cur next = cur : count next (cur + next)

sumOfEvenNum3 :: Integer -> Integer
sumOfEvenNum3 nTerms = sum $ takeWhile (<= nTerms) $ filter even fibonacci

-- list comprehansion
sumOfEvenNum4 :: Integer -> Integer
sumOfEvenNum4 nTerms =
  sum $
    takeWhile (<= nTerms) $
      [x | let count cur next = cur : count next (cur + next), x <- count 0 1, even x]


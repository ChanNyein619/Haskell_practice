import Control.Monad
palin :: Integer
palin = maximum $ do 
    x <- [100..999]
    y <- [100..999]
    guard $ isPalindromic $ x * y
    return $ x * y
  where
    isPalindromic n = let s = show n
                      in s == reverse s

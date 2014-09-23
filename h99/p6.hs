{-# LANGUAGE RankNTypes #-}

butlast :: forall t. [t] -> [t]
butlast [] = error "empty"
butlast [_] = []
butlast (x : _ : []) = [x]
butlast (x : xs) = x : butlast xs

isPalindrome :: forall t. Eq t => [t] -> Bool
isPalindrome []              = True
isPalindrome (_ : [])        = True
isPalindrome (x : y : [])    = x == y
isPalindrome (x : _: z : []) = x == z
isPalindrome (x : xs)        = x == last xs && isPalindrome (butlast xs)

main :: IO ()
main = do
  let ls = [1,2,3,2,1] :: [Integer]
  let ls2 = [1,3,3,2,1] :: [Integer]
  print $ isPalindrome ls
  print $ isPalindrome ls2
  print $ isPalindrome ([1,1] :: [Integer])
  print $ isPalindrome ([2] :: [Integer])
  print $ isPalindrome ([] :: [Integer])

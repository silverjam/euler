{-# LANGUAGE RankNTypes #-}

revlist :: forall a. [a] -> [a]

revlist []       = []
revlist (x : []) = [x]
revlist (x : xs) = revlist xs ++ [x]

main :: IO ()
main = do
  print $ revlist ([1,2,3] :: [Int])
  print $ revlist "abc"

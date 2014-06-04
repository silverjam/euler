{-# LANGUAGE RankNTypes #-}

isPrime :: Integer -> IO Bool
isPrime x =
  isPrime' 2 x
    where 
      isPrime' d n | d == n = return $ True
                   | otherwise = do
                     isPrime'' <- isPrime' (d+1) n
                     if not $ (n `mod` d /= 0) && isPrime'' then
                       do { print n; return False }
                     else return True


divisors :: forall a. Integral a => a -> [a]
divisors =
  divisors' 2
    where
      divisors' d n | d == n = []
                    | otherwise = 
                      if n `rem` d == 0 then
                        d : divisors' (d+1) n
                      else
                        divisors' (d+1) n

--primeDivisors :: forall a. Integral a => [a] -> [a]
--primeDivisors = filter isPrime

main :: IO ()
main = do
  --print $ primeDivisors $ divisors (6102 :: Int)
  --print $ last $ primeDivisors $ divisors (600851475143 :: Integer)
  v <- return (500 :: Integer)
  p <- isPrime v
  print $ p


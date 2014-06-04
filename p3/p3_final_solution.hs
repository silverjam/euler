

firstmult' :: Int -> [(Int, Int)]
firstmult' x = dropWhile ( (/=0) . snd ) divrems
  where rems = map (x `rem`) [2..x]
        divrems = zip [2..x] rems

firstmult :: Int -> Int
firstmult x = fst $ head fm
  where fm = if not $ null fm' then fm' else [(x,x)]
        fm' = firstmult' x

gcd' :: Int -> Int -> Int
gcd' 1 1 = 1
gcd' 1 2 = 1
gcd' x y =
  if remainder == 0 then x'
  else gcd' remainder x'
  where remainder = y' `rem` x'
        x' = if x < y then x else y
        y' = if x < y then y else x

gpf :: Int -> Int
gpf x = let fm1 = firstmult x
            chunk = x `div` fm1
            fm2 = firstmult chunk
        in 
          if fm2 == chunk then chunk
          else gpf $ chunk `div` fm2

main :: IO ()
main = 
  print $ gpf 600851475143

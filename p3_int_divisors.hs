{-# LANGUAGE RankNTypes #-}

divisors :: forall a. Integral a => a -> [a]
divisors x = takeWhile (/=1) $ divnums x
  where rems x' = zipWith rem (repeat x') nums
        numdiv x' = (flip $ zipWith ($)) nums $ map (,) $ rems x'
        divs x' = filter ((0==) . fst) $ numdiv x'
        nums = [ x - i | i <- [1 .. x-1] ]
        divnums = map snd . divs

main :: IO ()
main =
  print $ divisors 13195

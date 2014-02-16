

main :: IO ()
main = do
  let filtfun x = (x `mod` 3 == 0) || (x `mod` 5 == 0)
  let ls = filter filtfun ([1..999] :: [Int])
  print ls
  let s = sum ls
  print s
  return ()

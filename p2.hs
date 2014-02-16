import Data.Array.IO

fib :: IOArray Int Int -> Int -> IO Int
fib _ 0 = return 0
fib _ 1 = return 1
fib _ 2 = return 2
fib memo x = do
  val <- readArray memo x
  if val /= (-1) then
    return val
  else do
    nval <- fib memo (x-2)
    nval2 <- fib memo (x-1)
    let s = nval + nval2
    writeArray memo x s
    return s

gLIMIT :: Int
gLIMIT = 4000000

sumfibs :: IOArray Int Int -> Int -> Int -> IO Int
sumfibs memo acc term = do
  val <- fib memo term
  if val < gLIMIT then
    if val `mod` 2 == 0 then
      sumfibs memo (acc+val) (term+1)
    else
      sumfibs memo acc (term+1)
  else
    return acc

main :: IO ()
main = do
  let upto = 1000
  memo <- newArray (1,upto) (-1) :: IO (IOArray Int Int)
  theSum <- sumfibs memo 0 0
  print theSum

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Printf
import Data.Array.IO
import Control.Monad

dispArray :: forall b (a :: * -> * -> *) i a1.
                (Num i, Show a1, Ix i, MArray a a1 IO) =>
                a i a1 -> IO b
dispArray arr = do
    (a,_) <- getBounds arr
    printf "["
    dispArray' arr a
    printf "]\n"
        where dispArray' arr' i = do
                (a,b) <- getBounds arr'
                unless (i < a || i > b) $
                  do v <- readArray arr' i
                     putStr $ show v ++ ", "
                     dispArray' arr' (i+1)

markMultiples :: IOArray Int Int -> Int -> Int -> IO (IOArray Int Int)
markMultiples arr factor upTo = do
  let fact = (*factor)
  _ <- mapM (\x -> writeArray arr x 1) [fact i | i <- [2..upTo], fact i <= upTo]
  return arr

findNextPrime :: IOArray Int Int -> Int -> IO (Maybe Int)
findNextPrime arr x = do
  (_, upper) <- getBounds arr
  if x > upper then
    return Nothing
  else do 
    val <- readArray arr x
    if val == 0 then
      return $ Just x
    else
      findNextPrime arr (x+1)

sieve :: Int -> IO (IOArray Int Int)
sieve n = do
  arr <- newArray (1,n) 0 :: IO (IOArray Int Int)
  sieve' arr 2
  where sieve' arr current = do
               arr' <- markMultiples arr current n
               next <- findNextPrime arr (current+1)
               case next of
                 Just pnext -> sieve' arr' pnext
                 _ -> return arr

toPrimeList :: IOArray Int Int -> IO [Int]
toPrimeList arr = do
  (_, upper) <- getBounds arr
  toPrimeList' upper 1 arr

toPrimeList' :: Int -> Int -> IOArray Int Int -> IO [Int]
toPrimeList' upper idx arr =
  if idx >= upper then
    return []
  else do
    val <- readArray arr idx
    if val == 0 then do
      pl <- toPrimeList' upper nidx arr
      return $ idx : pl
    else
      toPrimeList' upper nidx arr
    where nidx = idx+1

main :: IO ()
main = do
  arr <- sieve 600
  plist <- toPrimeList arr
  print plist
  return ()

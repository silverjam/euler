mylast = head . reverse

mylast' [] = error "empty list"
mylast' (x:[]) = x
mylast' (_:xs) = mylast' xs

l1 = [1,2,3,4]
l2 = ['a','b','c','d']

butlast [] = error "empty"
butlast [x] = error "not enough elements"
butlast (x : y : []) = x
butlast (_ : xs) = butlast xs

kth x ls = kth' 0 x ls
    where kth' c x' ls' = if c == x 
          then head ls' else kth' (c+1) x (tail ls')

main = do
  print $ mylast l1
  print $ mylast l2
  print $ mylast' l1
  print $ mylast' l2
  print $ butlast l1
  print $ butlast l2
  --print $ (butlast [1] :: Int)
  --print $ (butlast [] :: Int)
  print $ kth 0 l1
  print $ kth 1 l1
  print $ kth 2 l1

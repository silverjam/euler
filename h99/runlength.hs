#!/usr/bin/env runhaskell

data RLE = Single Char
         | Multiple Int Char
         deriving (Show)

endcodeModified [] = []

endcodeModified (x:[]) = [Single x]

endcodeModified (x:y:[])
    | x == y     = [Multiple 2 x]
    | otherwise  = [Single x, Single y]

endcodeModified (x:xs) =

    let enc = endcodeModified xs
        head' = head enc

    in case head' of

        (Single y)     -> if x == y then
                            Multiple 2 x : tail enc
                          else
                            Single x : enc

        (Multiple c y) -> if x == y then
                            Multiple (c+1) x : tail enc
                          else
                            Single x : enc

main = do
    print $ endcodeModified "exit"
    print $ endcodeModified "exxxxxitttttttiinngs"

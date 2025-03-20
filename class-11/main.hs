import Data.ByteString (isSuffixOf)

main :: IO ()

isEven :: Int -> Bool
isEven num = num `mod` 2 == 0

main = do
    let double num = 2 * num 
    let result = double 10
    print ( result )
    let square num = num * num 
    let result = square 10
    print ( result )
    

    -- let highOrderAdd prefix = \sufix -> prefix ++ sufix
    
    let highOrderAdd prefix sufix =  prefix ++ sufix

    let haskellPrefix = highOrderAdd "haskell"

    print ( haskellPrefix "Leo" )
    print ( haskellPrefix "Coding" )

    -- Hash
    let hash num =  (num * num) `mod` 10 
    print ( hash 121 )


    
    --- Bigger of 3
    let biggerOfThree a b c = let ab = if a > b then a else b in if ab > c then ab else c
    print ( biggerOfThree 3 10 6 )


    -- isEven
    
    print (isEven 10)
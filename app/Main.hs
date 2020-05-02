module Main where

-- import Program2

type Domain = Either String

main :: IO ()
main = do
    s <- getLine
    case (getSquareRoot s) of
        Left msg -> putStrLn msg
        Right d  -> print d

checkNotEmpty :: String -> Domain String
checkNotEmpty "" = Left "empty parameter"
checkNotEmpty s  = Right s

getDouble :: String -> Domain Double
getDouble s = case reads s of
    []        -> Left "not a number"
    ((d,_):_) -> Right d

checkPositive :: Double -> Domain Double
checkPositive d 
    | d < 0     = Left "negative number"
    | otherwise = Right d

getSquareRoot :: String -> Domain Double
getSquareRoot s = checkNotEmpty s 
                >>= getDouble
                >>= checkPositive
                >>= return . sqrt

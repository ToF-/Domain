module Program1 where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.List ( groupBy
                 , sort
                 )

program1 :: IO ()
program1 = do
    args <- getArgs
    content <- readFile (args !! 0)
    let totals = ( map sumGroup . groupBy (same transactionCategory) . sort . map read . lines ) content
    putStrLn $ ( unlines . map show ) totals

same :: Eq (b) => (a -> b) -> a -> a -> Bool
same f a b = f a == f b

data Transaction = Transaction { transactionCategory :: String
                               , transactionAmount   :: Double 
                               }
    deriving (Eq,Ord)

instance Read Transaction where
    readsPrec _ input = case splitOn "," input of
                          [c,a] -> case reads a of
                                     ((d,_):_) -> [(Transaction c d,"")]
                                     [] -> []
                          _ -> []
        

data Total = Total { totalCategory :: String
                   , totalAmount   :: Double
                   }
instance Show Total where
    show t = (totalCategory t) ++ " : " ++ show (totalAmount t)

sumGroup
    :: [Transaction]
    -> Total
sumGroup txs = Total 
    (transactionCategory (head txs)) 
    (sum (map transactionAmount txs))



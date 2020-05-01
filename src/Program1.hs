module Program1           ( program1 )
    where

import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )

data Transaction = Transaction { transactionCategory :: String
                               , transactionAmount   :: Double }
    deriving (Eq,Ord)

-- read transaction from a csv format line eg. "Books, 24.65"
instance Read Transaction where
    readsPrec _ s = 
        case splitOn "," s of
          [c,a] -> case reads a of
                     ((d,_):_) -> [(Transaction c d,"")]
                     _ -> []
          _ -> []

data Summary = Summary { summaryCategory :: String
                       , summaryAmount   :: Double }

-- show summary in a csv format line
instance Show Summary where
    show t = 
        (summaryCategory t) 
        ++ ", " 
        ++ show (summaryAmount t)

-- produce total amount per category
summarize :: [Transaction] -> [Summary] 
summarize = map summary 
          . groupBy ( (==)    `on` transactionCategory ) 
          . sortBy  ( compare `on` transactionCategory )
    where
    summary :: [Transaction] -> Summary
    summary txs = Summary (category txs) (total txs) 
        where
        category = transactionCategory . head
        total    = sum . map transactionAmount
    
-- reads the given transaction file, outputs its summary
program1 :: IO ()
program1 = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let transactions = map read $ lines content
    putStrLn $ unlines $ map show $ summarize transactions


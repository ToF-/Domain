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

instance Read Transaction where
    readsPrec _ s = 
        case splitOn "," s of
          [c,a] -> case reads a of
                     ((d,_):_) -> [(Transaction c d,"")]
                     _ -> []
          _ -> []

data SummaryLine = SummaryLine { summaryCategory :: String
                               , summaryAmount   :: Double }

instance Show SummaryLine where
    show t = 
        (summaryCategory t) 
        ++ ", " 
        ++ show (summaryAmount t)

summarize :: [Transaction] -> [SummaryLine] 
summarize = map summary 
          . groupBy ( (==)    `on` transactionCategory ) 
          . sortBy  ( compare `on` transactionCategory )
    where
    summary :: [Transaction] -> SummaryLine
    summary txs = SummaryLine (category txs) (total txs) 
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


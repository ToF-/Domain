
module Program2           ( program2 )
    where

import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Control.Exception  ( IOException
                          , catch )

type Message = String

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

-- read a transaction or fail
readTransaction :: String -> Either Message Transaction
readTransaction s = 
    case reads s of
      []        -> Left $ "incorrect csv format : " ++ s
      ((t,_):_) -> Right t

-- read all transactions or fail
readTransactions :: String -> Either Message [Transaction]
readTransactions = mapM readTransaction . lines

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
    
-- turn an IOException into a failure
handle :: IOException -> IO (Either Message String)
handle e = return $ Left (show e) 

-- reads the given transaction file, outputs its summary
program2 :: IO ()
program2 = do
    args    <- getArgs
    if null args then putStrLn "no file name given"
                 else do
                     content <- (readFile (args !! 0) >>= return . Right) `catch` handle
                     case content of
                       Left msg -> putStrLn msg
                       Right text -> do
                            let transactions = readTransactions text
                            case transactions of
                                Left msg -> putStrLn msg
                                Right txs -> putStrLn $ unlines $ map show $ summarize txs



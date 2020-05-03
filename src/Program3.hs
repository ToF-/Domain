module Program3           
    where

import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Control.Exception  ()
import Control.Monad.Trans.Except ( ExceptT (..)
                                  , runExceptT
                                  , throwE
                                  )

import Control.Monad.IO.Class     ( liftIO )

type Message = String

data Transaction = 
    Transaction { transactionCategory :: String
                , transactionAmount   :: Double }
    deriving (Eq,Ord,Show)

instance Read Transaction where
    readsPrec _ s = 
        case splitOn "," s of
          [c,a] -> case reads a of
                     ((d,_):_) -> [(Transaction c d,"")]
                     _ -> []
          _ -> []

readTransaction :: String -> ExceptT Message IO Transaction
readTransaction s = 
    case reads s of
      []        -> throwE ("incorrect csv format : " ++ s)
      ((t,_):_) -> return t

readTransactions :: String -> ExceptT Message IO [Transaction]
readTransactions = mapM readTransaction . lines

data Summary = 
    Summary { summaryCategory :: String
            , summaryAmount   :: Double }

instance Show Summary where
    show t = 
        (summaryCategory t) 
        ++ ", " 
        ++ show (summaryAmount t)

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
    
firstArg :: [String] -> ExceptT Message IO FilePath
firstArg []     = throwE "no file name given"
firstArg (fp:_) = return fp

getTransactions :: ExceptT Message IO [Transaction]
getTransactions = do
    args         <- liftIO getArgs
    filePath     <- firstArg args
    content      <- liftIO $ readFile filePath
    transactions <- readTransactions content
    return transactions
    
report :: Either Message [Summary] -> String
report (Left msg)   = "Error: " ++ msg
report (Right [])   = "No transactions to summarize" 
report (Right sums) = unlines $ map show sums

program3 :: IO ()
program3 = do 
    transactions <- runExceptT getTransactions
    putStrLn $ report (summarize <$> transactions)


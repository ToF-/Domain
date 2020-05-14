module Program3           
    where

import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Control.Exception  ( IOException
                          , catch )
import Control.Monad.Trans.Except ( ExceptT (..)
                                  , runExceptT
                                  , throwE
                                  )

import Control.Monad.Trans.Class     ( lift )

type Message = String

type Domain = ExceptT Message IO

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

readTransaction :: String -> Domain Transaction
readTransaction s = 
    case reads s of
      []        -> throwE ("incorrect csv format : " ++ s)
      ((t,_):_) -> return t

readTransactions :: String -> Domain [Transaction]
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
    
report :: Either Message [Summary] -> String
report (Left msg)   = "Error: " ++ msg
report (Right [])   = "No transactions to summarize" 
report (Right sums) = unlines $ map show sums

catchE :: IO a -> Domain a
catchE action = ExceptT (action `catchIO` handle)
    where
    catchIO :: IO a -> (IOException -> IO (Either Message a)) -> IO (Either Message a)
    a `catchIO` h = (Right <$> a) `catch` h

    handle :: IOException -> IO (Either Message a)
    handle = return . Left . show

getFileContent :: FilePath -> Domain String
getFileContent fp = catchE $ readFile fp

getFileNameArg :: Domain FilePath
getFileNameArg = do
    args <- lift getArgs
    if null args then throwE "Error: no file name given" 
                 else return (args !! 0)

checkNotEmpty :: [Transaction] -> Domain [Transaction]
checkNotEmpty []  = throwE "Error: no transactions"
checkNotEmpty txs = return txs

checkNotEmptyCategory :: Transaction -> Domain Transaction
checkNotEmptyCategory (Transaction "" _) = throwE "Error: empty category"
checkNotEmptyCategory tx                 = return tx

computation :: Domain [Summary]
computation = do 
    filePath     <- getFileNameArg 
    content      <- getFileContent filePath
    unchecked    <- readTransactions content
    notEmpty     <- checkNotEmpty unchecked
    transactions <- mapM checkNotEmptyCategory notEmpty
    return $ summarize transactions

program3 :: IO ()
program3 = do
    transactions <- runExceptT computation
    putStrLn $ report transactions


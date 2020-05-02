module Program3           ( program3 )
    where

import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Control.Exception  ( IOException
                          , catch )
import Control.Monad.Except ( ExceptT (..)
                            , liftIO
                            , runExceptT
                            , throwError 
                            )
type Message = String

type Domain = ExceptT Message IO

domain :: a -> Domain a
domain = ExceptT . pure . Right

getIO :: IO a -> Domain a
getIO action = ExceptT $ fmap Right action `catch` handle
    where
    handle 
        :: IOException 
        -> IO (Either Message a)
    handle = return . Left . show

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
      []        -> throwError ("incorrect csv format : " ++ s)
      ((t,_):_) -> domain t

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
    
firstArg :: [String] -> Domain FilePath
firstArg []     = throwError "no file name given"
firstArg (fp:_) = domain fp

getTransactions :: Domain [Transaction]
getTransactions = do
    args         <- liftIO getArgs
    filePath     <- firstArg args
    content      <- getIO $ readFile filePath
    transactions <- readTransactions content
    return transactions
    
report :: Either Message [Summary] -> String
report (Left msg)   = "Error: " ++ msg
report (Right [])   = "No transactions to summarize" 
report (Right sums) = unlines $ map show sums

program3 :: IO ()
program3 = do 
    transactions <- runExceptT getTransactions
    let result = summarize <$> transactions
    putStrLn $ report result


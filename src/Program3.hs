module Program3           
    where

import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Data.Char          ( isAlphaNum )
import Control.Exception  ( IOException
                          , catch )
import Control.Monad.Trans.Except ( ExceptT (..)
                                  , runExceptT
                                  , throwE
                                  )

import Control.Monad.Trans.Class     ( lift )

data Category = Category { categoryLabel :: String }
    deriving (Eq,Ord,Show)

instance Read Category where
    readsPrec _ s = if length label > 0 
                       then return (Category label, rest) 
                       else []
        where 
        label = takeWhile (isLegal) s
        rest  = drop (length label) s
        isLegal c = isAlphaNum c || c == ' '

data Transaction = Transaction { transactionCategory :: Category
                               , transactionAmount   :: Double }
    deriving (Eq,Ord,Show)

instance Read Transaction where
    readsPrec _ line = do
        (categ,  rest1) <- reads line
        (_,      rest2) <- readComma rest1
        (number, rest3) <- reads rest2
        return $ (Transaction categ number, rest3)
            where
            readComma :: ReadS String
            readComma s = case lex s of
                            ((",",r):_) -> return (",",r)
                            _           -> []
type Message = String

type Domain = ExceptT Message IO

readTransaction :: String -> Domain Transaction
readTransaction s = 
    case reads s of
      []        -> throwE ("incorrect csv format : " ++ s)
      ((t,_):_) -> return t

readTransactions :: String -> Domain [Transaction]
readTransactions = mapM readTransaction . lines

type SummaryLine = Transaction

summarize :: [Transaction] -> [SummaryLine] 
summarize = map summary 
          . groupBy ( (==)    `on` transactionCategory ) 
          . sortBy  ( compare `on` transactionCategory )
    where
    summary :: [Transaction] -> SummaryLine
    summary txs = Transaction (category txs) (total txs) 
        where
        category = transactionCategory . head
        total    = sum . map transactionAmount
    
showSummaryLine :: SummaryLine -> String
showSummaryLine sl = categoryLabel (transactionCategory sl) ++ ", " ++ show (transactionAmount sl)

report :: Either Message [SummaryLine] -> String
report (Left msg)   = "Error: " ++ msg
report (Right [])   = "No transactions to summarize" 
report (Right sums) = unlines $ map showSummaryLine sums

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
    if null args then throwE "no file name given" 
                 else return (args !! 0)

checkNotEmpty :: [Transaction] -> Domain [Transaction]
checkNotEmpty []  = throwE "no transactions"
checkNotEmpty txs = return txs

checkNonZero :: Transaction -> Domain Transaction
checkNonZero (Transaction _ 0) = throwE "amount equal to zero"
checkNonZero tx                 = return tx

computation :: Domain [SummaryLine]
computation = do 
    filePath     <- getFileNameArg 
    content      <- getFileContent filePath
    unchecked    <- readTransactions content
    notEmpty     <- checkNotEmpty unchecked
    transactions <- mapM checkNonZero notEmpty
    return $ summarize transactions

program3 :: IO ()
program3 = do
    transactions <- runExceptT computation
    putStrLn $ report transactions


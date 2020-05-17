module Program3           
    where

import Data.Char                   ( isAlphaNum )
import Data.Function               ( on )
import Data.List                   ( groupBy
                                   , sortBy  
                                   )
import Control.Exception           ( IOException
                                   , catch 
                                   )
import Control.Monad.Trans.Class   ( lift )
import Control.Monad.Trans.Except  ( ExceptT (..)
                                   , runExceptT
                                   , throwE
                                   )
import System.Environment          ( getArgs )

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
type Domain  = ExceptT Message IO

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
showSummaryLine sl = categoryLabel (transactionCategory sl) 
                     ++ ", " ++ show (transactionAmount sl)

getFileContent :: FilePath -> Domain String
getFileContent fp = ExceptT $ (readFileE fp) `catch` handleE
    where
    readFileE :: FilePath -> IO (Either Message String)
    readFileE filePath =  Right <$> readFile filePath

    handleE :: IOException -> IO (Either Message String)
    handleE = return . Left . show

getFileNameArg :: Domain FilePath
getFileNameArg = do
    args <- lift getArgs
    if null args then lift promptForFileName
                 else return (args !! 0)
                     where
    promptForFileName :: IO String
    promptForFileName = putStrLn "please enter a file name:" >> getLine


checkNotEmpty :: [Transaction] -> Domain [Transaction]
checkNotEmpty []  = throwE "no transactions"
checkNotEmpty txs = return txs

checkNonZero :: Transaction -> Domain Transaction
checkNonZero (Transaction _ 0) = throwE "amount equal to zero"
checkNonZero tx                 = return tx

getTransactions :: Domain [Transaction]
getTransactions  = getFileNameArg 
               >>= getFileContent 
               >>= readTransactions 
               >>= checkNotEmpty  
               >>= mapM checkNonZero 

report :: Either Message [SummaryLine] -> String
report (Left msg)   = "Error: " ++ msg
report (Right sums) = unlines $ map showSummaryLine sums

program3 :: IO ()
program3 = do
    transactions <- runExceptT getTransactions
    putStrLn $ report $ summarize <$> transactions


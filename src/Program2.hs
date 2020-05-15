module Program2
    where

import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Data.Char          ( isAlphaNum )
import Control.Exception  ( IOException
                          , catch )

type Message = String

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
    
report :: [SummaryLine] -> String
report = unlines . map reportLine 
    where
    reportLine tx = 
        categoryLabel (transactionCategory tx) 
        ++ ", " ++ show (transactionAmount tx)

readTransaction :: String -> Either Message Transaction
readTransaction s = 
    case reads s of
      []        -> Left $ "Error: incorrect csv format : " ++ s
      ((t,_):_) -> Right t

readTransactions :: String -> Either Message [Transaction]
readTransactions = mapM readTransaction . lines

getFileContent :: FilePath -> IO (Either Message String)
getFileContent fp = (fmap Right $ readFile fp) `catch` handle 
    where
    handle :: IOException -> IO (Either Message String)
    handle = return . Left . ("Error: " ++) . show

getFileNameArg :: IO (Either Message String)
getFileNameArg = do
    args <- getArgs
    return $ if null args 
                    then Left "Error: no file name given" 
                    else Right (args !! 0)

checkNotEmpty :: [Transaction] -> Either Message [Transaction]
checkNotEmpty []  = Left "Error: no transactions"
checkNotEmpty txs = Right txs

checkNonZero :: Transaction -> Either Message Transaction
checkNonZero (Transaction _ 0) 
    = Left "Error: amount equal to zero"
checkNonZero tx                 = Right tx

-- reads the given transaction file, outputs its summary
program2 :: IO ()
program2 = do
    fileName <- getFileNameArg 
    case fileName of
        Left msg -> putStrLn msg
        Right fp -> do 
            content <- getFileContent fp
            case (content >>= readTransactions
                          >>= checkNotEmpty
                          >>= mapM checkNonZero) of
                Left msg -> putStrLn msg
                Right []  -> putStrLn $ "error: no transactions"
                Right txs -> putStrLn $ report $ summarize txs

module Program1
    where

import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Data.Char          ( isAlphaNum )

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

display :: Transaction -> String
display t = categoryLabel (transactionCategory t) 
           ++ ", " ++ show (transactionAmount t)


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
report = unlines . map display

-- reads the given transaction file, outputs its summary
program1 :: IO ()
program1 = do
    args    <- getArgs
    content <- readFile (head args)
    let transactions = map read $ lines content
    putStrLn $ report $ summarize transactions


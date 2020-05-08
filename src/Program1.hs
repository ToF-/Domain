module Program1
    where

import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
import Data.Char          ( isAlphaNum )

data Category = Category String
    deriving (Eq,Ord,Show)

data Transaction = Transaction { transactionCategory :: Category
                               , transactionAmount   :: Double }
    deriving (Eq,Ord)

instance Read Category where
    readsPrec _ s = if length label > 0 
                       then return (Category label, rest) 
                       else []
        where 
        label = takeWhile (isLegal) s
        rest  = drop (length label) s
        isLegal c = isAlphaNum c || c == ' '

instance Read Transaction where
    readsPrec _ line = do
        (categ,r1)  <- reads line
        (_,r2)      <- readComma r1
        (number,r3) <- (reads :: ReadS Double) r2
        let t = Transaction categ number
        return (t,r3)
            where
            readComma :: ReadS String
            readComma s = case lex s of
                            ((",",r):_) -> return (",",r)
                            _ -> fail ("no parse with" ++ s)
instance Show Transaction where
    show (Transaction (Category c) d) = 
        c ++ " " ++ show d

data SummaryLine = SummaryLine { summaryCategory :: Category
                               , summaryAmount   :: Double }

instance Show SummaryLine where
    show (sl) = 
        show (summaryCategory sl) 
        ++ ", " 
        ++ show (summaryAmount sl)

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


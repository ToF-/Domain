# domain

playing with `ExceptT` 

## A naive approach

Let's say we want a program that reads a csv file containing _transactions_ of the form _(category, amount)_, and prints the summary per category of all these transactions.

Given a file `transactions.csv` containing this data:
```
Groceries, 100.00
Investment, 4807.00
Groceries, 42.00
Savings, 102.00
Interest, 38.00
Groceries, 30.00
Equipment, 179.33
Investment, 1200.00
```

Then the command `summary transactions.csv` will output this:
```
Equipment, 179.33
Groceries, 172.0
Interest, 38.0
Investment, 6007.0
Savings, 102.0
```
After our program import list,
```haskell
import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
```

we define adequate data types.  We should be able to `read` a transaction from a `String` containing comma separated value:

```haskell
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
```
And we should be able to `show` a summary line as well:
```haskell
data SummaryLine = SummaryLine { summaryCategory :: String
                               , summaryAmount   :: Double }

instance Show SummaryLine where
    show t = 
        (summaryCategory t) 
        ++ ", " 
        ++ show (summaryAmount t)
```
To summarize the transactions by category, we sort them by category, group them by category, and for each group, create a summary line with the category and total amount of the group:
```haskell
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
```
The main function, given an file name on the command line, will read that file, convert its content into a list of `Transaction`s, compute and then print the summary.
```haskell
program1 :: IO ()
program1 = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let transactions = map read $ lines content
    putStrLn $ unlines $ map show $ summarize transactions

main :: IO ()
main = program1

```
What could go wrong?

- we could forget to specify a file name while using the program
- we could specify a file name that doesn't exist
- the file could contain data that it would fail to recognize as comma separated transaction fields
- the file could be empty, in which case nothing would be output

None of these conditions is adequately managed by our program, which means that given certain input, some of our function will not return a value, and the program will halt. Let's change this.

## Responding to failure conditions

The way to deal with failure is to use data types that can represent it. We'll call `Domain a` the type of values that can be either a `Message` or a "normal" value of type _a_.
```haskell
type Message = String

type Domain = Either Message
```

Our most frequent concern will be about the file data format, so let's create functions that will manage faulty csv lines:
```haskell
readTransaction :: String -> Domain Transaction
readTransaction s = 
    case reads s of
      []        -> Left $ "incorrect csv format : " ++ s
      ((t,_):_) -> Right t

readTransactions :: String -> Domain [Transaction]
readTransactions = mapM readTransaction . lines

```
The `readTransactions` function will, depending on its entry, either return a `Right` list of transactions, or a `Left` with the message corresponding to the first failure.

What if the file can't be open? Using `Control.Exception` will help us to deal with such situation:
```haskell
getFileContent :: FilePath -> IO (Domain String)
getFileContent fp = 
    (readFile fp >>= return . Right) `catch` handle
    where
    handle :: IOException -> IO (Domain String)
    handle e = return $ Left (show e) 
```
Our function returns a `IO (Domain String)` value, not a `Domain String` because there is no safe way to convert an IO value into a non-IO value. Since this function will be used in a the `main :: IO ()` context, it's not a problem. 

Another IO function has to do with getting the first argument on the command line:
```haskell
getFileNameArg :: IO (Domain String)
getFileNameArg = do
    args <- getArgs
    return $ if null args 
                    then Left "no file name given" 
                    else Right (args !! 0)
```

Our main program can now examine the values returned by `get`... functions and branch accordingly instead of halting:

```haskell
program2 :: IO ()
program2 = do
    fileName <- getFileNameArg 
    case fileName of
        Left msg -> putStrLn msg
        Right fp -> do 
            content <- getFileContent fp
            case (content >>= readTransactions) of
                Left msg -> putStrLn msg
                Right txs -> putStrLn $ unlines $ map show $ summarize txs
main :: IO ()
main = program2
```




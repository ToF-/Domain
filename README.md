# Scratching the surface of Monad Transformers

In this post I am playing with the `ExceptT` monad transformer in order to make a simple program even simpler.

Let's say we want to write a program that reads a csv file containing _transactions_ of the form _(category, amount)_, and prints a summary by category of these transactions.

For instance, given a file `transactions.csv` containing this data:
```
Groceries, 100.00
Investment, 4807.00
Groceries, 42.00
Savings, 500.00
Interest, 38.17
Groceries, 30.00
Equipment, 179.00
Investment, 1200.00
```

the command `summary transactions.csv` will output this:
```
Equipment, 179.0
Groceries, 172.0
Interest, 38.17
Investment, 6007.0
Savings, 500.0
```
## 1. First, a naive approach

Our program will 

- obtain the name of a file from the command line,
- read this file, splitting each line into _category_ and _amount_, so as to create _transactions_
- sort and group these _transactions_ by _category_, 
- sum these groups into _summary lines_
- and finally print these lines

Thus we’ll need to import the following functions:
```haskell
import System.Environment ( getArgs )
import Data.List.Split    ( splitOn )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
```
Now let’s define adequate data types.  

```haskell
data Transaction = Transaction { transactionCategory :: String
                               , transactionAmount   :: Double }
    deriving (Eq,Ord,Show)

data SummaryLine = SummaryLine { summaryCategory :: String
                               , summaryAmount   :: Double }
```
Since we should be able to `read` a Transaction from a `String` containing comma separated value, let's make this type an instance of `Read` and write a simplistic parser for it:

```haskell
instance Read Transaction where
    readsPrec _ s = 
        case splitOn "," s of
          [c,a] -> case reads a of
                     ((d,_):_) -> [(Transaction c d,"")]
                     _ -> []
          _ -> []
```
And we also should be able to `show` a Summary Line:
```haskell
instance Show SummaryLine where
    show t = 
        (summaryCategory t) 
        ++ ", " 
        ++ show (summaryAmount t)
```
To summarize the Transactions by category, we sort them by category, group them by category, and for each group, create a Summary Line with the category and total amount of the group:
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
Note how the `on` function elegantly helps us to write our logic, by composing the functions that are required by `sortBy` and `groupBy`. The signature of this function helps understand how it is doing its work. Since
```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```
then
```haskell
on compare :: (Ord b) => (a -> b) -> a -> a -> Ordering
```
and 
```haskell
on compare transactionCategory :: Transaction -> Transaction -> Ordering
```
which is conform to the type of function required by `sortBy`. Similarly, `on` composed with `(==)` will create a function of the type required by `groupBy`.

Now we can write our main function, which will get a file name on the command line, read that file, convert its content into a list of `Transaction`s, and then compute and print the summary.
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
And voilà, we have our program:
```
$ program1 data/transactions.csv ⏎
Equipment, 179.0
Groceries, 172.0
Interest, 38.17
Investment, 6007.0
Savings, 500.0
```

It is, indeed, a very naive program. What could go wrong? 

- we could forget to specify a file name when lauching the program from the command line
- we could specify a file name that doesn't correspond to an existing file
- the file could contain data that can't be read as comma separated transaction values
- the file could be empty, in which case nothing would be output

```
$ program1 ⏎
program1: Prelude.!!: index too large
$ program1 foo ⏎
program1: foo: openFile: does not exist (No such file or directory)
$ program1 data/wrong.csv ⏎
program1: Prelude.read: no parse
$ program1 data/empty.csv ⏎

```
None of these conditions except the last one is adequately managed by our program, which means that given certain inputs, some of our functions will not return a value, and the program will halt. Let's change this.

## 2. Responding to failure conditions

The way to deal with failure is to use data types that can represent the failure (for example in the form of a String) as well as the normal values. The `Either` monad data type is just designed for such representations. To make things a bit clearer, let's define a type synonym for the `String` used as messages.
```haskell
type Message = String
```
Our most frequent concern will be about the file data format, so let's create functions that will manage faulty csv data:
```haskell
readTransaction :: String -> Either Message Transaction
readTransaction s = 
    case reads s of
      []        -> Left $ "Error: incorrect csv format : " ++ s
      ((t,_):_) -> Right t

readTransactions :: String -> Either Message [Transaction]
readTransactions = mapM readTransaction . lines
```
The `mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)` function is used to apply a monadic action to the elements of a structure, here making a `[Either Message Transaction]` into an `[Either Message [Transaction]`.


What if the file can't be open? Using `Control.Exception` will help us to deal with such situation:
```haskell
getFileContent :: FilePath -> IO (Either Message String)
getFileContent fp = 
    (readFile fp >>= return . Right) `catch` handle
    where
    handle :: IOException -> IO (Either Message String)
    handle e = return $ Left $ "Error: " ++ (show e) 
```
Note that our function returns a `IO (Either Message String)` value, not a `Either Message String`. There is no safe way to convert an IO value into a non-IO value. That should not be a problem, because `getFileContent` will solely used within the context of an IO action and nowhere else.

Another IO function has to do with getting the first argument on the command line:
```haskell
getFileNameArg :: IO (Either Message String)
getFileNameArg = do
    args <- getArgs
    return $ if null args 
                    then Left "Error: no file name given" 
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
                Right []  -> putStrLn $ "Error: no transactions"
                Right txs -> putStrLn $ unlines $ map show $ summarize txs
main :: IO ()
main = program2
```
Here are examples of uses:
```
$ program2 ⏎
Error: no file name given
$ program2 foo ⏎
Error: foo: openFile: does not exist (No such file or directory)
$ program2 data/wrong.csv ⏎
Error: incorrect csv format : Foo, bar42
$ program2 data/empty.csv ⏎
Error: no transactions
$ program2 data/transactions.csv ⏎
Equipment, 179.0
Groceries, 172.0
Interest, 38.17
Investment, 6007.0
Savings, 500.0
```
## Monadic actions as isolated contexts
The construct:
```haskell
content >>= readTransaction
```
Seems to suggest a way to chain actions on values of type `Either Message a` instead of explicitely branching on every failure case. For example we can refactor our program to use an Either action to detect empty transaction lists, or to detect that at least one transaction has an empty category:
```haskell
checkNotEmpty :: [Transaction] -> Either Message [Transaction]
checkNotEmpty []  = Left "Error: no transactions"
checkNotEmpty txs = Right txs

checkNotEmptyCategory :: Transaction -> Either Message Transaction
checkNotEmptyCategory (Transaction "" _) = Left "Error: empty category"
checkNotEmptyCategory tx                 = Right tx

program2 :: IO ()
program2 = do
    fileName <- getFileNameArg 
    case fileName of
        Left msg -> putStrLn msg
        Right fp -> do 
            content <- getFileContent fp
            case (content >>= readTransactions 
                          >>= checkNotEmpty 
                          >>= mapM checkNotEmptyCategory) of
                Left msg -> putStrLn msg
                Right txs -> putStrLn $ unlines $ map show $ summarize txs
```

Question: We still have explicit branching in our main function. Could it possible to chain _all_ our `Either Message a` functions, like this ?

```haskell
wrong_program2 :: IO () -- won't compile
wrong_program2 = do
    case (getFileNameArg >>= getFileContent
                         >>= readTransactions 
                         >>= checkNotEmpty 
                         >>= mapM checkNotEmptyCategory) of
               Left msg -> putStrLn msg
               Right txs -> putStrLn $ unlines $ map show $ summarize txs
```
Answer: No. _ghc_ has no less than 6 complaints about this piece of code. Here's an (excerpt of) the first one

```
    • Couldn't match type ‘Either Message String’ with ‘[Char]’
      Expected type: Either Message String -> IO (Either Message String)
        Actual type: FilePath -> IO (Either Message String)
86 |     case (getFileNameArg >>= getFileContent
   |                              ^^^^^^^^^^^^^^

```
In essence: we cannot chain monadic actions from the IO monad to the Either monad, and vice versa. Since the `case` is examining a value of type `Either Message [Transaction]`, the expected type for actions leading to that value is `a -> Either Message b`. But we try to chain actions of type `a -> IO b`. That can't work.

We can bind monadic actions to distinct types through the same monad m :
```
ghci ⏎
> readFile "data/transactions.csv" >>= putStrLn ⏎
Groceries, 100.00
Investment, 4807.00
Groceries, 42.00
Savings, 500.00
Interest, 38.17
Groceries, 30.00
Equipment, 179.00
Investment, 1200.00

readTransaction "Foo,4807" >>= checkNotEmptyCategory ⏎
Right (Transaction {transactionCategory = "Foo", transactionAmount = 4807.0})

readTransaction ",42" >>= checkNotEmptyCategory ⏎
Left "Error: empty category"
```
but we cannot do that through different monadic types:
```
ghci ⏎
> readFile "data/transactions.csv" >>= readTransactions ⏎
<interactive>:3:38: error:
    • Couldn't match type ‘Either Message’ with ‘IO’
      Expected type: String -> IO [Transaction]
        Actual type: String -> Either Message [Transaction]
    • In the second argument of ‘(>>=)’, namely ‘readTransactions’
      In the expression:
        readFile "data/transactions.csv" >>= readTransactions
      In an equation for ‘it’:
          it = readFile "data/transactions.csv" >>= readTransactions
```









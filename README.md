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
## First, a naive approach

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
    deriving (Eq,Ord)

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

## Responding to failure conditions

The way to deal with failure is to use data types that can represent the failure (for example in the form of a String) as well as the normal values. The `Either` monad data type is just designed for such representations. To make things a bit clearer, let's define a type synonym for the `String` used as messages.
```haskell
type Message = String
```
Our most frequent concern will be about the file data format, so let's create functions that will manage faulty csv data:
```haskell
readTransaction :: String -> Domain Transaction
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
getFileContent :: FilePath -> IO (Domain String)
getFileContent fp = 
    (readFile fp >>= return . Right) `catch` handle
    where
    handle :: IOException -> IO (Domain String)
    handle e = return $ Left $ "Error: " ++ (show e) 
```
Note that our function returns a `IO (Either Message String)` value, not a `Either Message String`. There is no safe way to convert an IO value into a non-IO value. That should not be a problem, because the `getFileContent` function will solely used within the context of an IO action and nowhere else.

Another IO function has to do with getting the first argument on the command line:
```haskell
getFileNameArg :: IO (Domain String)
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
Seems to suggest a way that we could use to bind all the functions of type `a -> Either Message a` together. Indeed for example, these functions:
```haskell
checkNotEmpty :: String -> Either Message String
checkNotEmpty "" = Left "empty parameter"
checkNotEmpty s  = Right s

getDouble :: String -> Either Message Double
getDouble s = case reads s of
    []        -> Left "not a number"
    ((d,_):_) -> Right d

checkPositive :: Double -> Either Message Double
checkPositive d 
    | d < 0     = Left "negative number"
    | otherwise = Right d
```
Could be used in combination, forming in a single function the equivalent of 3 `case ... of` branchings.
```haskell
getSquareRoot :: String -> Either Message Double
getSquareRoot s = checkNotEmpty s 
                >>= getDouble
                >>= checkPositive
                >>= return . sqrt

main :: IO ()
main = do
    s <- getLine
    case getSquareRoot s of
        Left msg -> putStrLn $ "Error: " ++ s 
        Right d  -> print d
```

Could it possible to chain all our domain functions, like this ?

```haskell
wrongProgram :: IO () -- won't compile
wrongProgram = do
    fileName <- getFileNameArg 
    content <- getFileContent fileName
    result <- fmap summarize $ readTransactions content
    case result of
        Left msg   -> putStrLn msg
        Right sums -> putStrLn $ unlines $ map show $ sums
```

No, because `getFileNameArg` and `getFileContent` are `IO (Either Message a)` functions and not `Either Message a` function.

To understand this, we can rewrite this wrong program using `>>=` instead of left arrows:
```haskell
wrongProgram :: IO () -- won't compile
wrongProgram = do
    result <- getFileNameArg >>= getFileContent >>= readTransactions >>= return . summarize
    case result of
        Left msg   -> putStrLn msg
        Right sums -> putStrLn $ unlines $ map show $ sums
```
and then examine the type of the functions we are trying to bind:
```
:t (>>=) :: forall a b. m a -> (a -> m b) -> m b 

:t getFileNameArg :: IO (Either Message String)
:t (getFileNameArg >>=) :: (Either Message String -> IO b) -> IO b
:t getFileContent :: FilePath -> IO (Either Message String)

:t (getFileContent >>=) :: (Either Message String -> IO b) -> IO b
:t readTransactions :: String -> Either Message [Transaction]
:t (readTransactions >>=) :: ([Transaction] -> Either Message b) -> Either Message b
:t return summarize :: [Transactions] -> Either Message [SummaryLine]
```








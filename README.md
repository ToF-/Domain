# Scratching the surface of Monad Transformers

Haskell makes it possible to write statically typed, purely functional programs. This gives us two interesting conveniences:

- any incoherence in the type of our expressions and sequences can be spotted at compile time,
- we can rule out the use of partial functions, by wrapping their results in new data types, and compose values of these type.

However, when a program has to process IO (as any useful program will), static types and purity might seem to get in the way for a beginner. We can use the `IO` Monad to get `IO` values, and the `Either` Monad to deal with failures, but combining the two makes our programs cumbersome. 

Monad Transformers can help us write simpler programs, by hiding the boilerplate code that this combination requires.

In this blog post I present how to chain monadic actions and controls with the `ExceptT` Monad Transformer in Haskell through a small example, in 4 steps:
- writing a naïve implementation, which halts on IO exception
- improving its robustness with conditionals and pattern matching
- combining `IO` and `Either` using `ExceptT` 
- refactoring the program's chaining of actions
 
## Program #1: A naive solution

Let's say we want to write a program that reads a csv file containing transactions, which are composed of a category and an amount, and prints the total amount for each category.

For instance, given a file `transactions.csv` containing this data:
```
Groceries, 100.00
Savings, 500.00
Equipment, 32.00
Groceries, 42.00
Interest, 38.17
Groceries, 30.00
Equipment, 179.00
```

the command `summary transactions.csv` will output this:
```
Equipment, 211.0
Groceries, 172.0
Interest, 38.17
Savings, 500.0
```

Our program will 

- obtain the name of a file from the command line,
- read this file, splitting each line into _category_ and _amount_, so as to create _transactions_
- sort and group these _transactions_ by _category_, 
- sum these groups into _summary lines_
- and finally print these lines

Thus we’ll need to import the following functions:
```haskell
import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )
```
Let’s define adequate data types for our program. A _Category_ can be created by simply using a `String` as the label.

```haskell

data Category = Category { categoryLabel :: String }
    deriving (Eq, Show)
```
We need a way to `read` a `Category` from a `String`, so let's make this type an instance of the `Read` class. Parsing a category label amounts to reading alphanumeric chars and possibly spaces, and rejecting everything else. 
For intance `"Credit Cards Payments"` and `"1st aid kit"` are valid categories, while `"( wrong !)"` isn't.

`readsPrec` is the function that we need to implement. It has the signature ` :: Int -> String -> [(a,String)]` where the first argument is the precedence level (which we don't need to specify for our simple program), the second argument is the `String` to be parsed, and the result is a list of possible results. An empty list means that the string cannot be parsed to a value of the given type. 

```haskell
instance Read Category where
    readsPrec _ s = if not (null label) 
                       then return (Category label, rest) 
                       else []
        where 
        label = takeWhile (isLegal) s
        rest  = drop (length label) s
        isLegal c = isAlphaNum c || c == ' '
```
Examining the input string, `readsPrec` takes all its legal characters and returns a `Category` value and the remaining chararcters, or the empty list if not a legal character was found. 
Let's try to `read` a `Category` using _ghci_:
```
> import Program1.hs ⏎
> read "Foo" :: Category ⏎
Category "Foo"

> read "*$!" :: Category ⏎
*** Exception: Prelude.read: no parse

(reads :: ReadS Category) "Bar, 42" ⏎
[(Category "Bar",",42")]
```
`Transaction` it composed with a `Category` and a `Double`. Since we want to `read` transactions, we need to implement `readsPrec` for this type as well.
```haskell
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
```
`readsPrec` is chaining computations on the list monad: it first reads a `Category`, then a comma (discarding it), then a `Double` value. Chaining these three parsers ensures that the evaluation will result in an empty list as soon as one of the three parsers returns an empty list.

```
> read "Foo, 42" :: Transaction ⏎
Transaction {transactionCategory = Category "Foo", transactionAmount = 42.0}

> read ", 42" :: Transaction ⏎
*** Exception: Prelude.read: no parse

> read "Bar, i42" :: Transaction ⏎
*** Exception: Prelude.read: no parse
>
```
A summary line having the exact same structure as transaction, we can define it as a type synonym.

Also we should be able to `display` transactions:

```haskell
type SummaryLine = Transaction

display :: Transaction -> String
display t = categoryLabel (transactionCategory t) 
           ++ ", " ++ show (transactionAmount t)
```
To summarize the transactions, we sort them by category, group them by category, and for each group, create a `SummaryLine` with the category and total amount of the group:
```haskell
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
``` 
Note how `on` helps expressing the logic, by composing the functions that are required by `sortBy` and `groupBy`. 
Since
```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```
then
```haskell
on compare :: (Ord b) => (a -> b) -> a -> a -> Ordering
```
and thus
```haskell
on compare transactionCategory :: Transaction -> Transaction -> Ordering
```
which is conform to the type of function required by `sortBy`. Similarly, `on` composed with `(==)` will create a function of the type required by `groupBy`.

Reporting summary lines is done by mapping our `display` function for each line, and then `unline`ing, i.e. merging this list of `String`s into one single `String`:
```haskell
report :: [SummaryLine] -> String
report = unlines . map display
```

We can now write our main function, which will get a file name on the command line, read that file, convert its content into a list of `Transaction`s, and then compute and print the summary.
```haskell
program1 :: IO ()
program1 = do
    args    <- getArgs
    content <- readFile (head args)
    let transactions = map read $ lines content
    putStrLn $ report $ summarize transactions
```
Et voilà, we have our program:
```
$ program1 data/transactions.csv ⏎
Equipment, 211.0
Groceries, 172.0
Interest, 38.17
Savings, 500.0
```

It is, indeed, a very naive program. What could go wrong? 

- we could forget to specify a file name when lauching the program from the command line
- we could specify a file name that doesn't correspond to an existing file
- the file could contain data that can't be read as comma separated transaction values
- the file could be empty, in which case nothing would be output

```
$ program1 ⏎
program1: Prelude.head: empty list

$ program1 foo ⏎
program1: foo: openFile: does not exist (No such file or directory)

$ program1 data/wrong.csv ⏎
program1: Prelude.read: no parse

$ program1 data/empty.csv ⏎

```
None of these conditions is adequately managed by our program, which means that given certain inputs, some of our functions will not return a value, and the program will halt. Let's change this.

## Program #2: responding to failure conditions

We want to deal with failure conditions in a graceful way. Our program should not stop abruptly with a strange message like `no parse`, but instead not print a clear diagnostic, and possibly propose a way for the user to remedy the condition.

What parts of the program should change? Well, every part where the program calls a function that is not _total_. A function is said to be total if it returns a result for each possible value of its argument.

For example, `head` in the expression `content <- readFile (head args)` is not total and could halt the program, with an _"empty list"_ message. On the other hand, the function:
```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```
for example _is_ a total function, and will not interrupt the program. 

☞ _(`head` is used inside the function `summary` in the expression `transactionCategory . head`. Does it constitute a risk of halting the program in case we apply it on an empty list? Why?)_

If a function is not total, one safe way to use it is to combine it with a data type that can represent failure. The `Either` type constructor is just designed for such representations, and we will us it. To make things a bit clearer, let's first define a type synonym for the `String` used as messages.
```haskell
type Message = String
```
Our most frequent concern will be about the file data format, so let's create a reader function that will manage faulty csv data in a graceful way:
```haskell
readTransaction :: String -> Either Message Transaction
readTransaction s = 
    case reads s of
      []        -> Left $ "Error: incorrect csv format : " ++ s
      ((t,_):_) -> Right t

readTransactions :: String -> Either Message [Transaction]
readTransactions = mapM readTransaction . lines
```
The `mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)` function is used to chain a monadic action to the elements of a structure and return a single monadic value. Here, it transposes a `[Either Message Transaction]` into an `Either Message [Transaction]`.


What if the file can't be open? Using `Control.Exception` will help us dealing with such situation:
```haskell
getFileContent :: FilePath -> IO (Either Message String)
getFileContent fp = (fmap Right $ readFile fp) `catch` handle 
    where
    handle :: IOException -> IO (Either Message String)
    handle = return . Left . ("Error: " ++) . show
```
The `catch :: IOException e => IO a -> (e -> IO a) -> IO a` function is our "graceful exit" instrument here: given an IO action and a handler function, it will catch any IO exception, returning the result of applying the handler on the exception. In our case, what we want to do with the exception is :
- `show` it into a `String` starting with `"Error: "`,
- make this message a `Left` value, 
- `return` this left value, making it an `IO (Either Message String)`

In the case when no exception is triggered, `readFile` will give us an `IO String` value. What we do is to make that value an `Either Message String` by mapping the `Right` constructor to it.

Note that our function returns a `IO (Either Message String)` value. Why not return a `Either Message String` instead? Because there is no safe way to convert an IO value into a non-IO value. Any function dealing with IO is partial, not total, because IO actions are always prone to some failure condition. If we could compile a function with signature `IO a -> a` then two things would happen:
- Haskell's type checker would be much less useful to detect problems in our constructions,
- we would be hiding to ourselves some crucial concern with our program reliability.

Anyway, the fact that `getFileContent` returns a `IO` value should not be a problem, because it will be used within the context of an IO action and nowhere else.

Another IO function has to do with getting the first argument on the command line, now making the absence of argument a safe condition that will produce a `Left Message` value:
```haskell
getFileNameArg :: IO (Either Message String)
getFileNameArg = do
    args <- getArgs
    return $ if null args 
                    then Left "Error: no file name given" 
                    else Right (args !! 0)
```

Our main program will examine the values returned by `get`... functions and branch accordingly instead of halting:

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
                Right []  -> putStrLn $ "error: no transactions"
                Right txs -> putStrLn $ report $ summarize txs

main :: IO ()
main = program2
```
And now our program is dealing with failures in a slightly improved way:
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
## 3. Monadic actions as isolated contexts
The bind operation used in the `case .. of` instruction: 
```haskell
            ...
            content <- getfilecontent fp
            case (content >>= readtransactions) of
                left msg -> putstrln msg
                right []  -> putstrln $ "error: no transactions"
                right txs -> putstrln $ unlines $ map show $ summarize txs
```
can be chained with as many functions of the type `a -> Either Message b` as we want over the initial value of `content`. For instance we could add new controls to detect an empty transaction list, or to check that no transaction in the list has amount of zero.
```haskell

checkNotEmpty :: [Transaction] -> Either Message [Transaction]
checkNotEmpty []  = Left "Error: no transactions"
checkNotEmpty txs = Right txs

checkNonZero :: Transaction -> Either Message Transaction
checkNonZero (Transaction _ 0) 
    = Left "Error: amount equal to zero"

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
                Right txs -> putStrLn $ unlines $ map show $ summarize txs
```
This chaining of controls could give the impression that we've missed an opportunity to simplify the code of the whole function, by equally chaining the values obtained by `getFileNameArg` and `getFileContent`. Instead we used explicit `case ... of` branching.

Question: Could it be possible to chain _all_ our `Either Message a` functions, like this ?

```haskell
wrong_program2 :: IO () -- won't compile
wrong_program2 = do
    case (getFileNameArg >>= getFileContent
                         >>= readTransactions 
                         >>= checkNotEmpty 
                         >>= mapM checkNonZero) of
               Left msg -> putStrLn msg
               Right txs -> putStrLn $ unlines $ map show $ summarize txs
```
Answer: No. _ghc_ has no less than 6 complaints about this change to the function. Here's the first one:

```
    • Couldn't match type ‘Either Message String’ with ‘[Char]’
      Expected type: Either Message String -> IO (Either Message String)
        Actual type: FilePath -> IO (Either Message String)
86 |     case (getFileNameArg >>= getFileContent
   |                              ^^^^^^^^^^^^^^

```
In essence: we cannot chain monadic actions from the `IO` monad to the `Either` monad, and vice versa. Since the `case ... of` is examining a value of type `Either Message [Transaction]`, the expected type for actions leading to that value is `a -> Either Message b`. But we are trying to somehow get to that value through actions of type `a -> IO b`. That can't work.

We can __always__ bind monadic actions to distinct types through the __same monad__:
```
ghci ⏎
> readFile "data/transactions.csv" >>= putStrLn ⏎
Groceries, 100.00
Investment, 4807.00
. . .

readTransaction "Groceries,42" >>= checkNonZero ⏎
Right (Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 42.0})

readTransaction "Foo,0" >>= checkNonZero ⏎
Left "Error: amount equal to zero"
```
but we can __never__ do that through different monadic types:
```
ghci ⏎
> readFile "data/transactions.csv" >>= readTransactions ⏎
<interactive>:3:38: error:
    • Couldn't match type ‘Either Message’ with ‘IO’
      Expected type: String -> IO [Transaction]
        Actual type: String -> Either Message [Transaction]
```

Of course, it would be nice if we could...

## 4. Combining Monads with Monad Transformers

What we need in order to simplify the code is the ability to chain `Either` actions _inside_ the `IO` monad. 

This is exactly what the [`Control.Monad.Trans.Except`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Except.html) library is offering us:

> __Control.Monad.Trans.Except__
> 
> This monad transformer extends a monad with the ability to throw exceptions.
> 
> A sequence of actions terminates normally, producing a value, only if none of the actions in the sequence throws an exception. If one throws an exception, the rest of the sequence is skipped and the composite action exits with that exception.
> 
```haskell
> newtype ExceptT e m a
```
> A monad transformer that adds exceptions to other monads.
> 
> `ExceptT` constructs a monad parameterized over two things:
> 
> - _e_ - The exception type.
> - _m_ - The inner monad.
> 
> The `return` function yields a computation that produces the given value, while `>>=` sequences two subcomputations, exiting on the first exception.

Let's experiment on _ghci_. We start with importing our program, and the module.

```
>  import Program2.hs
>  import Control.Monad.Trans.Except
```

Let' try to create a pure `ExecptT Message IO [Transaction]` value, with the `ExceptT` constructor:
```
value = ExceptT $ return $ Right $ [Transaction (Category "Groceries") 42.0] ⏎

:type value ⏎
value :: Monad m => ExceptT e m [Transaction]
```
Now let's create a function to get a list of transactions from a file:
```
fromFile = ExceptT . fmap readTransactions . readFile ⏎

:type fromFile ⏎
fromFile :: FilePath -> ExceptT Message IO [Transaction]
```
Extracting the value is done with `runExceptT`:
```
runExceptT value ⏎
Right [Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 42.0}]

runExceptT $ fromFile "../data/transactions.csv" ⏎
Right [Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 100.0}
,Transaction {transactionCategory = Category {categoryLabel = "Savings"}, transactionAmount = 500.0}
,Transaction {transactionCategory = Category {categoryLabel = "Equipment"}, transactionAmount = 32.0}
,Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 42.0}
,Transaction {transactionCategory = Category {categoryLabel = "Interest"}, transactionAmount = 38.17}
,Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 30.0}
,Transaction {transactionCategory = Category {categoryLabel = "Equipment"}, transactionAmount = 179.0}]
```
Naturally our function doesn't handle exceptions :
```
> runExceptT $ fromFile "foo" ⏎
*** Exception: foo: openFile: does not exist (No such file or directory)
```
If we want it to return a `Left` value, we have to provide a handler:
```
handler = return . Left . show :: (IOException -> IO (Either Message [Transaction])) ⏎
fromFile fp = ExceptT $ fmap readTransactions (readFile fp) `catch` handler ⏎

runExceptT $ fromFile "foo" ⏎
Left "foo: openFile: does not exist (No such file or directory)"
```
And now our function handles exceptions correctly. 

Another example of how `ExceptT` makes `IO` and `Either` working together seamlessly: one of our functions extracts the csv file name from the arguments provided on the command line, returning a `Left` if no argument was given. Could it instead prompt the user for a file name?

Let's write a prompt function:
```
> promptForFileName = putStrLn "please enter a file name:" >> getLine ⏎
```
and then use it when matching the empty list pattern, or else extracting the first argument:
```
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = ExceptT $ fmap Right $ prompt
| getFileName (arg:_) = ExceptT $ return $ Right arg
| :} ⏎

> runExceptT $ getFileName ["data/transactions.csv"] ⏎
Right "data/transactions.csv"

> runExceptT $ getFileName [] ⏎
please enter a file name:
data/transactions.csv ⏎
Right "data/transactions.csv"

>
```
This works beautifully: in one case we convert an `IO String` into an `IO (Either Message String)` and then nest that value into an `ExceptT`. In the other case we nest a `Right` value into `IO` (getting also an `IO (Either Message String)`) and also nest that value into an `ExceptT`.

But all this converting is tedious. First, since `ExceptT` is a monad, it offers a `return` function. Let' use it.

```
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = ExceptT $ fmap Right $ prompt
| getFileName (arg:_) = return arg
| :}
```
Secondly, the combination `ExceptT . fmap Right` can be done using a general function found in `Control.Monad.Trans.Class`: 

> `lift :: Monad m => m a -> t m a`
> 
> Lift a computation from the argument monad to the constructed monad.
> 

```
import Control.Monad.Trans.Class ⏎
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = lift prompt
| getFileName (arg:_) = return arg
| :}
```

What we have done so far: 
- we have composed together an "either" monad with the IO monad, using the `ExceptT` monad transformer,
- we can hold values and extract them into an `Either` data type,
- which means we can chain monadic functions on these values and benefit from the built in of bind (`>>=`) for `Either` values,
- when a function leads to failure, the chaining is shortcut and we get a `Left` value,  
- we can also get values from IO operations, without having to use a distinct monad, by `lift`ing these operation into the `ExceptT` monad.
- thanks to exception `catch`ing, when a failure occurs on the IO operation, we also get a `Left` values.

## Program #3: A chain of actions that can fail gracefully

Let's integrate these ideas into our program.
```haskell
import Control.Monad.Trans.Except ( ExceptT (..)
                                  , runExceptT
                                  , throwE
                                  )

import Control.Monad.Trans.Class     ( lift )
```
`Domain` is the type of values possibly acquired from IO operations, and that can indicate failure:
```haskell
type Domain = ExceptT Message IO
```
We can rewrite our conversion and control functions, using `throwE :: Monad m => e -> ExceptT e m a` instead of `Left`:
```haskell
readTransaction :: String -> Domain Transaction
readTransaction s = 
    case reads s of
      []        -> throwE ("incorrect csv format : " ++ s)
      ((t,_):_) -> return t

readTransactions :: String -> Domain [Transaction]
readTransactions = mapM readTransaction . lines

checkNotEmpty :: [Transaction] -> Domain [Transaction]
checkNotEmpty []  = throwE "no transactions"
checkNotEmpty txs = return txs

checkNonZero :: Transaction -> Domain Transaction
checkNonZero (Transaction _ 0) = throwE "amount equal to zero"
checkNonZero tx                 = return tx
```
Acquiring the csv file name will follow the logic we experimented interactively:

```haskell
getFileNameArg :: Domain FilePath
getFileNameArg = do
    args <- lift getArgs
    if null args then lift promptForFileName
                 else return (args !! 0)
                     where
    promptForFileName :: IO String
    promptForFileName = putStrLn "please enter a file name:" >> getLine
```
Dealing with IO exception implies using and wraping the `catch` expression into the `ExceptT` monad:
```haskell
getFileContent :: FilePath -> Domain String
getFileContent fp = ExceptT $ (readFileE fp) `catch` handleE
    where
    readFileE :: FilePath -> IO (Either Message String)
    readFileE filePath =  Right <$> readFile filePath

    handleE :: IOException -> IO (Either Message String)
    handleE = return . Left . show
```
Note that we use the `<$>` an infix shortcut for `fmap`, since we need to apply the `Right` function into the IO value that `readFile` aquired.

Now we can chain all these acquiring and controlling functions into a single one:
```haskell
getTransactions :: Domain [Transaction]
getTransactions = do 
    filePath     <- getFileNameArg 
    content      <- getFileContent filePath
    unchecked    <- readTransactions content
    notEmpty     <- checkNotEmpty unchecked
    transactions <- mapM checkNonZero notEmpty
    return $ transactions
```
Of course, using variables and left arrow is one way to make the chaining of action explicit. Another way is to use the bind operator:
```haskell
getTransactions :: Domain [Transaction]
getTransactions  = getFileNameArg 
               >>= getFileContent 
               >>= readTransactions 
               >>= checkNotEmpty  
               >>= mapM checkNonZero 
```
Finally, we need a way to output the result of our program, be it a failure or a valid list of summary lines:

```haskell
report :: Either Message [SummaryLine] -> String
report (Left msg)   = "Error: " ++ msg
report (Right sums) = unlines $ map showSummaryLine sums
```
Our main program will get the transactions, summarize them, and print the report:
```haskell
program3 :: IO ()
program3 = do
    transactions <- runExceptT getTransactions
    putStrLn $ report $ summarize <$> transactions

main :: IO ()
main = program3
```
Again, `<$>` is used instead of `$`: since `transactions` is bound to an `Either Message [Transaction]` value, we have to map `summarize` to its value instead of just applying it.

## Conclusion

In this blog post, we went from a naïve haskell program doing IOs, to a less naïve implementation that deals with exceptions and failures, while trying to keep the program flow simple and the amount of boiler plate to a minimum. I hope you enjoyed it and learned from it. I would greatly appreciate feedback! You can write me at cthibauttof@gmail.com or send me a direct message on Twitter: @ToF_.

The distinct versions of the program can be found on [github](https://github.com/ToF-/Domain)

# Scratching the surface of Monad Transformers

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
## Program #1: A naive solution

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

We want to deal with failure conditions in a way that does not stop the program, but prints a message instead, and possibly propose a way for the user to remedy the condition. What parts of the program should change? Every part where the program calls a function that is not total.

For example, `head` in the expression `content <- readFile (head args)` is not total and will halt the program after displaying  _"empty list"_. On the other hand, the function:
```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```
_is_ a total function, and will not stop the program. 

_(`head` is used inside the function `summary` in the expression `transactionCategory . head`. Does it constitute a risk of halting the program in case we apply it on an empty list? Why?)_

If a function is not total, one safe way to use it is to combine it with a data type that can represent failure. The `Either` type constructor is just designed for such representations. To make things a bit clearer, let's define a type synonym for the `String` used as messages.
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
                Right []  -> putStrLn $ "error: no transactions"
                Right txs -> putStrLn $ report $ summarize txs

main :: IO ()
main = program2
```
Here are examples of use
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
The expression using `>>=` in the `case .. of` instruction: 
```haskell
            ...
            content <- getfilecontent fp
            case (content >>= readtransactions) of
                left msg -> putstrln msg
                right []  -> putstrln $ "error: no transactions"
                right txs -> putstrln $ unlines $ map show $ summarize txs
```
can be completed with as many functions of the type `a -> Either Message b` as we want over the initial value of `content`. For example, we could add new controls to detect an empty transaction list, or to check that no transaction in the list has amount of zero.
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

Could it be possible to chain _all_ our `Either Message a` functions, like this ?

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
Answer: No. _ghc_ has no less than 6 complaints about this change to the function. Here's (an excerpt of) the first one:

```
    • Couldn't match type ‘Either Message String’ with ‘[Char]’
      Expected type: Either Message String -> IO (Either Message String)
        Actual type: FilePath -> IO (Either Message String)
86 |     case (getFileNameArg >>= getFileContent
   |                              ^^^^^^^^^^^^^^

```
In essence: we cannot chain monadic actions from the IO monad to the Either monad, and vice versa. Since the `case ... of` is examining a value of type `Either Message [Transaction]`, the expected type for actions leading to that value is `a -> Either Message b`. But we are trying to somehow get to that value through actions of type `a -> IO b`. That can't work.

We can __always__ bind monadic actions to distinct types through the __same monad__ m :
```
ghci ⏎
> readFile "data/transactions.csv" >>= putStrLn ⏎
Groceries, 100.00
Investment, 4807.00
. . .

readTransaction "Foo,4807" >>= checkNonZero ⏎
Right (Transaction {transactionCategory = Category {categoryLabel = "Foo"}, transactionAmount = 42.0})
it :: Either Message Transaction

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
> ```haskell
> newtype ExceptT e m a
> ```
> A monad transformer that adds exceptions to other monads.
> 
> `ExceptT` constructs a monad parameterized over two things:
> 
> - _e_ - The exception type.
> - _m_ - The inner monad.
> 
> The `return` function yields a computation that produces the given value, while `>>=` sequences two subcomputations, exiting on the first exception.

Let's experiment on _ghci_. We start with importing the module, and defining a type synonym for exceptions that will make the signatures clearer.

```
>  import Control.Monad.Trans.Except
>  type Message = String
type Message = String ⏎
```
Now we can create some `ExceptT` values:
```
>  foo = return "foo" :: ExceptT Message IO String
foo :: ExceptT Message IO String
>  minusone = return "-1" :: ExceptT Message IO String
minusone :: ExceptT Message IO String
>  fortytwo = return "42" :: ExceptT Message IO String
fortytwo :: ExceptT Message IO String
```

If we want to extract and print the `Either` value underlying the transformed monad, we can use `runExceptT :: ExceptT e m a -> m (Either e a)`. This function does the inverse `ExceptT`.
```
>  runExceptT foo
Right "foo"
it :: Either Message String
```
We can also create a monadic actions of type `a -> ExceptT`. 

This one will convert a string into a number, or fail doing so:
```
> readE s = case reads s of [] -> throwE "not a number" ; ((n,_):_) -> return n
readE :: (Read a, Monad m) => String -> ExceptT [Char] m a
```
Let's also create an action to check for positive numbers:
```
>  checkP n | n < 0 = throwE "negative number" ; checkP n | otherwise = return n
checkP :: (Ord a, Num a, Monad m) => a -> ExceptT [Char] m a
```
And now let's write a little computation, one that will 
- convert a `String` into a `Double` value, or fail
- check that this value is positive, or fail
- apply the `sqrt` to that value
```
> computation v = sqrt <$> (v >>= readE >>= checkP)
computation ::
  (Floating b, Monad m, Read b, Ord b) =>
  ExceptT [Char] m String -> ExceptT [Char] m b
```
Since we are applying a function to monadic value, we need to use `<$>` (`fmap` would work as well). Now we can use our computation on our different values:
```
>  runExceptT $ computation foo ⏎
Left "not a number"
it :: Either [Char] Double
>  runExceptT $ computation minusone ⏎
Left "negative number"
it :: Either [Char] Double
>  runExceptT $ computation fortytwo ⏎
Right 6.48074069840786
it :: Either [Char] Double
```
And see that it works as well as if we were using the Either Monad itself.

But what about the IO part of this monad transformation? Can we define an `ExceptT` value that would be obtained via the input stream for example ?
```
getLineE = return getLine :: ExceptT Message IO String ⏎
 error:
    • Couldn't match type ‘IO String’ with ‘[Char]’
      Expected type: ExceptT String IO String
        Actual type: ExceptT String IO (IO String)
```
We cannot do that: `getLine :: IO String` and `return :: (Monad m) => a -> m a`, so `return getLine :: (Monad m) => m (IO String)`, which gives an `IO` monadic value inside a new monad, when what we need is a value inside a transformed `IO` monad.

The function `lift :: (MonadTrans t, Monad m) => m a -> t m a` wich "lifts" a computation from the argument monad to the constructed monad has to come to our rescue:
```
> import Control.Monad.Trans.Class ⏎
getLineE = lift getLine ⏎
getLineE :: MonadTrans t => t IO String
```
Now we can try our computation on values obtained through interactions with IO instead of pure values:
```
> runExceptT $ computation getLineE ⏎


```
Of course, the evaluation is waiting for our input on the keyboard. Nothing will happen until we enter some characters.
```
bar ⏎
Left "not a number"
it :: Either [Char] Double
>  runExceptT $ computation getLineE ⏎
-42 ⏎
Left "negative number"
it :: Either [Char] Double
>  runExceptT $ computation getLineE ⏎
100 ⏎
Right 10.0
it :: Either [Char] Double
> 
```
And this is how Monad Transformers can help us seamlessly compose together monadic actions inside an IO monad that would also behave like an execption monad.

## 5. A new program

We can now adjust our little transaction summary program.


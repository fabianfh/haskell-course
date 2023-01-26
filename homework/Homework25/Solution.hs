
import Data.Char (isDigit)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask) 
import Control.Monad.Trans (MonadTrans(lift))
import Data.Maybe (isJust, fromJust)

{-
Question 1
Write a program that asks the user for some integer numbers,
checks if the input is correct and returns a list of factorials
of these numbers. Use a ReaderT transformer parameterized by a list
to calculate the list of factorials for the input numbers.  

HINT: Import the Control.Monad.Trans module and use the lift function.
-}

returnFactorials :: ReaderT [Integer] [] Integer
returnFactorials = do
    list <- ask
    element <- lift list
    let factorial = product [1..element]
    return factorial

main1 :: IO ()
main1 = do
    putStrLn "Input some integer numbers:"
    inp <- getLine
    let list = words inp
        check = all (all isDigit) list
    if check
    then do
        let numbers = map read list :: [Integer]
            factorials = runReaderT returnFactorials numbers
        putStrLn "The factorials of your list are:"
        print factorials
    else do
        putStrLn "Incorrect input. Try again."
        main1

{-
Question 2
Write a program that asks the user to input 3 integer numbers,
then checks if the input is correct and returns the sum of them.

Use a ReaderT transformer parameterized by a Maybe type to calculate
the sum of the tree numbers. If the input contains charaters the
ReaderT transformer should return a Nothing value. 

HINT: You can use also here the lift function from the previous question.
-}

possibleSum :: ReaderT [String] Maybe Int
possibleSum = do
    list <- ask
    let maybeNums = parseList list
    num1 <- lift (maybeNums !! 0)
    num2 <- lift (maybeNums !! 1)
    num3 <- lift (maybeNums !! 2)
    return $ num1 + num2 + num3

parseList :: [String] -> [Maybe Int]
parseList list = map go list 
  where go i | all isDigit i = Just (read i)
             | otherwise = Nothing

main :: IO ()
main = do
    putStrLn "Input three integer numbers:"
    inp <- getLine
    let list = words inp
    case length list of
      3 -> do
        let sum = runReaderT possibleSum list
        if isJust sum
        then do
            putStrLn "Sum of numbers is:"
            print $ fromJust sum
        else do
            putStrLn "Input only numbers. Try again."
            main
      _ -> do
        putStrLn "Input only 3 numbers:"
        main
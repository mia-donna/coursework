module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )

--TYPES
data Customer = Customer {
  name :: Name,
  balance :: Balance,
  account :: Account
} deriving (Eq, Show)

data Coin = Head | Tail deriving (Show, Eq)  

type Account = Int
type Balance =  Int
type Name = String
type Value = Int



-- RANDOM GENERATORS
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail


-- CUSTOMER THREAD PROCESS : trying to get HEAD and put it in a box
process :: Name -> Customer -> MVar Customer -> IO () 
process name cust custbox = do
    c1 <- coinFlip
    putStrLn $ name ++ "'s turn" 
    if c1 == Head then do
        putMVar custbox cust
    else do    
        threadDelay 100
        process name cust custbox

main :: IO ()
main = do
    -- CREATE CUSTOMERS
    putStrLn $ "4 customers created."
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}
    let c4 = Customer {name = "C4", balance = 100, account = 4}
    
    -- CREATE A BOX FOR EACH
    custbox <- newEmptyMVar 


    -- FORK THE PROCESS
    putStrLn $ "4 customer threads being created."
    mapM_ forkIO [process "C1" c1 custbox, process "C2" c2 custbox, process "C3" c3 custbox, process "C4" c4 custbox]
    putStrLn $ "4 threads all run."

    -- If we run , then take twice, we ALWAYS get 2 values, and ALWAYS different customers
    first <- takeMVar custbox
    print first
    second <- takeMVar custbox
    print second

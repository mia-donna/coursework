module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )

-- TYPES
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
-- COIN
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

-- AMOUNT TO TRANSFER  
randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r

-- TRANSFER FUNCTION
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to)
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))    



-- CUSTOMER THREAD PROCESS : trying to get HEAD and put it in a box
process :: Name -> Customer -> MVar Customer -> MVar Value -> IO () 
process name cust custbox value = do
    c1 <- coinFlip
    -- putStrLn $ name ++ "'s turn" || - - enable this to see how many turn's it takes for each Customer to get Head and be entered into the raffle
    if c1 == Head then do
        putMVar custbox cust
        amount <- randomAmount
        putMVar value amount
    else do    
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        process name cust custbox value

main :: IO ()
main = do
    -- CREATE CUSTOMERS
    putStrLn $ ".******------ WELCOME! ------******."
    putStrLn $ ".******------ WE WILL RUN 10 RANDOM TRANSACTIONS OF RANDOM AMOUNTS OF RANDOM TIMINGS BETWEEN...YOU GUESSED IT! 5 RANDOMLY SELECTED PAIRS OF CUSTOMERS ------******."
    putStrLn $ "10 customers created."
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}
    let c4 = Customer {name = "C4", balance = 100, account = 4}
    let c5 = Customer {name = "C5", balance = 100, account = 5}
    let c6 = Customer {name = "C6", balance = 100, account = 6}
    let c7 = Customer {name = "C7", balance = 100, account = 7}
    let c8 = Customer {name = "C8", balance = 100, account= 8}
    let c9 = Customer {name = "C9", balance = 100, account = 9}
    let c10 = Customer {name = "C10", balance = 100, account = 10}
    
    -- CREATE A BOX FOR EACH
    custbox <- newEmptyMVar
    value <- newEmptyMVar 

    -- CREATE A VALUE FOR EACH 

    -- FORK THE PROCESS
    putStrLn $ "10 customer threads being created."
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    mapM_ forkIO [process "C1" c1 custbox value, process "C2" c2 custbox value, process "C3" c3 custbox value, process "C4" c4 custbox value, process "C5" c5 custbox value, process "C6" c6 custbox value,process "C7" c7 custbox value, process "C8" c8 custbox value, process "C9" c9 custbox value, process "C10" c10 custbox value ]
    putStrLn $ "threads all finished running."

    -- If we run , then take twice, we ALWAYS get 2 values, and ALWAYS different customers
    -- and this way they all run - and it's first two out the box

    v <- takeMVar value
    putStrLn $ "RANDOM VALUE IS: " ++ (show v)

    recipient <- takeMVar custbox 
    let print_name = print . name

    let reveal_recipient = print_name recipient
    putStrLn $ "RANDOM RECIPIENT | PAIR 1: "  
    reveal_recipient
    
    payee <- takeMVar custbox
    let reveal_payee = print_name payee
    putStrLn $ "RANDOM PAYEE | PAIR 1: "  
    reveal_payee

    recipient2 <- takeMVar custbox
    let reveal_recipient2 = print_name recipient2
    putStrLn $ "RANDOM RECIPIENT | PAIR 2: "  
    reveal_recipient2
    
    payee2 <- takeMVar custbox
    let reveal_payee2 = print_name payee2
    putStrLn $ "RANDOM PAYEE | PAIR 2: "  
    reveal_payee2

    recipient3 <- takeMVar custbox
    let reveal_recipient3 = print_name recipient3
    putStrLn $ "RANDOM RECIPIENT| PAIR 3: "  
    reveal_recipient3
    
    payee3 <- takeMVar custbox
    let reveal_payee3 = print_name payee3
    putStrLn $ "RANDOM PAYEE | PAIR 3: "  
    reveal_payee3

    recipient4 <- takeMVar custbox
    let reveal_recipient4 = print_name recipient4
    putStrLn $ "RANDOM RECIPIENT | PAIR 4: "  
    reveal_recipient4
    
    payee4 <- takeMVar custbox
    let reveal_payee4 = print_name payee4
    putStrLn $ "RANDOM PAYEE | PAIR 4: "  
    reveal_payee4

    recipient5 <- takeMVar custbox
    let reveal_recipient5 = print_name recipient5
    putStrLn $ "RANDOM RECIPIENT | PAIR 5: "  
    reveal_recipient5
    
    payee5 <- takeMVar custbox
    let reveal_payee5 = print_name payee5
    putStrLn $ "RANDOM PAYEE | PAIR 5: "  
    reveal_payee5

-- ||  TRANSFERS AND OUTPUT OF BALANCES
    putStrLn $ ".******------ TRANSFERS ------******."
    -- PAIR 1
    (payee, recipient) <- transfer payee recipient v
    putStrLn $ "****TRANSFER****  RECIPIENT DETAILS: " ++ (show recipient) ++ " PAYEE DETAILS : " ++ (show payee)
    -- PAIR 2
    (payee2, recipient2) <- transfer payee2 recipient2 v
    putStrLn $ "****TRANSFER****  RECIPIENT DETAILS: " ++ (show recipient2) ++ " PAYEE DETAILS : " ++ (show payee2)
    -- PAIR 1
    (payee3, recipient3) <- transfer payee3 recipient3 v
    putStrLn $ "****TRANSFER****  RECIPIENT DETAILS: " ++ (show recipient3) ++ " PAYEE DETAILS : " ++ (show payee3)
    -- PAIR 1
    (payee4, recipient4) <- transfer payee4 recipient4 v
    putStrLn $ "****TRANSFER****  RECIPIENT DETAILS: " ++ (show recipient4) ++ " PAYEE DETAILS : " ++ (show payee4)
    -- PAIR 1
    (payee5, recipient5) <- transfer payee5 recipient5 v
    putStrLn $ "****TRANSFER****  RECIPIENT DETAILS: " ++ (show recipient5) ++ " PAYEE DETAILS : " ++ (show payee5)
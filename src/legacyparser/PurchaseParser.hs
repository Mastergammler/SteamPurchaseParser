module PurchaseParser where 

import Data.List
import System.Environment

    -------------
    --  TYPES  --
    -------------

type Transaction = [String]
type TransactionList = [Transaction]

-----------------
--  CONSTANTS  --
-----------------

_PRUCHASE_START_STRING = "Einkauf"
_STEAM_CREDIT_STRING = "Purchase"
_CURRENCY_STRING = "EUR"
_MSG_STEAM_ACCOUNT_SPENT = "all of Steam"

_QM_CHAR = '?'
_COMMMA_CHAR = ','
_POINT_CHAR = '.'
_MINUS_CHAR = '-'
_ZERO_CHAR = '0'

_COND_STEAM_CREDIT item = not (_STEAM_CREDIT_STRING `isPrefixOf` (item !! 1))
_COND_STRING_MATCH str item = str `isPrefixOf` (item !! 1)

    ------------
    --  MAIN  --
    ------------

main = do
    purchasesPlain <- getContents
    args <- getArgs
    let pList = parsePlain purchasesPlain
        nonCrList = filterPurchases pList _COND_STEAM_CREDIT
        formatted = formatAmount nonCrList
    handleArgs args nonCrList
    showMoney nonCrList _MSG_STEAM_ACCOUNT_SPENT

    ----------------------
    --  CORE FUNCTIONS  --
    ----------------------

handleArgs :: [String] -> TransactionList -> IO ()
handleArgs [] _ = return ()
handleArgs (x:xs) tList = do 
    showMoney filtered x 
    handleArgs xs tList 
    where filtered = filterPurchases tList (_COND_STRING_MATCH x)

showMoney :: TransactionList -> String -> IO ()
showMoney tList for = putStrLn $ "Money spent on " ++ for ++ ": " ++ formattedAmount
    where formattedAmount = formatNumberStr rawAmount 0 ++ " " ++ _CURRENCY_STRING
          rawAmount = show . addPurchases $ tList

addPurchases :: TransactionList -> Double
addPurchases [] = 0 
addPurchases (item : tList) = itemAmount + addPurchases tList
    where itemAmount = read . last . formatAmountLine $ item

filterPurchases :: TransactionList -> (Transaction -> Bool) -> TransactionList
filterPurchases [] _ = []
filterPurchases (item : tList) condition = 
    if condition item 
        then item : filterPurchases tList condition
        else filterPurchases tList condition

    -------------------------
    --  UTILITY FUNCTIONS  --
    -------------------------

formatAmount :: TransactionList -> TransactionList
formatAmount [] = []
formatAmount (item : tList) = map formatAmountLine tList

formatAmountLine :: Transaction -> Transaction
formatAmountLine [] = []
formatAmountLine trans = init trans ++ [replaceSymbols . head . words $ last trans]

formatNumberStr :: String -> Int -> String
formatNumberStr [] _ = []
formatNumberStr list (-1) = list
formatNumberStr (x:xs) num = 
    if x == _POINT_CHAR
        then  [x,safeString xs 0,safeString xs 1]
        else x : formatNumberStr xs num

replaceSymbols :: String -> String
replaceSymbols = replace _MINUS_CHAR _ZERO_CHAR . replace _COMMMA_CHAR _POINT_CHAR . remove _QM_CHAR

parsePlain :: String -> TransactionList
parsePlain "" = error "No file is empty!"
parsePlain content = tail . parseFromLines . lines $ content

{- !! adds a empty list to the start of the  list, that has to be removed !! -}
parseFromLines :: [String] -> TransactionList
parseFromLines [] = []
parseFromLines lines = 
    if isDateLine curLine
        then [] : (curLine : item) : tList
        else (curLine : item) : tList
    where curLine = head lines
          restLines = safeTail lines
          item = safeHead . parseFromLines $ restLines
          tList = safeTail . parseFromLines $ restLines

isDateLine :: String -> Bool
isDateLine [] = False
isDateLine line = baseCond && (fstCond && sndCond && trdCond)
    where split = words line
          baseCond = length split >= 3
          fstCond = length (head split) == 2 || length (head split) == 1      
          sndCond = length (split !! 1) >= 3
          trdCond = length (split !! 2) == 4

remove :: Char -> String -> String
remove char [] = []
remove char (c:str) = 
        if c == char 
            then remove char str
            else c : remove char str 

replace :: Char -> Char -> String -> String 
replace old new [] = []
replace old new (c:str) = 
        if c == old
            then new : replace old new str
            else c : replace old new str

safeHead :: [[a]] -> [a]
safeHead [] = []
safeHead list = head list

safeTail :: [a] -> [a]
safeTail [] = []
safeTail list = tail list

safeString :: String -> Int -> Char
safeString list num =
    if length list <= num
        then '0'
        else list !! num
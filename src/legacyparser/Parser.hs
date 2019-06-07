--module SteamParser.SteamParser where 
  
import Data.List

_PURCHASE_START_STRING = "Einkauf"
_REMOVE_CHAR = '?'
_REPLACE_CHAR = ','
_NEW_CHAR = '.'
_EMPTY_LINE = ""
_REPLACE_LINE = '-'
_REPLACE_ZERO = '0'
_STEAM_CREDIT_STR = "Steam-Guthaben"

type PurchItem = [String]
type PurchList = [PurchItem]

      -------------
      --  DEBUG  --
      -------------

printListLasts :: PurchList -> IO ()
printListLasts list = print lastList
      where lastList = getLasts list
            getLasts :: PurchList -> [String]
            getLasts [] = []
            getLasts xs = map (replace _REPLACE_CHAR _NEW_CHAR . last) xs

debugRemove :: PurchList -> IO ()
debugRemove list = print $ remove _REMOVE_CHAR (head . words . last . head $ list)

      ------------
      --  MAIN  --
      ------------

main = do
      content <- getContents
      let list = parseToList $ lines content
          filterL = filterItems list _STEAM_CREDIT_STR
          filterF = filterFor filterL "Hacknet"
      putStrLn $ "Money spent: " ++ show (countMoney filterF) ++ " Euro."

countMoney :: PurchList -> Double
countMoney [] = 0
countMoney (x:xs) = moneyDouble + countMoney xs 
      where moneyLine = last x 
            moneyString = head . words $ moneyLine
            moneyDouble = read $ replace _REPLACE_LINE _REPLACE_ZERO (replace _REPLACE_CHAR _NEW_CHAR (remove _REMOVE_CHAR moneyString))

parseToList :: [String] -> [PurchItem]
parseToList [] = []
parseToList content = if curLine == _EMPTY_LINE
                        then [] : parseToList nextLines
                        else (curLine : item) : purchList
    where curLine = head content
          nextLines = safeTail content
          item = safeHead . parseToList $ nextLines
          purchList = safeTail . parseToList $ nextLines 


filterItems :: PurchList -> String -> PurchList
filterItems [] _ = []
filterItems (x:xs) name = 
      if isSteamCredit
            then restList
            else x : restList
      where restList = filterItems xs name
            isSteamCredit = name == (head . words $ x !! 1) 

filterFor :: PurchList -> String -> PurchList
filterFor [] _ = []
filterFor (x:xs) name = 
      if startsWith
            then x : restList
            else restList
      where restList = filterFor xs name
            startsWith =  name `isPrefixOf` (x !! 1)

      ---------------
      --  UTILITY  --
      ---------------

remove :: Char -> String -> String
remove char [] = []
remove char full@(c:str) = 
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
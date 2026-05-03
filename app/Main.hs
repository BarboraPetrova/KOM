{-# LANGUAGE DatatypeContexts #-}

module Main where
import Data.List (sort, elemIndex)
import Data.Maybe(fromJust)


newtype (Show a) => Table a = Table [[a]]

instance (Show a) => Show (Table a) where 
    show (Table x) = concatMap ((++ "\n") . show) x 

-- komprese: 
--      soubor -> bytes -> BWT -> MTF -> RLE -> výstup
--      [Byte] → BWT → [Byte] → MTF → [Int] → RLE → [(Int,Int)]
-- dekomprese: 
--      výstup -> RLE decode -> MTF decode -> BWT inverse -> původní data
--      [(Int,Int)] → RLE → [Int] → MTF → [Byte] → BWT → [Byte]

-- rotování seznamu o jeden prvek
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
rotate [] = []

createTable :: Ord a => [a] -> [[a]]
createTable str = sort $ take (length str) (iterate rotate str)


bwt :: [Char] -> ([Char], Int)
bwt str = (lastCol, index)
    where 
        table = createTable str
        lastCol = map last table
        index = fromJust $ elemIndex str table



{-
bwt :: [Char] -> ([Char], Int)
bwt s = (lastColumn, index)
    where
        rotation = take (length s) (iterate rotate s)
        sorted = sort rotation
        lastColumn = map last sorted
        index = fromJust $ elemIndex s sorted

ibwt :: ([String], Int) -> [String]
ibwt (lastColumn, index) = s
    where 
        n = length lastColumn -- posledni
        sorted = sort lastColumn -- prvni
        
-- tohle musím dodělat
-}

fj :: Maybe a -> a
fj (Just x) = x

main :: IO ()
main = print $ bwt "pepapapapapu"

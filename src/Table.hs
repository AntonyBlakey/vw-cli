module Table
       ( Table
       , Row(Title, Body, Separator)
       , Cell
       , clearRepeatingCells
       , Table.print
       ) where


import Data.List (transpose)


type Table = [Row]
data Row = Title [Cell] | Body [Cell] | Separator | Header
type Cell = String


clearRepeatingCells :: [Bool] -> Table -> Table
clearRepeatingCells mask = helper []
  where
    infiniteMask = mask ++ repeat False
    helper lastCells (Body cells : rest) = Body (clearDuplicates lastCells cells) : helper cells rest
    helper _ (h : t)                     = h : helper [] t
    helper _ []                          = []
    clearDuplicates old = zipWith3 (\m a b -> if m && a == b then "" else b) infiniteMask (old ++ repeat "")


print :: Table -> IO ()
print table = do

  mapM_ printRow canonicalTable
  printSeparatorRow '└' '─' '┴' '┘'

  where

    canonicalTable = padRows (columnCount table) $ simplify $ Header : table

    widths = columnWidths canonicalTable

    printBoldContentRow :: Char -> [String] -> IO ()
    printBoldContentRow border =
      putStrLn . foldr (\(width, cell) tl -> border : " " ++ padRight width cell ' ' ++ " " ++ tl) [border] . zip widths

    printContentRow :: Char -> [String] -> IO ()
    printContentRow border =
      putStrLn . foldr (\(width, cell) tl -> border : " " ++ padRight width cell ' ' ++ " " ++ tl) [border] . zip widths

    printSeparatorRow :: Char -> Char -> Char -> Char -> IO ()
    printSeparatorRow left line middle right =
      putStrLn $ (left :) $ drop 1 $ foldr (\width tl -> middle : replicate (2 + width) line ++ tl) [right] widths

    printRow :: Row -> IO ()
    printRow (Title x) = do
      printSeparatorRow '┏' '━' '┳' '┓'
      printBoldContentRow '┃' x
      printSeparatorRow '┡' '━' '╇' '┩'
    printRow (Body x) = printContentRow '│' x
    printRow Header = printSeparatorRow '┌' '─' '┬' '┐'
    printRow Separator = printSeparatorRow '├' '─' '┼' '┤'


simplify :: Table -> Table
simplify (Header : title@(Title _) : rest)    = simplify $ title : rest
simplify (Header : Separator : rest)          = simplify $ Header : rest
simplify (title@(Title _) : Separator : rest) = simplify $ title : rest
simplify (Separator : Separator : rest)       = simplify $ Separator : rest
simplify [Separator]                          = []
simplify (h : rest)                           = h : simplify rest
simplify []                                   = []

columnWidths :: Table -> [Int]
columnWidths = map maxLength . transpose . rowsWithCells

columnCount :: Table -> Int
columnCount = maxLength . rowsWithCells

padRows :: Int -> Table -> Table
padRows size = map padRow
  where
    padRow (Title cells) = Title $ padRight size cells ""
    padRow (Body cells)  = Body $ padRight size cells ""
    padRow x             = x

rowsWithCells :: Table -> [[Cell]]
rowsWithCells = foldr collectCells []
  where
    collectCells (Title cells) accum = cells : accum
    collectCells (Body cells) accum = cells : accum
    collectCells _  accum = accum

maxLength :: (Foldable x, Foldable y) => x (y z) -> Int
maxLength = foldr (max . length) 0

padRight :: Int -> [a] -> a -> [a]
padRight size list pad = list ++ replicate (size - length list) pad

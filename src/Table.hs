module Table
       ( Table
       , Row(Title, Body, Separator, Empty)
       , Cell
       , clearRepeatingCells
       , Table.format
       ) where


import Data.List (transpose)


type Table = [Row]
data Row = Title [Cell] | Body [Cell] | Separator | Empty | Header | Footer
type Cell = String


clearRepeatingCells :: [Bool] -> Table -> Table
clearRepeatingCells mask = helper []

  where

    infiniteMask = mask ++ repeat False

    clearDuplicates old = zipWith3 (\m a b -> if m && a == b then "" else b) infiniteMask (old ++ repeat "")

    helper lastCells (Body cells : rest) = Body (clearDuplicates lastCells cells) : helper cells rest
    helper _ (h : t)                     = h : helper [] t
    helper _ []                          = []


format :: Table -> [String]
format table = foldMap formatRow canonicalTable

  where

    canonicalTable = padRows (columnCount table) $ simplify $ Header : table ++ [Footer]

    widths = columnWidths canonicalTable

    contentRow :: Char -> [String] -> String
    contentRow border =
      foldr (\(width, cell) tl -> border : " " ++ padRight width cell ' ' ++ " " ++ tl) [border] . zip widths

    separatorRow :: Char -> Char -> Char -> Char -> String
    separatorRow left line middle right =
      (left :) $ drop 1 $ foldr (\width tl -> middle : replicate (2 + width) line ++ tl) [right] widths

    formatRow :: Row -> [String]
    formatRow Empty     = undefined
    formatRow (Title x) = [separatorRow '┏' '━' '┳' '┓', contentRow '┃' x, separatorRow '┡' '━' '╇' '┩']
    formatRow (Body x)  = [contentRow '│' x]
    formatRow Header    = [separatorRow '┌' '─' '┬' '┐']
    formatRow Separator = [separatorRow '├' '─' '┼' '┤']
    formatRow Footer    = [separatorRow '└' '─' '┴' '┘']


simplify :: Table -> Table
simplify (Empty : rest)                       = simplify rest
simplify (Header : title@(Title _) : rest)    = simplify $ title : rest
simplify (Header : Separator : rest)          = simplify $ Header : rest
simplify (title@(Title _) : Separator : rest) = simplify $ title : rest
simplify (Separator : Separator : rest)       = simplify $ Separator : rest
simplify (Separator : rest@[Footer])          = rest
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

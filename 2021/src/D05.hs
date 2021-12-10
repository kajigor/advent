{-# LANGUAGE TupleSections #-}

module D05 where

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad ( void )
import Text.Megaparsec.Char ( digitChar, spaceChar )
import Text.Megaparsec ( runParser, errorBundlePretty, many, some, Parsec, Parsec )
import Text.Printf ( printf )
import Data.Void ( Void(..) )
import Data.Either ( fromRight )
import Data.List ( group, sort )

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Integer
number = read <$> L.lexeme sc (some digitChar)

parser :: String -> Either String [Vent]
parser =
    runBundlingParser (many parseVent) ""
  where
    runBundlingParser parser filePath =
        mapLeft errorBundlePretty . runParser parser filePath
      where
        mapLeft f (Left x) = Left $ f x
        mapLeft _ (Right x) = Right x

parseCoord :: Parser Coord
parseCoord = do
  x <- number
  symbol ","
  y <- number
  return (x, y)

parseVent :: Parser Vent
parseVent = do
  x <- parseCoord
  symbol "->"
  y <- parseCoord
  return (x, y)

type Coord = (Integer, Integer)
type Vent = (Coord, Coord)

parse :: String -> [Vent]
parse str =
  fromRight [] (parser str)

task1 :: String -> Int
task1 str =
    let vents = parse str in
    let relevant = filter (\x -> horizontal x || vertical x) vents in
    let coords = map fillIn relevant in
    length $ filter (>1) $ map length $ group $ sort $ concat coords

task2 :: String -> Int
task2 str =
    let vents = parse str in
    let relevant = filter (\x -> horizontal x || vertical x || diagonal x) vents in
    let coords = map fillIn relevant in
    length $ filter (>1) $ map length $ group $ sort $ concat coords

horizontal ((x1, _), (x2, _)) = x1 == x2
vertical ((_, y1), (_, y2)) = y1 == y2
diagonal ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

fillIn ((x1, y1), (x2, y2)) | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2]
                            | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2]
                            | otherwise = zip (range x1 x2) (range y1 y2)
range x y | x < y = [x .. y]
range x y = [x, (x-1) .. y]




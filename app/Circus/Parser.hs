module Circus.Parser where

import Circus.Types
import Data.Map qualified as M
import Data.Maybe
import Data.Vector qualified as V
import Text.Parsec
import Text.Parsec.String

-- | Парсер банка
bank :: Parser Bank
bank = M.fromList <$> many bankLine
  where
    bankLine = (,) <$> manyTill alphaNum (char ':') <*> shapes <* (char ';' >> many (char '\n'))
    shapes = shape `sepBy1` char '+'
    shape = try rectP <|> try circP <|> try lineP <|> try ssP <|> try scP
    num = do
      spaces
      neg <- option "" (string "-")
      n <- many1 digit
      d <- option "" ((:) <$> char '.' <*> many1 digit)
      pure $ read (neg <> n <> d)
    rectP = Rect <$> (string "rect" *> num) <*> num
    circP = Circ <$> (string "circ" *> num)
    ssP = SimpleSquare <$ string "ss"
    scP = SimpleCircle <$ string "sc"
    lineP = Line <$> (string "line" *> num) <*> num <*> num <*> num

-- | Парсер рисунка
drawing :: Bank -> GridConfig -> Parser Drawing
drawing bk gc = do
  els <- many (el <* char ';')
  spaces
  let minX = minimum $ fmap (\(_, (_, x, _), _) -> x) els
      minY = minimum $ fmap (\(_, (_, _, y), _) -> y) els
      elsNorm = fmap (\(n, (e, x, y), r) -> (n, (e, x - minX + 1, y - minY + 1), r)) els
  ls <- many (link elsNorm <* char ';')
  pure $
    Drawing
      { matrix =
          foldl
            (\c (name, (e, x, y), r) -> vupd c x y (Just (PlacedElement name e r)))
            (V.replicate (gridHeight gc) (V.replicate (gridWidth gc) Nothing))
            elsNorm,
        links = ls,
        gridConfig = gc
      }
  where
    vupd vec row col newValue =
      vec V.// [(row, (vec V.! row) V.// [(col, newValue)])]
    alpha = ['A' .. 'Z']
    el = do
      nm <- many1 $ oneOf alpha
      nn <- many1 digit
      _ <- char '('
      x <- read <$> many1 digit
      _ <- char ','
      y <- read <$> many1 digit
      r <-
        (char ')' >> pure 0)
          <|> ( do
                  _ <- char ','
                  read <$> many1 digit <* char ')'
              )
      pure (nm <> nn, (bk M.! nm, x * 3, y * 3), r)
    link :: [(String, (Element, Int, Int), Int)] -> Parser Link
    link els' = do
      n1 <- many1 alphaNum
      _ <- char '('
      d1 <- direction
      _ <- string ")-"
      n2 <- many1 alphaNum
      _ <- char '('
      d2 <- direction
      _ <- char ')'
      let els = fmap (\(x, (_, y, z), r) -> (x, ((y, z), r))) els'
          (cs1, r1) = fromJust $ lookup n1 els
          (cs2, r2) = fromJust $ lookup n2 els
      pure $
        Link
          cs1
          (iterate cyclicSucc d1 !! r1)
          cs2
          (iterate cyclicSucc d2 !! r2)
    direction =
      choice
        [ L <$ char 'L',
          R <$ char 'R',
          U <$ char 'U',
          D <$ char 'D'
        ]

fromRight :: (Show l) => Either l r -> r
fromRight (Right r) = r
fromRight (Left l) = error $ show l

parseDrawing :: String -> String -> IO Drawing
parseDrawing = parseDrawingWithGrid defaultGrid

parseDrawingWithGrid :: GridConfig -> String -> String -> IO Drawing
parseDrawingWithGrid gc bfname dfname = do
  br <- readFile bfname
  dr <- readFile dfname
  let bk = fromRight $ parse bank "" br
  pure $ fromRight $ parse (drawing bk gc) "" dr

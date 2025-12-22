module Circus.Router where

import Circus.Types
import Control.Monad (filterM)
import Control.Monad.State
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector ((!))

data Point = Point Int Int deriving (Show, Eq, Ord)

data Segment = Seg Point Point deriving (Show, Eq, Ord)

type Wire = [Segment]

data Grid = Grid
  { gw :: Int,
    gh :: Int,
    blocked :: Set Point,
    used :: Set Segment
  }
  deriving (Show)

type Router a = State Grid a

-- | Создание сетки из распаршенного Drawing
mkGrid :: Drawing -> Grid
mkGrid dr = Grid (2 * w + 1) (2 * h + 1) blocks S.empty
  where
    gc = gridConfig dr
    w = gridWidth gc
    h = gridHeight gc
    m = matrix dr
    blocks =
      S.fromList
        [ Point (2 * x) (2 * y)
        | x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          isJust (m ! x ! y)
        ]

-- | Взятие точки выхода из элемента
exit :: (Int, Int) -> Direction -> Point
exit (x, y) U = Point (2 * x) (2 * y + 1)
exit (x, y) D = Point (2 * x) (2 * y - 1)
exit (x, y) L = Point (2 * x - 1) (2 * y)
exit (x, y) R = Point (2 * x + 1) (2 * y)

-- | Хелпер создания сегмента
seg :: Point -> Point -> Segment
seg a b = if a <= b then Seg a b else Seg b a

-- | Получение близжайших валидных точек
near :: Point -> Router [Point]
near (Point x y) = do
  g <- get
  let ps = [Point (x - 1) y, Point (x + 1) y, Point x (y - 1), Point x (y + 1)]
      ok (Point px py) = px >= 0 && px < gw g && py >= 0 && py < gh g
  pure $ filter ok ps

-- | Проверка на свободность клетки
canGo :: Point -> Point -> Router Bool
canGo a b = do
  g <- get
  let s = seg a b
      taken = S.member s (used g)
      hitsBlock = S.member a (blocked g) || S.member b (blocked g)
  pure $ not taken && not hitsBlock

-- | BFS-обход
bfs :: Point -> Point -> Router (Maybe [Point])
bfs start end = go [(start, [start])] (S.singleton start)
  where
    go :: [(Point, [Point])] -> S.Set Point -> Router (Maybe [Point])
    go [] _ = pure Nothing
    go ((cur, path) : rest) seen
      | cur == end = pure $ Just (reverse path)
      | otherwise = do
          ok <- filterM (canGo cur) =<< near cur
          let new = filter (`S.notMember` seen) ok
              seen' = foldl' (flip S.insert) seen new
          go (rest ++ [(p, p : path) | p <- new]) seen'

-- | Превращение точек в сегменты
toWire :: [Point] -> Wire
toWire (a : b : ps) = seg a b : toWire (b : ps)
toWire _ = []

-- | Пометить все сегменты провода как занятые
occupy :: Wire -> Router ()
occupy w = modify $ \g -> g {used = foldl' (flip S.insert) (used g) w}

-- | Провести Связь
route :: Link -> Router (Maybe (Wire, Link))
route lnk@(Link p1 d1 p2 d2) = do
  let a = exit p1 d1
      b = exit p2 d2
  path <- bfs a b
  case path of
    Nothing -> pure Nothing
    Just ps -> do
      let w = toWire ps
      occupy w
      pure $ Just (w, lnk)

-- | Проводка всех связей
routeAll :: [Link] -> Router [(Wire, Link)]
routeAll ls = catMaybes <$> traverse route ls

-- | Запуск роутера
runRouter :: Drawing -> [(Wire, Link)]
runRouter dr = evalState (routeAll (links dr)) (mkGrid dr)

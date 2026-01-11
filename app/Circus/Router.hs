module Circus.Router where

import Circus.Types
import Control.Monad (filterM)
import Control.Monad.State
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector ((!))
import GHC.Stack (HasCallStack)

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
    ls = links dr
    points = concatMap (\l -> [p1 l, p2 l]) ls
    directions = concatMap (\l -> [c1 l, c2 l]) ls
    exits = zipWith exit points directions
    gc = gridConfig dr
    w = gridWidth gc
    h = gridHeight gc
    m = matrix dr
    blocks =
      S.fromList $
        [ Point (2 * x) (2 * y)
        | x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          isJust (m ! x ! y)
        ]
          ++ exits

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
canGo :: Point -> Point -> Point -> Point -> Router Bool
canGo start end a b = do
  g <- get
  let s = seg a b
      taken = S.member s (used g)
      hitsBlock = S.member a (blocked g) || S.member b (blocked g)
  pure $ not taken && (not hitsBlock || b == end || a == start)

-- | BFS-обход
bfs :: Point -> Point -> Router (Maybe [Point])
bfs start end = go [(start, [start])] (S.singleton start)
  where
    go :: [(Point, [Point])] -> S.Set Point -> Router (Maybe [Point])
    go [] _ = pure Nothing
    go ((cur, path) : rest) seen
      | cur == end = pure $ Just (reverse path)
      | otherwise = do
          ok <- filterM (canGo start end cur) =<< near cur
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

isHorizontal, isVertical :: Segment -> Bool
isHorizontal (Seg (Point _ ay) (Point _ by)) = ay == by
isVertical (Seg (Point ax _) (Point bx _)) = ax == bx

-- | Провести Связь
route :: (HasCallStack) => Link -> Router (Maybe (Wire, Link))
route lnk@(Link p1 d1 p2 d2) = do
  let a = exit p1 d1
      b = exit p2 d2
  path <- bfs a b
  case path of
    Nothing -> pure Nothing
    Just ps -> do
      let w = toWire $ ps
      occupy w
      mapM_ occupy
        . map (uncurry reverseRotation)
        . filter (uncurry isRotation)
        $ zip w (drop 1 w)
      pure $ Just (w, lnk)
  where
    isRotation :: Segment -> Segment -> Bool
    isRotation
      (Seg (Point ax ay) (Point bx by))
      (Seg (Point cx cy) (Point dx dy)) =
        ax == bx && cy == dy || ay == by && cx == dx
    reverseRotation :: Segment -> Segment -> [Segment]
    reverseRotation
      s1@(Seg (Point ax ay) (Point bx by))
      (Seg (Point cx cy) (Point dx dy)) =
        concat
          [ [ if dx == bx
                then seg (Point bx ay) (Point (bx + 1) ay)
                else seg (Point (ax - 1) ay) (Point ax ay),
              if cy == ay
                then seg (Point cx (cy - 1)) (Point cx cy)
                else seg (Point dx (dy + 1)) (Point dx dy)
            ]
          | isHorizontal s1
          ]
          ++ concat
            [ [ if bx == dx
                  then seg (Point (dx + 1) cy) (Point dx cy)
                  else seg (Point (cx - 1) cy) (Point cx cy),
                if ay == cy
                  then seg (Point ax (ay - 1)) (Point ax ay)
                  else seg (Point bx (by + 1)) (Point bx by)
              ]
            | isVertical s1
            ]

-- | Проводка всех связей
routeAll :: [Link] -> Router [(Wire, Link)]
routeAll ls = catMaybes <$> mapM route ls

-- | Запуск роутера
runRouter :: Drawing -> [(Wire, Link)]
runRouter dr = evalState (routeAll (links dr)) (mkGrid dr)

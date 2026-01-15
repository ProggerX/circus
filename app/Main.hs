{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Circus.Parser
import Circus.Router
import Circus.Types
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Data.Function
import Data.Vector ((!))
import Data.Vector.Storable qualified as V
import Data.Word (Word8)
import Foreign.ForeignPtr
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Rendering.OpenGL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import System.Environment

fi :: Int -> Float
fi = fromIntegral

rect :: Float -> Float -> Picture
rect a b = lineLoop $ rectanglePath a b

picture :: Drawing -> Picture
picture dr = Pictures [elements, wires]
  where
    gc = gridConfig dr
    w = gridWidth gc
    h = gridHeight gc
    cs = cellSize gc
    es = elementSize gc
    elements = Pictures $ row <$> [0 .. h - 1]
    row i = Pictures $ dsp i <$> [0 .. w - 1]
    dsp :: Int -> Int -> Picture
    dsp i j = translate (fi i * cs) (fi j * cs) $ case (matrix dr ! i ! j) of
      Nothing -> mempty
      Just el -> rotate (fi $ elRot el * 90) $ Pictures $ drawShape <$> elType el
    drawShape = \case
      SimpleSquare -> rect es es
      SimpleCircle -> circle (es / 2)
      Circus.Types.Line x1 y1 x2 y2 -> line [(x1, y1), (x2, y2)]
      Circ r -> circle r
      Rect rw rh -> rect rw rh
    wires = Pictures $ wireWithMarkers <$> runRouter dr
    wireWithMarkers (segs, Link pos1 dir1 pos2 dir2) =
      Pictures $
        (segToLine <$> segs)
          ++ [dirMarker dir1 (connPoint pos1 dir1), dirMarker dir2 (connPoint pos2 dir2)]
    segToLine (Seg (Point x1 y1) (Point x2 y2)) =
      line [(toPixel x1 y1), (toPixel x2 y2)]
    toPixel x y = (fi x * cs / 2, fi y * cs / 2)
    dirMarker dir (px, py) = translate px py $ arrow dir
    arrow dir =
      (circleSolid 3.5) & uncurry
        translate
        case dir of
          U -> (0, 3.5)
          D -> (0, -3.5)
          L -> (-3.5, 0)
          R -> (3.5, 0)
    -- arrow U = polygon [(0, 6), (-4, -2), (4, -2)]
    -- arrow D = polygon [(0, -6), (-4, 2), (4, 2)]
    -- arrow L = polygon [(-6, 0), (2, -4), (2, 4)]
    -- arrow R = polygon [(6, 0), (-2, -4), (-2, 4)]
    connPoint (x, y) dir =
      let cx = fi x * cs
          cy = fi y * cs
       in case dir of
            R -> (cx + es / 2, cy)
            L -> (cx - es / 2, cy)
            U -> (cx, cy + es / 2)
            D -> (cx, cy - es / 2)

main :: IO ()
main = do
  p <- parseDrawing "elements.txt" "example.txt"
  putStrLn $ "Links: " ++ show (links p)
  putStrLn $ "Wires: " ++ show (runRouter p)

  let pic = picture p
  args <- getArgs
  case args of
    ("save" : _) -> toFile pic
    _ -> display (InWindow "Window" (800, 800) (20, 20)) white pic

toFile :: Picture -> IO ()
toFile pic = do
  rs <- initState

  let w = 2048
      h = 2048
      pos = GL.Position 0 0

  True <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'Visible False)
  mwin <- GLFW.createWindow w h "Export" Nothing Nothing
  case mwin of
    Nothing -> error "fuck"
    Just win ->
      GLFW.makeContextCurrent (Just win)

  displayPicture (w, h) (makeColor 255 255 255 0) rs 1.0 (scale 1.2 1.2 $ translate (-400) (-400) pic)
  saveFrame "output.png" pos (fromIntegral w) (fromIntegral h)

saveFrame :: FilePath -> GL.Position -> Int -> Int -> IO ()
saveFrame path pos w h = do
  let nBytes = w * h * 4
  fptr <- mallocForeignPtrArray nBytes :: IO (ForeignPtr Word8)

  withForeignPtr fptr $ \ptr -> do
    let pdata = GL.PixelData GL.RGBA GL.UnsignedByte ptr
    GL.readPixels pos (GL.Size (fromIntegral w) (fromIntegral h)) pdata

    let vec = V.unsafeFromForeignPtr0 (castForeignPtr fptr) nBytes
    let image = Image w h vec :: Image PixelRGBA8

    writePng path (dropAlphaLayer $ trim $ flipVertically image)
    putStrLn $ "Frame saved to " ++ path

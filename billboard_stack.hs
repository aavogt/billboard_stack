{-# LANGUAGE BlockArguments #-}

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import Data.List
import Linear
import Paths_billboard_stack
import Raylib.Core
import Raylib.Core.Models
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import qualified Raylib.Util.Colors as Colors
import System.FilePath.Glob

main = do
  imgPath <- getDataFileName "img"
  pngs <- glob (imgPath ++ "/*.png")
  let zs = map (read . filter isDigit) pngs
  (pngs, zs) <- return $ unzip $ sortOn snd $ zip pngs zs
  let cam = Camera3D (V3 500 600 700) 0 (V3 0 1 1) 82 CameraOrthographic
  putStrLn $ "Image path: " ++ show pngs
  n <- newIORef 0
  withWindow 640 480 "billboard_stack" 5 \wr -> do
    textures <- mapM (loadTextureFromImage <=< loadImage) pngs -- could load on demand?
    forever do
      n <- atomicModifyIORef n (\n -> ((n + 1) `mod` length zs, n))
      drawing do
        mode3D cam do
          clearBackground Colors.black
          ((textures `zip` zs) !! n) & \(texture, z) -> do
            drawBillboardPro
              cam
              texture
              (Rectangle 0 0 500 500)
              (V3 10 0 (z / 50))
              (V3 0 0 1)
              (V2 500 500)
              (V2 0 0)
              0
              Colors.white

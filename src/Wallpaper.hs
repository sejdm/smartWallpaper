{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TemplateHaskell #-}


module Wallpaper (
  hStackedWallSimple
  , vStackedWallSimple
  , vBackdrop
  , hBackdrop
  , vStandardBackdrop
  , hStandardBackdrop
  , hStackedWall
  , (||||)
  , (====)
  , pvcat
  , phcat
  , widthed
  , heighted
  , mkWallpaper

  , WallpaperSettings (..)
  , Wallpaper (..)
  , canvascolour
  , canvasdimensions
  , canvaspadding
  , thewallpaper
  , wallpath
  ) where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (pad)
import Data.List
import System.Process

hBackdrop = strutX
vBackdrop = strutY
hStandardBackdrop = hBackdrop 100
vStandardBackdrop = vBackdrop 100

hStackedWallSimple = centerXY . hcat . map (<> hStandardBackdrop)



vStackedWallSimple = centerXY . vcat . map (<> hStandardBackdrop)

hStackedWall = hcat . map (\(i, n) -> i <> hBackdrop n)

x |||| y = x ||| strutX 5 ||| y
x ==== y = x === strutY 5 === y

pvcat = vcat . intersperse (strutY 5)
phcat = vcat . intersperse (strutX 5)

widthed s = sized (mkWidth s)
heighted s = sized (mkHeight s)

background cp (x, y) c p = widthed (100*(x-cp)/x) p <> rect 100 (100*y/x) # fc c # lc c


data Wallpaper = PureWall (Diagram B) | IOWall (IO (Diagram B))
data WallpaperSettings = WallpaperSets
                         {
                           _canvascolour :: Colour Double
                         , _canvasdimensions :: (Double, Double)
                         , _canvaspadding :: Double
                         , _thewallpaper :: Wallpaper
                         , _wallpath :: FilePath
                         }



makeLenses ''WallpaperSettings


instance Default WallpaperSettings where
  def = WallpaperSets {
    _canvascolour = white
    , _canvasdimensions = (1366, 768)
    , _canvaspadding = 50
    , _thewallpaper = PureWall mempty
    , _wallpath = "/home/shane/cal.svg"
                      }

mkWallpaper s = do p <- case (s ^. thewallpaper) of
                     PureWall w -> return $ background (s ^. canvaspadding) (s ^. canvasdimensions) (s ^. canvascolour) w
                     IOWall w' -> background (s ^. canvaspadding) (s ^. canvasdimensions) (s ^. canvascolour) <$> w'
                   renderRasterific (s ^. wallpath) (dims2D x y) p
                   spawnProcess "/usr/bin/display" ["-window", "root", s ^. wallpath]
                   where (x, y) = s ^. canvasdimensions


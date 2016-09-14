{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TemplateHaskell #-}


module RandomImage (randomImageIO, RandomImageSettings (..), randompath, randdepth) where

import Diagrams.Prelude hiding (pad)
import System.Process
import Data.List (intersperse, isInfixOf, isSuffixOf)
import Data.Char
import System.Exit
import System.Random
import System.Directory
import Control.Monad

data RandomImageSettings = RandomSet
                    {
                      _randompath :: FilePath
                    , _randdepth :: Int
                    }
instance Default RandomImageSettings where
  def = RandomSet {_randompath = "", _randdepth = 1}

makeLenses ''RandomImageSettings

isImageFile x = any (`isSuffixOf` x) [".png", ".jpg", ".svg", ".gif"]

randomFile (p, g) = do d <- filter (\t -> t /= "." && t /= "..") <$> getDirectoryContents p
                       return $ (p ++ "/" ++ d !! (n `mod` length d), g')
                         where (n, g') = random g


randomImageFile (p, g) = do d <- filter isImageFile <$> getDirectoryContents p
                            return $ (p ++ "/" ++ d !! (n `mod` length d), g')
                              where (n, g') = random g
randIm 1 x = randomImageFile x
randIm n x = do y <- randomFile x
                randIm (n-1) y

isNotParent t = t /= "." && t /= ".."

randImg  p  g  = do d <- getDirectoryContents p
                    let (n, g') = random g
                    let im = filter isImageFile d
                    di' <- filterM (\t -> doesDirectoryExist (p ++ "/" ++ t)) d
                    let di = filter isNotParent di'
                    let p' = p ++ "/" ++ di !! (n `mod` length di)

                    if im /= [] then
                       return $ p ++ "/" ++ im !! (n `mod` length im)
                    else
                       randImg p' g'




randomImageIO s  = do g <- newStdGen
                      --(i, g') <- randomFile (randompath s, g)
                      --(i',_) <- randomImageFile (i, g')
                      --(i',_) <- randIm (randdepth s) (randompath s, g)
                      i' <- randImg (s ^. randompath) g
                      do res <-  loadImageEmb i'
                         return $ case res of
                           Right img -> image img # sized (mkHeight 50) # moveOriginBy (0 ^& (25))
                           Left _ -> text "error" # scale 3

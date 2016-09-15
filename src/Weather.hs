{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TemplateHaskell #-}


module Weather (weatherIO, WeatherSettings (..), city, iconsize, iconpath, showname) where

import Diagrams.Prelude hiding (pad)
import System.Process
import Data.List (intersperse, isInfixOf)
import Data.Char
import System.Exit

data WeatherSettings = WeatherSet
                    {
                        _city :: String
                      , _iconsize :: Double
                      , _iconpath :: FilePath
                      , _showname :: Bool
                       , _weatherFunction :: WeatherSettings -> String -> FilePath

                    }
makeLenses ''WeatherSettings

instance Default WeatherSettings where
  def = WeatherSet
    {
        _city = "pune"
      , _iconsize = 50
      , _iconpath = ""
      , _showname = True
      , _weatherFunction = weatherIcon
    }


weatherIcon :: WeatherSettings -> String -> FilePath
weatherIcon st s = st ^. iconpath ++ "/" ++ theFile s
  where
    s' = map toLower s
    theFile s | todayIs "cloudy" = "cloudy.png"
              | todayIs "patchy rain" = "rain.png"
              | todayIs "light rain" = "rain.png"
              | todayIs "rain" = "rain.png"
              | todayIs "drizzle" = "rain.png"
              | todayIs "overcast" = "overcast.png"
              | todayIs "clear" = "clear.png"
              | otherwise = "unknown.png"
     where todayIs x = x `isInfixOf` s'


weatherImage s   = do (e,n,_) <- readCreateProcessWithExitCode (shell ("wget --tries=1 --timeout=4 -q -O- wttr.in/" ++ mycity ++ " | sed 's/\\x1b\\[[0-9;]*m//g' | head -n 6 ")) []
                      if e == ExitSuccess && n /= "" then
                        do let temp = if length n >=4 then drop 15 $ ( lines n)!!3 else ""
                           let (a1, a2) = (take 2 temp, take 2 (drop 5 temp))
                           res <-  loadImageEmb ((s ^. weatherFunction) s n)
                           return $ case res of
                             Right img -> image img # sized (mkWidth w) === alignedText 0.5 1 (a1 ++ " - " ++ a2 ++ " C") # fontSize 16 # bold
                             Left _ -> alignedText 0.5 1 (dropWhile (not . isLetter) $ lines n!!2) # fontSize 16 # bold === strutY 10  === alignedText 0.5 1 (a1 ++ " - " ++ a2 ++ " C") # fontSize 16 # bold
                        else do im <-  loadImageEmb "/home/shane/Dropbox/icons/internetDown.jpg"
                                return $ case im of
                                  Right img -> image img # sized (mkWidth w)
                                  Left _ -> mempty
                      where mycity = s ^. city
                            w = s ^. iconsize

weatherIO s = do  w <- weatherImage s
                  return $ ( (if s ^. showname then (text (toUpper h : t) # scale 7 === strutY 5) else mempty) === w) # centerXY
                    where (h:t) = s ^. city

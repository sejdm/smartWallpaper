{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
import Diagrams.Prelude hiding (pad, (<>))
import Diagrams.Backend.Rasterific
import Calendar
import System.Environment
import Weather
import Wallpaper
import RandomImage
import TodoParsers
import Data.Monoid


myTodoSettings = def & todosources .~
     ("/home/shane/diary.org" `parsedWith` orgStyle
  <> "/home/shane/Dropbox/org/holidays.org" `parsedWith` orgStyle
  <> "/home/shane/Dropbox/Application/applications.org" `parsedWith` alterTextWith ("APP: "++) . orgStyle
  <> "/home/shane/Dropbox/britishcouncilholidays.txt" `parsedWith` useStyle bclStyle)

     & todoweekattribute .~ (fc blue # bold)
     & todotimeattribute .~ (fc green # bold # opacity 0.8)
     & entryformatter .~ defaultFormatUsingPath "/home/shane/Dropbox/icons"



myWeatherSettings = def & city .~ "pune"
                        & showname .~ False
                        & iconpath .~ "/home/shane/Dropbox/weathericons"



-- Simple wallpaper

mysimple = hStackedWallSimple <$> sequence [
    todoIO myTodoSettings
  , calendarIO def
  , mempty
                                             ]


weather = hStackedWallSimple <$> sequence [
    translateX (-20) . sized (mkHeight 190) <$> ( todoIO $ myTodoSettings)
  , calendarIO def
  , weatherIO $ myWeatherSettings
  ]

pictures = hStackedWallSimple . map centerXY <$> sequence [
   (translateX (-100) . scale 1.2) <$> todoIO myTodoSettings , widthed 250 <$> (randomImageIO $ def & randompath .~ "/home/shane/Pictures/")
   , (====) <$> calendarIO def <*> (weatherIO myWeatherSettings)
                                             ]


-- Free style: simply get the objects and then place them as you wish
new = do x <- todoIO def
         y <- calendarIO def
         z <- weatherIO $ def & showname .~ False
         return $ x # scale 0.8 # translateX (-120) <> y <> z # translateX 100

mychoice "pictures" = pictures
mychoice "weather" = weather
mychoice "simple" = weather
mychoice "new" = new
mychoice _ = weather


main = do n <- getArgs
          mkWallpaper $ def & thewallpaper .~ IOWall (if n /= [] then mychoice (n!!0) else weather)
                            & canvasdimensions .~ (1366, 768)

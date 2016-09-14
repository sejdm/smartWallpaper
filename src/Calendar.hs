{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TemplateHaskell #-}


module Calendar (
  calendarIO
  , CalendarSettings (..)
  , calbgcol
  , calfgcol
  , calhighlight
  , calweekattribute
  , TodoListSettings (..)
  , caldayattribute
  , calhighlightattribute
  , weekcol
  , calopacity
  , todoIO
  , todoweekattribute
  , todotimeattribute
  , numberoftododays
  , todolistseparator
  , thenFormat
  , entryformatter
  , todosources
  , alterTextWith
  , defaultFormatUsingPath
  ) where

import Diagrams.Prelude hiding (pad, (<>))
import Data.Monoid
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Data.List
import Data.Char
import Diagrams.Backend.Rasterific (B)
import Choice
import Control.Monad
import TodoFormat
import TodoParsers


getExternalImg h f alt = do res <-  loadImageEmb f
                            return $ case res of
                              Right img -> image img # sized h
                              Left _ -> alt


data CalendarSettings = CalSet
                        {
                          _calbgcol :: Colour Double
                        , _calfgcol :: Colour Double
                        , _calhighlight :: Colour Double
                        , _weekcol :: Colour Double
                        , _calopacity :: Double
                        , _calweekattribute :: Diagram B -> Diagram B
                        , _caldayattribute :: Diagram B -> Diagram B
                        , _calhighlightattribute :: Diagram B -> Diagram B
                        }

instance Default CalendarSettings where
  def = CalSet
    {
        _calbgcol = white
      , _calfgcol = black
      , _calhighlight = red
      , _weekcol = blue
      , _calopacity = 0
      , _calweekattribute = fc blue # bold
      , _calhighlightattribute = fc red # bold
      , _caldayattribute = fc black
    }
makeLenses ''CalendarSettings



correction = translateY (-0.1)



data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Enum)

instance Show Month where
         show Jan = "January"
         show Feb = "February"
         show Mar = "March"
         show Apr = "April"
         show May = "May"
         show Jun = "June"
         show Jul = "July"
         show Aug = "August"
         show Sep = "September"
         show Oct = "October"
         show Nov = "November"
         show Dec = "December"

data Week = None | Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq, Enum, Show)

formatWeek s d x = txt <> bck
                  where att = if d == x then s ^. calhighlightattribute else s ^. calweekattribute
                        txt = text (" " ++ show x ++ " ") # fontSize (local 0.25) # font "FreeSans Medium" # att # correction
                        bck = rect 1 0.6 # lc mybg # fc mybg # opacity (s ^. calopacity)
                        mybg = s ^. calbgcol
                        myfg = s ^. calfgcol
 --                       bck = rect 1 0.6 # lc mybg # opcty # fc myfg


data Date = Blank | Date Int deriving (Eq)

formatDate s d d' = txt <> bck
                             where att = if d == d' then s ^. calhighlightattribute else s ^. caldayattribute
                                   t = case d' of
                                            Blank -> "     "
                                            (Date n) -> if n <= 9 then "   " ++ show n ++ " " else "  " ++ show n ++ " "
                                   txt = text t # font "droid" # fontSize (local 0.4) # att # correction
                                   bck = square 1 # lc mybg # fc mybg # opacity (s ^. calopacity)
                                   mybg = s ^. calbgcol
                                   myfg = s ^. calfgcol



formatCal s h f i n = map h [Sun .. Sat] : map (map f) theDates
    where theDates =  chunksOf 7 $ replicate i Blank ++ map Date [1..n]
          mybg = s ^. calbgcol
          myfg = s ^. calfgcol

calendar s x = txt # centerX === (vcat $ map hcat $ formatCal s (formatWeek s (toEnum i')) (formatDate s (Date d)) (i `mod` 7) n) # centerX
    where (y,m,d) = toGregorian x
          n = gregorianMonthLength y m
          (_, _, i) = toWeekDate $ fromGregorian y m 1
          (_, _, i'') = toWeekDate $ fromGregorian y m d
          i' = i'' `mod` 7 + 1

          --txt = (text (mname ++ ", "++ show y) # font "droid" # fontSize (local 0.4) # fc myfg # translateX 3)
          txt = (text (mname ++ ", "++ show y) # font "droid" # fontSize (local 0.4) # fc myfg  <> rect 10 0.5 # fc mybg # lc mybg # opacity (s ^. calopacity)) # translateY (-0.5)
          mname = show $ (toEnum (m-1) :: Month)
          mybg = s ^. calbgcol
          myfg = s ^. calfgcol

thecalendar' s x = withEnvelope (circle 75 :: D V2 Double) (calendar s x # sized (mkWidth 150) # centerXY)
  where
           mybg = s ^. calbgcol
           myfg = s ^. calfgcol

calendarIO s = do x <- (localDay . utcToLocalTime (minutesToTimeZone 330)) <$> getCurrentTime
                  return $ thecalendar' s x
   where
            mybg = s ^. calbgcol
            myfg = s ^. calfgcol




















-- The Todo section

data TodoListSettings = TodoListSets {
    _todoweekattribute :: Diagram B -> Diagram B
  , _todotimeattribute :: Diagram B -> Diagram B
  , _numberoftododays :: Integer
  , _todolistseparator :: Diagram B
  , _entryformatter :: String -> Choicest TodoFormat
  , _todosources :: IO [(MyTime, String)]
                                     }
applic = rect 0.7 0.7

checkMark = (strokeLoop $ closeLine $ fromVertices [p2 (-0.035, 0.045), p2 (0, 0), p2 (0.1,0.1), p2 (0, 0.03)]) # lwL 0.38 # fc green # lc green # opacity 0.7

paid =  (alignedText 0.5 0.5 "PAID" # bold # scale 1.7 # fc red <> rect 6.5 2.5 # lc red) # rotateBy (1/32)

strikeout = rect 15 0.07 # fc green # lc green # lwL 0.07 # alignL



x |==| y = x ||| strutX 0.1 ||| y
x |=| y = (|==|) <$> x <*> y

icie :: FilePath -> String -> IO (Diagram B)
icie p f = getExternalImg (mkWidth 2) (p++f) mempty

useIcon :: String -> IO (Diagram B)
useIcon p = getExternalImg (mkWidth 2) ("/home/shane/haskellStacks/calendar/icons/"++p) mempty

addIcon :: String -> Diagram B -> IO (Diagram B)
addIcon i p = do i' <- useIcon i
                 return (i' |==| p)


newtype TodoFormat = TodoFormat (String -> String, String -> Diagram B,  Diagram B -> Diagram B, Diagram B -> IO (Diagram B))



tryFormat :: (String -> Bool) -> (String -> String, String -> Diagram B,  Diagram B -> Diagram B, Diagram B -> IO (Diagram B)) -> String -> Choicest TodoFormat
tryFormat b f t | b t = pure $ TodoFormat f
                | otherwise = mempty

tryF b (s, fs, p) = tryFormat b (s, stdTxt, fs, p)



stdTxt = alignedText 0 0.5

applyFormat :: (String -> TodoFormat) -> String -> IO (Diagram B)
applyFormat f t =  ( p . fs . rs . s) t
  where TodoFormat (s, rs, fs, p) = f t

combineFormat :: TodoFormat -> TodoFormat -> TodoFormat
combineFormat (TodoFormat(s1, rs1, fs1, p1)) (TodoFormat(s2, rs2, fs2, p2)) = (TodoFormat(s2 . s1, rs2, fs1 . fs2,p1 >=> p2))

combineChoiceFormat (Choicest Nothing) x = x
combineChoiceFormat x (Choicest Nothing) = x
combineChoiceFormat x y = combineFormat <$> x <*> y

thenFormat :: (String -> Choicest TodoFormat) -> (String -> Choicest TodoFormat) -> (String -> Choicest TodoFormat)
thenFormat f1 f2 t = combineChoiceFormat (f1 t) (f2 t)


alterTextWith f xs = map (g f) xs
    where g f (x, t) = (x, f t)

defaultFormatUsingPath :: FilePath -> String -> Choicest TodoFormat
defaultFormatUsingPath p =
  (
      tryF (isInfixOf "[#A]") (remWord "[#A]", bold, pure)
   <> tryF (isInfixOf "[#B]") (remWord "[#B]", italic, pure)
   )
  `thenFormat`
  (
     tryF ((||) <$> isInfixOf "buy" . map toLower <*> isInfixOf "shopping" . map toLower) (id, id, addIcon "shopping.png")
  <> tryF (isInfixOf "APP:") (remWord "APP:", bold, addIcon "envelope.png")
  <> tryF ((||) <$> isInfixOf "APP:" <*> isInfixOf "application" . map toLower) (remWord "APP:", italic, addIcon "envelope.png")
  <> tryF ((||) <$> isInfixOf "bill" . map toLower <*> isInfixOf "pay" . map toLower) (id, id, addIcon "bill.png")
  <> tryF (isInfixOf "flight" . map toLower) (id, id, addIcon "flight.png")
  <> tryF (isInfixOf "birthday" . map toLower) (id, id, addIcon "birthday.png")
  <> tryF (isInfixOf "train" . map toLower) (id, id, addIcon "train.png")
  <> tryF (isInfixOf "HOL:") (remWord "HOL:", id, addIcon "holiday.png")
  <> tryF (isInfixOf ":holiday:") (remWord ":holiday:", id, addIcon "holiday.png")
  )
  `thenFormat`
  (
      tryF ((&&) <$> isInfixOf "DONE" <*> ((||) <$> isInfixOf "bill" . map toLower <*> isInfixOf "pay" . map toLower)) (remWord "DONE", id, pure . ((paid # scale 0.7 # translateX 6 # opacity 0.5) <>) . (checkMark |==|))
   <> tryF (isInfixOf "DONE") (remWord "DONE", id, pure . (checkMark |==|))
   <> tryF (isInfixOf "CANCELED") (remWord "CANCELED", id, pure . (strikeout <>))

  )
  where useIcon = icie p
        addIcon i p = do i' <- useIcon i
                         return (i' |==| p)

remWord :: String -> String -> String
remWord s = unwords . (\\ [s]) . words



defaultSources =
    "/home/shane/diary.org" `parsedWith` orgStyle
 <> "/home/shane/Dropbox/org/holidays.org" `parsedWith` orgStyle
 <> "/home/shane/Dropbox/Application/applications.org" `parsedWith` alterTextWith ("APP: "++) . orgStyle
 <> "/home/shane/Dropbox/britishcouncilholidays.txt" `parsedWith` useStyle bclStyle




instance Default TodoListSettings where
  def = TodoListSets {
    _todoweekattribute = fc blue # bold
    , _todotimeattribute = fc green # bold # opacity 0.8
    , _numberoftododays = 7
    , _todolistseparator = strutY 0.8
    , _entryformatter = defaultFormatUsingPath ""
    , _todosources = defaultSources
        }

makeLenses ''TodoListSettings








dayRange n d = map ($ d) $ map addDays [0..(n-1)]

filterDays xs d = ((\(_,_,l)-> l) (toWeekDate d), filter ((matchDay d) . fst) xs)

-- Filters only the matching dates to avoid a space leak owing to loading everything at once.
takeOnlyDays n d = filter (\w -> let d' = (fst) w in any ((flip matchDay d')) (dayRange n d))

forDays n d xs = map (filterDays xs) (dayRange n d)

forDaysF n d = forDays n d . sortBy (\x y -> compareTime (fst x) (fst y)) . takeOnlyDays n d



dayWeekName n = show (toEnum (1 + (n) `mod` 7) :: Week)





formatTodoEntryIO mf st (t, e) = do p <- mf (dropWhile (==' ') e)
                                    return $ (((processTime t) ||| p) <> strutY 2) # translateX (-10)
                                      where processTime x | hasTime x =  text (take 5 $ show $ localTimeOfDay t) # (st ^. todotimeattribute) <> strutX 5
                                                          | otherwise = text "     " # (st ^. todotimeattribute) <> strutX 5

                                                            where t = unWhen x


formatTodoListIO f st xs = vcat <$> mapM (formatTodoEntryIO f st) xs


formatTodoDayIO f st (n, s) = do x <- formatTodoListIO f st s
                                 return $ ( ( alignedText 0.5 0.5 (dayWeekName n) # (st ^. todoweekattribute)<> strutY 3 <> strutX 3) # translateX (-5)  === x)

formatTodoIO f st x = (vcat . intersperse (st ^. todolistseparator)) <$> mapM (formatTodoDayIO f st) x


todoIO   st = do xx <- st ^. todosources

                 d <- (localDay . utcToLocalTime (minutesToTimeZone 330)) <$> getCurrentTime

                 (centerXY . scale 3) <$> (formatTodoIO todofn st $ forDaysF (st ^. numberoftododays) d xx)

      where todofn = applyFormat (unchoice (TodoFormat (id, alignedText 0 0.5, id, pure) ) . (st ^. entryformatter) ) . remWord "TODO"

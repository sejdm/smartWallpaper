module TodoParsers
       (
         orgStyle
       , remStyle
       , bclStyle
       , useStyle
       , parsedWith
       )
       where
import TodoFormat
import Data.Time.Format
import Data.Time.LocalTime
import Data.List
import Control.Applicative
import Data.Foldable
import Data.Maybe



-- Org style

isOrgTitle :: String -> Bool
isOrgTitle [] = False
isOrgTitle (h:_) = h == '*'


isOrgDate = undefined

myformat f n t = parseTimeM True defaultTimeLocale f (take n t) :: Maybe LocalTime

formatOrgFullDay t = toMyTimeAllDay <$> myformat "<%Y-%m-%d %a>" 16 t

formatOrgTime t = toMyTime <$> myformat "<%Y-%m-%d %a %H:%M>" 23 t

formatOrgTimeDaily t = daily <$> myformat "<%Y-%m-%d %a %H:%M +1d>" 27 t
formatOrgTimeDailyAllDay t = dailyFull <$> myformat "<%Y-%m-%d %a +1d>" 21 t


formatOrgTimeYearly t = yearly <$> myformat "<%Y-%m-%d %a %H:%M +1y>" 27 t
formatOrgTimeYearlyAllDay t = yearlyFull <$> myformat "<%Y-%m-%d %a +1y>" 21 t

formatOrgTimeWeekly t = weekly <$> myformat "<%Y-%m-%d %a %H:%M +1w>" 27 t
formatOrgTimeWeeklyAllDay t = weeklyFull <$> myformat "<%Y-%m-%d %a +1w>" 21 t

formatOrgTimeMonthly t = monthly <$> myformat "<%Y-%m-%d %a %H:%M +1m>" 27 t
formatOrgTimeMonthlyAllDay t = monthlyFull <$> myformat "<%Y-%m-%d %a +1m>" 21 t


formatOrgRange t = do
  f1 <- myformat "<%Y-%m-%d %a>--" 18 t
  f2 <- myformat "--<%Y-%m-%d %a>" 18 (drop 16 t)
  return (fromTo f1 f2)

formatOrgRangeTime t = do
  f1 <- myformat "<%Y-%m-%d %a %H:%M>--" 24 t
  f2 <- myformat "--<%Y-%m-%d %a>" 18 (drop 22 t)
  return (dailyFromTo f1 f2)

formatOrgAny t = formatOrgRangeTime t <|> formatOrgRange t <|> formatOrgTimeYearly t <|> formatOrgTimeYearlyAllDay t <|> formatOrgTimeMonthlyAllDay t <|> formatOrgTimeMonthly t <|> formatOrgTimeWeekly t  <|> formatOrgTimeDaily t <|> formatOrgTimeWeeklyAllDay t <|> formatOrgTimeDailyAllDay t <|> formatOrgTime t <|> formatOrgFullDay t

removeOrgStars = dropWhile (==' ') . dropWhile (=='*')
getOrgDate t = asum $ map formatOrgAny $ tails t
--getOrgDate t = formatOrgTime t

headingStyle bt gd f t= checkOrgLines [] "" (lines t)
  where checkOrgLines _ _ [] = []
        checkOrgLines xs h a@(l:ls)
          | bt l = checkOrgLines xs l ls
          | otherwise = case gd l of
              (Just x) -> (x, f h) : checkOrgLines "" "" ls
              _ -> checkOrgLines xs h ls

orgStyle :: String -> [(MyTime, String)]
orgStyle  = headingStyle isOrgTitle getOrgDate removeOrgStars


-- Non org style

parseDateWith :: String -> Int -> Bool -> String -> Maybe (MyTime, String)
parseDateWith f n b t = (,) <$> ((if b then toMyTimeAllDay else toMyTime) <$> (parseTimeM True defaultTimeLocale f (take n t) :: Maybe LocalTime)) <*> Just (drop n t)

parseDateWithE f n b t = (,) <$> ((if b then toMyTimeAllDay else toMyTime) <$> maybeToEither t (parseTimeM True defaultTimeLocale f (take n t) :: Maybe LocalTime)) <*> Right (drop n t)

maybeToEither _ (Just x) = Right x
maybeToEither t Nothing = Left t

changeText :: (String -> String) -> (MyTime, String) -> (MyTime, String)
changeText f (x, t) = (x, f t)

maybeWhen :: String -> Maybe (MyTime, String)
maybeWhen = parseDateWith "%Y/%m/%d %l:%M%P" 18 False

maybeAllDay = parseDateWith "%Y/%m/%d" 10 True

maybeHolidayStyle t = (,) <$> (toMyTimeAllDay <$> (parseTimeM True defaultTimeLocale "%d %b %Y") (take 11 t)) <*> Just (take 23 $ drop 11 t)

-- use parseLine' if this gives trouble
remStyle t = (maybeWhen t <|> maybeAllDay t)

bclStyle t = changeText (\y -> "BCL closed (" ++ tail y ++ ")") <$> maybeHolidayStyle t


useStyle :: (String -> Maybe a) -> String -> [a]
useStyle p = catMaybes . map p . lines



parsedWith p f = f <$> readFile p
infixl 8 `parsedWith`

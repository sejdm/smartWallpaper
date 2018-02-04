module TodoFormat
       (
         MyTime
       , hasTime
       , whenToRec
       , unWhen
       , whenToDay
       , whenToTime
       , whenToDayOfMonth
       , whenToDayOfWeek
       , whenToDuration
       , matchDay
       , compareTime
       , toMyTime
       , toMyTimeAllDay
       , daily
       , monthly
       , weekly
       , yearly
       , dailyFull
       , monthlyFull
       , weeklyFull
       , yearlyFull
       , dailyFromTo
       , fromTo
       )
       where

import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

data Recurrence = NotRecurrent | Monthly | Yearly | Weekly | Daily | DailyRange LocalTime LocalTime deriving (Show, Eq)

data MyDuration = FullDay | NoDuration | EndTime LocalTime deriving (Eq)

data MyTime = TheTime MyDuration Recurrence LocalTime deriving (Eq)

--data When = When Recurrence LocalTime | AllDay Recurrence LocalTime | AllDayRange When When | OnlyDay Day deriving (Show, Eq)

thrd (_, _, z) = z
snd' (_, z, _) = z

  {-
instance Eq When where
  (When Weekly x) == (OnlyDay y) = thrd (toWeekDate (localDay x)) == thrd (toWeekDate (y))
  (OnlyDay x) == (When Weekly y) = thrd (toWeekDate (x)) == thrd (toWeekDate (localDay y))

  (OnlyDay x) == (When _ y) = x == localDay y
  (When _ x) == (OnlyDay y) = y == localDay x

  (OnlyDay x) == (AllDay _ y) = x == localDay y
  (AllDay _ x) == (OnlyDay y) = localDay x == y
-}

hasTime (TheTime NoDuration _ _) = True
hasTime _ = False

whenToRec (TheTime NoDuration r _) = r
whenToRec (TheTime FullDay r _) = r
whenToRec (TheTime _ r _) = r

unWhen (TheTime NoDuration _ x) = x
unWhen (TheTime _ _ x) = x

whenToDay :: MyTime -> Day
whenToDay = localDay . unWhen

whenToTime :: MyTime -> TimeOfDay
whenToTime = localTimeOfDay . unWhen

whenToDayOfWeek :: MyTime -> Int
whenToDayOfWeek = thrd . toWeekDate . whenToDay

whenToDayOfMonth :: MyTime -> Int
whenToDayOfMonth = thrd . toGregorian . whenToDay

whenToDuration :: MyTime -> MyDuration
whenToDuration (TheTime d _ _ ) = d


matchDay :: Day -> MyTime -> Bool
matchDay d (TheTime (EndTime t) _ t') = localDay t' <= d && d <= localDay t
matchDay y x = case whenToRec x of
  Weekly -> thrd (toWeekDate y) == thrd (toWeekDate d)
  Monthly -> thrd (toGregorian y) == thrd (toGregorian d)
  Yearly -> (thrd (toGregorian y) == thrd (toGregorian d)) && (snd' (toGregorian y) == snd' (toGregorian d))
  DailyRange t' t -> localDay t' <= d && d <= localDay t
  Daily -> True
  _ -> y == d
  where d = whenToDay x

instance Ord MyTime where
  compare (TheTime NoDuration _ x) (TheTime NoDuration _ y) = compare x y
  compare (TheTime FullDay _ x) (TheTime FullDay _ y) = compare x y
  compare (TheTime NoDuration _ x) (TheTime FullDay _ y) = if localDay x == localDay y then GT else compare x y
  compare (TheTime FullDay _ x) (TheTime NoDuration _ y) = if localDay x == localDay y then LT else compare x y

compareTime :: MyTime -> MyTime -> Ordering
compareTime (TheTime NoDuration _ x) (TheTime NoDuration _ y) = compare (localTimeOfDay x) (localTimeOfDay y)
compareTime (TheTime NoDuration _ x) (TheTime _ _ y) = GT
compareTime (TheTime _ _ x) (TheTime NoDuration _ y) = LT
compareTime (TheTime _ _ x) (TheTime _ _ y) = EQ


toMyTimeAllDay = TheTime FullDay NotRecurrent
toMyTime = TheTime NoDuration NotRecurrent
weekly = TheTime NoDuration Weekly
monthly = TheTime NoDuration Monthly
daily = TheTime NoDuration Daily
yearly = TheTime NoDuration Yearly


weeklyFull = TheTime FullDay Weekly
monthlyFull = TheTime FullDay Monthly
dailyFull = TheTime FullDay Daily
yearlyFull = TheTime FullDay Yearly


dailyFromTo  x y = TheTime NoDuration (DailyRange x y) x
fromTo f1 f2 = TheTime (EndTime f2) NotRecurrent f1

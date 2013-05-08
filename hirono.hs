module Hirono (calendar) where

import Data.Time (fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- 曜日型定義
data DaysOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq, Ord, Show)

succDayOfWeek :: DaysOfWeek -> DaysOfWeek
succDayOfWeek Sun = Mon
succDayOfWeek Mon = Tue
succDayOfWeek Tue = Wed
succDayOfWeek Wed = Thu
succDayOfWeek Thu = Fri
succDayOfWeek Fri = Sat
succDayOfWeek Sat = Sun

prevDayOfWeek :: DaysOfWeek -> DaysOfWeek
prevDayOfWeek Sun = Sat
prevDayOfWeek Mon = Sun
prevDayOfWeek Tue = Mon
prevDayOfWeek Wed = Tue
prevDayOfWeek Thu = Wed
prevDayOfWeek Fri = Thu
prevDayOfWeek Sat = Fri

toDaysOfWeek :: Int -> DaysOfWeek
toDaysOfWeek 1 = Mon
toDaysOfWeek dow = succDayOfWeek $ toDaysOfWeek (dow - 1)

daysOfWeek :: DaysOfWeek -> [ DaysOfWeek ]
daysOfWeek d = iterate succDayOfWeek d

daysOfMonth :: Int -> DaysOfWeek -> [ (Int, DaysOfWeek) ]
daysOfMonth monthLength beginningDayOfMonth = zip [1..monthLength] (daysOfWeek beginningDayOfMonth)

calendarHead = take 7 $ map show $ daysOfWeek Sun

calendarBody :: [(Int, DaysOfWeek)] -> [ [ String ] ]
calendarBody [] = [[]]
calendarBody ((1, Sun) : xs) = (show 1 : y) : ys
                               where
                                 (y : ys) = calendarBody xs
calendarBody ((1, dow) : xs) = ("" : y) : ys
                               where
                                 (y : ys) = calendarBody ((1, prevDayOfWeek dow) : xs)
calendarBody ((d, dow):xs) | dow == Sun = []:thisWeek
                           | otherwise  = thisWeek
                             where
                               (y:ys) = calendarBody xs
                               thisWeek = (show d:y):ys

calendar :: Integer -> Int -> [[String]]
calendar year month = calendarHead : calendarBody (daysOfMonth (gregorianMonthLength year month) (toDaysOfWeek dow))
                      where
                        (_, _, dow) = toWeekDate (fromGregorian year month 1)

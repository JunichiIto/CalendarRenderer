import Data.Time.Clock
import Data.Time.Calendar
import Data.List.Split

import Data.Time
import Data.Time.Calendar.WeekDate
 

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []

main = do
      let (_,_,wday) = toWeekDate $ fromGregorian 2013 4 1
      let offset = take wday $ repeat "   "
      let calendar = map (rjust 3) $ map show $ take (gregorianMonthLength 2013 4) [1..]
      print $ myMap concat $ splitEvery 7 $ concat [offset, calendar]

    -- putStrLn $ "Days in this month: " ++ (show $ gregorianMonthLength 2013 5)
-- import Data.Time
-- import System.Locale
-- 
-- dayAndTime :: ZonedTime -> (Day, TimeOfDay)
-- dayAndTime zt = let lt = zonedTimeToLocalTime zt
--                     day  = localDay lt
--                     time = localTimeOfDay lt
--                 in (day, time)
-- 
-- day  = fst . dayAndTime
-- time = snd . dayAndTime
-- 
-- diffTime :: TimeOfDay -> TimeOfDay -> TimeOfDay
-- diffTime t1 t2 = timeToTimeOfDay 
--                  (timeOfDayToTime t2 - timeOfDayToTime t1)
--                        
-- main = do 
--   -- Day 型の値の生成
--   print $ fromGregorian 2010 4 1
-- 
--   -- 2010.4.1 の 100 日後は？
--   print $ addDays 100 $ fromGregorian 2010 4 1
--   -- 2010.4.1 から大晦日までは何日？
--   print $ fromGregorian 2010 12 31 `diffDays` fromGregorian 2010 4 1
-- 
--   -- 現在の時刻を取得
--   now <- getZonedTime
-- 
--   -- 現在の日付から 100 日後は？
--   print $ addDays 100 (day now)
--   -- 現在から大晦日までは何日？
--   print $ fromGregorian 2010 12 31 `diffDays` (day now)
-- 
--   -- 後何分？
--   print $ diffTime (time now) (TimeOfDay 22 0 0) 
-- 
--   -- フォーマット
--   print $ formatTime defaultTimeLocale "%Y--%m--%d" 
--             $ fromGregorian 2010 4 1

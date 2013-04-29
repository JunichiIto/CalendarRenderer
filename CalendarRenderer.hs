module CalendarRenderer (renderCalendar) where

import qualified Data.List.Split as L (chunksOf)
import Data.Time (formatTime, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T (pack, unpack, center)
import System.Locale (defaultTimeLocale)

dayLength :: Int
dayLength = 3

renderCalendar :: Integer -> Int -> String
renderCalendar year month = do
  let day = fromGregorian year month 1
  unlines $ header day ++ body day
 
header :: Day -> [String]
header day = do
  let weekLength = dayLength * 7
  let monthYear = T.unpack $ T.center weekLength ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" day
  [monthYear, " Su Mo Tu We Th Fr Sa"]

body :: Day -> [String]
body day = do
  let (year,month,_) = toGregorian day
  let rjust width s = replicate (width - length s) ' ' ++ s
  let calendar = map (rjust dayLength) $ map show $ take (gregorianMonthLength year month) [1..]
  map concat $ L.chunksOf 7 $ firstWeekOffset day ++ calendar

firstWeekOffset :: Day -> [String]
firstWeekOffset day = do
  let (_,_,wday) = toWeekDate day
  take wday $ repeat $ replicate dayLength ' '

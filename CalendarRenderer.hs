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
  let center = T.unpack . T.center weekLength ' ' . T.pack
  let monthYear = center $ formatTime defaultTimeLocale "%B %Y" day
  [monthYear, " Su Mo Tu We Th Fr Sa"]

body :: Day -> [String]
body day = do
  let rjust s = replicate (dayLength - length s) ' ' ++ s
  let format = rjust . show
  let (year,month,_) = toGregorian day
  let calendar = map format $ take (gregorianMonthLength year month) [1..]
  map concat $ L.chunksOf 7 $ firstWeekOffset day ++ calendar

firstWeekOffset :: Day -> [String]
firstWeekOffset day = do
  let (_,_,wday) = toWeekDate day
  replicate wday $ replicate dayLength ' '

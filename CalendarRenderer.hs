module CalendarRenderer (renderCalendar) where

import qualified Data.List.Split as L (chunksOf)
import Data.Time (formatTime, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T (pack, unpack, center, justifyRight, Text)
import System.Locale (defaultTimeLocale)

dayLength = 3
weekLength = dayLength * 7

renderCalendar year month = do
  let day = fromGregorian year month 1
  unlines $ header day ++ body day
 
header day = do
  let center = applyTextFunction $ T.center weekLength ' '
  let monthYear = center $ formatTime defaultTimeLocale "%B %Y" day
  [monthYear, " Su Mo Tu We Th Fr Sa"]

body day = map concat $ L.chunksOf 7 $ firstWeekOffset day ++ calendar day

calendar day = do
  let rjust = applyTextFunction $ T.justifyRight dayLength ' '
  let (year, month, _) = toGregorian day
  map (rjust . show) [1..gregorianMonthLength year month]

firstWeekOffset day = do
  let (_, _, wday) = toWeekDate day
  let offsetLength = if wday == 7 then 0 else wday
  replicate offsetLength $ replicate dayLength ' '

applyTextFunction f = T.unpack . f . T.pack

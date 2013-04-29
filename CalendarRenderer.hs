module CalendarRenderer (renderCalendar) where

import qualified Data.List.Split as L (chunksOf)
import Data.Time (formatTime, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T (pack, unpack, center, justifyRight, Text)
import System.Locale (defaultTimeLocale)

dayLength :: Int
dayLength = 3

weekLength :: Int
weekLength = dayLength * 7

renderCalendar :: Integer -> Int -> String
renderCalendar year month = do
  let day = fromGregorian year month 1
  unlines $ header day ++ body day
 
header :: Day -> [String]
header day = do
  let center = applyTextFunction $ T.center weekLength ' '
  let monthYear = center $ formatTime defaultTimeLocale "%B %Y" day
  [monthYear, " Su Mo Tu We Th Fr Sa"]

body :: Day -> [String]
body day = map concat $ L.chunksOf 7 $ firstWeekOffset day ++ calendar day

calendar :: Day -> [String]
calendar day = do
  let rjust = applyTextFunction $ T.justifyRight dayLength ' '
  let (year, month, _) = toGregorian day
  map (rjust . show) $ [1..gregorianMonthLength year month]

firstWeekOffset :: Day -> [String]
firstWeekOffset day = do
  let (_, _, wday) = toWeekDate day
  replicate wday $ replicate dayLength ' '

applyTextFunction :: (T.Text -> T.Text) -> String -> String
applyTextFunction f = T.unpack . f . T.pack

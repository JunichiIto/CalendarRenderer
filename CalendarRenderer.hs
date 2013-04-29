module CalendarRenderer (renderCalendar) where

import qualified Data.List.Split as L (chunksOf)
import Data.Time (formatTime, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T (pack, unpack, center)
import System.Locale (defaultTimeLocale)
 
rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

header :: Day -> Int -> [String]
header d dayLength = do
  let weekLength = dayLength * 7
  let monthYear = T.unpack $ T.center weekLength ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" d
  [monthYear, " Su Mo Tu We Th Fr Sa"]

firstWeekOffset :: Day -> Int -> [String]
firstWeekOffset d dayLength = do
  let (_,_,wday) = toWeekDate $ d
  take wday $ repeat $ replicate dayLength ' '

body :: Day -> Int -> [String]
body d dayLength = do
  let (y,m,_) = toGregorian d
  let calendar = map (rjust dayLength) $ map show $ take (gregorianMonthLength y m) [1..]
  map concat $ L.chunksOf 7 $ concat [firstWeekOffset d dayLength, calendar]

renderCalendar :: Integer -> Int -> String
renderCalendar y m = do
  let dayLength = 3
  let day = fromGregorian y m 1
  unlines $ concat [header day dayLength, body day dayLength]

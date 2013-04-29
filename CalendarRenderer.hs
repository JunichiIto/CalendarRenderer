module CalendarRenderer (renderCalendar) where

import qualified Data.List.Split as L (chunksOf)
import Data.Time (formatTime, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text as T (pack, unpack, center)
import System.Locale (defaultTimeLocale)
 
rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

renderCalendar :: Integer -> Int -> String
renderCalendar y m = do
  let dayLength = 3
  let weekLength = dayLength * 7
  let monthYear = T.unpack $ T.center weekLength ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" $ fromGregorian y m 1
  let header = [monthYear, " Su Mo Tu We Th Fr Sa"]
  let (_,_,wday) = toWeekDate $ fromGregorian y m 1
  let offset = take wday $ repeat $ replicate dayLength ' '
  let calendar = map (rjust dayLength) $ map show $ take (gregorianMonthLength y m) [1..]
  let body = map concat $ L.chunksOf 7 $ concat [offset, calendar]
  unlines $ concat [header, body]

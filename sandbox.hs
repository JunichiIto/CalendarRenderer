import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.List.Split as L

import Data.Time
import Data.Time.Calendar.WeekDate

import qualified Data.Text as T

import System.Locale (defaultTimeLocale)
 
rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

renderCalendar :: Integer -> Int -> String
renderCalendar y m = do
      let h1 = T.unpack $ T.center 21 ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" $ fromGregorian y m 1
      let h2 = " Su Mo Tu We Th Fr Sa"
      let (_,_,wday) = toWeekDate $ fromGregorian y m 1
      let offset = take wday $ repeat "   "
      let calendar = map (rjust 3) $ map show $ take (gregorianMonthLength y m) [1..]
      let body = map concat $ L.chunksOf 7 $ concat [offset, calendar]
      unlines $ concat [[h1, h2], body]

main = do
      putStr $ renderCalendar 2013 4

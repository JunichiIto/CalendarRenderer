import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.List.Split as L

import Data.Time
import Data.Time.Calendar.WeekDate

import qualified Data.Text as T

import System.Locale (defaultTimeLocale)
 
rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

main = do
      putStrLn $ T.unpack $ T.center 21 ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" $ fromGregorian 2013 4 1
      putStrLn " Su Mo Tu We Th Fr Sa"
      let (_,_,wday) = toWeekDate $ fromGregorian 2013 4 1
      let offset = take wday $ repeat "   "
      let calendar = map (rjust 3) $ map show $ take (gregorianMonthLength 2013 4) [1..]
      putStr $ unlines $ map concat $ L.chunksOf 7 $ concat [offset, calendar]

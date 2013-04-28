import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.List.Split as L

import Data.Time
import Data.Time.Calendar.WeekDate

import qualified Data.Text as T

import System.Locale (defaultTimeLocale)

import Test.Hspec
 
rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

renderCalendar :: Integer -> Int -> String
renderCalendar y m = do
  let dayLength = 3
  let weekLength = dayLength * 7
  let h1 = T.unpack $ T.center weekLength ' ' $ T.pack $ formatTime defaultTimeLocale "%B %Y" $ fromGregorian y m 1
  let h2 = " Su Mo Tu We Th Fr Sa"
  let (_,_,wday) = toWeekDate $ fromGregorian y m 1
  let offset = replicate wday "   "
  let calendar = map (rjust dayLength) $ map show $ take (gregorianMonthLength y m) [1..]
  let body = map concat $ L.chunksOf 7 $ concat [offset, calendar]
  unlines $ concat [[h1, h2], body]

main :: IO ()
main = hspec $ do
  describe "renderCalendar" $ do
    it "returns calendar" $ do
      renderCalendar 2013 4 `shouldBe` "      April 2013     \n Su Mo Tu We Th Fr Sa\n     1  2  3  4  5  6\n  7  8  9 10 11 12 13\n 14 15 16 17 18 19 20\n 21 22 23 24 25 26 27\n 28 29 30\n"

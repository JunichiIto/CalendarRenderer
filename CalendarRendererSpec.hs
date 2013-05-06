import Test.Hspec
import CalendarRenderer
import Hirono
 
main :: IO ()
main = hspec $ do
  describe "renderCalendar" $ do
    it "returns calendar for 2013 4" $ do
      renderCalendar 2013 4 `shouldBe` "      April 2013     \n Su Mo Tu We Th Fr Sa\n     1  2  3  4  5  6\n  7  8  9 10 11 12 13\n 14 15 16 17 18 19 20\n 21 22 23 24 25 26 27\n 28 29 30\n"

    it "returns calendar for 2013 12" $ do
      renderCalendar 2013 12 `shouldBe` "    December 2013    \n Su Mo Tu We Th Fr Sa\n  1  2  3  4  5  6  7\n  8  9 10 11 12 13 14\n 15 16 17 18 19 20 21\n 22 23 24 25 26 27 28\n 29 30 31\n"

    it "returns calendar for all 2013" $ do
      (unlines $ map (renderCalendar 2013) [1..12]) `shouldBe` "     January 2013    \n Su Mo Tu We Th Fr Sa\n        1  2  3  4  5\n  6  7  8  9 10 11 12\n 13 14 15 16 17 18 19\n 20 21 22 23 24 25 26\n 27 28 29 30 31\n\n    February 2013    \n Su Mo Tu We Th Fr Sa\n                 1  2\n  3  4  5  6  7  8  9\n 10 11 12 13 14 15 16\n 17 18 19 20 21 22 23\n 24 25 26 27 28\n\n      March 2013     \n Su Mo Tu We Th Fr Sa\n                 1  2\n  3  4  5  6  7  8  9\n 10 11 12 13 14 15 16\n 17 18 19 20 21 22 23\n 24 25 26 27 28 29 30\n 31\n\n      April 2013     \n Su Mo Tu We Th Fr Sa\n     1  2  3  4  5  6\n  7  8  9 10 11 12 13\n 14 15 16 17 18 19 20\n 21 22 23 24 25 26 27\n 28 29 30\n\n       May 2013      \n Su Mo Tu We Th Fr Sa\n           1  2  3  4\n  5  6  7  8  9 10 11\n 12 13 14 15 16 17 18\n 19 20 21 22 23 24 25\n 26 27 28 29 30 31\n\n      June 2013      \n Su Mo Tu We Th Fr Sa\n                    1\n  2  3  4  5  6  7  8\n  9 10 11 12 13 14 15\n 16 17 18 19 20 21 22\n 23 24 25 26 27 28 29\n 30\n\n      July 2013      \n Su Mo Tu We Th Fr Sa\n     1  2  3  4  5  6\n  7  8  9 10 11 12 13\n 14 15 16 17 18 19 20\n 21 22 23 24 25 26 27\n 28 29 30 31\n\n     August 2013     \n Su Mo Tu We Th Fr Sa\n              1  2  3\n  4  5  6  7  8  9 10\n 11 12 13 14 15 16 17\n 18 19 20 21 22 23 24\n 25 26 27 28 29 30 31\n\n    September 2013   \n Su Mo Tu We Th Fr Sa\n  1  2  3  4  5  6  7\n  8  9 10 11 12 13 14\n 15 16 17 18 19 20 21\n 22 23 24 25 26 27 28\n 29 30\n\n     October 2013    \n Su Mo Tu We Th Fr Sa\n        1  2  3  4  5\n  6  7  8  9 10 11 12\n 13 14 15 16 17 18 19\n 20 21 22 23 24 25 26\n 27 28 29 30 31\n\n    November 2013    \n Su Mo Tu We Th Fr Sa\n                 1  2\n  3  4  5  6  7  8  9\n 10 11 12 13 14 15 16\n 17 18 19 20 21 22 23\n 24 25 26 27 28 29 30\n\n    December 2013    \n Su Mo Tu We Th Fr Sa\n  1  2  3  4  5  6  7\n  8  9 10 11 12 13 14\n 15 16 17 18 19 20 21\n 22 23 24 25 26 27 28\n 29 30 31\n\n"
  describe "Hirono/calendar" $ do
    it "returns calendar list" $ do
      calendar 2013 5 `shouldBe` [["Sun","Mon","Tue","Wed","Thu","Fri","Sat"],["","","","1","2","3","4"],["5","6","7","8","9","10","11"],["12","13","14","15","16","17","18"],["19","20","21","22","23","24","25"],["26","27","28","29","30","31"]]

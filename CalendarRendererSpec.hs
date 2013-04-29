import Test.Hspec
import CalendarRenderer
 
main :: IO ()
main = hspec $ do
  describe "renderCalendar" $ do
    it "returns calendar" $ do
      renderCalendar 2013 4 `shouldBe` "      April 2013     \n Su Mo Tu We Th Fr Sa\n     1  2  3  4  5  6\n  7  8  9 10 11 12 13\n 14 15 16 17 18 19 20\n 21 22 23 24 25 26 27\n 28 29 30\n"

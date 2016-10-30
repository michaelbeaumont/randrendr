{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec

import Data.Monoid
import qualified Data.Text as T
import qualified Text.Parsec as P

import Randrendr
import Types
import Parse


main :: IO ()
main = hspec $
  do describe "Randrendr" $
       do describe "parseScreen" $
            do it "parses a single active screen" $
                 test1 `shouldBe` Right (Just testScreen1)
               it "parses a screen in off state" $
                 test2 `shouldBe` Right Nothing
          describe "parseScreens" $
            do it "parses multiple screens" $
                 test3 `shouldBe` expect3
    where
      test1 = P.parse parseScreen "Screen text 1" screenText1
      test2 = P.parse parseScreen "Screen text 2" screenText2
      test3 = P.parse parseScreens "Screen text 3" screenText3
      expect3 =
        Right [ testScreen1
              , testScreen31
              ]

testScreen1 :: Screen
testScreen1 = Screen "HDMI1" (1920, 1080) (1366, 1)

testScreen31 :: Screen
testScreen31 = Screen "LVDS1" (1378, 766) (0, 1)

screenText1 :: T.Text
screenText1 = "output HDMI1\nggfgdfg\nmode 1920x1080\npos 1366x1\nrate"
screenText2 :: T.Text
screenText2 = "output HDMI1\noff"
screenText3 :: T.Text
screenText3 =
  screenText1 <> "\n"
  <> screenText2 <> "\n" <>
  "output LVDS1\nuseless\nmode 1378x766\npos 0x1\nignore"

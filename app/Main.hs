{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (stdin)
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as P
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Randrendr
import Parse

main :: IO ()
main =
  do input <- TIO.hGetContents stdin
     let eScreens = P.parse parseScreens "stdin" input
     case eScreens of
       Left e -> print e
       Right screens ->
         mainWith $
         makeScreens screens # frame 10
  where
    makeScreens =
      mconcat . fmap makeScreen

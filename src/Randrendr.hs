module Randrendr
    ( makeScreen
    ) where

import Control.Arrow
import Control.Monad
import qualified Data.Text as T

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Types

makeScreen :: Screen -> Diagram B
makeScreen s =
  (title <> screen # frame 10)
  # fontSize titleSize
  # alignBL
  # translate pos
  # reflectY
  where
    tupleToInt = join (***) fromIntegral
    size = tupleToInt (s_size s)
    bgColor = gray `withOpacity` 0.8
    outlineColor = black
    pos = r2 $ tupleToInt (s_pos s)
    title =
      text (T.unpack $ s_name s)
      # fontSize titleSize
      # reflectY
    screen =
      uncurry rect size
      # fcA bgColor
      # lc outlineColor
    titleSize = local 150

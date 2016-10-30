module Types where

import qualified Data.Text as T

type Width = Int
type Height = Int

data Screen =
  Screen
  { s_name :: T.Text
  , s_size :: (Width, Height)
  , s_pos :: (Width, Height)
  } deriving (Show, Eq)

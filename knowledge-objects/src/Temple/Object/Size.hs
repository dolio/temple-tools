
module Temple.Object.Size where

data Size
  = Fine
  | Diminutive
  | Tiny
  | Small
  | Medium
  | Large
  | Huge
  | Gargantuan
  | Colossal
  deriving (Bounded, Enum, Eq, Ord, Show)

module Pangram
    ( isPangram
    ) where
import           Data.Char                      ( isLetter
                                                , ord
                                                , toLower
                                                )
import           Data.List                      ( sort )

isPangram :: String -> Bool
isPangram = isPangram' ['a' .. 'z'] . sort . map toLower . filter isLetter

isPangram' :: String -> String -> Bool
isPangram' []       _        = True
isPangram' _        []       = False
isPangram' (x : xs) (y : ys) = case compare xChr yChr of
    LT -> False
    EQ -> isPangram' xs ys
    GT -> isPangram' (x : xs) ys
  where
    xChr = ord x
    yChr = ord y

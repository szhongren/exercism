module DNA
    ( toRNA
    ) where
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                , mapMaybe
                                                )

toRNA :: String -> Either Char String
toRNA rna | all (isJust . toRNASingle) rna = Right $ mapMaybe toRNASingle rna
          | otherwise = Left $ head $ filter (isNothing . toRNASingle) rna

toRNASingle :: Char -> Maybe Char
toRNASingle c = case c of
    'G' -> Just 'C'
    'C' -> Just 'G'
    'T' -> Just 'A'
    'A' -> Just 'U'
    _   -> Nothing

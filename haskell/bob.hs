{-# LANGUAGE OverloadedStrings #-}
module Bob
    ( responseFor
    ) where

import           Data.Char                      ( isLetter
                                                , isSpace
                                                , isUpper
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

responseFor :: String -> String
responseFor "" = "Fine. Be that way!"
responseFor prompt
    | all isSpace strippedPrompt
    = "Fine. Be that way!"
    | last strippedPrompt == '?' && any isLetter strippedPrompt && all
        isUpper
        (filter isLetter strippedPrompt)
    = "Calm down, I know what I'm doing!"
    | last strippedPrompt == '?'
    = "Sure."
    | any isLetter strippedPrompt
        && all isUpper (filter isLetter strippedPrompt)
    = "Whoa, chill out!"
    | otherwise
    = "Whatever."
    where strippedPrompt = strip prompt

lstrip :: String -> String
lstrip "" = ""
lstrip (x : xs) | isSpace x = lstrip xs
                | otherwise = x : xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = rstrip . lstrip

responseFor' :: Text -> Text
responseFor' prompt
    | T.last prompt == '?' && T.all isUpper prompt
    = "Calm down, I know what I'm doing!"
    | T.last prompt == '?'
    = "Sure."
    | T.all isUpper prompt
    = "Whoa, chill out!"
    | T.length prompt == 0
    = "Fine. Be that way!"
    | otherwise
    = "Whatever."


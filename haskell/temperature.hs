module Temperature
    ( tempToC
    , tempToF
    ) where
tempToC :: Integer -> Float
tempToC f = (fromIntegral f - 32) / 1.8

tempToF :: Float -> Integer
tempToF c = ceiling (c * 1.8 + 32)

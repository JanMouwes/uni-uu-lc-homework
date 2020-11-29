module Chapter4 where


-- Examples of 

-- data Digits = Single Dig | Multiple Dig Digits

-- data SignedInt = SD (Maybe Sign) Digits
-- data Sign = Plus | Minus

type Parser s r = [s] -> (r, [s])

-- floatLiteral :: Parser Char Float
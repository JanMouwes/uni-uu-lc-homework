module Chapter4 where

import Data.Char

-- Exercise 4.1
data FloatLiteral = 
    Full Int (Maybe Int) (Maybe Int) (Maybe FloatSuffix)
    | NoInt Int (Maybe Int) (Maybe FloatSuffix)
    | JustExponent Int Int (Maybe FloatSuffix)
    | JustSuffix Int (Maybe Int) FloatSuffix
    deriving Show

data FloatData = Fl Int Int Int

data FloatSuffix = SmallF | BigF | SmallD | BigD 
    deriving Show

type ParseState = (Maybe Int, Maybe Int, Maybe Int, Maybe FloatSuffix)

floatLiteral :: String -> Maybe FloatLiteral
floatLiteral = constructFloat . parseFloat
    where
        constructFloat :: ParseState -> Maybe FloatLiteral
        constructFloat (Nothing, Just fract, exp, fls) = Just $ NoInt fract exp fls
        constructFloat (Just i, Nothing, Just exp, fls) = Just $ JustExponent i exp fls
        constructFloat (Just i, Nothing, exp, Just fls) = Just $ JustSuffix i exp fls
        constructFloat (Just i, fract, exp, fls) = Just $ Full i fract exp fls
        constructFloat _ = Nothing

        parseFloat :: String -> ParseState
        parseFloat str = fst $ parseIntPart str (Nothing, Nothing, Nothing, Nothing)

        parseIntPart :: String -> ParseState -> (ParseState, String)
        parseIntPart str (_, fr, ex, suf) = case takeDigits str of
            ([], rest) -> parseFractPart rest (Nothing, fr, ex, suf)
            (ints, rest) -> parseFractPart rest (Just $ read ints, fr, ex, suf)

        parseFractPart :: String -> ParseState -> (ParseState, String)
        parseFractPart str (int, _, ex, suf) = 
            let (mayb, rest) = parsePrefixedInt '.' str
            in parseExpPart rest (int, mayb, ex, suf)

        parseExpPart :: String -> ParseState -> (ParseState, String)
        parseExpPart str (int, fr, _, suf)= 
            let (mayb, rest) = parsePrefixedInt 'E' str
            in parseSufPart rest (int, fr, mayb, suf)

        parseSufPart :: String -> ParseState -> (ParseState, String)
        parseSufPart ('F':rest) (int, fr, ex, _) = ((int, fr, ex, Just BigF), rest)
        parseSufPart ('f':rest) (int, fr, ex, _) = ((int, fr, ex, Just SmallF), rest)
        parseSufPart ('D':rest) (int, fr, ex, _) = ((int, fr, ex, Just BigD), rest)
        parseSufPart ('d':rest) (int, fr, ex, _) = ((int, fr, ex, Just SmallD), rest)
        parseSufPart rest (int, fr, ex, _) = ((int, fr, ex, Nothing), rest)

        parsePrefixedInt :: Char -> String -> (Maybe Int, String)
        parsePrefixedInt _ [] = (Nothing, "")
        parsePrefixedInt c (f:str)
            | f == c = let (digs, rest) = takeDigits str in (Just $ read digs, rest)
            | otherwise = (Nothing, f:str)

        takeDigits :: String -> (String, String)
        takeDigits = span isDigit

-- Exercise 4.2
signedFloat :: String -> Maybe Float
signedFloat str = case floatLiteral str of
    Nothing -> Nothing
    Just fl -> Just $ toFloat $ simplifyFloat fl
    where 
        toFloat :: FloatData -> Float
        toFloat (Fl int fr ex) = 
            read (show int ++ "." ++ show fr) * (10^ex)

simplifyFloat :: FloatLiteral -> FloatData
simplifyFloat (NoInt fr ex suf)         = simplifyFloat (Full 0 (Just fr) ex suf)
simplifyFloat (JustExponent int ex suf) = simplifyFloat (Full int (Just 0) (Just ex) suf)
simplifyFloat (JustSuffix int ex suf)   = simplifyFloat (Full int (Just 0) ex (Just suf))
simplifyFloat (Full int (Just fr) (Just ex) _) = Fl int fr ex
simplifyFloat (Full int (Just fr) Nothing _)   = Fl int fr 0
simplifyFloat (Full int Nothing (Just ex) _)   = Fl int 0 ex
simplifyFloat (Full int Nothing Nothing _)     = Fl int 0 0
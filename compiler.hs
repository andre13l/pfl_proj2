module Compiler where

import Datastructs
import Data.Char (isDigit)

compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MulExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB TrueB = [Tru]
compB FalseB = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]

compile :: Program -> Code
compile program = concatMap compileStm program
    where
        compileStm :: Stm -> Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
        compileStm (While bexp stm) = [Loop (compB bexp) (compileStm stm)]
        compileStm NoopStm = []


-- verify if this is according the rules defined 
-- lexer takes a string as input and returns a list of tokens.
-- the span is used to extract the longest prefix of characters that are not in the set " +-*;()=". This prefix is considered a token (word).
-- The function then recursively calls itself with the remaining characters (rest) in the input string.
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':cs) = ":=" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('>':'=':cs) = ">=" : lexer cs
lexer (c:cs)
        | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
        | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (c:cs)
                                    in word : lexer rest


-- este foi o gpt que fez, depois temos de ver qual deixamos ficar
{-
lexer :: String -> [String]
lexer "" = []
lexer s@(c:cs)
  | isSpace c = lexer cs          -- Skip whitespace
  | isAlpha c && isLower c = takeWhile isAlphaNum s : lexer (dropWhile isAlphaNum s)
  | isDigit c = takeWhile isDigit s : lexer (dropWhile isDigit s)
  | otherwise = [c] : lexer cs
-}


-- -------------------------------------------------------------
-- need to check if this is correct
{-
type Parser a = [String] -> Maybe (a, [String])

parseAexp :: Parser Aexp
parseAexp tokens = case parseTerm tokens of
    Just (term, "-":tokens') -> case parseAexp tokens' of
        Just (aexp, tokens'') -> Just (SubExp term aexp, tokens'')
        _ -> Just (term, tokens')
    Just (term, tokens') -> Just (term, tokens')
    _ -> Nothing

parseTerm :: Parser Aexp
parseTerm (token:tokens)
    | all isDigit token = Just (Num (read token), tokens)
    | otherwise = Just (Var token, tokens)

parseBexp :: Parser Bexp
parseBexp tokens = case parseAexp tokens of
    Just (aexp1, "=":tokens') -> case parseAexp tokens' of
        Just (aexp2, tokens'') -> Just (EquExp aexp1 aexp2, tokens'')
        _ -> Nothing
    _ -> Nothing

parseStm :: Parser Stm
parseStm (token:":=":tokens) = case parseAexp tokens of
    Just (aexp, tokens') -> Just (Assign token aexp, tokens')
    _ -> Nothing
parseStm _ = Nothing

parseProgram :: Parser Program
parseProgram [] = Just ([], [])
parseProgram tokens = case parseStm tokens of
    Just (stm, ";":tokens') -> case parseProgram tokens' of
        Just (program, tokens'') -> Just (stm:program, tokens'')
        _ -> Just ([stm], tokens')
    Just (stm, tokens') -> Just ([stm], tokens')
    _ -> Nothing

parse :: String -> Maybe Program
parse = fmap fst . parseProgram . lexer
-}

-- -------------------------------

parse :: String -> Program
parse input = buildData (lexer input)

buildData :: [String] -> [Stm]
buildData [] = []
buildData tokens = case parseStm tokens of
    (stm, restTokens) -> stm : buildData restTokens

parseStm :: [String] -> (Stm, [String])
parseStm ("if":rest) = parseIf rest
parseStm ("while":rest) = parseWhile rest
parseStm (var:":=":rest) = parseAssign var rest
parseStm _ = (NoopStm, [])

parseIf :: [String] -> (Stm, [String])
parseIf tokens = case parseBexp tokens of
    (condition, "then":rest1) ->
        case parseStm rest1 of
        (thenBranch, "else":rest2) ->
            case parseStm rest2 of
            (elseBranch, rest3) -> (If condition thenBranch elseBranch, rest3)
        _ -> (NoopStm, [])
    _ -> (NoopStm, [])

parseWhile :: [String] -> (Stm, [String])
parseWhile tokens = case parseBexp tokens of
    (condition, "do":rest1) ->
        case parseStm rest1 of
        (body, rest2) -> (While condition body, rest2)
    _ -> (NoopStm, [])

parseAssign :: String -> [String] -> (Stm, [String])
parseAssign var tokens = case parseAexp tokens of
  (expression, rest) -> (Assign var expression, rest)

parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens = parseAddExp tokens

parseAddExp :: [String] -> (Aexp, [String])
parseAddExp tokens =
    let (left, rest1) = parseMulExp tokens
    in case rest1 of
        ("+":rest2) ->
        let (right, rest3) = parseAddExp rest2
        in (AddExp left right, rest3)
        _ -> (left, rest1)

parseMulExp :: [String] -> (Aexp, [String])
parseMulExp tokens =
    let (left, rest1) = parseAexpAtom tokens
    in case rest1 of
        ("*":rest2) ->
        let (right, rest3) = parseMulExp rest2
        in (MulExp left right, rest3)
        _ -> (left, rest1)

parseAexpAtom :: [String] -> (Aexp, [String])
parseAexpAtom ("(":rest) =
    let (aexp, rest') = parseAexp rest
    in case rest' of
        (")":rest'') -> (aexp, rest'')
        _ -> (NoopAexp, rest)
parseAexpAtom (var:rest) = (Var var, rest)
parseAexpAtom (num:rest) = (Num (read num), rest)
parseAexpAtom _ = (NoopAexp, [])


parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens = parseOrExp tokens

parseOrExp :: [String] -> (Bexp, [String])
parseOrExp tokens =
    let (left, rest1) = parseAndExp tokens
    in case rest1 of
        ("or":rest2) ->
        let (right, rest3) = parseOrExp rest2
        in (OrExp left right, rest3)
        _ -> (left, rest1)

parseAndExp :: [String] -> (Bexp, [String])
parseAndExp tokens =
    let (left, rest1) = parseBexpAtom tokens
    in case rest1 of
        ("and":rest2) ->
        let (right, rest3) = parseAndExp rest2
        in (AndExp left right, rest3)
        _ -> (left, rest1)

parseBexpAtom :: [String] -> (Bexp, [String])
parseBexpAtom ("(":rest) =
    let (bexp, rest') = parseBexp rest
    in case rest' of
        (")":rest'') -> (bexp, rest'')
        _ -> (NoopBexp, rest)
parseBexpAtom ("not":rest) =
    let (bexp, rest') = parseBexpAtom rest
    in (Not bexp, rest')
parseBexpAtom ("true":rest) = (TrueB, rest)
parseBexpAtom ("false":rest) = (FalseB, rest)
parseBexpAtom tokens =
    let (left, rest1) = parseAexp tokens
    in case rest1 of
        ("=":rest2) ->
        let (right, rest3) = parseAexp rest2
        in (Eq left right, rest3)
        ("<=":rest2) ->
        let (right, rest3) = parseAexp rest2
        in (LeExp left right, rest3)
        _ -> (NoopBexp, rest1)
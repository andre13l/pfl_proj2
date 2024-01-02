module Compiler where

import Datastructs

import Data.Char (isDigit, isAlpha)

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = Push 0 : compA a1 ++ [Add] ++ compA a2 ++ [Add]
compA (SubExp a1 a2) = Push 0 : compA a1 ++ [Sub] ++ compA a2 ++ [Add]
compA (MulExp a1 a2) = Push 1 : compA a1 ++ [Mult] ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB TrueB = [Tru]
compB FalseB = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = [Tru] ++ compB b1 ++ [Branch (compB b2) [Fals]]
compB (LeExp a1 a2) = [Fals] ++ compA a1 ++ compA a2 ++ [Le]
compB (Eq b1 b2) = [Fals] ++ compB b1 ++ compB b2 ++ [Equ]
compB (DoubleEq a1 a2) = [Fals] ++ compA a1 ++ compA a2 ++ [Equ]

compile :: Super -> Code
compile stm = concatMap compileStm stm
    where
        compileStm :: Stm -> Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
        compileStm (While bexp stm) = compB bexp ++ [Loop (compile stm) (compB bexp ++ [Branch (compile stm) [Noop]])]
        compileStm NoopStm = []
        


-- verify if this is according the rules defined 
-- lexer takes a string as input and returns a list of tokens.
-- the span is used to extract the longest prefix of characters that are not in the set " +-*;()=". This prefix is considered a token (word).
-- The function then recursively calls itself with the remaining characters (rest) in the input string.

-- Auxiliar functions to test chars
isSpaceAux :: Char -> Bool
isSpaceAux ch | ch == ' ' = True
           | ch == '\n' = True
           | ch == '\t' = True
           | otherwise = False

lexer :: String -> [String]
lexer [] = []
lexer (':' : '=' : restStr) = ":=" : lexer restStr
lexer ('<' : '=' : restStr) = "<=" : lexer restStr
lexer ('=' : '=' : restStr) = "==" : lexer restStr
lexer ('>' : '=' : restStr) = ">=" : lexer restStr
lexer ('=' : restStr) = "=" : lexer restStr
lexer ('(' : restStr) = "(" : lexer restStr
lexer (')' : restStr) = ")" : lexer restStr
lexer ('+' : restStr) = "+" : lexer restStr
lexer ('-' : restStr) = "-" : lexer restStr
lexer ('*' : restStr) = "*" : lexer restStr
lexer ('/' : restStr) = "/" : lexer restStr
lexer ('<' : restStr) = "<" : lexer restStr
lexer ('>' : restStr) = ">" : lexer restStr
lexer ('&' : restStr) = "&" : lexer restStr
lexer ('|' : restStr) = "|" : lexer restStr
lexer ('!' : restStr) = "!" : lexer restStr
lexer ('&':'&': restStr) = "&&" : lexer restStr
lexer ('|':'|': restStr) = "||" : lexer restStr
lexer (ch : restStr)
        | ch `elem` " +-*;()=" = if ch == ' ' then lexer restStr else [ch] : lexer restStr
        | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (ch : restStr)
                                    in word : lexer rest
-- lexer (ch : restStr)
--         | ch `elem` " +-*;()=" = if ch == ' ' then lexer restStr else [ch] : lexer restSTr
--         | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (ch : restStr) 
--             in case rest of
--                 (':':'=':rest) -> word : "=" : lexer rest
--                 _ -> word : lexer rest
--
-- lexer str@(ch : restStr)
--   | isAlpha ch = let (varStr, restStr') = span isAlphaNumOrUnderscore str
--                   in varStr : lexer restStr'
--   | isDigit ch = let (digitStr, restStr') = span isDigit str
--                   in digitStr : lexer restStr'
--   | otherwise = [ch] : lexer restStr


-- -------------------------------------------------------------

-- parse :: String -> Super
-- parse input = buildData (lexer input)

-- buildData :: [String] -> [Stm]
-- buildData [] = []
-- buildData tokens = case parseStm tokens of
--     (stm, restTokens) -> stm : buildData restTokens

-- parseStm :: [String] -> (Stm, [String])
-- parseStm ("if" : rest) = parseIf rest
-- parseStm ("while" : rest) = parseWhile rest
-- parseStm (var : ":=" : rest) = parseAssign var rest
-- parseStm _ = (NoopStm, [])

-- parseIf :: [String] -> (Stm, [String])
-- parseIf tokens = case parseBexp tokens of
--     (condition, "then" : rest1) ->
--         case parseStm rest1 of
--         (thenBranch, "else" : rest2) ->
--             case parseStm rest2 of
--             (elseBranch, rest3) -> (If condition thenBranch elseBranch, rest3)
--         _ -> (NoopStm, [])
--     _ -> (NoopStm, [])

-- parseWhile :: [String] -> (Stm, [String])
-- parseWhile tokens = case parseBexp tokens of
--     (condition, "do" : rest1) ->
--         case parseStm rest1 of
--         (body, rest2) -> (While condition body, rest2)
--     _ -> (NoopStm, [])

-- parseAssign :: String -> [String] -> (Stm, [String])
-- parseAssign var tokens = case parseAexp tokens of
--   (expression, rest) -> (Assign var expression, rest)

-- parseAexp :: [String] -> (Aexp, [String])
-- parseAexp tokens = parseAddExp tokens

-- parseAddExp :: [String] -> (Aexp, [String])
-- parseAddExp tokens =
--     let (left, rest1) = parseMulExp tokens
--     in case rest1 of
--         ("+" : rest2) ->
--             let (right, rest3) = parseAddExp rest2
--             in (AddExp left right, rest3)
--         _ -> (left, rest1)

-- parseMulExp :: [String] -> (Aexp, [String])
-- parseMulExp tokens =
--     let (left, rest1) = parseAexpAtom tokens
--     in case rest1 of
--         ("*" : rest2) ->
--             let (right, rest3) = parseMulExp rest2
--             in (MulExp left right, rest3)
--         _ -> (left, rest1)

-- parseAexpAtom :: [String] -> (Aexp, [String])
-- parseAexpAtom ("(" : rest) =
--     let (aexp, rest') = parseAexp rest
--     in case rest' of
--         (")" : rest'') -> (aexp, rest'')
--         _ -> (Var "NoopAexp", rest)  -- Use Var constructor to match Aexp data structure
-- parseAexpAtom (var : rest) = (Var var, rest)  -- Use Var constructor to match Aexp data structure
-- parseAexpAtom (num : rest) = (Num (read num), rest)
-- parseAexpAtom _ = (Var "NoopAexp", [])  -- Use Var constructor to match Aexp data structure

-- parseOrExp :: [String] -> (Bexp, [String])
-- parseOrExp tokens =
--     let (left, rest1) = parseOrExp tokens
--     in case rest1 of
--         ("or" : rest2) ->
--             let (right, rest3) = parseOrExp rest2
--             in (OrExp left right, rest3)  -- Now using the OrExp constructor directly
--         _ -> (left, rest1)


-- parseBexp :: [String] -> (Bexp, [String])
-- parseBexp tokens = parseOrExp tokens

-- parseBexpAtom :: [String] -> (Bexp, [String])
-- parseBexpAtom ("(" : rest) =
--     let (bexp, rest') = parseBexp rest
--     in case rest' of
--         (")" : rest'') -> (bexp, rest'')
--         _ -> (bexp, rest')  -- If no closing parenthesis is found, return the parsed Bexp
-- parseBexpAtom ("not" : rest) =
--     let (bexp, rest') = parseBexpAtom rest
--     in (Not bexp, rest')  -- Use Not constructor to match Bexp data structure
-- parseBexpAtom ("true" : rest) = (TrueB, rest)
-- parseBexpAtom ("false" : rest) = (FalseB, rest)
-- parseBexpAtom tokens =
--     let (left, rest1) = parseAexp tokens
--     in case rest1 of
--         ("=" : rest2) ->
--             let (right, rest3) = parseAexp rest2
--             in (Eq left right, rest3)
--         ("<=" : rest2) ->
--             let (right, rest3) = parseAexp rest2
--             in (LeExp left right, rest3)
--         _ -> (TrueB, rest1)  -- Use NoopBexp constructor to match Bexp data structure



-- -----------------------------------------------------------------------------------
-- 

parse :: String -> Super
parse input = buildData (lexer input)

buildData :: Parser Super
buildData [] = []
buildData (firstToken:tokens)
    | firstToken == "if" = parseIf (firstToken:tokens)
    | firstToken == "while" = parseWhile (firstToken:tokens)
    | otherwise = case break (== ";") (firstToken:tokens) of
        (firstStm, ";":second) -> parseAssign firstStm: buildData second

        
parseAssign :: Parser Stm
parseAssign (token:":=":tokens) = Assign token (parseAexp tokens)

-- Parses an arithmetic expression
parseAexp :: Parser Aexp
parseAexp tokens = case nextValidToken (tokens,[]) "+" of
    (firstSegment, "+":secondSegment) -> AddExp (parseAexp firstSegment) (parseAexp secondSegment)
    _ -> case nextValidToken (tokens,[]) "-" of
        (firstSegment, "-":secondSegment) -> SubExp (parseAexp firstSegment) (parseAexp secondSegment)
        _ -> case nextValidToken (tokens,[]) "*" of
            (firstSegment, "*":secondSegment) -> MulExp (parseAexp firstSegment) (parseAexp secondSegment)
            _ -> case break (all isDigit) (reverse tokens) of
                (_, number:firstSegment) | check (reverse firstSegment) -> Num (read number)
                _ -> case break (all isAlpha) (reverse tokens) of
                    (_, name:firstSegment) | check (reverse firstSegment) -> Var name
                    _->case break (== "(") (reverse tokens) of
                        (middleAfter, "(":_) -> case break (==")") (reverse middleAfter) of
                            (middle, ")":_) -> parseAexp middle

-- Parses a boolean expression
parseBexp :: Parser Bexp
parseBexp tokens = case nextValidToken (tokens,[]) "and" of
    (firstSegment, "and":secondSegment) -> AndExp  (parseBexp firstSegment) (parseBexp secondSegment)
    _ -> case nextValidToken (tokens,[]) "=" of
        (firstSegment, "=":secondSegment) -> Eq (parseBexp firstSegment) (parseBexp secondSegment)
        _ -> case nextValidToken (tokens,[]) "not" of
            (_,"not":secondSegment) -> Not (parseBexp secondSegment)
            _ -> case nextValidToken (tokens,[]) "==" of
                (firstSegment, "==":secondSegment) -> DoubleEq (parseAexp firstSegment) (parseAexp secondSegment)
                _ -> case nextValidToken (tokens,[]) "<=" of
                    (firstSegment, "<=":secondSegment) -> LeExp (parseAexp firstSegment) (parseAexp secondSegment)
                    _ -> case break (== "True") (reverse tokens) of
                        (_, "True":firstSegment) | check (reverse firstSegment) -> TrueB
                        _ -> case break (== "False") (reverse tokens) of
                            (_, "False":firstSegment) | check (reverse firstSegment) -> FalseB
                            _->case break (== "(") (reverse tokens) of
                                (middleAfter, "(":_) -> case break (==")") (reverse middleAfter) of
                                    (middle, ")":_) -> parseBexp middle

-- Parses an if statement
parseIf :: Parser Super
parseIf ("if":tokens) = case break (=="then") tokens of
    (ifStm , rest) -> case breakOnElse ([],rest) 0 of
        ("then":"(":thenStm', "else":"(":elseStm') ->
            let ")":thenStm = reverse thenStm'
                (elseStm,")":";":next) = breakOnParenthesis ([],elseStm') 0
            in If (parseBexp ifStm) (buildData (reverse thenStm)) (buildData elseStm):buildData next
        ("then":"(":thenStm', "else":elseStm) ->
            let ")":thenStm = reverse thenStm'
            in [If (parseBexp ifStm) (buildData (reverse thenStm)) [parseAssign elseStm]]
        ("then":thenStm, "else":"(":elseStm') ->
            let (elseStm,")":";":next) = breakOnParenthesis ([],elseStm') 0
            in (If (parseBexp ifStm) [parseAssign thenStm] (buildData elseStm):buildData next)
        ("then":thenStm, "else":elseStm) -> case break (==";") elseStm of
            (elseStm, ";":next) -> If (parseBexp ifStm) [parseAssign thenStm] [parseAssign elseStm]:buildData next

-- Parses a while statement
parseWhile :: Parser Super
parseWhile ("while":tokens) = case break (== "do") tokens of
    ("(":whileStm, "do":"(":after) ->
        let (doStm,")":";":next) = breakOnParenthesis ([],after) 0
        in (While (parseBexp whileStm) (buildData doStm):buildData next)
    ("(":whileStm, "do":doStm) -> [While (parseBexp whileStm) [parseAssign doStm]]
    (whileStm, "do":"(":after) ->
        let (doStm,")":";":next) = breakOnParenthesis ([],after) 0
        in (While (parseBexp whileStm) (buildData doStm):buildData next)
    (whileStm, "do":doStm) -> [While (parseBexp whileStm) [parseAssign doStm]]

breakOnElse :: ([String], [String]) -> Int -> ([String], [String])
breakOnElse (_, []) _ = ([], [])
breakOnElse (left, x:right) count
    | x == "if" = breakOnElse (left ++ [x], right) (count+1)
    | x == "else" && (count /= 0) = breakOnElse (left ++ [x], right) (count-1)
    | x == "else" = (left, x:right)
    | otherwise = breakOnElse (left ++ [x], right) count


breakOnParenthesis :: ([String], [String]) -> Int -> ([String], [String])
breakOnParenthesis (left, []) _ = (left, [])
breakOnParenthesis (left, x:right) count
    | x == "(" = breakOnParenthesis (left ++ [x], right) (count+1)
    | x == ")" && (count /= 0) = breakOnParenthesis (left ++ [x], right) (count-1)
    | x == ")" = (left, x:right)
    | otherwise = breakOnParenthesis (left ++ [x], right) count

-- Used to find the next valid token in the list of tokens
nextValidToken :: ([String],[String]) -> String -> ([String],[String])
nextValidToken ([], x:right) token
    | x==token = ([], x:right)
nextValidToken ([], right) _ = ([], right)
nextValidToken (left, []) token = nextValidToken (init left , [last left]) token
nextValidToken (left, x:right) token
    | x == token && check left = (left , x:right)
    | otherwise = nextValidToken (init left , last left:x:right) token

check :: [String] -> Bool
check = (== 0) . foldl updateCount 0
  where
    updateCount count "(" = count + 1
    updateCount count ")" = max 0 (count - 1)
    updateCount count _   = count

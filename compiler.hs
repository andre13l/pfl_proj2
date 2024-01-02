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
        

lexer :: String -> [String]
lexer [] = []
lexer (':':'=':cs) = ":=" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('>':'=':cs) = ">=" : lexer cs
lexer (c:cs)
        | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
        | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (c:cs) 
            in case rest of
                (':':'=':rest) -> word : "=" : lexer rest
                _ -> word : lexer rest

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

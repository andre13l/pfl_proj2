module Tests where
import Datastructs
import Compiler
import Data.Map.Strict as HashMap

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

run (Push n:rest, stack, state) = run (rest, SInt n:stack, state)

run (Add:rest, SInt i1:SInt i2:stack, state) = run (rest, SInt (i1 + i2):stack, state)
run (Add:rest, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Mult:rest, SInt i1:SInt i2:stack, state) = run (rest, SInt (i1 * i2):stack, state)
run (Mult:rest, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Sub:rest, SInt i1:SInt i2:stack, state) = run (rest, SInt (i1 - i2):stack, state)
run (Sub:rest, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Tru:rest, stack, state) = run (rest, TT:stack, state)

run (Fals:rest, stack, state) = run (rest, FF:stack, state)

run (Equ:rest, value1:value2:stack, state) = run (rest, value:stack, state)
    where value = if value1 == value2 then TT else FF

run (Le:rest, SInt i1:SInt i2:stack, state) = run (rest, value:stack, state)
    where value = if i1 <= i2 then TT else FF
run (Le:rest, _:_:stack, state) = error "Runtime error: Invalid operation"

run (And:rest, TT:TT:stack, state) = run (rest, TT:stack, state)
run (And:rest, TT:FF:stack, state) = run (rest, FF:stack, state)
run (And:rest, FF:TT:stack, state) = run (rest, FF:stack, state)
run (And:rest, FF:FF:stack, state) = run (rest, FF:stack, state)
run (And:rest, SInt i:stack, state) = error "Runtime error: Invalid operation"

run (Neg:rest, TT:stack, state) = run (rest, FF:stack, state)
run (Neg:rest, FF:stack, state) = run (rest, TT:stack, state)
run (Neg:rest, SInt i:stack, state) = error "Runtime error: Invalid operation"

run (Fetch key:rest, stack, state) = run (rest, value:stack, state)
    where value = case HashMap.lookup key state of
            Just val -> val
            Nothing -> error "Runtime error: Variable not found"

run (Store key:rest, stacktop:stack, state) = run (rest, stack, insertIntoState key stacktop state)

run (Branch code1 code2:rest, TT:remainingStack, state) = run (code1, remainingStack, state)
run (Branch code1 code2:rest, FF:remainingStack, state) = run (code2, remainingStack, state)
run (Branch code1 code2:rest, SInt i:remainingStack, state) = error "Runtime error: Invalid operation"

run (Loop code1 code2:rest, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)

run (Noop:rest, stack, state) = run (rest, stack, state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

runTests :: IO ()
runTests = do
    print "1st part tests:"
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ testAssembler [Push (-20),Push (-11), Le] == ("False","")
    print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

runTests2 :: IO ()
runTests2 = do
    putStrLn ""
    print "2nd part tests:"
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "x := 0 - 2;" == ("","x=-2")
    print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

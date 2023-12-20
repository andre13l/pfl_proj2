module Datastructs where

import Data.List
import qualified Data.Map.Strict as HashMap
import Data.Map (toList)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop | Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackItem = SInt Integer | TT | FF deriving (Show, Eq)
type Stack = [StackItem]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

stackValue :: StackItem -> String
stackValue (SInt n) = show n
stackValue TT = "True"
stackValue FF = "False"

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValue stack)

type Key = String
type Val = StackItem

type State = HashMap.Map Key Val

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = HashMap.empty

insertIntoState :: Key -> Val -> State -> State
insertIntoState = HashMap.insert

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ stackValue val | (var, val) <- toList state]

-- Arithmetic Expressions
data Aexp
  = Num Integer              -- Numeric constant
  | Var String               -- Variable
  | AddExp Aexp Aexp            -- Addition
  | SubExp Aexp Aexp            -- Subtraction
  | MulExp Aexp Aexp            -- Multiplication
  deriving Show

-- Boolean Expressions
data Bexp
  = TrueB                    -- True constant
  | FalseB                   -- False constant
  | Eq Aexp Aexp             -- Equality
  | LeExp Aexp Aexp             -- Less than or equal
  | Not Bexp                 -- Logical NOT
  | AndExp Bexp Bexp            -- Logical AND
  deriving Show

-- Statements
data Stm
  = Assign String Aexp       -- Assignment: variable := arithmetic expression
  | If Bexp Stm Stm          -- Conditional: if boolean expression then statement else statement
  | While Bexp Stm           -- Loop: while boolean expression do statement
  | NoopStm                  -- Noop
  deriving Show

-- Program
type Program = [Stm]

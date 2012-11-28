
{- Module: DCPL.PostFix.Eval

   A simple Evaluator for the Postfix Language.
-}


module DCPL.PostFix.Eval
( postfix
) where


import DCPL.PostFix.Command
import Data.List
import Control.Monad


type Stack = [Command]


left msg stack = Left $ msg ++ "\nStack: " ++ show stack

eval:: Command -> Stack -> Either String Stack 
eval n@(Num _) = push n
eval s@(Seq _) = push s
eval c = op
   where
      op = case c of
         Add -> arithm (+)
         Sub -> arithm (-)
         Mul -> arithm (*)
         Div -> arithm quot
         Rem -> arithm rem
         Eq ->  bool (==)
         Lt ->  bool (<)
         Gt ->  bool (>)
         Pop -> pop
         Swap -> swap
         Sel -> sel
         NGet -> nget
         Exec -> exec


push c = Right . (c:)

arithm op (Num x:Num y:xs) = push (Num $ op x y) xs
arithm _ st = left "Not enough numbers for arithmetic operation" st

bool op (Num x:Num y:xs) = let res = if op y x then 1 else 0 in push (Num res) xs
bool _ st = left "Not enough numbers for boolean operation" st

pop (_:xs) = Right xs
pop st = left "Can't pop on empty stack" st

swap (x:y:xs) = Right $ y:x:xs
swap st = left "Not enough values to swap" st

sel (x:y:Num z:xs) = let res = if z == 0 then x else y in push res xs 
sel st@(_:_:_:_) = left "The third parameter on the stack must be a numeral" st 
sel st  = left "Not enough values to select from" st

nget st@(Num i:xs) 
   | length xs > (i-1) = case xs !! (i-1) of
      n@(Num _) -> push n xs
      _ -> left ("The stack element at index " ++ show i ++ " is not a numeral") st
   | otherwise = left ("Index " ++ show i ++ " too large for nget") st 
nget st = left "The parameter for nget must be a numeral" st

exec (Seq cmds:xs) = evalCmds xs cmds 
exec st@(top:_) = left ("Exec expects a sequence on top of stack but found: " ++ show top) st
exec st = left "Can't perform Exec on empty stack" st

evalCmds = foldM (flip eval)


postfix :: Stack -> [Command] -> (Either String Int) 
postfix st cmds = evalCmds st cmds >>= extractResult
   where 
      extractResult (Num n:xs) = Right n
      extractResult (_:_) = Left "Result value is not a numeral"
      extractResult _ = Left "No result value left on stack"


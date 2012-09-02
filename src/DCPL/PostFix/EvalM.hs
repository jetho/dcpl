
module DCPL.PostFix.EvalM

where

import DCPL.PostFix.Command
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity


type Stack = [Command]
type EvalM a = StateT Stack (ErrorT String Identity) a

eval :: Command -> EvalM ()
eval s@(Seq _) = push s
eval n@(Num _) = push n 
eval c = manipStack operation
	where operation = case c of
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
		--todo  NGet -> nget
		--todo -> exec 

manipStack action = do
	stack <- get
	action(stack)

push c = modify (c:) >> return ()

arithm op (Num x:Num y:_) = modify (\stack -> (Num $ op x y) : drop 2 stack) >> return ()
arithm op _ = throwError "Not enough numbers for arithmetic operation"

bool op (Num x:Num y:_) = let res = if op y x then 1 else 0 in push (Num res) >> return ()
bool op _ = throwError "Not enough numbers for boolean operation"

pop (_:xs) = put xs >> return ()
pop [] = throwError "Can't pop on empty stack"

swap (x:y:xs) = put (y:x:xs) >> return ()
swap _ = throwError "Not enough values to swap"

sel (x:y:Num z:xs) = let res = if z == 0 then x else y in push res >> return ()
sel (_:_:_:_) = throwError "Third parameter on the stack must be numerical" 
sel _  = throwError "Not enough values to select from"

		
runEvalM :: Stack -> EvalM a -> (Either String (a, Stack))
runEvalM st e = runIdentity(runErrorT (runStateT e st))

--evalSeq = foldl1 (>>) . map eval

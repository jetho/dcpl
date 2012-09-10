
{- Module: DCPL.PostFix.EvalM

   An alternative implementation of the PostFix evaluator using a monad transformer
   stack consisting of StateT and ErrorT. While this is probably overkill for the 
   simple PostFix language, it's instructive to play with some other approaches :) 
-}


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
		NGet -> nget
		Exec -> exec
      

manipStack op = do
   stack <- get
   op(stack)

push c = modify (c:) >> return ()

arithm op (Num x:Num y:_) = modify (\stack -> (Num $ op x y) : drop 2 stack) >> return ()
arithm op _ = throwError "Not enough numbers for arithmetic operation"

bool op (Num x:Num y:_) = let res = if op y x then 1 else 0 in push (Num res)
bool op _ = throwError "Not enough numbers for boolean operation"

pop (_:xs) = put xs >> return ()
pop [] = throwError "Can't pop on empty stack"

swap (x:y:xs) = put (y:x:xs) >> return ()
swap _ = throwError "Not enough values to swap"

sel (x:y:Num z:xs) = let res = if z == 0 then x else y in push res 
sel (_:_:_:_) = throwError "The third parameter on the stack must be a numeral" 
sel _  = throwError "Not enough values to select from"

nget (Num (i+1):xs) 
	| length xs > i = case xs !! i of
		n@(Num _) -> push n
		_ -> throwError $ "The stack element at index " ++ show i ++ " is not a numeral"
	| otherwise = throwError $ "Index " ++ show i ++ " too large for nget"
nget _ = throwError "The parameter for nget must be a numeral"

exec (Seq cmds:xs) = put xs >> chainCmds cmds
exec (top@_:_) = throwError $ "Exec expects a sequence on top of stack but found: " ++ show top
exec _ = throwError "Can't perform Exec on empty stack"

chainCmds = foldl1 (>>) . map eval

runEvalM :: Stack -> EvalM a -> (Either String (a, Stack))
runEvalM st e = runIdentity(runErrorT (runStateT e st))

extractResult (_, Num n:xs) = Right n
extractResult (_, _:_) = Left "Result value is not a numeral"
extractResult _ = Left "No result value left on stack"

postfix :: Stack -> [Command] -> (Either String Int) 
postfix st cmds = runEvalM st (chainCmds cmds) >>= extractResult 


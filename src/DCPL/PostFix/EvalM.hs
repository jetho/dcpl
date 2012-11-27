
{- Module: DCPL.PostFix.EvalM

   An alternative implementation of the PostFix evaluator using a monad transformer
   stack consisting of StateT and ErrorT. While this is probably overkill for the 
   simple PostFix language, it's instructive to play with some other approaches :) 
-}


module DCPL.PostFix.EvalM
( postfix
) where


import DCPL.PostFix.Command
import Data.List
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity


type Stack = [Command]
type EvalM a = StateT Stack (ErrorT String Identity) a


eval :: Command -> EvalM ()
eval s@(Seq _) = push s
eval n@(Num _) = push n 
eval c = stackExec op
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
      

stackExec op = do
   stack <- get
   op stack

throwErrorS msg = stackExec $ throwError . (++) (msg ++ "\nStack: ") . show
  

push c = modify (c:)

arithm op (Num x:Num y:_) = modify (\stack -> (Num $ op x y) : drop 2 stack)
arithm op _ = throwErrorS "Not enough numbers for arithmetic operation"

bool op (Num x:Num y:_) = let res = if op y x then 1 else 0 in push (Num res)
bool op _ = throwErrorS "Not enough numbers for boolean operation"

pop (_:xs) = put xs
pop [] = throwErrorS "Can't pop on empty stack"

swap (x:y:xs) = put (y:x:xs)
swap _ = throwErrorS "Not enough values to swap"

sel (x:y:Num z:xs) = let res = if z == 0 then x else y in push res 
sel (_:_:_:_) = throwErrorS "The third parameter on the stack must be a numeral" 
sel _  = throwErrorS "Not enough values to select from"

nget (Num i:xs) 
   | length xs > (i-1) = case xs !! (i-1) of
      n@(Num _) -> push n
      _ -> throwErrorS $ "The stack element at index " ++ show i ++ " is not a numeral"
   | otherwise = throwErrorS $ "Index " ++ show i ++ " too large for nget" 
nget _ = throwErrorS "The parameter for nget must be a numeral"

exec (Seq cmds:xs) = put xs >> chainCmds cmds
exec (top:_) = throwErrorS $ "Exec expects a sequence on top of stack but found: " ++ show top
exec _ = throwErrorS "Can't perform Exec on empty stack"

chainCmds = mapM_ eval

runEvalM :: Stack -> EvalM a -> (Either String (a, Stack))
runEvalM st e = runIdentity(runErrorT (runStateT e st))

postfix :: Stack -> [Command] -> (Either String Int) 
postfix st cmds = runEvalM st (chainCmds cmds) >>= extractResult
   where 
      extractResult (_, Num n:xs) = Right n
      extractResult (_, _:_) = Left "Result value is not a numeral"
      extractResult _ = Left "No result value left on stack"


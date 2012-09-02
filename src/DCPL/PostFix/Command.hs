
module DCPL.PostFix.Command
where

data Command = Num Int
             | Eq | Gt | Lt   
             | Swap | Pop | Sel | NGet | Exec
             | Add | Sub | Mul | Div | Rem
             | Seq [Command]
  deriving (Show, Eq)
module Program(Command(Inv,Swap,Push,Pop,NE,NZ),Program,exec) where

data Command = Inv | Swap | Push | Pop | NE [Command] | NZ [Command]

type Program = [Command]

type Config = (Program,[Bool],[Bool],Bool)

instance Show Command where
  show Inv = "!"
  show Swap = "_"
  show Push = "v"
  show Pop = "^"
  show (NE p) = "(" ++ concatMap show p ++ ")"
  show (NZ p) = "[" ++ concatMap show p ++ "]"

step :: Config -> Config
step (Inv:p,l,r,a) = (p,l,r,not a)
step (Swap:p,l,r,a) = (p,r,l,a)
step (Push:p,l,r,a) = (p,a:l,r,a)
step (Pop:p,a:l,r,_) = (p,l,r,a)
step (Pop:p,[],r,a) = (p,[],r,a)
step (NE _:p,[],r,a) = (p,[],r,a)
step (NE i:p,l,r,a) = (\(l',r',a') -> step (NE i:p,l',r',a')) $ simulate i (l,r,a)
step (NZ _:p,l,r,False) = (p,l,r,False)
step (NZ i:p,l,r,a) = (\(l',r',a') -> step (NZ i:p,l',r',a')) $ simulate i (l,r,a)
step x = x

simulate :: Program -> ([Bool],[Bool],Bool) -> ([Bool],[Bool],Bool)
simulate [] c = c
simulate p (l,r,a) = (\(p',l',r',a') -> simulate p' (l',r',a')) $ step (p,l,r,a)

exec :: Program -> [Bool] -> [Bool]
exec p b = (\(_,r,_) -> r) $ simulate p (b,[],False)

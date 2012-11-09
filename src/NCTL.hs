module NCTL where

data NCTL a =
    NCTLFalse
  | NCTLAtom a
  | NCTLNot (NCTL a)
  | NCTLOr (NCTL a) (NCTL a) 
  | EX (NCTL a)
  | EU (NCTL a) (NCTL a)
  | EG (NCTL a)
  | Nested (NCTL a)
  deriving (Show,Eq)

nctlToStr :: (Show a) => NCTL a -> String
nctlToStr NCTLFalse = "False"
nctlToStr (NCTLAtom a) = show a
nctlToStr (NCTLNot f) = "~" ++ nctlToStr f
nctlToStr (NCTLOr f g) = (nctlToStr f) ++ " \\/ " ++ (nctlToStr g)
nctlToStr (EX f) = "EX " ++ (nctlToStr f)
nctlToStr (EU f g) = "EU(" ++ (nctlToStr f) ++ ", " ++ (nctlToStr g) ++ ")"
nctlToStr (EG f) = "EG " ++ (nctlToStr f)


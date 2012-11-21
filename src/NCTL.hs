module NCTL where

data NCTL a b =
    NCTLFalse
    | NCTLTrue
    | NCTLAtom a
    | NCTLNot (NCTL a b)
    | NCTLOr (NCTL a b) (NCTL a b) 
    | EX (NCTL a b)
    | EU (NCTL a b) (NCTL a b)
    | EG (NCTL a b)
    | Nmod (NNCTL a b) -- nested transition modality
    deriving (Show,Eq)

data NNCTL a b =
    NNCTLFalse
    | NNCTLTrue
    | NNCTLAtom (NTrans b)
    | NNCTLNot (NNCTL a b)
    | NNCTLOr (NNCTL a b) (NNCTL a b) 
    | NEX (NNCTL a b)
    | NEU (NNCTL a b) (NNCTL a b)
    | NEG (NNCTL a b)
    | NNmod (NCTL a b) -- nested transition modality 
    deriving (Show,Eq)

newtype NTrans a = NTrans (Int,a) -- t1[n2] = (2,t1)
                 deriving (Show,Ord,Eq) 

nctlToStr :: (Show a, Show b) => NCTL a b -> String
nctlToStr NCTLFalse = "False"
nctlToStr NCTLTrue = "False"
nctlToStr (NCTLAtom a) = show a
nctlToStr (NCTLNot f) = "~" ++ nctlToStr f
nctlToStr (NCTLOr f g) = (nctlToStr f) ++ " \\/ " ++ (nctlToStr g)
nctlToStr (EX f) = "EX " ++ (nctlToStr f)
nctlToStr (EU f g) = "EU(" ++ (nctlToStr f) ++ ", " ++ (nctlToStr g) ++ ")"
nctlToStr (EG f) = "EG " ++ (nctlToStr f)
nctlToStr (Nmod f) = "[" ++ (nnctlToStr f) ++ "]"

nnctlToStr :: (Show a, Show b) => NNCTL a b -> String
nnctlToStr NNCTLFalse = "False"
nnctlToStr NNCTLTrue = "True"
nnctlToStr (NNCTLAtom a) = show a
nnctlToStr (NNCTLNot f) = "~" ++ nnctlToStr f
nnctlToStr (NNCTLOr f g) = (nnctlToStr f) ++ " \\/ " ++ (nnctlToStr g)
nnctlToStr (NEX f) = "EX " ++ (nnctlToStr f)
nnctlToStr (NEU f g) = "EU(" ++ (nnctlToStr f) ++ ", " ++ (nnctlToStr g) ++ ")"
nnctlToStr (NEG f) = "EG " ++ (nnctlToStr f)
nnctlToStr (NNmod f) = "[" ++ (nctlToStr f) ++ "]"



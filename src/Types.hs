module Types where
import Lexer ()
import Data.List (nub)

type LPOD = [([Atom],[Literal])]

data Literal
    = NotL Atom
    | PosL Atom
    deriving (Eq, Show)

data Atom
    = NotA String
    | PosA String
    deriving (Eq, Show)

showLit :: Literal -> String
showLit (NotL a) = "not " ++ showAtom a
showLit (PosL a) = showAtom a

lit2atom :: Literal -> Atom
lit2atom (NotL a) = a
lit2atom (PosL a) = a

showAtom :: Atom -> String
showAtom (NotA a) = "-" ++ a
showAtom (PosA a) = a

showAtomIgnNot :: Atom -> String
showAtomIgnNot (PosA a) = a
showAtomIgnNot (NotA a) = a

getallatoms :: [[Atom]] -> [[Literal]] -> [Atom]
getallatoms atomlists litlists = nub allatoms
  where
    atomlist = concat atomlists
    atomsfromlits = concatMap (map lit2atom) litlists
    allatoms = atomlist ++ atomsfromlits
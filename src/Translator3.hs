module Translator where
import Types
import Data.List (partition, nub)

-- split LPOD rules into regular and ordered disjunctive (regular rules only have 1 atom in their head)
splitRules :: LPOD -> (LPOD, LPOD)
splitRules = partition (\(headAtoms, _) -> length headAtoms == 1)

getM :: LPOD -> Int
getM = length

concatsep :: [String] -> String -> String
concatsep [] _ = ""
concatsep [x] _ = x
concatsep (x:xs) sep = x ++ sep ++ concatsep xs sep

commentRule :: ([Atom], [Literal]) -> String
commentRule (atoms, lits) = "% " ++ concatsep (map showAtom atoms) " * " ++ (if not (null lits) then " <- " else "") ++ concatsep (map showLit lits) ", " ++ "."

makeRegRuleBN :: ([Atom], [Literal]) -> Int -> String
makeRegRuleBN (alist, litlist) m = commentRule (alist, litlist) ++ "\n" ++ makeHead alist ++ makeBody litlist ++ ".\n" ++ makeRegFstar m (alist, litlist)
    where
        makeHead [atom] = showAtom atom
        makeBody [] = ""
        makeBody lits = " :- " ++ concatsep (map showLit lits) ", "

makeRegFstar :: Int -> ([Atom], [Literal]) -> String
makeRegFstar m ([head], body) = "isFstar(" ++ (showAtom head) ++ ") :- not " ++ (showAtom head) ++ concatMap trueORisFstarAtom posatoms ++ concatMap negateAtom notatoms ++ ".\n"
    where
        posatoms = [atom | PosL atom <- body]
        notatoms = [atom | NotL atom <- body]
        trueORisFstarAtom atom = ", isTFstar(" ++ (showAtom atom) ++ ")"
        negateAtom atom = ", not " ++ (showAtom atom)

-- get the ordered disjunction lenghts
getNs :: LPOD -> [Int]
getNs = map getN
    where
        getN (atlist, _) = length atlist

makeOPRule :: Int -> Int -> String
makeOPRule i n = "{" ++ "op(" ++ show i ++ ",X" ++ show i ++ ") : " ++ printRange i ++ "} = 1."
    where
        printRange i = "X" ++ show i ++ " = 0.." ++ show n

makeODrules :: LPOD -> Int -> [Int] -> [String]
makeODrules rules m ns = concatMap (\(r, i, n) -> translaterule r i m n) (zip3 rules [1..m] ns)

translaterule :: ([Atom],[Literal]) -> Int -> Int -> Int -> [String]
translaterule (head, body) i m n = [commentRule (head, body), makeOPRule i n, makeBodyRule i m body, makeBodyFalse i m, makeBodyTrue i m,""] ++ makeOptionRules head i m n ++ makeFstars (head, body) i m n ++ [""]

makeBodyRule :: Int -> Int -> [Literal] -> String
makeBodyRule i m body = "body_" ++ show i ++ (if not (null body) then " :- " else "") ++ concatsep (map showLit body) ", " ++ "."

makeBodyFalse :: Int -> Int -> String
makeBodyFalse i m = ":- " ++ "op("++ show i ++ ",0), body_" ++ show i ++ "."

makeBodyTrue :: Int -> Int -> String
makeBodyTrue i m = ":- " ++ "op("++ show i ++ ",X" ++ show i ++ "), X" ++ show i ++ " > 0, not " ++ "body_" ++ show i ++ "."

makeOptionRules :: [Atom] -> Int -> Int -> Int -> [String]
makeOptionRules head i m n = zipWith (\ atom r -> makeOption atom head i m r) head [1..n] ++ [""] ++ map (makeOptionConstr head i m) [1..n] ++ [""]

makeOption :: Atom -> [Atom] -> Int -> Int -> Int -> String
makeOption atom head i m r = showAtom atom ++ " :- " ++ "body_" ++ show i ++ ", op(" ++ show i ++ "," ++ show r ++ ")" ++ (if r > 1 then ", " else "") ++  concatsep (map (\s -> "not " ++ showAtom s) (take (r-1) head)) ", " ++ "."

makeOptionConstr :: [Atom] -> Int -> Int -> Int -> String
makeOptionConstr atlist i m r = ":- " ++ "body_" ++ show i ++ ", not op(" ++ show i ++ "," ++ show r ++ "), " ++ concatsep (map (\s -> "not " ++ showAtom s) (take (r-1) atlist)) ", " ++ (if r > 1 then ", " else "") ++ showAtom (atlist !! (r-1)) ++ "."

makeFstars :: ([Atom],[Literal]) -> Int -> Int -> Int -> [String]
makeFstars (head, body) i m n = makeFstarHeadOD head i m n ++ [""] ++ [makeFstarBodyAux i m body] ++ [""]

makeFstarBodyAux :: Int -> Int -> [Literal] -> String
makeFstarBodyAux i m body = "isFstar(body_" ++ show i ++ ") :- not body_" ++ show i ++ concatMap trueORisFstarAtom posatoms ++ concatMap negateAtom notatoms ++ "."
    where
        posatoms = [atom | PosL atom <- body]
        notatoms = [atom | NotL atom <- body]
        trueORisFstarAtom atom = ", isTFstar(" ++ (showAtom atom) ++ ")"
        negateAtom atom = ", not " ++ showAtom atom

makeFstarHeadOD :: [Atom] -> Int -> Int -> Int -> [String]
makeFstarHeadOD head i m n = map (makerulebn10 head i m) [1..n]
    where
        isFstarAtom atom = "isFstar(" ++ showAtom atom ++ ")"
        makerulebn10 head i m r = "isFstar(" ++ showAtom (head !! (r-1)) ++ ") :- isTFstar(body_" ++ show i ++ "), " ++ concatsep (map isFstarAtom (take (r-1) head)) ", " ++ (if r > 1 then ", " else "") ++ "not " ++ showAtom (head !! (r-1)) ++ "."

-- make isTFstar rules for all atoms in the LPOD and M* set of all atoms of the LPOD that take the F* value
makeTFstarMstar :: [[Atom]] -> [[Literal]] -> Int -> [String]
makeTFstarMstar headlist bodylist m = "% isTFstar rules" : concatMap makeTrueFstars (getallatoms headlist bodylist) ++ makeTFbodyaux m ++ "\n% make M* set" : map (`flattenfstar` m) (nub (concat headlist))
    where
        flattenfstar atom m = "mstar(" ++ showAtom atom ++ ") :- isFstar(" ++ showAtom atom ++ ")."
        makeTrueFstars atom = ["isTFstar(" ++ showAtom atom ++ ") :- isFstar(" ++ showAtom atom ++ ").","isTFstar(" ++ showAtom atom ++ ") :- " ++ showAtom atom ++ "."]--, ":- isFstar(" ++ showAtom atom ++ "), " ++ showAtom atom ++ ".\n"]
        makeTFbodyaux m = [ "isTFstar(" ++ "body_" ++ show i ++ ") :- isFstar(" ++ "body_" ++ show i ++ ").\nisTFstar(" ++ "body_" ++ show i ++ ") :- " ++ "body_" ++ show i ++ "." | i <- [1..m]]

translateLPOD :: LPOD -> [String]
translateLPOD rules = map (`makeRegRuleBN` m) regrules ++ [""] ++ makeODrules odrules m ns ++ [""] ++ makeTFstarMstar headlist bodylist m
    where
        (regrules, odrules) = splitRules rules
        m = getM odrules
        ns = getNs odrules
        headlist = map fst rules
        bodylist = map snd rules
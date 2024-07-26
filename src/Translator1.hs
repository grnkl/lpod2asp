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

-- add the assumption degree list (X1,...,Xm) to the atom
addXs :: String -> Int -> String
addXs at 0 = at
addXs at m = at ++ "(" ++ concat ["X" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ")"

commentRule :: ([Atom], [Literal]) -> String
commentRule (atoms, lits) = "% " ++ concatsep (map showAtom atoms) " * " ++ (if not (null lits) then " <- " else "") ++ concatsep (map showLit lits) ", " ++ "."

makeRegRules :: ([Atom], [Literal]) -> Int -> String
makeRegRules (alist, litlist) m = commentRule (alist, litlist) ++ "\n" ++ makeHead alist ++ makeBody litlist ++ ".\n" ++ makeRegFstar m (alist, litlist)
    where
        makeHead [atom] = addXs (showAtom atom) m
        makeBody lits = " :- " ++ concatsep (map (\s -> addXs (showLit s) m) (PosL (PosA "ap"):lits)) ", "

makeRegFstar :: Int -> ([Atom], [Literal]) -> String
makeRegFstar m ([head], body) = "isFstar(" ++ (showAtom head) ++ addXs "" m ++ ") :- " ++ addXs "ap" m ++ ", not " ++ (showAtom head) ++ addXs "" m ++ concatMap trueORisFstarAtom posatoms ++ concatMap negateAtom notatoms ++ ".\n"
    where
        posatoms = [atom | PosL atom <- body]
        notatoms = [atom | NotL atom <- body]
        -- trueORisFstarAtom atom = ", 1 {" ++ addXs (showAtom atom) m ++ "; isFstar(" ++ addXs (showAtom atom) m ++ ")}"
        trueORisFstarAtom atom = ", isTFstar(" ++ addXs (showAtom atom) m ++ ")"
        negateAtom atom = ", not " ++ addXs (showAtom atom) m

-- get the ordered disjunction lenghts
getNs :: LPOD -> [Int]
getNs = map getN
    where
        getN (atlist, _) = length atlist

makeAPrules :: Int -> [Int] -> [String]
makeAPrules m ns = [makerule1 m ns, makerule2 m]
    where
        makerule1 m ns = "{" ++ addXs "ap" m ++ ": " ++ printRanges m ns ++ "}."
        makerule2 m = "#minimize {-1," ++ addXs "" m ++ ":" ++ addXs "ap" m ++ "}."
        printRanges m ns = concatsep (map printRange [1..m]) ", "
            where
                printRange i = "X" ++ show i ++ " = 0.." ++ show (ns !! (i-1))

makeODrules :: LPOD -> Int -> [Int] -> [String]
makeODrules rules m ns = concatMap (\(r, i, n) -> translaterule r i m n) (zip3 rules [1..m] ns)

translaterule :: ([Atom],[Literal]) -> Int -> Int -> Int -> [String]
translaterule (head, body) i m n = [commentRule (head, body), makeBodyRule i m body, makeBodyFalse i m, makeBodyTrue i m,""] ++ makeAssumptions head i m n ++ makeFstars (head, body) i m n ++ [""]

makeBodyRule :: Int -> Int -> [Literal] -> String
makeBodyRule i m body = addXs ("body_" ++ show i) m ++ " :- " ++ concatsep (map (\s -> addXs (showLit s) m) (PosL (PosA "ap"):body)) ", " ++ "."

makeBodyFalse :: Int -> Int -> String
makeBodyFalse i m = ":- " ++ addXs "ap" m ++ ", X" ++ show i ++ " = 0, " ++ addXs ("body_" ++ show i) m ++ "."

makeBodyTrue :: Int -> Int -> String
makeBodyTrue i m = ":- " ++ addXs "ap" m ++ ", X" ++ show i ++ " > 0, not " ++ addXs ("body_" ++ show i) m ++ "."

makeAssumptions :: [Atom] -> Int -> Int -> Int -> [String]
makeAssumptions head i m n = zipWith (\ atom r -> makeSelectHeadAtom atom i m r) head [1..n] ++ [""] ++ map (makeSelectionConstr head i m) [1..n] ++ [""]

makeSelectHeadAtom :: Atom -> Int -> Int -> Int -> String
makeSelectHeadAtom atom i m r = addXs (showAtom atom) m ++ " :- " ++ addXs ("body_" ++ show i) m ++ ", X" ++ show i ++ " = " ++ show r ++ "."

makeSelectionConstr :: [Atom] -> Int -> Int -> Int -> String
makeSelectionConstr atlist i m r = ":- " ++ addXs ("body_" ++ show i) m ++ ", X" ++ show i ++ " != " ++ show r ++ ", " ++ concatsep (map (\s -> "not " ++ addXs (showAtom s) m) (take (r-1) atlist)) ", " ++ (if r > 1 then ", " else "") ++ addXs (showAtom (atlist !! (r-1))) m ++ "."

makeFstars :: ([Atom],[Literal]) -> Int -> Int -> Int -> [String]
makeFstars (head, body) i m n = makeFstarHeadOD head i m n ++ [""] ++ [makeFstarBodyAux i m body] ++ [""] -- ++ makeFstarsFromBody head i m n

makeFstarBodyAux :: Int -> Int -> [Literal] -> String
makeFstarBodyAux i m body = "isFstar(body_" ++ show i ++ addXs "" m ++ ") :- " ++ addXs "ap" m ++ ", not body_" ++ show i ++ addXs "" m ++ concatMap trueORisFstarAtom posatoms ++ concatMap negateAtom notatoms ++ "."
    where
        posatoms = [atom | PosL atom <- body]
        notatoms = [atom | NotL atom <- body]
        -- trueORisFstarAtom atom = ", 1 {" ++ addXs (showAtom atom) m ++ "; isFstar(" ++ addXs (showAtom atom) m ++ ")}"
        trueORisFstarAtom atom = ", isTFstar(" ++ addXs (showAtom atom) m ++ ")"
        negateAtom atom = ", not " ++ addXs (showAtom atom) m

makeFstarHeadOD :: [Atom] -> Int -> Int -> Int -> [String]
makeFstarHeadOD head i m n = map (makefstarhead head i m) [1..n]
    where
        isFstarAtom atom = "isFstar(" ++ addXs (showAtom atom) m ++ ")"
        makefstarhead head i m r = "isFstar(" ++ addXs (showAtom (head !! (r-1))) m ++ ") :- " ++ addXs "ap" m ++ ", isTFstar(body_" ++ show i ++ addXs "" m ++ "), " ++ concatsep (map isFstarAtom (take (r-1) head)) ", " ++ (if r > 1 then ", " else "") ++ "not " ++ addXs (showAtom (head !! (r-1))) m ++ "."

-- make isTFstar rules for all atoms in the LPOD and isFstar/(m+1) rules for all atoms in then LPOD rule heads
flattenFstars :: [[Atom]] -> [[Literal]] -> Int -> [String]
flattenFstars headlist bodylist m = "% isTFstar rules" : concatMap makeTrueFstars (getallatoms headlist bodylist) ++ makeTFbodyaux m ++ "\n% make M* sets" : map (`flattenfstar` m) (nub (concat headlist))
    where
        flattenfstar atom m = "isFstar(" ++ concat ["X" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ", " ++ showAtom atom ++ ") :- isFstar(" ++ addXs (showAtom atom) m ++ ")."
        makeTrueFstars atom = ["isTFstar(" ++ addXs (showAtom atom) m ++ ") :- isFstar(" ++ addXs (showAtom atom) m ++ ").","isTFstar(" ++ addXs (showAtom atom) m ++ ") :- " ++ addXs (showAtom atom) m ++ "."]--, ":- isFstar(" ++ addXs (showAtom atom) m ++ "), " ++ addXs (showAtom atom) m ++ ".\n"]
        makeTFbodyaux m = [ "isTFstar(" ++ addXs ("body_" ++ show i) m ++ ") :- isFstar(" ++ addXs ("body_" ++ show i) m ++ ").\nisTFstar(" ++ addXs ("body_" ++ show i) m ++ ") :- " ++ addXs ("body_" ++ show i) m ++ "." | i <- [1..m]]

makePrefRules :: Int -> [String]
makePrefRules m = ["% preference rules", makeCIntersection m, makeCSet m, makeSubset, makePAS m]

makeCIntersection :: Int -> String
makeCIntersection m = "count_intersection(" ++ addXs "" m  ++ "," ++ addYs "" m ++ ", Cnt) :- " ++ addXs "ap" m ++ ", " ++ addYs "ap" m ++ ", Cnt = #count{A : isFstar(" ++ concat ["X" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ",A), isFstar(" ++ concat ["Y" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ",A)}."

makeCSet :: Int -> String
makeCSet m = "count_set(" ++ addXs "" m  ++ ", Cnt) :- " ++ addXs "ap" m ++ ", Cnt = #count{A : isFstar(" ++ concat ["X" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ",A)}."

makeSubset :: String
makeSubset = "subset(S1,S2) :- count_intersection(S1,S2,CI), count_set(S1,C1), count_set(S2,C2), CI = C1, C1 < C2."

makePAS :: Int -> String
makePAS m = "pAS" ++ addXs "" m ++ " :- " ++ addXs "ap" m ++ ", {subset(P," ++ addXs "" m ++ ")}0."

addYs :: String -> Int -> String
addYs at 0 = at
addYs at m = at ++ "(" ++ concat ["Y" ++ show i ++ if i /= m then "," else "" | i <- [1..m]] ++ ")"

-- optional: show only pAS/m atoms (if you just want to see which assumption program produces the most preferred answer set)
showPAS :: Int -> String
showPAS m = "#show pAS/" ++ show m ++ "."

translateLPOD :: LPOD -> [String]
translateLPOD rules = makeAPrules m ns ++ [""] ++ map (`makeRegRules` m) regrules ++ [""] ++ makeODrules odrules m ns ++ [""] ++ flattenFstars headlist bodylist m ++ ["\n"] ++ makePrefRules m ++ ["", showPAS m]
    where
        (regrules, odrules) = splitRules rules
        m = getM odrules
        ns = getNs odrules
        headlist = map fst rules
        bodylist = map snd rules
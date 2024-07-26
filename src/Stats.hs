module Stats where
import Types
import Data.List (partition, nub)

splitRules :: LPOD -> (LPOD, LPOD)
splitRules = partition (\(headAtoms, _) -> length headAtoms == 1)

getM :: LPOD -> Int
getM = length

concatsep :: [Int] -> String -> String
concatsep [] _ = ""
concatsep [x] _ = show x
concatsep (x:xs) sep = show x ++ sep ++ concatsep xs sep

getNs :: LPOD -> [Int]
getNs = map getN
    where
        getN (atlist, _) = length atlist

-- print lpod stats: total rules, o.d. rules, total atoms, distinct atoms, max o.d. length, prod of all o.d. lengths

getStats :: LPOD -> [Int]
getStats rules = [length rules, getM odrules, length allatoms, length (nub allatoms), maximum ns, product ns]
    where
        (regrules, odrules) = splitRules rules
        m = getM odrules
        ns = getNs odrules
        headlist = map fst odrules
        allatoms = allAtoms rules

allAtoms :: LPOD -> [String]
allAtoms = concatMap ruleAtoms

ruleAtoms :: ([Atom],[Literal]) -> [String]
ruleAtoms (atoms, lits) = map showAtom atoms ++ map (showAtomIgnNot . lit2atom) lits

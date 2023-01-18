module DepTree where

import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Token
import qualified Data.Map as Map
import Debug.Trace
import Help 
import Text.Regex.TDFA
import Text.Regex
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy as LB


data Tree a = Tree a [Tree a] | Empty 
              deriving Show

instance (Eq a) => Eq (Tree a) where
  Empty == Empty = True
  x == y = (root x == root y) && (deps x) == (deps y)

type Metadata = Map.Map T.Text T.Text


-- lazily parse a conll file into a list of trees with associated metadata (from comment lines that inlude an `=`).
-- filter through (not . null) in case there are spurious empty lines in the input
conllFileToTrees :: String -> IO [(Tree Token, Metadata)]
conllFileToTrees filepath =
  do
  conll <- fmap LT.decodeUtf8 (LB.readFile filepath)
  return (conllToTrees' conll)

conllToTrees' :: LT.Text -> [(Tree Token, Metadata)]
conllToTrees' = ((map (\ x -> conllToTree x ([], Map.empty))) . (Data.List.filter (not . Data.List.null)) . (Data.List.Split.splitOn [LT.empty]) . LT.lines) 

conllToTrees :: IO [(Tree Token, Metadata)]
conllToTrees =
  do
    conll <- fmap LT.decodeUtf8 LB.getContents
    return (conllToTrees' conll)


conllToTree :: [LT.Text] -> ([(Token, Int)], Metadata) -> (Tree Token, Metadata)
conllToTree [] (tokens, metadata) = ((treeFromTokens tokens), metadata)
conllToTree (l:ls) (tokens, metadata) =
  case strictL =~ "^#([^=]*)(=?)(.*)" :: (T.Text, T.Text, T.Text, [T.Text]) of
    (_, _, _, []) -> if strictL =~ "^[0-9]+-[0-9]+"
                     then conllToTree ls (tokens, metadata) -- we have multi-token line: ignore
                     else conllToTree ls ((constructToken strictL):tokens, metadata)
    (_, _, _, (attr:_:val:[])) -> if val == T.empty -- there was no equal sign, so no structure in the metadata: ignore
                                  then conllToTree ls (tokens, metadata)
                                  else conllToTree ls (tokens, (Map.insert (T.strip attr) (T.strip val) metadata))
    ukwn -> error (show ukwn)
  where strictL = (LT.toStrict l)

treeToConll :: Tree Token -> T.Text
treeToConll tree =
  (T.intercalate (T.singleton '\n')
   (map tokenToConll sortedTokens))
  `T.append` (T.singleton '\n')
  where sortedTokens =  sort (filter (\(t,h) -> (not . isRoot)(t)) (tokensWithHead tree)) 

-- TODO: in fact, the index of the root should not necessarily be 0, but the head index that does not refer to one of the tokens. Also, that should be unique
treeFromTokens :: [(Token, Int)] -> Tree Token
treeFromTokens tokens =
  let rootIndex =
        case (nub (map snd tokens)) \\ (map (index . fst) tokens) of
          r:[] -> r
          x    -> error ("Several heads " ++ (show x) ++ " in " ++ (show tokens))
  in constructTree (Root rootIndex (T.pack "root")) tokens
  

root :: Tree a -> a
root (Tree r _) =  r

isntEmpty :: Tree a -> Bool
isntEmpty Empty = False
isntEmpty _ = True
  
deps :: Tree a -> [Tree a]
deps (Tree _ deps) =  deps

lexicalSubtrees :: Tree Token -> [Tree Token]
lexicalSubtrees tree = filter (not . isRoot . root) (subtrees tree)

subtrees :: Tree a -> [Tree a]
subtrees tree =
  tree:(properSubtrees tree)
  where properSubtrees (Tree r deps) =
          deps ++ (concat (map properSubtrees deps)) 

-- pretty print tree
ppt :: Show a => Tree a -> [Char]
ppt (Tree r deps) =
  output (Tree r deps) 0 
  where
    output (Tree r deps) indents =
      intercalate "\n" 
     ((take (indents*2) (cycle " ") ++ (show r) ) : (map (\x -> (output x (indents + 1))) deps))


inlineRel :: Tree Token -> String
inlineRel (Tree root@(Root _ _)  deps) =
  "[ " ++ (intercalate "," (map inlineRel deps)) ++ " ]"
inlineRel (Tree token []) =  
  showRelation token ++ " " ++ show token 
inlineRel (Tree token deps) =  
  showRelation token ++ " " ++ show token ++ " [ " ++ (intercalate " , " (map inlineRel deps)) ++ " ]"

  
-- pretty print tree with relations
pptRel :: Tree Token -> [Char]
pptRel (Tree r deps) =
  output (Tree r deps) 0 
  where
    output (Tree r deps) indents =
      intercalate "\n" 
     ((take (indents*2) (cycle " ") ++ (showRelation r) ++ "--" ++ (show r) ) : (map (\x -> (output x (indents + 1))) deps))

highestIndex :: Tree Token -> Int
highestIndex tree = maximum (map index (tokens tree))

highestGlobalIndex :: (Tree Token, Path Token) -> Int
highestGlobalIndex t =
  let globalTree = fst (goTop t) in highestIndex globalTree


nonRootIndices :: Tree Token -> [Int]
nonRootIndices (Tree (Root _ _) deps) = sort (concat (map nonRootIndices deps))
nonRootIndices (Tree r deps) = sort ((index r):concat (map nonRootIndices deps))

indices :: Tree Token -> [Int]
indices (Tree r deps) = 
  sort ( (index r) : 
        concat (map indices deps))

-- returns the tokens in a tree        
tokens :: Tree Token -> [Token]
tokens (Tree r deps) =
  r : concat(map tokens deps)

-- returns the tokens in a tree paired with the index of their head
tokensWithHead :: Tree Token -> [(Token, Int)]
tokensWithHead tree =
  tokensWithHead' 0 tree
  where tokensWithHead' hdIdx (Tree r deps)  =
          (r, hdIdx) : concat (map (tokensWithHead' (index r)) deps)

lexicalTokens :: Tree Token -> [Token]
lexicalTokens t = filter (not . isRoot) (tokens t)

sortedTokens :: Tree Token -> [Token]
sortedTokens tree =
  sort (tokens tree)

ppYield :: Tree Token -> String
ppYield tree = (intercalate " " (map show (sortedTokens tree)))

ppStringYield :: Tree Token -> String
ppStringYield tree = (intercalate " " (map (T.unpack . form) (filter (not . isRoot) (sortedTokens tree))))

treeIndex (Tree r deps) =
  index r

wellnested :: Tree Token -> Bool
wellnested tree = null (collectIllnestedSubtrees tree)

-- returns an integer corresponding to the number of U one needs to go
-- from tree1 to get to a node that dominates t2
illnestDegree :: (Tree Token, Path Token) -> (Tree Token, Path Token) -> Int
illnestDegree (t1, p1) (t2, _) = illnestDegree' (findTree (t1, p1) t2)
    where illnestDegree' [] = 0
          illnestDegree' dirs = if all (== R) dirs
                                then 1
                                else (length . filter (== U)) dirs 

-- This function searches for all illnested subtrees in a given
-- tree. For each illnested subtree, it returns a triple consisting of
-- 1. The first tree in the illnest together with its path from the
-- original tree 2. The second tree in the illnest 3. The directions
-- needed to go from the first tree to the second Notice that the
-- second tree in the illnest will always be the highest tree
-- generating the illnest. There could be subtrees of the second tree
-- that also illnests with the first tree. However, these will be
-- found as first trees in another illnest.
collectIllnestedSubtrees :: Tree Token -> [((Tree Token, Path Token), (Tree Token, Path Token))]
collectIllnestedSubtrees tree = collectIllnestedSubtrees' (goBottom (tree, Top)) []
    where collectIllnestedSubtrees' (_, Top) tt = tt
          collectIllnestedSubtrees' treeWithPath tt =
              collectIllnestedSubtrees' (goNext treeWithPath) ((interleavedPairs treeWithPath) ++ tt)

ppIllnestedSubtree ((tree1, path1), (tree2, path2)) =
    (ppYield tree1) ++ " and " ++ (ppYield tree2) ++
                        "(degree " ++ (show (illnestDegree (tree1, path1) (tree2, path2))) ++ "): " ++
                                       (pathDescription (tree1, path1) dirs)
    where dirs = findTree (tree1, path1) tree2

ppIllnestPath ((tree1, path1), (tree2, _)) =
    (showRelation (root tree1)) ++ "-->" ++ (pathDescription (tree1, path1) (filter (== U) dirs))
    where dirs = findTree (tree1, path1) tree2

ppIllnestFunctions ((tree1, _), (tree2, _)) =
    show (sort $ nub (mapFunctions tree1 ++ mapFunctions tree2))

                 
mapFunctions tree =
    map (relation . root) (discontinuousDependents tree)

-- returns an array including the index of the root and contiguous integers belonging to items in the subtree of the root
rootDomain :: Tree Token -> [Int]
rootDomain tree =
    fromJust (find (elem (index (root tree))) (contDomains (indices tree)))

-- TODO: memoize this
discontinuousDependents :: Tree Token -> [Tree Token]
discontinuousDependents tree =
  snd (contOrderDomain2 tree)
  
contOrderDomain2 :: Tree Token -> (Tree Token, [Tree Token])
contOrderDomain2 Empty = (Empty, [])
contOrderDomain2 tree =
  discardSubtrees (rootDomain tree) tree


discardSubtrees :: [Int] -> Tree Token -> (Tree Token, [Tree Token])
discardSubtrees _ Empty = (Empty, [])
discardSubtrees range (Tree r deps) =
          if (index r) `elem` range
          then (Tree r (((filter isntEmpty) . fst . unzip) (map (discardSubtrees range) deps)), ((concat . snd . unzip) (map (discardSubtrees range) deps))) 
          else (Empty, ((Tree r deps):(snd (discardSubtrees ((rootDomain (Tree r deps)) ++ range) (Tree r deps)))))

disjointSubtrees :: Path a -> [Tree a]
disjointSubtrees path = collectSisters path []
    where collectSisters Top ts = ts
          collectSisters (Path h l p r) ts = collectSisters p (l ++ r ++ ts)

interleavers :: (Tree Token, Path Token) -> [Tree Token]
-- save some time since trees without dependents never interleave
-- we could also check for projectivity since projective trees never interleave
-- a more optimal algorithm would start the search from any gaps in the yield of the tree
interleavers ((Tree _ []), _) = []
interleavers (tree, path) = filter (\x -> interleave (tree, x)) (disjointSubtrees path) 


interleavedPairs :: (Tree Token, Path Token) -> [((Tree Token, Path Token), (Tree Token, Path Token))]
interleavedPairs treeWithPath = map (\x -> (treeWithPath, go treeWithPath (findTree treeWithPath x))) (interleavers treeWithPath)

pathDescription :: (Tree Token, Path Token) -> [Direction] -> String
pathDescription (tree, path) directions =
    intercalate "-->" (map (\(x,y) -> (show y) ++ ":" ++ (showRelation x)) (zip (collectNodes (tree, path) (directions)) directions))

illnestPairDegree :: (Tree Token, Path Token) -> Tree Token -> Int
illnestPairDegree treeWithPath tree2 = (length . filter (== U)) (findTree treeWithPath tree2)

-- NB: these functions rely on checking indices alone!
isInitialSubtree :: Tree Token -> Tree Token -> Bool
isInitialSubtree tree1 tree2 =
    (indices tree1) == (take (length (indices tree1)) (delete 0 (indices tree2)))

isFinalSubtree :: Tree Token -> Tree Token -> Bool
isFinalSubtree tree1 tree2 =
    (indices tree1) == reverse (take (length (indices tree1)) (reverse (indices tree2)))

{--
-- remove for now. This is annotation specific and the underlying function isClauseHead has been removed from Token.hs

isClauseInitialSubtree :: (Tree Token, Path Token) -> Bool 
isClauseInitialSubtree (tree, path) = isClauseInitialSubtree' tree (tree, path)
  where isClauseInitialSubtree' tree (searchTree, Top) = isInitialSubtree tree searchTree
        isClauseInitialSubtree' tree (searchTree, path) = 
          if clausal searchTree
          then isInitialSubtree tree searchTree || isClauseInitialSubtree' tree (goClosestClause $ goUp (searchTree, path))
          else isClauseInitialSubtree' tree (goClosestClause (searchTree, path))


-- returns a string describing the position of a tree within the global tree reconstructed from its path
position :: (Tree Token, Path Token) -> String
position (tree1, path) =
    case ((isInitialSubtree tree1 tree2), (isFinalSubtree tree1 tree2), (isClauseInitialSubtree (tree1, path))) of
      (True, False, _) -> "Initial"
      (True, True, _)  -> "All"
      (False, True, _) -> "Final"
      (False, False, False) -> "Medial"
      (False, False, True) -> "Clauseinitial"
    where tree2 = fst $ goTop (tree1, path)


mapDiscontinuousPositions :: (Tree Token, Path Token) -> [String]
mapDiscontinuousPositions (tree, path) =
  map (\x -> position (findTree2 (tree, path) x)) (discontinuousDependents tree)
  

ppIllnestPositions ((tree1, path1), (tree2, path2)) =
    show (sort $ nub ((mapDiscontinuousPositions (tree1, path1)) ++ (mapDiscontinuousPositions (tree2, path2))))

goClosestClause :: (Tree Token, Path Token) -> (Tree Token, Path Token)
goClosestClause (tree, Top) = (tree, Top)
goClosestClause (tree, path) =
    if clausal tree
    then (tree, path)
    else goClosestClause (goUp (tree, path))
           


--}     

-- This does not test for disjointness so it only gives us illnested
-- trees on the assumption that the trees are disjoint
interleave :: (Tree Token, Tree Token) -> Bool
interleave (t1, t2) =
  let l1 = head (indices t1)
      r1 = last (indices t1)
      l2 = head (indices t2)
      r2 = last (indices t2)
  in (any (`elem` [l1..r1]) (indices t2)) && (any (`elem` [l2..r2]) (indices t1))

projective :: Tree Token -> Bool
projective tree =
  null (holes (indices tree))

-- returns a string describing whether the head is (alone) among the intruders in a tree
intervenors :: (Tree Token, Path Token) -> String
intervenors (Tree r [], _) = "none"
intervenors (tree, path)
    | (projective tree) = "none"
    | (holes $ indices tree) == [treeIndex $ fst $ goUp (tree, path)] = "head only"
    | ((treeIndex $ fst $ (goUp (tree, path))) `elem` (holes $ indices tree)) = "head+"
    | otherwise = "no head"
       

-- refactor this one
--constructTree :: Token -> [Token] -> Tree Token
constructTree root xs = 
  Tree 
    root 
    (sortBy (\x y -> (treeIndex x) `compare` (treeIndex y))
           (map (\x -> constructTree (fst(x)) xs) 
           (filter (\x -> (snd(x) == index(root))) 
                  xs)))


-- drops the subtree with the given rootindex (if there is one) and
-- returns the resulting tree plus Maybe the removed tree (if there is
-- one).
dropByIndex :: Tree Token -> Int -> (Tree Token, Maybe (Tree Token)) -- the second part is the removed token?
dropByIndex (Tree r deps) idx
  | index r == idx = error "removes the whole tree"
  | any (\x -> treeIndex x == idx) deps = 
    ((Tree r $ filter (\x -> treeIndex x /= idx) deps), (find (\x -> treeIndex x == idx) deps))
  | otherwise = 
      (Tree r (map (\x -> fst $ dropByIndex x idx) deps), pickFirstJust (map (\x -> snd $dropByIndex x idx) deps))

                                               
-- picks the first Just value in a list of Maybe values.
pickFirstJust :: [Maybe a] -> Maybe a
pickFirstJust [] = Nothing
pickFirstJust ((Just x):_) = Just x                                                              
pickFirstJust (_:xs) = pickFirstJust xs

-- returns +True+ if the first tree dominates the second
dominates :: (Eq a) => Tree a -> Tree a -> Bool                       
dominates (Tree root deps) tree2 =
    (tree2 `elem` deps) || any (\x -> dominates x tree2) deps

-- returns all daughters with a specific relation
daughtersByRelation :: (Tree Token) -> T.Text -> [Tree Token]
daughtersByRelation (Tree _ deps) rel = filter (\d -> (relation $ root d) == rel) deps

hasDaughterByRelation :: (Tree Token) -> T.Text -> Bool
hasDaughterByRelation t rel = not . null $ daughtersByRelation t rel

-- TODO also look at features and other cues for passive
isPassive :: Tree Token -> Bool
isPassive t
  | hasDaughterByRelation t (T.pack "aux:pass") = True
  | otherwise = False


-- add node(s) along a path unless they already exists. Return to the original position in the tree
addUnlessExists :: (Tree Token, Path Token) -> [T.Text] -> (Tree Token, Path Token)
addUnlessExists tree rels = go (fst (addUnlessExists' tree rels [])) (snd (addUnlessExists' tree rels []))  -- return a tree and a set of directions.
  where addUnlessExists' tree [] dirs = (tree, dirs)
        addUnlessExists' tree (r:rs) dirs = addUnlessExists' (findOrCreateUniqueDaughterByRelation tree r) rs (U:dirs)

-- TODO pattern matching against Text
findOrCreateUniqueDaughterByRelation :: (Tree Token, Path Token) -> T.Text -> (Tree Token, Path Token)
findOrCreateUniqueDaughterByRelation (tree, path) rel
  | rel == (T.pack "mother") =  goUp (tree, path)
  | otherwise =   let  scanTree (tree, path) rel =
                         if (relation (root tree)) == rel
                         then (tree, path)
                         else scanTree (goRight (tree, path)) rel
                       newIdx = highestGlobalIndex (tree, path) + 1
                  in case filter (\x ->  (relation x) == rel) (map root (deps tree)) of
                       [] -> goDown (Tree (root tree) ((Tree (Token {index = newIdx, form = T.empty, lemma =T.empty, coarsePos = T.empty, pos = T.empty, feat = Map.empty, relation = rel, Token.trace = False }) []):(deps tree)), path) -- This attaches to the left of the current node!
                       [d] -> scanTree (goDown(tree, path)) rel
                       d:dd:_ -> error "Multiple such daughters"

-- navigate the tree by a string of (relation, features)
-- constraints. The feature string should follow the CONLLU standard
-- and matches a daughter's features iff it subsumes that daughters
-- features. The relation string is interpreted as a regular
-- expression, so ".*" matches any daughter. "§" at the end of the
-- relation string is "meta Kleene star", matching any number of
-- daughter nodes that matches the relation string. So "nsubj§" nodes any number of nsubj edges down. 
-- TODO: it must be possible to write this in a prettier way.
navigate :: (Tree Token, Path Token) -> [(T.Text, T.Text)] -> [(Tree Token, Path Token)]
navigate t [] = return t
navigate t (c:[]) = do
  if (fst c) =~ (T.pack "§$")
    then t:(kleeneNavigate t (T.dropWhileEnd (=='§') (fst c), (snd c))) 
    else navigate' t c
navigate t (c:crit) = do
  new <- if (fst c) =~ (T.pack "§$")
         then t:(kleeneNavigate t (T.dropWhileEnd (=='§') (fst c), (snd c))) 
         else navigate' t c
  navigate new crit

-- A constructive version of the above: will create any daughters not present and add them to the tree. NB constraints under § will not be interpreted constructively. This means that even this function can return an empty set of treepaths.
navigateConstructive :: (Tree Token, Path Token) -> [(T.Text, T.Text)] -> [(Tree Token, Path Token)]
navigateConstructive t [] = return t 
navigateConstructive t (c:[]) = do
  if (fst c) =~ (T.pack "§$")
    then t:(kleeneNavigate t (T.dropWhileEnd (=='§') (fst c), (snd c))) 
    else navigateConstructive' t c
navigateConstructive t (c:crit) = do
  new <- if (fst c) =~ (T.pack "§$")
         then t:(kleeneNavigate t (T.dropWhileEnd (=='§') (fst c), (snd c))) 
         else navigateConstructive' t c
  navigateConstructive new crit


kleeneNavigate :: (Tree Token, Path Token) -> (T.Text, T.Text) -> [(Tree Token, Path Token)]
kleeneNavigate tree crit = --Debug.Trace.trace (show tree)(
  concat (map (\d -> d:(kleeneNavigate d crit)) matchingdaughters)
--  )
  where matchingdaughters = navigate' tree crit

-- TODO implement a current node symbol
navigate' :: (Tree Token, Path Token) -> (T.Text, T.Text) -> [(Tree Token, Path Token)]
navigate' treepath@(t, p) crit@(c1, c2)
  | c1 == (T.singleton '^') && (depth p) == 0 = []  -- NB we now allow going to the artificial root node
  | c1 == (T.singleton '^') = [goUp treepath]
  | null (deps t) = []
  | otherwise =   collectMatchingSisters (goDown treepath) crit []
  where collectMatchingSisters (tree, path@(Path _ _ _ [])) crit acc = if DepTree.match (tree, path) crit
                                                                       then (tree,path):acc
                                                                       else acc
        collectMatchingSisters tree crit acc = if DepTree.match tree crit
                                               then collectMatchingSisters (goRight tree) crit (tree:acc)
                                               else collectMatchingSisters (goRight tree) crit acc

-- TODO: implement a current node symbol
navigateConstructive' :: (Tree Token, Path Token) -> (T.Text, T.Text) -> [(Tree Token, Path Token)]
navigateConstructive' treepath@(t, path@(Path mother l p r)) crit@(rel, features)
  | rel == (T.singleton '^') && (depth path == 0) = error "No you cannot create a mother node here!"
  | otherwise =   let existingdaughters = (navigate' treepath crit) in
                    if null existingdaughters
                    then [goDown ((Tree (root t) (newdaughter:(deps t))), path) ]
                    else existingdaughters
  where newdaughter = Tree tToken { index = (highestGlobalIndex treepath + 1), relation = (if rel == (T.pack ".*") then (T.pack "gf") else rel), feat = createFeatures features } []
    
-- This is the function used to match functional uncertainties. It relies on createFeatures rather than tokenFromString, which should not be necessayr
match :: (Tree Token, Path Token) -> (T.Text, T.Text) -> Bool
match (tree, path) (rel, featurestring) =
  matchTokenToString (root tree) (T.pack "relation=" <> rel <> (if (featurestring == T.empty || featurestring == T.singleton '_' )
                                                                then T.empty
                                                                else T.singleton ',' <> featurestring))

-- deterministic navigation by relations (including "mother").
-- Return +Nothing+ if the relation does not exist or is not unique
navigateByRelation :: [T.Text] -> (Tree Token, Path Token) -> Maybe (Tree Token, Path Token)
navigateByRelation [] t      = Just t
navigateByRelation (r:rs) t  = findUniqueDaughterByRelation t r >>= navigateByRelation rs

-- TODO pattern match against Text
findUniqueDaughterByRelation :: (Tree Token, Path Token) -> T.Text -> Maybe (Tree Token, Path Token)
findUniqueDaughterByRelation (tree, path) rel
  | rel == (T.singleton '^') && path == Top = Nothing
  | rel == (T.singleton '^') = Just (goUp (tree, path))
  | otherwise =   let  scanTree (tree, path) rel =
                         if (relation (root tree)) == rel
                         then (tree, path)
                         else scanTree (goRight (tree, path)) rel
                  in case filter (\x ->  (relation x) == rel) (map root (deps tree)) of
                       []     -> Nothing
                       [d]    -> Just (scanTree (goDown(tree, path)) rel)
                       d:dd:_ -> Nothing


-- Searches for tree2 starting from tree1 and returns the path traversed
findTree :: (Show a, Eq a) => (Tree a, Path a) -> (Tree a) -> [Direction]
findTree (tree1, path) tree2 = findTreeWithPath (tree1,path) tree2 []
    where findTreeWithPath (tree1, Top) tree2 directions 
                     | tree1 == tree2 = (reverse directions)
                     | (dominates tree1 tree2) =  findTreeWithPath (goDown (tree1, Top)) tree2 (D:directions)
                     | otherwise = error ("\n" ++ (ppt tree2) ++ "\nnot found (1) in\n " ++ (ppt tree1))
          findTreeWithPath (tree1, Path h l p r) tree2 directions
                     | tree1 == tree2 = (reverse directions)
                     | (dominates tree1 tree2) = findTreeWithPath (goDown (tree1, Path h l p r)) tree2 (D:directions)
                     | (null r) = findTreeWithPath (goUp (tree1, Path h l p r)) tree2 (U:directions)
                     | otherwise = findTreeWithPath  (goRight (tree1, Path h l p r)) tree2 (R:directions)

-- Searches for tree2 starting from tree1, and returns tree2 with a path
findTree2 :: (Show a, Eq a) => (Tree a, Path a) -> Tree a -> (Tree a, Path a)
findTree2 (tree1, Top) tree2
                     | tree1 == tree2 = (tree2, Top)
                     | (dominates tree1 tree2) = findTree2 (goDown (tree1, Top)) tree2
                     | otherwise = error ("\n" ++ (ppt tree2) ++ "\nnot found (2) in\n " ++ (ppt tree1))
findTree2 (tree1, Path h l p r) tree2
                     | tree1 == tree2 = (tree2, Path h l p r)
                     | (dominates tree1 tree2) = findTree2 (goDown (tree1, Path h l p r)) tree2
                     | (null r) = findTree2 (goUp (tree1, Path h l p r)) tree2
                     | otherwise = findTree2 (goRight (tree1, Path h l p r)) tree2



data Direction = U | D | R | L
                 deriving (Eq, Show)


collectNodes :: Show a => (Tree a, Path a) -> [Direction] -> [a]
collectNodes (tree, path) dirs = collectNodes' (tree, path) dirs []
    where collectNodes' (tree, path) [] nodes = (reverse nodes)
          collectNodes' (tree, path) (d:ds) nodes = 
              let newPosition = go (tree, path) [d]
              in  collectNodes' newPosition ds ((root (fst newPosition)):nodes)

                          
go :: Show a => (Tree a, Path a) -> [Direction] -> (Tree a, Path a)
go treewithpath [] = treewithpath
go treewithpath (d:ds) =
    case d of
      U -> go (goUp treewithpath) ds
      D -> go (goDown treewithpath) ds
      R -> go (goRight treewithpath) ds
      L -> go (goLeft treewithpath) ds

-- a path contains the mother node, the list of left sisters, the path from the mother node to the top, and the list of right sisters
data Path a = Top | Path a [Tree a] (Path a) [Tree a] 
          deriving (Show, Eq)

-- gives the lenght of the path to the top
depth :: Path a -> Int
depth Top = 0
depth (Path _ _ p _) = 1 + depth p

-- goes up and forgets the current subtree
removeSubtree :: Show a => (Tree a, Path a) -> (Tree a, Path a)
removeSubtree (_, Top) =
  error "Cannot remove the whole tree!"
removeSubtree (_, Path h l p r) =
  (Tree h ((reverse l) ++ r), p)

-- goes down to the leftmost daughter
goDown :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goDown (Tree r [], _) =
  error "At the bottom!"
goDown (Tree r (d:ds), p) =
  (d, Path r [] p ds)

-- goes down to the unique daughter. Throws an error if there is no daughter or the daughter is not unique
goDownUniquely :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goDownUniquely (Tree r [], p) =
  error ("At the bottom of " ++ show (Tree r [], p))
goDownUniquely (Tree r (d1:d2:d3), p) =
  error ("Multiple daughters in " ++ show (Tree r (d1:d2:d3), p))
goDownUniquely (Tree r (d1:[]), p) =
  (d1, Path r [] p [])

-- goes down until it it hits a non Root node. Throws an error if there are several paths or if it does not meet a non-Root node.
goDownToNonRoot :: (Tree Token, Path Token) -> (Tree Token, Path Token)
goDownToNonRoot treepath@(Tree (Token _ _ _ _ _ _ _ _) _, a) = treepath
goDownToNonRoot (Tree r [], p) =
  error ("At the bottom of " ++ show (Tree r [], p))
goDownToNonRoot (Tree r (d1:d2:d3), p) =
  error ("Multiple daughters in " ++ show (Tree r (d1:d2:d3), p))
goDownToNonRoot (Tree r (d1:[]), p) =
  goDownToNonRoot(d1, Path r [] p [])
  
goDownToLowestRoot :: (Tree Token, Path Token) -> (Tree Token, Path Token)
goDownToLowestRoot treepath@(Tree (Root _ _) [Tree (Root _ _) _], _) = goDownToLowestRoot(goDown treepath)
goDownToLowestRoot treepath@(Tree (Root _ _) deps, p) =
  if all (\(Tree h _) -> ((not . isRoot) h)) deps
  then treepath
  else error ("Something unexpected about " ++ (show treepath))
goDownToLowestRoot treepath = error ("Something unexpected about " ++ (show treepath))



goRight :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goRight (_, Path _ _ _ []) =
  error "At the right edge!"
goRight (t, Path h l p (r:rs)) =
  (r, Path h (t:l) p rs)
  
goRightmost :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goRightmost (t, Path h l p []) =
  (t, Path h l p [])
goRightmost (t, p) =
  goRightmost (goRight (t, p))

goLeftmost :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goLeftmost (t, Path h [] l r) = 
  (t, Path h [] l r)
goLeftmost (t, p) = 
  goLeftmost (goLeft (t, p))

goLeft :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goLeft (_, Path _ [] _ _) =
  error "At the left edge!"
goLeft (t, Path h (l:ls) p r) =
  (l, Path h ls p (t:r))

goUp :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goUp (t, Top) = 
  error ("On the top! Cannot go up in " ++ (show t))
goUp (t, Path h l p r) =
  (Tree h ((reverse l) ++ t:r), p)

goTop :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goTop (t, Top) =
  (t, Top)
goTop (t, p) =
  goTop (goUp (t, p))

-- go to the top, and then down one step
goRealRoot :: (Tree Token, Path Token) -> (Tree Token, Path Token)
goRealRoot = goDownUniquely . goTop
  
goBottom :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goBottom (Tree r [], p) =
  (Tree r [], p)
goBottom (t, p) =
  goBottom (goDown (t, p))

goToIndex :: Int -> (Tree Token, Path Token) -> (Tree Token, Path Token)
goToIndex index treepath =
  searchIndex index ((goBottom . goTop) treepath)
  where searchIndex index (tree, path)
          | ((treeIndex tree) == index) = (tree, path)
          | otherwise = searchIndex index (goNext (tree, path))
          
isRightmost :: (Tree a, Path a) -> Bool
isRightmost (_, Path _ _ _ []) = True
isRightmost _ = False


-- TODO check that traverseTree and subtrees yield the same answer (modulo the order) 
traverseTree :: Show a => Tree a -> [Tree a]
traverseTree tree = traverseTree' (goBottom (tree, Top)) []
  where traverseTree' (_, Top) acc = acc
        traverseTree' tree' acc = traverseTree' (goNext tree') ((fst tree'):acc)

-- goes to the next node in a left to right bottom up tree traversal
goNext :: Show a => (Tree a, Path a) -> (Tree a, Path a)
goNext (_, Top) =
    error "No next node!"
goNext (tree, Path h l p []) =
    goUp (tree, Path h l p [])
goNext (tree, path) =
    goBottom (goRight (tree, path))

-- returns Maybe the next node in a depth-first tree traversal. The
-- second argument is a boolean which is +True+ if the last move was a
-- U (i.e. we are done with a subtree). Returns Nothing if we call it
-- on a top node we have already visisted.
goNext' :: Show a => ((Tree a, Path a), Bool)  -> Maybe ((Tree a, Path a), Bool)
goNext' ((_, Top), True) = Nothing
-- we have been here before, so visit sisters if possible; otherwise go up.
goNext' (treepath@(t, Path h l p (r:rs)), True) = Just ((goRight treepath), False)
goNext' (t, True) = goNext'((goUp t), True)
-- we have not been here before, so visit a daughter if possible, otherwise a sister, otherwise go up.
goNext' (treepath@(Tree r (d:ds), _), False) = Just ((goDown treepath), False)
goNext' (treepath@(_, Path h l p (r:rs)), False) = Just ((goRight treepath), False)
goNext' (t, False) = goNext'((goUp t), True)

-- bounded depth-first traversal of tree. Takes a treepath and a
-- bounding node, plus a flag that starts and False and turns to True
-- once we go upwards. Visit depth-first and returns +Nothing+ if the
-- next move would hit the bounding node.
nextNode :: (Eq a, Show a) => (Tree a, Path a) -> a -> Bool -> Maybe (Tree a, Path a)
-- visit a daughter if possible and we have not just gone up the tree
nextNode treepath@(Tree _ (d:ds), _) _ False  = Just (goDown treepath)
-- when there is no daughter:
nextNode treepath@(Tree rt deps, Path h _ _ r) bound flag
  -- if we are at the bounding node, we are done (since there are no daughters)
  | rt == bound   = Nothing
  -- if there are sisters, go right
  | r /= []       = Just (goRight treepath)
  -- if our head is the bound, we are done (since there are no daughters and no sisters)
  | h == bound    = Nothing
  -- otherwise we go up and set the flag to +True+
  | otherwise     = nextNode (goUp treepath) bound True

attachLeft :: Tree a -> (Tree a, Path a) -> (Tree a, Path a)
attachLeft _ (_, Top) = 
  error "Can't attach to the left of the top node!"
attachLeft newtree (t, Path h l p r) =
  (t, Path h (newtree:l) p r)
  
attachRight :: Tree a -> (Tree a, Path a) -> (Tree a, Path a)
attachRight _ (_, Top) = 
  error "Can't attach to the right of the top node!"
attachRight newtree (t, Path h l p r) =
  (t, Path h l p (newtree:r))


-- return a tree with all dependents of the current node dropped
dropDependents :: (Tree a, Path a) -> (Tree a, Path a)
dropDependents (Tree r ds, p) = (Tree r [], p)

-- returns a list of all mother nodes in the path, from top to bottom
nodePath :: Path a -> [a] -> [a]
nodePath Top nodes = nodes
nodePath (Path m _ p _) nodes =  nodePath p (m:nodes)

motherNode :: Show a => (Tree a, Path a) -> a
motherNode (t, Top) = error ((show t) ++ " does not have a mother ")
motherNode treepath = treepathRoot $ goUp treepath

treepathRoot :: (Tree a, Path a) -> a
treepathRoot (Tree r _, _) = r

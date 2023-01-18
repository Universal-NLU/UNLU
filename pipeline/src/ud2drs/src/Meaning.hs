module Meaning
where


import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString
import Data.Text.Encoding
import Text.Regex
import Text.Regex.TDFA
import Text.Read
import Data.List
import Data.List.Split
import Data.String.Utils
import Debug.Trace
import qualified Data.Map as Map

import Token
import DepTree
import Help

data OffsetOption = NoOffsets | Offsets

data Template = Template T.Text Int
  deriving Show

readTemplatesFromFile :: String -> IO (Map.Map T.Text Template)
readTemplatesFromFile templateFile = do
  templateData <- fmap decodeUtf8 (Data.ByteString.readFile templateFile)
  return (constructTemplateMap templateData)

constructTemplateMap :: T.Text -> Map.Map T.Text Template
constructTemplateMap templateSection =
  let firstMap = readTemplates ((filterComments . T.lines) templateSection) Map.empty in
    removeTemplateCalls (Map.keys firstMap) firstMap

removeTemplateCalls :: [T.Text] -> Map.Map T.Text Template -> Map.Map T.Text Template
removeTemplateCalls [] templateMap = templateMap
removeTemplateCalls (t:ts) templateMap =
  removeTemplateCalls ts (Map.adjust (removeTemplateCallFromTemplate templateMap) t templateMap)

removeTemplateCallFromTemplate :: Map.Map T.Text Template -> Template ->  Template
removeTemplateCallFromTemplate templateMap template =
  until (\(Template text valency) -> null (findTemplateCalls text))
        (\(Template text valency) -> Template (applyTemplates templateMap text) valency)
        template

readTemplates :: [T.Text] -> Map.Map T.Text Template -> Map.Map T.Text Template
readTemplates [] templateMap = templateMap
readTemplates (t:ts) templateMap =
  let (name, template) = templateFromString t in
    readTemplates ts (Map.insert name template templateMap)

-- creation of templates from strings
-- parses a string definition of a template and returns a pair of its name and its definition
-- calls validate before it returns the template
templateFromString :: T.Text -> (T.Text, Template)
templateFromString templateRule =
  let (template, def) = T.breakOn (T.singleton '=') templateRule
      definition = T.strip $ T.dropWhile (== '=') def
  in
    if   definition == T.empty
    then error ("No body definition in '" ++ (show templateRule) ++ "'")
    else case (T.words template) of
           (name:valency:[]) -> case ((readMaybe (T.unpack valency)) :: Maybe Int) of
                                  Just n -> (name, (validate (Template definition n)))
                                  Nothing -> error "Template valency must be a number"
           _ -> error ("Missing template valency for " ++ (show template) )
                                  
-- throw an error if there is a mismatch if there are pointers numbered higher than the valency of the template
validate :: Template -> Template
validate (Template def valency) =
  if all (<= valency)  (map (\x -> ((read . T.unpack . (T.drop 1)) x) :: Int) ((getAllTextMatches (def =~ "#[1-9]+")) :: [T.Text]))
  then Template def valency
  else error ("Invalid template " ++ (show (Template def valency)))

-- application of templates to a string
applyTemplates ::  Map.Map T.Text Template -> T.Text -> T.Text
applyTemplates templateMap line =
  substituteTemplates line (findTemplateCalls line) templateMap
  
-- finds template calls in the string
-- will return hits like this: ["@v(^)","@n(^ ^)","@e ","@f"]
findTemplateCalls :: T.Text -> [T.Text]
findTemplateCalls rule = getAllTextMatches (rule =~ "@([^( ])+(\\(([^)])*\\))?") :: [T.Text]

-- takes a string and the template calls within it, plus the template map, and performs the substitution
substituteTemplates :: T.Text -> [T.Text] -> Map.Map T.Text Template -> T.Text
substituteTemplates string [] _ = string
substituteTemplates string (t:ts) templateMap =
  let (templateName, args) = templateAndArguments t
      template = (if Map.member templateName templateMap
                  then fromJust (Map.lookup templateName templateMap)
                  else error ("Unknown template " ++ (T.unpack templateName)))
      newstring =  T.replace t (instantiateTemplate template args) string
  in substituteTemplates newstring ts templateMap

instantiateTemplate :: Template -> [T.Text] -> T.Text
instantiateTemplate (Template text 3) (x:y:z:[])  = ((T.replace (T.pack "#1") x) . (T.replace (T.pack "#2") y) . (T.replace (T.pack "#3") z)) text
instantiateTemplate (Template text 2) (x:y:[]) =    ((T.replace (T.pack "#1") x) . (T.replace (T.pack "#2") y)) text
instantiateTemplate (Template text 1) (x:[]) =      T.replace (T.pack "#1") x text
instantiateTemplate (Template text 0) [] =        text
instantiateTemplate template args = error ("Template " ++ (show template ) ++ " was called with the wrong number of arguments:" ++ (show args))

templateAndArguments :: T.Text -> (T.Text, [T.Text])
templateAndArguments call =
  case call =~ "\\(([^)]*)\\)" :: (T.Text, T.Text, T.Text, [T.Text]) of
    (template, _, _, [])        -> (template, []) 
    (template, _, _, (args:[])) -> (template, processTemplateArgs args)

processTemplateArgs :: T.Text -> [T.Text]
processTemplateArgs args =
  if (T.isPrefixOf (T.pack "\"") cleanArgs) && (T.isSuffixOf (T.pack "\"") cleanArgs)
  then splitTemplateArgs cleanArgs
  else [cleanArgs]
  where cleanArgs = T.strip args

splitTemplateArgs :: T.Text -> [T.Text]
splitTemplateArgs args =
  T.splitOn (T.pack "\" \"") cleanArgs
  where cleanArgs = T.dropAround (=='"') args


readRules :: String -> Map.Map T.Text Template -> IO [T.Text]
readRules ruleFile templateMap = do
  rules <- fmap decodeUtf8 (Data.ByteString.readFile ruleFile)
  return (map (applyTemplates templateMap) (filterComments (T.lines rules)))

-- remove blank lines and lines starting with whitespace and #
-- TODO (?): inline comments? remove everything after #
filterComments :: [T.Text] -> [T.Text]
filterComments = filter (\x -> (not (x =~ "^[:space:]*#" || x =~ "^[:space:]*$" ))) 

-- a chopped tree is a tree plus a goal
--type ChoppedTree = (Tree Token, T.Text)
type ChoppedTree = ((Tree Token, Path Token), T.Text)

choppable :: [T.Text] -> (Tree Token, Path Token) -> T.Text
choppable [] _ =
  T.empty
choppable (rule:rs) treepath
    | matchRule treepath criterion && (T.null (T.strip goal)) = T.empty
    | matchRule treepath criterion = instantiateUniquely (goal, treepath)
    | otherwise = choppable rs treepath
  where criterion:goal:[] = (map T.strip (T.splitOn (T.pack "->") rule))

chopDependents :: [T.Text] -> (Tree Token, Path Token) -> ((Tree Token, Path Token), [ChoppedTree])
chopDependents _ treepath@(Tree r [], _) = (treepath, [])
chopDependents chopRules treepath@(Tree r deps, path) =
  let dependents = collectDependents chopRules (goDown treepath) []
      (chopped, nonChopped) = partition (\(_, goal) -> goal /= T.empty) dependents
  in
    ((Tree r (map (\((d,_),_) -> d) nonChopped), path), chopped)

-- traverses the sister nodes in a tree and pairs them with the associated proof goal
collectDependents :: [T.Text] -> (Tree Token, Path Token) -> [ChoppedTree] -> [ChoppedTree]
collectDependents chopRules treepath@(_, Path _ _ _ []) accumulator = reverse ((treepath, choppable chopRules treepath):accumulator)
collectDependents chopRules treepath@(tree, path) accumulator =
  collectDependents chopRules (goRight treepath) ((treepath, choppable chopRules treepath):accumulator)

-- wrapper for getLexicalEntries with the correct initial
-- values. Takes a rule file, a chop rule file and a ChoppedTree
-- (which is a tree, its embedding path and its goal) and returns a
-- list of readings (pairs of meanings, a tree, and a set of chopped
-- off trees).
getMeaning :: OffsetOption -> [T.Text] -> [T.Text] ->  ChoppedTree  ->  [([T.Text], ChoppedTree, [ChoppedTree])]
getMeaning offsetOption rules chopRules (treepath@(Tree r _, _), goal) =
  map
  (\(mcs, t, cts) -> (mcs, (t, goal), cts))
  (getLexicalEntries offsetOption rules chopRules r ([], treepath, []))

-- lexical entries for a whole subtree. Takes a set of rules, a
-- boolean flag that should start at false, plus a pair of an
-- (initially empty) set of meaning constructors and the root of the
-- subtree you want to get entries for, and a list of chopped off
-- trees. The boolean flag is used by goNext' and indicates whether
-- the current node has been visited before.

  -- Major FIXME: the use of goNext' can take us out of the chopped tree because it comes with its context

getLexicalEntries :: OffsetOption -> [T.Text] -> [T.Text] -> Token ->  ([T.Text], (Tree Token, Path Token), [ChoppedTree]) -> [([T.Text], (Tree Token, Path Token), [ChoppedTree])]
getLexicalEntries offsetOption headRules chopRules bound (mcs, treepath, choppedTrees) =
  do
    (mc, tp, ct) <- getLexicalEntry offsetOption headRules chopRules ([], treepath)
    case nextNode tp bound False of
      Nothing -> return (mc ++ mcs, tp, ct ++ choppedTrees)
      Just t  -> getLexicalEntries offsetOption headRules chopRules bound (mc ++ mcs, t, ct ++ choppedTrees)

-- takes a list of rules and a pair of MCs constructed so far and a
-- tree. Finds one rule, applies that to get possibly different
-- instantiations and returns a set of MCs with different
-- instantiations appended to the MCs, and paired with the possibly
-- modified tree. If we encounter a rule with an empty body, stop
-- looking for more matching rules. If we encounter a rule with the
-- body "STOP", stop looking for more matching rules *and* ignore the
-- subtree under the current node.
--
-- TODO this has by now become very complex and should be refactored
getLexicalEntry :: OffsetOption -> [T.Text] -> [T.Text] -> ([T.Text], (Tree Token, Path Token)) -> [([T.Text], (Tree Token, Path Token), [ChoppedTree])]
getLexicalEntry _ [] chopRules (mcs, treepath) =
  return (mcs, choppedTree, choppedDependents) -- chop here!
  where (choppedTree, choppedDependents) = chopDependents chopRules treepath
getLexicalEntry _ _ chopRules (_, treepath@((Tree (Root _ _) _), _)) = return ([], choppedTree, choppedDependents) -- chop here too
  where (choppedTree, choppedDependents) = chopDependents chopRules treepath
getLexicalEntry offsetOption (r:rs) chopRules (mcs, treepath@(tree, path))
  | not (matchRule treepath criterion) = getLexicalEntry offsetOption rs chopRules (mcs, treepath)
  | rule == T.empty = return (mcs, choppedTree, choppedDependents) -- chop here too
  | rule == (T.pack "STOP") = return(mcs, dropDependents(treepath), []) --ignoring all dependents, so no need to chop
  | otherwise =  (map (\(mc, tp) -> (if mc == T.empty
                                     then mcs
                                     else (name `T.append` (T.pack " : ") `T.append` mc):mcs, tp))
                  (parseEntry offsetOption rule treepath))
                 >>= getLexicalEntry offsetOption rs chopRules

  where criterion:rule:[] = (map T.strip (T.splitOn (T.pack "->") r))
        compactCriterion = (T.replace (T.singleton ':') T.empty) (T.intercalate (T.singleton ',') (map (T.strip . sec . (T.splitOn (T.singleton '='))) (T.splitOn (T.singleton ',') criterion)))
        name = ((lemma . root . fst) treepath) `T.append` ((T.pack . show . treeIndex . fst) treepath) `T.append` compactCriterion 
        (choppedTree, choppedDependents) = chopDependents chopRules treepath

matchRule :: (Tree Token, Path Token) -> T.Text -> Bool
matchRule treepath criteria =
  all (matchCriterion treepath) (T.splitOn (T.singleton ';') criteria)


  -- If there is a FU in the criterion, it is matched in case there is a token that matches the FU and the criteria inside {}. If the FU is negated, the criterion is satisfied if either no token matches the FU or none of the tokens that match the FU satisfy the criteria inside {}.
matchCriterion :: (Tree Token, Path Token) -> T.Text -> Bool
matchCriterion treepath criterion =
  -- match with a regular expressing which, if matching, yields three submatches: a (potential) negation, an FU, and any constraints on that FU (inside the curly brackets)
  -- TODO: note that we presuppose that the constraints on the FU come last. Anything after the curly brackets will effectivley be ignored.
  --                                   "([^{]+)(\\{(.*)\\})?"
  case (T.strip criterion) =~ "^(~?)\\s*([^{]+)\\{([^}]*)\\}$" :: (T.Text, T.Text, T.Text, [T.Text]) of
    -- case 1: the set of submatches is empty. This means either that we have the empty string (a corner case I probably didn't think about) or that the string does not end with stuff inside curly brackets. That in turn means that we only have specifications of a single token. Therefore we simply check it with matchToken.
    -- Notice, incidentally, that this is why both the following work:
    -- 1) relation = root, coarsePos = VERB
    -- 2) relation = root; coarsePos = VERB
    -- The first is interpreted as a single criterion with two specs on the token, the second as two different criteria (but necessarily matching the same token)
    (_, _, _, []) ->  (case T.stripPrefix (T.pack "~") (T.strip criterion) of
                         Just negatedCriterion -> not (matchTokenToString (root (fst treepath)) negatedCriterion)
                         Nothing -> (matchTokenToString (root (fst treepath)) (T.strip criterion))
                      )
    -- case 2: we have a functional uncertainty, so we process that and then match all instantiations of the fu against the feature specs in the brackets
    (_, _, _, neg:fu:criterion':[]) -> if neg == T.empty
                                       then ((not (null (token fu))) && any (\t -> matchTokenToString t criterion') (token fu))
                                       else ((null (token fu)) || all (\t -> not (matchTokenToString t criterion')) (token fu))
  where token fu = map (\x -> (root (fst x))) (navigate treepath (embellish fu))


-- return a list of pairs with instantiated strings and treepaths (because instantiation may have changed the tree  )
-- TODO: support for multiple local names
parseEntry :: OffsetOption -> T.Text -> (Tree Token, Path Token) -> [(T.Text, (Tree Token, Path Token))]
parseEntry offsetOption entry treepath =
  if entry =~ "\\|\\|"
  then (T.splitOn (T.pack "||") entry) >>= (\x -> parseEntry offsetOption x treepath)
  else if (T.strip entry) == T.empty
       then return (T.empty, treepath)
       else if let no = T.count (T.pack " : ") (entry) in no < 1 || no > 2
            then error ("Too many (or too few) colons in " ++ (T.unpack entry))
            else let semanticsL:typestring:localName = T.splitOn (T.pack " : ") entry
                     semanticsWithOffsets = insertOffsets offsetOption semanticsL treepath
                     semantics = T.replace (T.pack ":LEMMA:") (findLemma offsetOption (fst treepath)) semanticsWithOffsets -- TODO: this is where we have the pure lambda term, where we can substitute in character offsets etc. Wrap all this in the function which currently does the lemma substitution etc. NB This also means we need to pass down an option all the way here.
                     rootResolved = T.replace (T.pack "%R") ((T.pack . show . index . root . fst . goRealRoot) treepath) typestring
                     conjunctsResolved =  if T.isInfixOf (T.pack "%C") rootResolved
                                          then T.replace (T.pack "%C") (coordinationID treepath) rootResolved
                                          else rootResolved
                     localNameResolved = if (not . null) localName
                                         then instantiateLocalName (head localName) (conjunctsResolved, treepath)
                                         else [(conjunctsResolved, treepath)]
                 in (map (\(ty, tp) -> ((semantics `T.append` (T.pack " : ") `T.append` ty ), tp)) localNameResolved) >>= instantiateEntry
  where name = let r = (root (fst treepath))
               in lemma r `T.append` (T.singleton '(') `T.append` (T.pack (show (index r))) `T.append` (T.singleton ')')

insertOffsets :: OffsetOption -> T.Text -> (Tree Token, Path Token) -> T.Text
insertOffsets NoOffsets a _ = processEqualities
                              . processInequalities
                              .  (\x -> T.pack (subRegex (mkRegex ":INTR:\\([^)]+\\),?") (T.unpack x) ""))
                              . (T.replace (T.singleton '^') T.empty)
                              . (T.replace (T.singleton '*') T.empty)
                              . (\x -> T.pack (subRegex (mkRegex "\\{[^}]*\\}") (T.unpack x) ""))
                              $ a
insertOffsets Offsets a treepath@(t, _) =
  let currentNode = T.pack ("§" ++ (show $ treeIndex t) ++ "§")
      motherNode  = T.pack ("§" ++ (show $ treeIndex (fst (goUp treepath))) ++ "§")
  in (T.replace (T.singleton '^') motherNode)
     . (T.replace (T.singleton '*') currentNode)
     . (T.replace (T.pack ":INTR:") (T.pack "INTR"))
     . processFU treepath
     $ a


processInequalities :: T.Text -> T.Text
processInequalities text =
  case text =~ "NEQ\\(([^,]+),([^)]+)\\)" :: (T.Text, T.Text, T.Text, [T.Text]) of
    (pre, _, _, [])                -> pre
    (pre, match, post, (v1:v2:[])) -> processInequalities (pre <> T.singleton '(' <> v1 <> T.pack "!=" <> v2 <> T.singleton ')' <> post)

processEqualities :: T.Text -> T.Text
processEqualities text =
  case text =~ "EQ\\(([^,]+),([^)]+)\\)" :: (T.Text, T.Text, T.Text, [T.Text]) of
    (pre, _, _, [])                -> pre
    (pre, match, post, (v1:v2:[])) -> processEqualities (pre <> T.singleton '(' <> v1 <> T.pack "=" <> v2 <> T.singleton ')' <> post)


processFU :: (Tree Token, Path Token) -> T.Text -> T.Text
--processFU treepath text = text
-- TODO: support for multiple offsets in a single entry. We currently only match the first instance of curly brackets!
processFU treepath text =
  let (pre, match, post, _) = text =~ "\\{[^}]*\\}" :: (T.Text, T.Text, T.Text, [T.Text])
  in if match == T.empty
     then pre
     else pre <> (resolve treepath (T.splitOn (T.singleton '|') match)) <> post

resolve :: (Tree Token, Path Token) -> [T.Text] -> T.Text
-- default to the current node if nothing matched
resolve treepath@(tree, _) [] =  T.pack ("§" ++ (show $ treeIndex tree) ++ "§")
resolve treepath@(tree, _) (fu:fus) =
  let path = map (\w -> (w, T.singleton '_')) (T.words ((T.dropWhile (== '{')) . (T.dropWhileEnd (== '}')) $ fu))
  in
    case navigate treepath path of
    []         -> resolve treepath fus
    (x, _):[]  -> T.pack ("§" ++ (show $ treeIndex x) ++ "§")
    -- default to current node if one fu yielded several matches
    x:xs       -> T.pack ("§" ++ (show $ treeIndex tree) ++ "§") 

-- returns a unique ID for coordination structures, consisting of the
-- concatenation of `10` and the indices of each conjunct. This
-- function is only defined for tokens that are or have Conj daugthers
-- and raises an error if called on other tokens.
coordinationID :: (Tree Token, Path Token) -> T.Text
coordinationID treepath@(tree, path)
  | hasDaughterByRelation tree (T.pack "conj") = T.pack . concat $ map (show . treeIndex) (tree:(daughtersByRelation tree (T.pack "conj")))
  | (relation $ root tree) == (T.pack "conj") = coordinationID (goUp treepath)
  | otherwise = error (show treepath ++ " is not a conjunct")


findLemma :: OffsetOption -> Tree Token -> T.Text
findLemma offsetOption (Tree r deps) =
  let lemmaTks = lemmaTokens (Tree r deps)
  in (T.intercalate (T.singleton '_') (map normalizeLemma lemmaTks)) <>
     case offsetOption of
       Offsets   ->  T.singleton '§' <> T.pack (intercalate "_" (map (show . index) lemmaTks)) <> T.singleton '§'
       NoOffsets ->  T.empty

lemmaTokens :: Tree Token -> [Token]
lemmaTokens (Tree r deps) =
  sortBy (\x y -> compare (index x) (index y))
  (r:(concat(
      map
      lemmaTokens
      (filter (\x -> (relation $ root x) `elem` [ (T.pack "fixed"), (T.pack "flat"), (T.pack "goeswith") ] ) deps))))

normalizeLemma :: Token -> T.Text
normalizeLemma token =
  let lemma' = T.toLower .
               (T.replace (T.pack "(") T.empty) .
               (T.replace (T.pack ")") T.empty) .
               (T.replace (T.pack "-") (T.pack "_")) .
               (T.replace (T.pack "%-N") (T.pack "percent")) .
               (T.replace (T.pack "%") (T.pack "percent")) .
               (T.replace (T.pack "$-N") (T.pack "dollar")) .
               (T.replace (T.pack "$") (T.pack "dollar")) .
               (T.replace (T.pack "€-N") (T.pack "euro")) .
               (T.replace (T.pack "€") (T.pack "euro")) .
               (T.replace (T.pack "£-N") (T.pack "pound")) .
               (T.replace (T.pack "£") (T.pack "pound")) .
               (T.replace (T.pack "¥-N") (T.pack "yen")) .
               (T.replace (T.pack "¥") (T.pack "yen"))
               $ lemma token
  in if   (lemma' `elem` dangerousLemmata || (T.length lemma') == 1)
     then lemma' <> T.singleton '_'  <> ((T.singleton . T.head) $ coarsePos token)
     else lemma'

dangerousLemmata = map T.pack ["not", "exist"]

instantiateUniquely :: (T.Text, (Tree Token, Path Token)) -> T.Text
instantiateUniquely (entry, treepath) =
  case instantiateEntry (entry, treepath) of
    [] -> error "No instantiation!"
    (x:y:z) -> error "Multiple instantiations!"
    ((instantiatedEntry, newTreepath):[]) -> if goTop treepath == goTop newTreepath
                                             then instantiatedEntry
                                             else error ("Tree was changed during instantiation from " ++ (show (goTop treepath)) ++ " to " ++ (show (goTop newTreepath)) ++ " yielding " ++ (show instantiatedEntry)) 


instantiateEntry :: (T.Text, (Tree Token, Path Token)) -> [(T.Text, (Tree Token, Path Token))]
instantiateEntry (entry, treepath) =
  let semantics:typestring:[] = T.splitOn (T.pack " : ") entry
      rootResolved = T.replace (T.pack "%R") ((T.pack . show . index . root . fst . goRealRoot) treepath) typestring
      conjunctsResolved =  if T.isInfixOf (T.pack "%C") rootResolved
                          then T.replace (T.pack "%C") (coordinationID treepath) rootResolved
                          else rootResolved
  in map
     (\x -> (semantics `T.append` (T.pack " : ") `T.append` (fst x), snd x))
     (instantiateFU (conjunctsResolved, treepath))


instantiateFU :: (T.Text, (Tree Token, Path Token)) -> [(T.Text, (Tree Token, Path Token))]
instantiateFU (typestring, treepath) =
-- this matches every occurence of a letter followed by a parenthesis,
-- with a submatch on everything inside the parenthesis
  case typestring =~ "[a-zA-Z]\\(([^)0-9]+)\\)" of
       -- no more FUs left? return
       (_, _, _, [])            -> [(typestring, treepath)]
       (pre, match, post, [fu]) -> (map
                                       (\x -> let replacement = T.replace fu ((T.pack . show . treeIndex . fst) x) match
                                              in (pre `T.append` replacement  `T.append` post, goToIndex (treeIndex (fst treepath)) x))
                                       (navigateOrConstruct treepath (embellish fu))) >>= instantiateFU

-- we use string rather than T.Text internally in this function as I know no other way of doing query-replace
instantiateLocalName :: T.Text -> (T.Text, (Tree Token, Path Token)) -> [(T.Text, (Tree Token, Path Token))]
instantiateLocalName localname (entry, treepath) =
  let [var, fU] = if localname =~ " = "
                  then splitOn " = " (T.unpack localname)
                  else error ("No localname equation in " ++ (T.unpack localname))
      variable = if var =~ "%.*" then mkRegex (strip var) else error ("Malformed local name " ++ var)
      treepaths = navigateOrConstruct treepath (embellish (T.pack fU))
  in map
     (\x -> (T.pack (subRegex variable (T.unpack entry) (show (treeIndex (fst x)))), goToIndex (treeIndex (fst treepath)) x))
     treepaths
  
-- Note: ! refers to the current node and therefore gets removed
embellish :: T.Text -> [(T.Text, T.Text)]
embellish fU = map
               (\x -> let (_, _, _, [rel, _, feats]) = x =~ relationAndFeatures :: (T.Text, T.Text, T.Text, [T.Text])
                      in (fixUp rel, feats))
               (T.words (T.replace (T.singleton '!') T.empty fU))
  -- three matches: everything before any parenthesis, the parenthesis and the thing inside the parenthesis
  where relationAndFeatures = "([^{]+)(\\{(.*)\\})?"
        fixUp = (T.replace (T.pack "gf") (T.pack ".*")) . (T.replace (T.pack "*") (T.pack "§")) -- change to the format navigate wants

-- navigate along a given FU. If nothing is found, fall back on constructing it. But notice that even navigateConstructive can fail, i.e. return an empty list of treepaths, if the bottom of the functional uncertainty has a Kleene star and so is not interpreted constructively.
navigateOrConstruct :: (Tree Token, Path Token) -> [(T.Text, T.Text)] -> [(Tree Token, Path Token)]
navigateOrConstruct treepath path =
  case navigate treepath path of
    [] -> navigateConstructive treepath path
    x  -> x

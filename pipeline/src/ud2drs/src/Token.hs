module Token where

import qualified Debug.Trace
import Help
import Data.List
--import Data.List.Split
--import Data.String.Utils
--import Text.Regex.Posix
import Text.Regex
import Text.Regex.TDFA
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe

data Token = 
  Token {index    :: Int, 
         form     :: T.Text, 
         lemma    :: T.Text, 
         coarsePos:: T.Text,
         pos      :: T.Text, 
         feat     :: Map.Map T.Text T.Text, 
         relation :: T.Text ,
         trace    :: Bool } |
  Root  {index    :: Int,
         form     :: T.Text}
-- major TODO: in the code that chops trees, we now treat Root nodes
-- as just abstract nodes rather than only roots, i.e. there can be a
-- whole path of Root nodes before the lexical tree starts, which
-- implies a different semantics for this data structure.


  
instance Eq Token where
  a == b = index a == index b && isTrace a == isTrace b

instance Show Token where
  show t =   (show (T.unpack (form t))) ++ "(" ++ show(index t) ++ ")" ++
             if isTrace t
             then "t"
             else ""

instance Ord Token where
  x `compare` y = (index x) `compare` (index y)

showRelation :: Token -> String
showRelation (Root _ _) = ""
showRelation token = T.unpack (relation token)
                  
showWithRelation :: Token -> String
showWithRelation token = (show token) ++ "-" ++ (showRelation token)

inspect :: Token -> String
inspect (Token index form lemma coarsePos pos feat relation trace) =
  "{ index=" ++ (show index) ++
  ", form=" ++ (show form) ++
  ", lemma=" ++ (show lemma) ++
  ", coarsePos=" ++ (show coarsePos) ++
  ", pos=" ++ (show pos) ++
  ", features=" ++ (show feat) ++
  ", relation=" ++ (show relation) ++
  ", trace=" ++ (show trace) ++ "}"
inspect r@(Root _ _) = show r
                         
-- constructs a token from a conll-style line
-- paired with the index of its head
-- TODO - make this into an instance of Read?
-- TODO fix line starting with subRegex
constructToken :: T.Text -> (Token, Int)
constructToken l =
  (Token 
    (read (T.unpack (attrs !! 0)) :: Int)
    (((T.replace (T.singleton '"') T.empty) . (T.replace (T.singleton ',') T.empty)) (attrs !! 1))
    --        (subRegex (mkRegex (T.singleton ',')) (subRegex (mkRegex (T.singleton '"')) (attrs !! 1) "") "")  --delete commas and quote marks as they just cause trouble and this is only for easy access
    (attrs !! 2)          
    (attrs !! 3)
    (attrs !! 4)         
    (createFeatures (attrs !! 5))
    (attrs !! 7)
    False, read (T.unpack (attrs !! 6)) :: Int)
  where attrs = (T.splitOn (T.singleton '\t') l)

createFeatures :: T.Text -> Map.Map T.Text T.Text
createFeatures string =
  if string == (T.pack "_")
  then Map.empty
  else Map.fromList(map (tuplify . T.splitOn (T.singleton '=')) (T.splitOn (T.singleton '|') string))
--feature :: String -> Token -> Maybe String
--feature f tk = Map.lookup f (feat tk) 
                      
isRoot :: Token -> Bool
isRoot (Root _ _) = True
isRoot _ = False

isTrace :: Token -> Bool
isTrace (Token _ _ _ _ _ _ _ True) = True
isTrace _ = False

-- tToken is used as a default token for matching. The index field is ignored in matching, so an arbitrary 0 is assigned.
tToken = Token {index = 0, form =(T.pack ".*"), lemma = (T.pack ".*"), coarsePos = (T.pack ".*"), pos = (T.pack ".*"), feat = Map.empty, relation = (T.pack ".*"), trace = False }

matchTokenToString :: Token -> T.Text -> Bool
matchTokenToString token tokenSpec =
  -- we can have empty tokenSpecs so use dropWhile to make sure we don't end up with the empty string as a criterion
  let strippedTokenSpec = T.strip tokenSpec
      (neg, pos) = Data.List.partition (\x -> x =~ "!=" :: Bool) (dropWhile (== T.empty) (T.splitOn (T.singleton ',') strippedTokenSpec)) in 
    matchNegative token neg &&
    matchToken token (tokenFromString pos)

matchNegative :: Token -> [T.Text] -> Bool
matchNegative token tokenSpec =
  all
  (\(f,v) -> not (if f == T.pack "form"            then (form token) =~ wrap v
                   else if f == T.pack "lemma"     then (lemma token) =~ wrap v
                   else if f == T.pack "coarsePos" then (coarsePos token) =~ wrap v
                   else if f == T.pack "pos"       then (pos token) =~ wrap v
                   else if f == T.pack "relation"  then (relation token) =~ wrap v
                   else if Map.lookup f (feat token) == Nothing then False
                   else fromJust (Map.lookup f (feat token)) =~ wrap v ))
  (map (tuplify . (map T.strip) . T.splitOn (T.pack "!=")) tokenSpec)
  where wrap x = (T.singleton '^') `T.append` T.strip x `T.append` (T.singleton '$')

conllFields :: [T.Text]
conllFields = map T.pack ["index", "form", "lemma", "coarsePos", "pos", "relation"]

-- Create a new token from specified attributes with defaults. Any attribute that does not correspond to
-- a conll column will be interpreted as a feature.
-- TODO: do we need both this one and constructToken? Probably yes, this one is different as it creates a token used for matching, with regex fields.
tokenFromString :: [T.Text] -> Token
tokenFromString attrs=
  let attributes =
        if   attrs == []
        then Map.empty
        else Map.fromList (map (tuplify . (map T.strip) . T.splitOn (T.singleton '=')) attrs)
      -- interpret any keys that aren't conll columns as features
      featureList = Map.filterWithKey (\k _ -> not (k `elem` conllFields)) attributes
  in Token
     (read (T.unpack (Map.findWithDefault (T.pack "0") (T.pack "index") attributes)) :: Int)
     (wrap(Map.findWithDefault (T.pack ".*") (T.pack "form") attributes))
     (wrap(Map.findWithDefault (T.pack ".*") (T.pack "lemma") attributes))
     (wrap(Map.findWithDefault (T.pack ".*") (T.pack "coarsePos") attributes))
     (wrap(Map.findWithDefault (T.pack ".*") (T.pack "pos") attributes))
     featureList
     (wrap(Map.findWithDefault (T.pack ".*") (T.pack "relation") attributes))
     False
  where wrap x = (T.singleton '^') `T.append` T.strip x `T.append` (T.singleton '$')
  -- insert caret and dollar to make sure we match against the whole string

-- the fields of the second token are interpreted as regexes to be matched against the string values of the other token. The features field is special: here the features of the second token must subsume the features of the first one.
matchToken :: Token -> Token -> Bool
matchToken (Root _ _) _ = False
matchToken realToken testToken =
  let match = 
        (form realToken) =~ (form testToken) &&
        (lemma realToken) =~ (lemma testToken) &&
        (coarsePos realToken) =~ (coarsePos testToken) &&
        (pos realToken) =~ (pos testToken) &&
        (relation realToken) =~ (relation testToken) &&
  --  (trace realToken) == (trace testToken) &&
        matchFeatures (feat realToken) (feat testToken)
--  in Debug.Trace.trace ("Matching " ++ inspect realToken ++ " and " ++ inspect testToken ++ ": " ++ (show match) )
    in match

-- TODO: use the isSubmapOf function???
-- Compares two feature structures and yields +True+ iff the second feature structure subsumes the first, i.e. all features present in the second feature structure has the same value for that feature in the first feature structure
matchFeatures :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Bool
matchFeatures f1 f2 =
  all (matchFeature f1 f2) (Map.keys f2)
  where matchFeature f1 f2 feature =
          Map.lookup feature f2 == Map.lookup feature f1

-- Takes a token and the index of its head and constructs a conll line
tokenToConll :: (Token, Int) -> T.Text
tokenToConll ((Root _ _), _) = error ("The articificial root cannot be serialized as conll")
tokenToConll ((Token idx form lemma coarsePos pos feat relation _), hd) =
  T.intercalate (T.singleton '\t') ((T.pack (show idx)):form:lemma:coarsePos:pos:(featuresToString feat):(T.pack (show hd)):relation:(T.singleton ('_')):(T.singleton ('_')):[] )

-- Conll-style formatting of the feature map
featuresToString :: Map.Map T.Text T.Text -> T.Text
featuresToString f =
  T.intercalate (T.singleton '|') (Map.foldrWithKey (\ k a b -> (k `T.append` (T.singleton '=') `T.append` a):b) [] f)

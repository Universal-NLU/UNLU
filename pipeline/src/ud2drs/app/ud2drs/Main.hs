module Main where

import Control.Parallel.Strategies
import System.Environment
import System.IO
import Token
import DepTree
import Help
import Meaning
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Debug.Trace
import qualified Data.Map as Map

main :: IO ()
main = do

  arguments <- getArgs
  let (options, args) = partition (isPrefixOf "-") arguments
  let (ruleFile:chopRuleFile:templateFile:[]) =
        case args of
          []       -> ["rules.dat","chopRules.dat","templates.dat"]
          r:[]     -> [r,"chopRules.dat","templates.dat"]
          r:c:[]   -> [r,c,"templates.dat"]
          r:c:t:[] -> [r,c,t]

  templates <- readTemplatesFromFile templateFile
  rules <- readRules ruleFile templates 
  chopRules <- readRules chopRuleFile templates

  let offsetOption = if "-i" `elem` options
                     then Offsets
                     else NoOffsets

  let loggingOption = not ("-n" `elem` options)

  trees <- conllToTrees

  let (out, err) = parParseTrees offsetOption loggingOption rules chopRules trees
  putStrLn out
  hPutStrLn stderr err
  
parParseTrees :: OffsetOption -> Bool -> [T.Text] -> [T.Text] -> [(Tree Token, Metadata)] -> (String, String)
parParseTrees offsetOption loggingOption rules chopRules trees =
  let out =
        parMap
        rdeepseq
        (\(tree, m) -> let id = case Map.lookup (T.pack "sent_id") m of
                             Just x -> (" (sent_id=" ++ (T.unpack x) ++ ")")
                             Nothing -> ""
                       in "*** sentence" ++ id ++ "\n" ++ (indentedMeanings offsetOption rules chopRules ComputerReadableFormat tree))
        trees
      err =
        (if not loggingOption
         then []
         else parMap
              rdeepseq
              (\(tree, m) -> let id = case Map.lookup (T.pack "sent_id") m of
                                   Just x -> (" (sent_id=" ++ (T.unpack x) ++ ")")
                                   Nothing -> ""
                             in "*** sentence" ++ id ++ "\n" ++ (ppStringYield tree) ++ "\n" ++ (indentedMeanings offsetOption rules chopRules HumanReadableFormat tree))
              trees)
  in (concat out, concat err)

-- calls output to get a list of meanings and yields a string representation of them
indentedMeanings :: OffsetOption -> [T.Text] -> [T.Text] -> Format -> Tree Token -> String
indentedMeanings offsetOption rules chopRules format tree =
  concatMeanings (output offsetOption rules chopRules format 0 tree) 1
  where concatMeanings [] _     = ""
        concatMeanings (m:[]) i = "** reading " ++ (show i) ++ "\n" ++ m
        concatMeanings (m:ms) i = "** reading " ++ (show i) ++ "\n" ++ m ++ concatMeanings ms (i+1)

output :: OffsetOption -> [T.Text] -> [T.Text] -> Format -> Int -> Tree Token -> [String]
output offsetOption rules chopRules format indents tree =
  let rootGoal = choppable chopRules (goRealRoot (tree, Top)) in
    if rootGoal == T.empty
      then error("Found no goal for the root " ++ (show tree))
      else getMeaning offsetOption rules chopRules (goRealRoot (tree, Top), rootGoal) >>= reading2strings offsetOption rules chopRules format 0

reading2strings :: OffsetOption -> [T.Text] -> [T.Text] -> Format -> Int -> ([T.Text], ChoppedTree, [ChoppedTree]) -> [String]
reading2strings offsetOption rules chopRules format indents (mcs, (rootTree, rootGoal), choppedTrees) =
  let rootMeaning = reading2string format indents rootTree rootGoal mcs in
    case choppedTrees of
      []  ->  return (rootMeaning)
      cts -> do depSetMeaning <- (sequence (map (getMeaning offsetOption rules chopRules) choppedTrees))
                meanings      <- (sequence (map (reading2strings offsetOption rules chopRules format (indents + 1)) depSetMeaning))
                return (rootMeaning ++ "\n" ++ unlines meanings)
    
reading2string :: Format -> Int -> (Tree Token, Path Token) -> T.Text -> [T.Text] -> String
reading2string format indents treepath goal mcs =
  (if format == HumanReadableFormat
  then offset ++ show treepath ++ "\n"
  else "") ++
  offset ++ "*goal: " ++ (T.unpack goal) ++ "\n" ++
  unlines (map (\x -> offset ++ (T.unpack (formatMeaning format x))) mcs)
  where offset = take (2 * indents) (cycle " ")

data Format = ComputerReadableFormat | HumanReadableFormat deriving Eq
formatMeaning :: Format -> T.Text -> T.Text
formatMeaning ComputerReadableFormat mc =
  let [lemma, lambda, typus] = T.splitOn (T.pack " : ") mc
  in  lambda  `T.append` (T.pack " : ")  `T.append` typus
formatMeaning HumanReadableFormat mc = 
  let [lemma, lambda, typus] = T.splitOn (T.pack " : ") mc
  in  lambda  `T.append` (T.pack " : ")  `T.append` typus `T.append` (T.singleton '%') `T.append` lemma  


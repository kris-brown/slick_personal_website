{-# LANGUAGE OverloadedStrings #-}
module Latex where

import Text.LaTeX (render)
import Text.LaTeX.Base.Parser (parseLaTeXFile)
import Text.LaTeX.Base.Syntax (LaTeX(..),  TeXArg(..), matchEnv)
import Data.Text (unpack, Text, concat, pack, splitOn)
import Data.Char (isDigit)
import Data.Tuple.Extra (uncurry3)
import Data.Map (fromList, Map, (!))
import Debug.Trace (trace)
import Anki (Card (..), CardType(..), Deck (..))
import Prelude hiding (concat)


fp :: Map Deck FilePath
fp = fromList [(Sketch, "/Users/ksb/sevensketches/main.tex"),
               (Aluffi, "/Users/ksb/aluffinotes/main.tex")]

-- TOC STUFF - in theory could be automatically parsed from the TeX file
chaps Sketch = [
      "Chapter 1: Generative Effects",
      "Chapter 2: Resources - monoidal preorders and enrichment",
      "Chapter 3: Databases - categories, functors, (co)limits",
      "Chapter 4: Collaborative design - profunctors, categorification, monoidal categories",
      "Chapter 5: Signal flow graphs - props, presentations, proofs",
      "Chapter 6: Circuits - hypergraph categories and operads",
      "Chapter 7: Logic of behavior - sheaves, toposes, languages"]

-- ChapNum.X -- the starting X of each section, indexed by chapter
secs Sketch = [[("1.1: More than the sum of their parts", 1),("1.2: What is order?", 10),("1.3: Meets and joins", 81),("1.4: Galois connections", 95)],
      [("2.1: Getting from a to b", 1),("2.2: Symmetric monoidal preorders", 2),("2.3: Enrichment", 46),("2.4: Constructions on V-categories", 64),("2.5: Computing presented V-categories with matrix mult.", 80)],
      [("3.1: What is a database?", 1),("3.2: Categories", 6),("3.3: Functors, natural transformations, and databases", 35),("3.4: Adjunctions and data migration", 65),("3.5: Bonus introduction to limits and colimits", 79)],
      [("4.1: Can we build it?",1),("4.2: Enriched profunctors",2),("4.3: Categories of profunctors",19),("4.4: Categorification",44),("4.5: Profunctors form a compact closed category",55)],
      [("5.1: Comparing systems as interacting signal processors",1),("5.2: Props and presentations",2),("5.3: Simplified signal flow graphs",36),("5.4: Graphical linear algebra",56)],
      [("6.1: The ubiquity of network languages",1),("6.2: Colimits and connection",1),("6.3: Hypergraph categories",51),("6.4: Decorated cospans",68),("6.5: Operads and their algebras",89)]]
-- ChapNum.X -- the starting X of each part, indexed by chapter
parts Sketch = [
      [("1.1.1: A first look at generative effects", 1),("1.1.2: Ordering systems", 5),("1.2.1: Review of sets, relations, and functions",10),("1.2.2: Preorders",30),("1.2.3: Monotone maps",59),("1.3.1: Definition and basic examples",81),("1.3.2: Back to observations and generative effects",92),("1.4.1: Definition and examples",95),("1.4.2: Back to partitions",102),("1.4.3: Basic theory",108),("1.4.4: Closure operators",120),("1.4.5: Level shifting",124)],
      [("2.1.1: Introduction",1),("2.2.1: Definition and first examples",2),("2.2.2: Introducing wiring diagrams",10),("2.2.3: Applied examples",22),("2.2.4: Abstract examples",28),("2.2.5: Monoidal monotone maps",41),("2.3.1: V-categories",46),("2.3.2: Preorders as Bool-categories",49),("2.3.3: Lawvere metric spaces",51),("2.3.4: V-variations on preorders and metric spaces",61),("2.4.1: Changing the base of enrichment",64),("2.4.2: Enriched functors",69),("2.4.3: Product V-categories",74),("2.5.1: Monoidal closed preorders",80),("2.5.2: Quantales",90),("2.5.3: Matrix multiplication in a quantale",99)],
      [("3.1.1: Introduction", 1),("3.2.1: Free categories", 6),("3.2.2: Presenting categories via path equations", 16),("3.2.3: Preorders and free categories", 21),("3.2.4: Important categories in mathematics", 24),("3.2.5: Isomorphisms in a category", 28),("3.3.1: Sets and functions as databases", 35),("3.3.2: Functors", 35),("3.3.3: Databases as Set-valued functors", 44),("3.3.4: Natural transormations", 49),("3.3.5: The category of instances on a schema", 60),("3.4.1: Pulling back data along a functor", 65),("3.4.2: Adjunctions", 69),("3.4.3: Left and right pushforward functors", 75),("3.4.4: Single set summaries of databases", 75),("3.5.1: Terminal objects and products", 79),("3.5.2: Limits", 91),("3.5.3: Finite limits in Set", 95),("3.5.4: A brief note on colimits", 102)],
      [("4.1: Introduction",1), ("4.2.1: Feasibility relationships as Bool-profunctors",2), ("4.2.2: V-profunctors",8), ("4.2.3: Back to co-design diagrams",18), ("4.3.1: Composing profunctors",19), ("4.3.2: The categories V-Prof and Feas",23), ("4.3.3: Fun profunctor facts- companions, conjoints, collages",34), ("4.4.1: The basic idea of categorification",44), ("4.4.2: A reflection on wiring diagrams",44), ("4.4.3: Monoidal categories",45), ("4.4.4: Categories enriched in a symmetric monoidal category",51), ("4.5.1: Compact closed categories",55), ("4.5.2: Feas as a compact closed category",63)],
      [("5.1.1: Introduction",1),("5.2.1: Definition and first examples",2),("5.2.2: The prop of port graphs",13),("5.2.3: Free constructions and universal properties",19),("5.2.4: The free prop on a signature",25),("5.2.5: Props via presentations",33),("5.3.1: Rigs",36),("5.3.2: The iconography of signal flow graphs",43),("5.3.3: The prop of matrices over a rig",48),("5.3.4: Turning signal flow graphs into matrices",52),("5.3.5: The idea of functorial semantics",56),("5.4.1: A presentation of Mat(R)",56),("5.4.2: Aside - monoid objects in a monoidal category",65),("5.4.3: Signal flow graphs - feedback and more",75)],
      [("6.1.1: Introduction",1),("6.2.1: Initial objects",1),("6.2.2: Coproducts",11),("6.2.3: Pushouts",19),("6.2.4: Finite colimits",30),("6.2.5: Cospans",43),("6.3.1: Frobenius monoids",51),("6.3.2: Wiring diagrams for hypergraph categories",59),("6.3.3: Definition",60),("6.4.1: Symmetric monoidal functors",68),("6.4.2: Decorated cospans",71),("6.4.3: Electric circuits",79),("6.5.1: Operads design wiring diagrams",89),("6.5.2: Operads from symmetric monoidal categories",93),("6.5.3: The operad for hypergraph props",98)],
      [("7.1.1: Introduction", 1)]]

envs :: String -> Bool
envs = flip elem ["prop", "defn", "exercise", "example"]

tex :: Deck -> IO LaTeX
tex deck = do (Right t) <- parseLaTeXFile $ fp ! deck
              return t

lshow :: LaTeX -> IO()
lshow = putStrLn . unpack . render

cardArgs :: LaTeX -> [(String, [TeXArg], LaTeX)]
cardArgs = matchEnv envs

getRaw :: TeXArg ->  Text
getRaw (FixArg (TeXRaw t)) = t
getRaw x = trace (show x) undefined

ctMap :: Map String CardType
ctMap = fromList [("exercise", Question), ("prop", Prop), ("example", Ex), ("defn", Def)]

mkCard_ :: Deck -> String -> [TeXArg] -> LaTeX -> Card
mkCard_ deck ct args a = Card (ctMap ! ct) (concat [pack ct, " ", title])
                         (getFront ct args a) (getBack ct a) chap sec part page
  where getBack "example" _ = ""
        getBack _ a = render a
        (title, page) = (getRaw $ args !! 0, getNum $ args !! 1)
        getNum x = (read (unpack $ getRaw x) :: Int)
        (chap, sec, part) = locate deck title

locate :: Deck -> Text -> (Text, Text, Text)
locate deck title =  (getChap deck ci, getSec deck ci y, getPart deck ci y)
  where  [x, y] = trace (show title) $ map (read . filter isDigit . unpack) $ splitOn "." title
         ci = x - 1

getChap :: Deck -> Int -> Text
getChap deck chapind = (chaps deck) !! chapind
getSec :: Deck ->  Int -> Int -> Text
getSec deck chapind i = indexTOC ((secs deck) !! chapind) i
getPart :: Deck ->  Int -> Int -> Text
getPart deck chapind i= indexTOC ((parts deck) !! chapind) i

indexTOC :: [(Text, Int)] -> Int -> Text
indexTOC ((t,j):pairs) i
 | null pairs = t
 | i >= j && i < (snd $ head pairs) = t
 | otherwise = indexTOC pairs i


getFront :: String -> [TeXArg] -> LaTeX -> Text
getFront "example" _ a = render a
getFront _  [_, _, q] _ = render q

mkCard :: Deck -> (String, [TeXArg], LaTeX) -> Card
mkCard deck = uncurry3 (mkCard_ deck)


{-# LANGUAGE OverloadedStrings #-}
module Latex where

import Text.LaTeX (render)
import Text.LaTeX.Base.Parser (parseLaTeXFile)
import Text.LaTeX.Base.Syntax (LaTeX(..),  TeXArg(..), matchEnv)
import Data.Text (unpack, Text, concat, pack)
import Data.Tuple.Extra (uncurry3)
import Data.Map (fromList, Map, (!))
import Debug.Trace (trace)
import Anki (Card (..), CardType(..))
import Prelude hiding (concat)

fp :: Map String FilePath
fp = fromList [("sketch", "/Users/ksb/sevensketches/main.tex"),
               ("aluffi", "/Users/ksb/aluffinotes/main.tex")]

envs :: String -> Bool
envs = flip elem ["prop", "defn", "exercise", "example"]

tex :: String -> IO LaTeX
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

mkCard_ :: String -> [TeXArg] -> LaTeX -> Card
mkCard_ ct args a = Card (ctMap ! ct) (concat [pack ct, " ", getRaw $ args !! 0])
                         (getFront ct args a) (getBack ct a)  "<CHAP>" "<SEC>" "<PART>"
                         (getNum $ args !! 1)
  where getBack "example" _ = ""
        getBack _ a = render a
        getNum x = (read (unpack $ getRaw x) :: Int)

getFront :: String -> [TeXArg] -> LaTeX -> Text
getFront "example" _ a = render a
getFront _  [_, _, q] _ = render q


mkCard = uncurry3 mkCard_


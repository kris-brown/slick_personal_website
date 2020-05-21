{-# LANGUAGE OverloadedStrings #-}
module Main where

import Anki (doAnki, Card (..), Deck)
import Latex (tex, cardArgs, mkCard)
import System.Environment (getArgs)
import Data.Text (isInfixOf, concat)

sketch :: Deck -> IO [Card]
sketch deck = do x <- tex deck
                 let cards = mkCard deck <$> cardArgs x
                 return $ filter (not . tdnc) cards
    where findpat pat c = isInfixOf pat $ Data.Text.concat [cardFront c, cardBack c]
          todo = findpat "TODO"
          nocard = findpat "NOCARD"
          tdnc x = todo x || nocard x


main :: IO ()
main = do
    args <- ((fmap read) <$> getArgs)
    putStrLn("The arguments are "++show(args))
    mapM_ (doAnki sketch) args

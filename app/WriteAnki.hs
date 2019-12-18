module Main where

import Anki (doAnki, Card)
import Latex (tex, cardArgs, mkCard)


sketch :: String -> IO [Card]
sketch deck = do x <- tex deck
                 let cards = mkCard <$> cardArgs x
                 return cards

main :: IO ()
main = mapM_ (doAnki sketch) ["sketch", "aluffi"]

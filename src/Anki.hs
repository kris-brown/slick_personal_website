{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns #-}

module Anki (doAnki, Card(..), CardType(..), Deck(..)) where

import           Control.Monad         (when)
import           Data.Aeson            (FromJSON (..), Result (..), ToJSON (..), Value (..), object,
                                        (.=), fromJSON)
import qualified Data.ByteString.Char8 as S8
import           Data.HashMap.Strict   ((!))
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text, concat, pack, unpack, replace, isInfixOf, append, splitOn, intercalate)
import qualified Data.Vector           as V
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple   (getResponseBody, httpJSON,
                                        setRequestBodyJSON)
import Prelude               hiding (concat, (!), intercalate)
import Text.RawString.QQ
import Text.Regex.PCRE.ByteString.Utils            (substituteCompile)
import Text.Pandoc.Options (def, ReaderOptions, WriterOptions)
import Text.Pandoc.Class (runIOorExplode, setVerbosity)
import Text.Pandoc.Logging (Verbosity(..))
import Text.Pandoc.Readers.LaTeX (readLaTeX)
import Text.Pandoc.Writers.HTML (writeHtml4String)
import Data.ByteString.UTF8 (fromString, toString)
import Control.Concurrent (threadDelay)
import Debug.Trace (traceIO)

------
data Deck  = Sketch | Aluffi deriving (Read, Show, Eq, Ord)

data CardType = Ex | Prop | Def | Question  deriving (Eq,Show)

data Card = Card {
    cardType  :: CardType,
    cardTitle :: Text,
    cardFront :: Text,
    cardBack  :: Text,
    cardChap  :: Text,
    cardSect  :: Text,
    cardPart  :: Text,
    cardPage  ::  Int
} deriving (Eq,Show)

qCard = Card Def "defn 2.2" "What is 1+1?" "It's 2" "Chp1 math" "Sec 1: addition" "Part 1: Intro" 3

isCloze :: Card -> Bool
isCloze c = isInfixOf "\\," $ append (cardFront c) (cardBack c)

ro = def :: ReaderOptions
wo = def :: WriterOptions

----------------------------------
doAnki :: (Deck -> IO [Card]) -> Deck -> IO ()
doAnki cardfun deck = do
    createDeck deck
    cards <- cardfun deck
    mapM_ (addAnkiNote deck) cards
    let seen = map cardTitle cards
    removeUnseen deck seen  -- cardsInfo seems to not be working...
----------------------------------
fields :: Card -> IO Value
fields c = do
    (front, back) <- cardProcess c
    let x = ["Text" .= concat [front, "\n<br><hr><br>\n",back]]
    let y = ["Front" .= front, "Back" .= back]
    return $ object $ ["Title"  .= cardTitle c,
                    "Chapter" .=  (cardChap c),
                    "Section" .=  ( cardSect c),
                    "Part"    .=   (cardPart c) ,
                    "Page"    .= show (cardPage c) ]
                    ++ (if isCloze c then x else y)

cardProcess :: Card -> IO (Text, Text)
cardProcess c = do
    front <- process $ cardFront c
    back <- process $ cardBack c
    return (front, back)

cssdefault = ".card {\n font-family: times new roman;\n font-size: 30px;\n text-align: center;\n color: black;\n background-color: white;\n line-height: 40px;\n}\n"

addAnkiNote :: Deck -> Card -> IO()
addAnkiNote deck c = do
    threadDelay 50000
    noteids <- findNotes deck c
    when (length noteids > 1) $ do error $ show ("Duplicates found!",c,noteids)
    when (length noteids == 1) $ do
        let [noteid] = noteids
        updateNoteFields deck c noteid
    when (length noteids == 0) $ do
        addNote deck c
    return ()

-- Any Titles that are NOT found in the current list of titles are removed
removeUnseen :: Deck -> [Text] -> IO ()
removeUnseen deck ids = do
    cardIDs <- findCards deck
    req <- cardsInfo cardIDs
    let unseen = mapMaybe (checkSeen ids) $ req
    when (length unseen > 0) $ do
        putStrLn $ "remove " ++ show (length unseen) ++ " notes? y/n -->"
        myString <- getLine
        when (head myString == 'y') $ do
            deleteNotes unseen
    return ()

-- AKA: partial function city, population 8
checkSeen :: [Text] -> Value -> Maybe Value
checkSeen ids (Object x) =
    case x ! "fields" of
        Object y -> case y ! "Title" of
            Object z ->  case z ! "value" of
                String s -> if s `elem` ids
                            then Nothing
                            else Just (x ! "note")

-- | Apply custom replacement function to segments of text between delimiters
-- e.g. processSegment "|" (const "XXX") "abc|def|ghi" = "abc|XXX|ghi"
-- Assumes that there are an even number of delimiters
processSegment :: Text -> (Text -> Text) -> Text -> Text
processSegment delim replFn doc = intercalate delim (proc chunks)
  where chunks = splitOn delim doc           -- split on delimiter
        proc (a:b:c) = a:(replFn b):(proc c) -- apply fn to every other region
        proc [c] = [c]                       -- don't apply fn to final region
        proc [] = undefined                  -- impossible if even #

hideDollar :: Text -> Text
hideDollar = processSegment "tikzpicture" (replace "$" "¥")

-- Combine all text processing
process :: Text -> IO Text
process x = do Right preprocessed <- ppx
               let pptxt = pack $ toString $ preprocessed
               y <- to_html $ replace "``" "\"" $ replace "\\," "##" pptxt
               let yb = fromString $ unpack y
               Right subbe_ <- substituteCompile "##([^#]+)##" yb "{{c1::\\1}}"
               Right subbe__ <- substituteCompile "{\\$([^$]+)\\$}" subbe_ "{\\1}"
               Right subbed <- substituteCompile "\\$([^$]+)\\$" subbe__ "[$$]\\1[/$$]"
               let res =  replace "¥" "$" $ pack $ toString subbed
               return $ replace "<table>" "<table border=\"1\">" res
    where ppx = substituteCompile "\\\\hyperref\\[([^\\]]+)\\]\\{([^}]+)}" bx  "\\2 (\\1)"
          bx = fromString $ unpack $ hideDollar x

-- test :: Text
-- test = "In a power set, the \\hyperref[D1.81]{meet} of a collection of subsets is their \\,intersection\\,, while the \\hyperref[D1.81]{join} is their \\,union\\,."
-- testb = fromString $ unpack test
-- testhtml  = to_html test
-- tsub z = substituteCompile " ([^ ]+) " (fromString $ unpack z) "{{c1::\\1}}"

to_html :: Text -> IO Text
to_html inp = runIOorExplode $ do
        setVerbosity ERROR
        lat <- readLaTeX ro inp
        writeHtml4String wo lat

--
--------------------------------------------------------------------

-- | Send JSON request to Anki, receive JSON value
mkReq :: String -> Value -> Bool -> IO Value
mkReq action params debug = do
    let req = object [ "action" .= action,"version".=(6 :: Int), "params" .= params]
    let request = setRequestBodyJSON req  "GET http://localhost:8765"
    response <- httpJSON request
    let (Object x) = getResponseBody response
    when debug $ print req
    when debug $ S8.putStrLn $ Yaml.encode x
    return $ x ! "result"

-- Specialized instances of mkReq
createDeck :: Deck -> IO ()
createDeck deck = do
    mkReq "createDeck" (object ["deck" .= (show deck)]) False
    return ()

findNotes :: Deck -> Card -> IO [Int]
findNotes deck c = do
    res <- mkReq "findNotes" (object ["query" .= query]) False
    let Success ns = (fromJSON res) :: Result [Int]
    return ns
  where query = concat ["title:'" , cardTitle c,"' deck:",pack (show deck)]

updateNoteFields :: Deck -> Card -> Int -> IO ()
updateNoteFields deck c noteid = do
    fs <- fields c
    let note = object (note1 ++ ["fields"    .= fs])
    mkReq "updateNoteFields" (object ["note" .= note]) False
    return ()
  where note1 = ["deckName"  .= show deck,
                 "modelName" .= (if isCloze c then "Cloze" else "Basic" :: String),
                 "tags"      .= [show $ cardType c],
                 "id"        .= toJSON noteid]

addNote :: Deck -> Card -> IO ()
addNote deck c = do
    fs <- fields c
    let note = object (note1 ++ ["fields" .= fs])
    mkReq "addNote" (object ["note" .= note]) False
    return ()
  where note1 = ["deckName"  .= show deck,
                 "modelName" .= (if isCloze c then "Cloze" else "Basic" :: String),
                 "tags"      .= [show $ cardType c]]

findCards :: Deck -> IO [Int]
findCards deck = do
    res <- mkReq "findCards" (object ["query" .= ("deck:"++(show deck))]) False
    let Success ns = (fromJSON res) :: Result [Int]
    return ns

cardsInfo :: [Int] -> IO [Value]
cardsInfo cardIDs = do
    x <- mkReq "cardsInfo" (object ["cards" .= toJSON cardIDs]) False
    Array res <- mkReq "cardsInfo" (object ["cards" .= toJSON cardIDs]) False
    return $ V.toList res

noint = toJSON ([1582961275791] :: [Int])
testci = mkReq "cardsInfo" (object ["cards" .= noint]) False

deleteNotes :: [Value] -> IO ()
deleteNotes unseen = do
    mkReq "deleteNotes" (object ["notes" .= unseen]) False
    return ()
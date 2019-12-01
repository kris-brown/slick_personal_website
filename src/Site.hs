{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Site (exportHTML) where

import           Data.String       (IsString)
import           System.Directory  (copyFile)
import           System.Process    (callCommand)
import           Text.Format       (format)
import           Text.RawString.QQ
orgs :: IsString a => [a]
orgs =  ["aluffi", "logic", "sketch","nonclassical"] --

exportHTML :: IO ()
exportHTML = mapM_ exportHTML_ orgs

exportHTML_ :: String -> IO ()
exportHTML_ fname = do
    copyFile (format "/Users/ksb/{0}.org" [fname]) (format "/Users/ksb/slick_personal_website/site/docs/{0}.org" [fname])

    callCommand (format [r|/Applications/Emacs.app/Contents/MacOS/Emacs -batch -Q -l ~/.emacs --visit=/Users/ksb/slick_personal_website/site/docs/{0}.org --eval="(progn (org-html-export-as-html) (princ (buffer-string)))" | sed 's/##//g' | src/addHead.sh > /Users/ksb/slick_personal_website/site/docs/{0}_org.html|] [fname])

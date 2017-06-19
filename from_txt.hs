#!/usr/bin/env stack
-- stack --resolver lts-8.11 --install-ghc runghc --package text
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (maximumBy, group)
import Data.Function (on)

main :: IO ()
main = getArgs >>= mapM_ process

process :: FilePath -> IO ()
process x = do
  let name = (iterate init . drop 1 . dropWhile (/= '/') $ x) !! 4
      outFile = "poems/" ++ name ++ ".tex"
  input <- T.readFile x
  let out = procPoem name (T.lines input)
  T.writeFile outFile (T.unlines out)

procPoem :: String -> [Text] -> [Text]
procPoem name xs = titleTex : label : versewidth : beginVerse : addPar body' ++ [endVerse]
  where
    (title:incipit:body) = map clean xs
    useIncipit = not (T.null incipit)
    body' = removeExtraBlanks $ if useIncipit then incipit : body else body
    titleWithIncipit = title <> " " <> incipit
    titleTex = if useIncipit then "\\PoemTitle[" <> titleWithIncipit <> "]{" <> title <> "}" else "\\PoemTitle{" <> title <> "}"
    label = "\\label{ch:" <> T.pack name <> "}"
    width = maximumBy (compare `on` T.length) body'
    versewidth = "\\settowidth{\\versewidth}{" <> width <> "}"
    beginVerse = "\\begin{verse}[\\versewidth]"
    endVerse = "\\end{verse}"
    clean = T.replace ". . ." "\\ldots" . T.concatMap cleanChar
    cleanChar '—' = "---"
    cleanChar '‘' = "`"
    cleanChar '’' = "'"
    cleanChar '“' = "``"
    cleanChar '”' = "''"
    cleanChar '&' = "\\&"
    cleanChar '\t' = " \\qquad "
    cleanChar x = T.singleton x

addPar :: [Text] -> [Text]
addPar (x:xs@(y:xs')) = if y == "" then x:y:addPar xs' else (x <> "\\\\*"):addPar xs
addPar xs = xs

removeExtraBlanks :: [Text] -> [Text]
removeExtraBlanks = concat . map f . filter (/= ["[new stanza]"]) . group
  where
    f ("":_) = [""]
    f x = x

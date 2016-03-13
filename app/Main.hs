module Main where

import           Control.Concurrent.Async
import           Data.Char                (isPunctuation)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import qualified NGram
import PJN



langs :: [(String, [FilePath])]
langs = [
      ("deutsch", [
          "lang/2momm10.txt"
        , "lang/4momm10.txt"
        , "lang/5momm10.txt"
        , "lang/8momm10.txt"
        ])
    , ("italian", [
          "lang/54.txt"
        , "lang/q.txt"
        ])
    , ("spanish", [
          "lang/spanish.txt"
        , "lang/spanish1.txt"
        ])
    , ("polish", [
          "lang/polski.txt"
        , "lang/polski2.txt"
        , "lang/polski3.txt"
        ])
    , ("finnish", [
          "lang/finnish.txt"
        , "lang/finnish1.txt"
        ])
    , ("english", [
          "lang/Harry Potter 1 Sorcerer\'s_Stone.txt"
        , "lang/Harry Potter 2 Chamber_of_Secrets.txt"
        , "lang/Harry Potter 3 Prisoner of Azkaban.txt"
        , "lang/Harry Potter 4 and the Goblet of Fire.txt"
        ])
    ]

type Wisdom = [(String, NGram.NGram T.Text)]

readNGrams :: Int -> IO [(String, NGram.NGram T.Text)]
readNGrams n = mapConcurrently readLang langs
    where
        readLang :: (String, [FilePath]) -> IO (String, NGram.NGram T.Text)
        readLang (lang, files) = do
            ngrams <- mapConcurrently (flip nGramFromFile n) files
            return (lang, NGram.unions ngrams)

hit :: Int -> Wisdom -> NGram.NGram T.Text -> T.Text
hit n wisdom ngram = let hits = (fmap.fmap) (flip NGram.intersection ngram) wisdom
                     in T.pack $ show hits

loop :: Int -> Wisdom -> IO ()
loop n wisdom = do
    ln <- (T.toLower . T.filter (not . isPunctuation)) <$> T.getLine
    let gram = foldl (flip NGram.insert) (NGram.empty n) $ makeNGram n $ T.words ln
    T.putStrLn $ hit n wisdom gram
    loop n wisdom

main :: IO ()
main = do
    grams4 <- readNGrams 4
    print "yup?"
    loop 4 grams4

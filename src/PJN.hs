module PJN (
      nGramFromFile
    , makeNGram
    ) where


import Data.Char (isPunctuation)
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Text as T
import qualified Data.List as L
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as TextIO

import           NGram (NGram)
import qualified NGram


nGramFromFile :: FilePath -> Int -> IO (NGram T.Text)
nGramFromFile file n = do
  let eff = TextIO.readFile file
            >-> normalize
            >-> P.map T.words
            >-> P.mapFoldable (makeNGram n)
  TextIO.runSafeT $ runEffect $ P.fold (flip NGram.insert) (NGram.empty n) id eff

{-# SPECIALIZE makeNGram :: Int -> [T.Text] -> [[T.Text]] #-}
makeNGram :: (Ord a, Show a) => Int -> [a] -> [[a]]
makeNGram n l = take (length l - (n - 1)) . map (take n) . L.tails $ l

-- makeNGram' :: Monad m => Int -> Pipe [a] [[a]] m r
-- makeNGram' n = do
--     l <- await


normalize :: Monad m => Pipe T.Text T.Text m r
normalize = Text.filter (not . isPunctuation) >-> Text.toLower

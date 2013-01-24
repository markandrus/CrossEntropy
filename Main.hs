module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Either
import Data.Functor.Identity
import Data.Function
import Data.List
import System.Directory
import System.FilePath.Posix
import System.IO
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

{- Frequency and Entropy Calculations -}

-- | Takes a list of counts.
calcFreqs cs =
  let n = fromIntegral $ sum cs
  -- in  map ((/n) . fromIntegral) cs
  in  map (\c -> fromIntegral c / n) cs

-- | Takes a list of frequencies.
calcLogOfFreqs = map (logBase 2)

-- | Takes a list of frequencies.
calcEntropyOfFreqs ps = (-1) * (sum . zipWith (*) ps $ calcLogOfFreqs ps)

-- | Takes two lists of frequencies.
calcCrossEntropy ps qs = (-1) * (sum . zipWith (*) ps $ calcLogOfFreqs qs)

{- Parsers -}

-- | Parses a comma-separated character/integer pair.
pair = (,) <$> letter                 <* char ','
           <*> (read <$> many1 digit) <* newline

-- | Parses many comma-separated character/integer pairs.
pairs = many1 pair <* eof

{- Helpers -}

isCSV = (== ".csv") . takeExtension

getCSVs = filter isCSV <$> getDirectoryContents "./input"

parseCSVs = mapM $ \filename -> do
  contents <- openFile ("./input/" ++ filename) ReadMode >>= hGetContents
  case parse pairs filename contents of
    Left  error  -> fail $ show error
    Right parsed -> return parsed

writeFreqCSVs filenames languages =
  forM_ (zip filenames languages) $ \(filename, pairs) -> do
    let name     = dropExtension filename
        freqs    = calcFreqs $ map snd pairs
        logFreqs = calcLogOfFreqs freqs
    handle <- openFile ("./output/" ++ name ++ ".freqs.csv") WriteMode
    hPutStrLn handle "\"Letter\",\"Frequency\",\"Log Frequency\""
    forM_ (map fst pairs `zip` freqs `zip` logFreqs) $ \((pairs, freq), logFreq) ->
      hPutStrLn handle $  pairs         : ","
                       ++ show freq    ++ ","
                       ++ show logFreq
    hClose handle

writeCrossEntropyCSVs filenames languages =
  forM_ (permutations $ zip filenames languages) $ \(pairs@((filename,l):_)) -> do
    let name           = dropExtension filename
        freqs          = calcFreqs $ map snd l
        entropy        = calcEntropyOfFreqs freqs
        crossEntropies = map (calcCrossEntropy freqs . calcFreqs . map snd . snd) pairs
        deltas         = map (\c -> c-entropy) crossEntropies
        result         = sortBy (compare `on` fst)
                       $ zip (map fst pairs) (map show deltas)
    handle <- openFile ("./output/" ++ name ++ ".cross-entropies.csv") WriteMode
    hPutStrLn handle . unlines $ map (\(a,b) -> dropExtension a ++ "," ++ b) result
    hClose handle

{- Main -}

main = do
  filenames <- getCSVs
  languages <- parseCSVs filenames
  writeFreqCSVs          filenames languages
  writeCrossEntropyCSVs  filenames languages

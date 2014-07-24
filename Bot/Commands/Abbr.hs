module Bot.Commands.Str

where

import Data.List.Split
import Data.List.Utils

import Bot.Abbr.Words

findWord [] x = x
findWord (w:ws) x
    | x == fst w = snd w
    | otherwise  = findWord ws x

replaceAbbr = join " " . map (findWord abbr) . words

hasAbbr = any ((Nothing /=) . flip lookup abbr) . words

repl (x:y:[]) = y
repl (x:ws)   = x

s = repl . splitOn "/"

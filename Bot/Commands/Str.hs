module Bot.Commands.Str

where

import Data.List
import Data.List.Split
import Data.List.Utils

import Bot.Abbr.Words

--
-- Check word for abbreviation and replace
--
findWord :: Eq a => [(a, a)] -> a -> a
findWord [] x = x
findWord (w:ws) x
    | x == fst w = snd w
    | otherwise  = findWord ws x

--
-- Find and replace abbreviations in string
--
replaceAbbr :: String -> [Char]
replaceAbbr = join " " . map (findWord abbr) . words

--
-- Check if string has any abbreviation
--
hasAbbr :: String -> Bool
hasAbbr = any ((Nothing /=) . flip lookup abbr) . words

--
-- Prepare parts for replacement
--
parts :: [[Char]] -> ([Char], [Char])
parts (x:[])   = (x, "")
parts (x:y:xs) = (x, y)

--
-- Split string by "/"
--
split' :: [Char] -> ([Char], [Char])
split' = parts . splitOn "/"

--
-- Get a sed format string "x/y" and replace x with y in target string
--
subst :: [Char] -> [Char] -> [Char]
subst sub orig
    | orig == ""             = ""
    | sub  == ""             = orig
    | from == ""             = orig
    | (from, to) == ("", "") = orig
    | isPrefixOf from orig   = to ++ subst sub (drop (length from) orig)
    | otherwise              = h  ++ subst sub t
    where
        (from, to) = split' sub
        (h, t)     = splitAt 1 orig

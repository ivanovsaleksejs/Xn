module Bot.Commands.Str where

import Control.Monad.RWS (get)

import Data.List
import Data.List.Split
import Data.List.Utils

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods
import Bot.General
import Bot.Commands.History
import Bot.Abbr.Words

-- Check word for abbreviation and replace
findWord :: Eq a => [(a, a)] -> a -> a
findWord [] x = x
findWord (w:ws) x
    | x == fst w = snd w
    | otherwise  = findWord ws x

-- Find and replace abbreviations in string
replaceAbbr :: String -> [Char]
replaceAbbr = join " " . map (findWord abbr) . words

-- Check if string has any abbreviation
hasAbbr :: String -> Bool
hasAbbr = any ((Nothing /=) . flip lookup abbr) . words

-- Prepare parts for replacement
parts :: [[Char]] -> ([Char], [Char])
parts (x:[])   = (x, "")
parts (x:y:xs) = (x, y)

-- Split string by "/"
split' :: [Char] -> ([Char], [Char])
split' = parts . splitOn "/"

-- Get a sed format string "x/y" and replace x with y in target string
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

-- Get sender's last message that is not s/ command
lastmsg :: (String -> Bool) -> String -> MessageStack -> String -> String
lastmsg check a stack p
    | length f == 0 = ""
    | otherwise     = snd $ head f
    where
        f  = multiFilter [senderMsgs, valid, hasPattern] stack
        senderMsgs = (a == "" ||) . (a==) . sender . snd
        valid      = check . snd
        hasPattern = (p == "" ||) . isInfixOf from . clean . snd
        (from, _)  = split' p

-- Send a substituted message
substitute :: String -> Net ()
substitute s = do
    stack <- get
    let
        tgt    = target s
        author = sender s
        pattn  = drop 2 $ clean s
        last   = clean $ lastmsg (not . isPrefixOf "s/" . clean) author stack pattn
    privmsg tgt $ addSender author ++ " " ++ subst pattn last

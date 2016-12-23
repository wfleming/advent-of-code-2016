module D14Lib where

import Debug.Trace
import Data.Maybe
import Data.List
import Util

{- | find the first triplet char in a string, if once exists -}
rep3Char :: String -> Maybe Char
rep3Char (c1 : c2 : c3 : t)
    | c1 == c2 && c2 == c3 = Just c1
    | otherwise = rep3Char (c2 : c3 : t)
rep3Char _ = Nothing

{- | determine if a given char repeats in a string 5 times consecutively -}
rep5Char :: Char -> String -> Bool
rep5Char target (c1 : c2 : c3 : c4 : c5 : t)
    | c1 == c2 && c2 == c3 && c3 == c4 && c4 == c5 && target == c1 = True
    | otherwise = rep5Char target (c2 : c3 : c4 : c5 : t)
rep5Char _ _ = False

buildHashes :: [Int] -> String -> [(Int, String)]
buildHashes idxs salt = map (\i -> (i, md5 $ salt ++ show i)) idxs

-- determines if hash at head of list is a valid key
isKey :: [(Int, String)] -> Bool
isKey ((_, h) : t) = isJust targetChar && any hasTarget5 (take 1000 t)
  where
    targetChar = trace ("looking for 3char in " ++ h ++ ": " ++ (show $ rep3Char h)) $ rep3Char h
    hasTarget5 = trace "DEBUG hasTarget5" $ rep5Char (fromMaybe (error "wtf") targetChar) . snd
isKey _ = False

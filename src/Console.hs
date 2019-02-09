module Console
  ( prompt
  , defPrompt
  , promptMaybe
  , nonEmptyPrompt
  , passwordPrompt
  , boolPrompt
  , defBoolPrompt
  ) where

import           Control.Exception (bracket_)
import           Data.Char         (isSpace, toLower)
import           Data.List         (dropWhileEnd)
import           Data.Maybe        (fromMaybe)
import           Lib
import           System.IO         (hFlush, hGetEcho, hSetEcho, stdin, stdout)
import           Text.Read         (readMaybe)

_prompt :: Bool -> String -> IO String
_prompt echo prefix = do
  putStr prefix
  putStr "> "
  hFlush stdout
  withEcho echo getLine

prompt :: String -> IO String
prompt = _prompt True

passwordPrompt :: String -> IO String
passwordPrompt prefix = _prompt False prefix <* putStrLn ""

defStrPrompt :: String -> String -> IO String
defStrPrompt prefix defaultVal = promptMaybe (prefix ++ " [" ++ defaultVal ++ "]") |> fromMaybe defaultVal

defPrompt :: (Show x, Read x) => String -> x -> IO x
defPrompt prefix defaultVal = readUntilSuccess $ defStrPrompt prefix (show defaultVal)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

promptMaybe :: String -> IO (Maybe String)
promptMaybe prefix = prompt prefix |> trim |> optIt
  where
    optIt str =
      if null str
        then Nothing
        else Just str

nonEmptyPrompt :: String -> IO String
nonEmptyPrompt prefix = promptMaybe prefix >>= retryIfNeeded
  where
    retryIfNeeded strOpt =
      case strOpt of
        Nothing -> nonEmptyPrompt prefix
        Just s  -> return s

truthy :: String -> String
truthy str =
  let truthyLower str
        | null str = str
        | head str == 'y' || str == "true" = show True
        | head str == 'n' || str == "false" = show False
        | otherwise = str
   in truthyLower $ map toLower str

_readUntilSuccess modif getter = do
  inp <- getter
  let out = fmap modif readMaybe inp
  case out of
    Nothing -> readUntilSuccess getter
    Just a  -> return a

readUntilSuccess :: (Read a) => IO String -> IO a
readUntilSuccess = readIshUntilSuccess id

readIshUntilSuccess :: (Read a) => (String -> String) -> IO String -> IO a
readIshUntilSuccess changeInput getter = do
  inp <- getter
  let out = readMaybe $ changeInput inp
  case out of
    Nothing -> readIshUntilSuccess changeInput getter
    Just a  -> return a

boolPrompt :: String -> IO Bool
boolPrompt prefix = readIshUntilSuccess truthy $ nonEmptyPrompt prefix

defBoolPrompt :: String -> Bool -> IO Bool
defBoolPrompt prefix defaultVal = readIshUntilSuccess truthyOrBlank $ prompt newPrefix
  where
    newPrefix =
      prefix ++
      " " ++
      if defaultVal
        then "[Y/n]"
        else "[y/N]"
    truthyOrBlank str
      | null str = show defaultVal
      | otherwise = truthy str

withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

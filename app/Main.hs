module Main where

import qualified Console
import           Control.Exception  (catch, throwIO)
import           Control.Monad      (forM_, unless, when)
import           Data.Version       (showVersion)
import           Lib
import           Paths_blegh        (version)
import           System.Environment
import           System.IO.Error    (isEOFError)

main :: IO ()
main = catch mainActions (\e -> unless (isEOFError e) $ throwIO e)

{-
main = interact (unlines . map printIsPalindrome . lines)

palindrome str = filtered == reverse filtered
  where
    filtered = filter isAlpha $ map toLower str

printIsPalindrome str =
  if palindrome str
    then "yes!"
    else "no!"
-}
mainActions :: IO ()
mainActions = do
  getProgName >>= putStrLn
  putStrLn $ showVersion version
  putStrLn "Arguments:"
  getArgs >>= print
  putStrLn ""
  putStrLn "Hello"
  inp <- Console.prompt "echo"
  putStr "You typed: "
  putStrLn inp
  inp <- Console.passwordPrompt "pw"
  putStr "password is: "
  putStrLn inp
  runPi

runPi :: IO ()
runPi = do
  doit <- Console.defBoolPrompt "Run Pi ratio code" True
  when doit $ do
    inp <- Console.defPrompt "Count down from" 50
    forM_ (ratiosToStrs (sortedGoodRatiosFromMax inp)) putStrLn

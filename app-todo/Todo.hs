module Main where

import           Control.Monad      (unless)
import qualified Data.HashMap.Lazy  as Map
import           Data.Version       (showVersion)
import           Lib
import           Paths_blegh        (version)
import           System.Directory   (doesFileExist, removeFile, renameFile)
import           System.Environment
import           System.FilePath    (takeDirectory)
import           System.IO          (hClose, hPutStr, openTempFile)

main :: IO ()
main = do
  putStr "todo v"
  putStrLn $ showVersion version
  putStrLn ""
  (cmd:args) <- getArgs
  let Just meth = Map.lookup cmd dispatch
  meth args

dispatch = Map.fromList [("add", add), ("view", view), ("remove", remove), ("bump", bump)]

add [fileName, todoItem] = do
  exists <- doesFileExist fileName
  unless exists (writeFile fileName "")
  rewriteFilePure fileName (++ [todoItem])

todoEntries fileName = do
  contents <- readFile fileName
  return $ lines contents

view [fileName] = do
  entries <- todoEntries fileName
  let newEntries = zipWith (\i line -> show i ++ " - " ++ line) [1 ..] entries
  mapM_ putStrLn newEntries

rewriteFilePure fileName entryChanger = rewriteFile fileName $ return . entryChanger

rewriteFile fileName entryChanger = do
  entries <- todoEntries fileName
  newEntries <- entryChanger entries
  (fName, fHandle) <- openTempFile (takeDirectory fileName) "todo_tmp"
  hPutStr fHandle $ unlines newEntries
  hClose fHandle
  removeFile fileName
  renameFile fName fileName

extractEntry idx entries =
  let line = entries !! idx
      (a, b) = splitAt idx entries
      newEntries = a ++ drop 1 b
   in (line, newEntries)

removeEntry idx entries =
  let (line, newEntries) = extractEntry idx entries
   in do putStrLn $ "Removing " ++ line
         return newEntries

remove [fileName, entryNum] = do
  idx <- readIO entryNum |> pred
  rewriteFile fileName $ removeEntry idx

doBump idx entries =
  let (line, newEntries) = extractEntry idx entries
   in return (line : newEntries)

bump [fileName, entryNum] = do
  idx <- readIO entryNum |> pred
  rewriteFile fileName $ doBump idx

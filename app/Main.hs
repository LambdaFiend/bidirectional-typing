module Main where

import Lexer
import Parser
import Syntax
import Display
import Helper
import Typing

import Data.Char
import Data.List
import System.Directory
import System.Console.ANSI

main :: IO ()
main = do
  path <- readFile "config.txt"
  let path' = words $ map (\x -> if x == '=' then ' ' else x) path
  isValid <- (\ws -> case ws of [x, y] -> handlePath y >>= \b -> return b; _ -> return False) path'
  if isValid
    then do
      txt <- readFile $ (\ws -> case ws of [x, y] -> y; _ -> error "Main: there was a serious problem with the filepath") path'
      let cfg = words txt
      term <- getTermFromAST txt
      case term of
        Left e -> return ()
        Right term' -> do
          printTerm term'
          putStrLn ""
          printType $ synth' term'
          return ()
    else return ()

handlePath :: FilePath -> IO Bool
handlePath path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      readFile path
      return True
    else do
      putStrRed "Path leads nowhere\n"
      return False

putStrYellow :: String -> IO ()
putStrYellow s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr s
  setSGR [Reset]

putStrRed :: String -> IO ()
putStrRed s = do
  setSGR [SetColor Foreground Vivid Red]
  putStr s
  setSGR [Reset]

putStrGreen :: String -> IO ()
putStrGreen s = do
  setSGR [SetColor Foreground Vivid Green]
  putStr s
  setSGR [Reset]

getTokens :: String -> IO [Token]
getTokens txt = return $ alexScanTokens txt

getAST :: String -> IO (Either String TermNode)
getAST txt = do
  tok <- getTokens txt
  let tokErr = filter (\x -> case x of Token _ (ERROR e) -> True; _ -> False) tok
  case tokErr of
    (x:xs) -> return $ Left $ (\(Token fi (ERROR e)) -> e ++ showFileInfo fi) $ x
    [] -> return $ parser tok

getTermFromAST :: String -> IO (Either String TermNode)
getTermFromAST txt = do
  ast <- getAST txt
  case ast of
    Left e -> do
      putStrRed $ e ++ "\n"
      return $ Left ""
    Right ast' -> return $ Right $ genIndex' ast'

printTerm :: TermNode -> IO ()
printTerm t = do
  let display = findDisplayErrors' $ showTerm' t
  case display of
    Left e -> do
      putStrYellow "There was a "
      putStrRed "term display error:\n"
      putStrLn e
      putStrRed "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
    Right s -> do
      putStrYellow "Showing the "
      putStrGreen "term:\n"
      putStrRed "> "
      putStrLn s

printType :: Type -> IO ()
printType ty = do
  let errs = findTypeErrors' ty
  if errs /= ""
    then do
      putStrYellow "There was a "
      putStrRed "type error:\n"
      putStrLn errs 
      putStrRed "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
    else do
      putStrYellow "Showing its "
      putStrGreen "type:\n"
      putStrRed "> "
      putStrLn $ showType' ty

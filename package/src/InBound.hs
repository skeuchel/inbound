
import Data.List
import Data.List.Split

import System.Directory
import System.Environment
import Text.Printf
import Text.Parsec.String
import Text.Show.Pretty

import InBound.Parser
import InBound.Syntax
import InBound.AG
import InBound.CopyRules

import qualified Ag.Syntax as Ag
import qualified Ag.AG as Ag
import qualified Ag.Pretty.Common as Ag

moduleNameFromFilePath :: FilePath -> String
moduleNameFromFilePath fp = bn
  where fn = last $ splitOn "/" fp
        bn = if isSuffixOf ".inbound" fn
                then take (length fn - length ".inbound") fn
                else fn

main :: IO ()
main = do
  args <- getArgs
  case args of
   [fp] -> do
     ex <- doesFileExist fp
     if ex
       then do
         let moduleName = moduleNameFromFilePath fp
         result <- parseFromFile (pSpecification moduleName) fp
         case result of
           Left err   -> print err
           Right spec -> process spec
       else usage
   _ -> usage
  return ()

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ printf "Usage: %s <filename>" prog

process :: Specification -> IO ()
process spec =
  do
    -- putStrLn "Original spec:"
    -- putStrLn (ppShow spec)
    -- putStrLn ""
    --
    -- putStrLn "Completed spec:"
    -- putStrLn (ppShow (completion spec))
    -- putStrLn ""

    -- putStrLn "Elaborated spec:"
    -- putStrLn (ppShow (elaborateSpec spec))
    -- putStrLn ""

    Ag.putDoc (Ag.ppSpecification (elaborateSpec (completion spec)))
    putStrLn ""

-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import Bio.GenbankParser
import System.Directory
import Control.Monad    
import Data.Either
import Data.Either.Unwrap
    
main = do
  args <- getArgs
  let input_file = (head args)
  let output_file = (last args)
                                      
  -- read Clustal outputfile
  --input_file_content <- readFile input_file                       
  parsedinput <- readGenbankGeneric input_file 
  print parsedinput

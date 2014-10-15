-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import System.IO
import Data.List
import Bio.GenbankTools
import Bio.GenbankParser  
import Data.Either
    
main = do
  args <- getArgs
  let input_file = (head args)
  let output_file = (last args)
                                      
  -- read Clustal outputfile
  parsedinput <- readGenbank input_file
  print parsedinput

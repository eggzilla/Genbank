-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import Bio.GenbankTools
import Bio.GenbankParser  

main :: IO ()
main = do
  args <- getArgs
  let input_file = (head args)
                                      
  -- read Clustal outputfile
  parsedinput <- readGenbank input_file
  print parsedinput

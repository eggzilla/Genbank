-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import Biobase.Genbank.Tools
import Biobase.Genbank.Import

main :: IO ()
main = do
  args <- getArgs
  let input_file = (head args)
                                      
  -- read Clustal outputfile
  parsedinput <- readGenbank input_file
  print parsedinput

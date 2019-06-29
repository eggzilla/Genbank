-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import Biobase.Genbank.Tools
import Biobase.Genbank.Import
import Biobase.Genbank.Export
import Biobase.GFF3.Export
import Data.Either.Unwrap

main :: IO ()
main = do
  args <- getArgs
  let input_file = (head args)
  -- read Clustal outputfile
  parsedInput <- readGenbank input_file
  if isRight parsedInput
    then do
      let rightInput = (fromRight parsedInput)
      let rawheader = L.unpack (accession rightInput)
      let header = ">" + rawheader + "\n"
      let gbkseqdata = show origin rightInput
      let upperCaseSeq = map toUpper gbkseqdata
      let seqLines = 
      print gbkseqdata
    else (print (fromLeft parsedInput))

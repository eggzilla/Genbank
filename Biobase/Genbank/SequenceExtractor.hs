-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import Biobase.Genbank.Tools
import Biobase.Genbank.Import
import Biobase.Genbank.Export
import Biobase.GFF3.Export
import Data.Either.Unwrap
import Biobase.Fasta.Strict
import qualified Biobase.Types.BioSequence as BS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
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
      let gbkheader = BS.SequenceIdentifier (B.pack rawheader)
      let gbkseqdata = (origin rightInput)
      let gbkfasta = Fasta gbkheader gbkseqdata
      putStr (B.unpack (fastaToByteString 80 gbkfasta))
    else (print (fromLeft parsedInput))

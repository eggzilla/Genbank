{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Extract sequence from genbank file

module Main where
    
import System.Console.CmdArgs
import Biobase.Genbank.Tools
import Biobase.Genbank.Import
import Data.Either.Unwrap
import Biobase.Fasta.Strict
import qualified Biobase.Types.BioSequence as BS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Paths_Genbank (version) 
import Data.Version (showVersion)

data Options = Options
  { inputFilePath :: String,
    outputFilePath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { inputFilePath = def &= name "i" &= help "Path to input fasta file",
    outputFilePath = def &= name "o" &= help "Path to output file"
  } &= summary ("Genbank sequence extractor " ++ genbankVersion) &= help "Florian Eggenhofer - 2019-2020" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  parsedInput <- readGenbank inputFilePath
  if isRight parsedInput
    then do
      let rightInput = (fromRight parsedInput)
      let rawheader = L.unpack (accession rightInput)
      let gbkheader = BS.SequenceIdentifier (B.pack rawheader)
      let gbkseqdata = (origin rightInput)
      let gbkfasta = Fasta gbkheader gbkseqdata
      writeFile outputFilePath (B.unpack (fastaToByteString 80 gbkfasta))
    else (print (fromLeft parsedInput))

genbankVersion :: String
genbankVersion = showVersion Paths_Genbank.version
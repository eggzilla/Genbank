{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Parser test script
--   read from file and directly print parsing output

module Main where

import System.Console.CmdArgs
import qualified Biobase.Genbank.Import as BGI
import Biobase.Genbank.Export
import Biobase.GFF3.Export()
import Data.Either.Unwrap
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
  } &= summary ("Genbank converter " ++ genbankVersion) &= help "Florian Eggenhofer - 2019-2020" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  parsedInput <- BGI.readGenbank inputFilePath
  if isRight parsedInput
    then do
      let gffoutput = show $ genbankToGFF3 (fromRight parsedInput)
      writeFile outputFilePath gffoutput
    else (print (fromLeft parsedInput))

genbankVersion :: String
genbankVersion = showVersion Paths_Genbank.version
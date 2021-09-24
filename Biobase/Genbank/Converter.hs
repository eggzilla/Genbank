{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Convert genbank to GFF3 format

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
    inputAccession :: String,
    inputKeyAttribute :: String,
    outputFilePath :: String
  } deriving (Show,Data,Typeable)

options :: Options
options = Options
  { inputFilePath = def &= name "i" &= help "Path to input gbk file",
    inputAccession = def &= name "a" &= help "Accession to use in the output file",
    inputKeyAttribute = "gene" &= name "k" &= help "Attribute to use to construct feature hierachy e.g. gene or locus_tag, default: gene",
    outputFilePath = def &= name "o" &= help "Path to output file"
  } &= summary ("Genbank converter " ++ genbankVersion) &= help "Florian Eggenhofer - 2019-2020" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  parsedInput <- BGI.readGenbankHierachicalFeatures inputKeyAttribute inputFilePath
  if isRight parsedInput
    then do
      let gffoutput = show $ genbankFeaturesToGFF3 inputAccession (fromRight parsedInput)
      writeFile outputFilePath gffoutput
    else (print (fromLeft parsedInput))

genbankVersion :: String
genbankVersion = showVersion Paths_Genbank.version

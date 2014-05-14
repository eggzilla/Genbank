-- | Parse Genebank format

module Bio.GenebankParser (
                       parseGenebank,
                       readGenebankParser,
                       module Bio.GenebankParserData
                      ) where

import Bio.GenebankParserData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
import Data.List
import Data.Maybe

-- | Parse the input as Genbank datatype
genParserGenbank :: GenParser Char st Genbank
genParserGenbank = do
  string "LOCUS"
  many1 space
  locus <- many1 (noneOf " ")
  many1 space
  length <- many1 (noneOf " ")
  string " bp"
  many1 space
  moleculeType <- many1 (noneOf " ")
  many1 space
  circular <- many1 (noneOf " ")
  many1 space
  division <- many1 (noneOf " ")
  many1 space
  creationDate <- many1 (noneOf "\n")
  newline
  string "DEFINITION"
  many1 space
  definition <- many1 (noneOf "\n")
  newline
  string "ACCESSION"
  many1 space
  accession <- many1 (noneOf "\n")
  newline
  string "VERSION"
  many1 space
  version <- many1 (noneOf " ")
  many1 space
  geneIdentifier <- many1 (noneOf "\n")
  newline
  string "DBLINK"
  many1 space
  dblink <- many1 (noneOf "\n")
  newline
  string "KEYWORDS"
  many1 space
  keywords <- many1 (noneOf "\n")
  newline
  string "SOURCE"
  many1 space
  source <- many1 (noneOf "\n")
  many1 space
  string "ORGANISM"
  many1 space
  organism <- many1 (noneOf "\n")
  newline
  many1 space
  lineage <- manyTill (string "REFERENCE")
  references <- many1 genParseReference
  string "COMMENT"
  many1 space
  lineage <- manyTill (string "FEATURES")
  features <- genParseFeatures
  string "CONTIG"
  many1 space
  contig <- many1 (noneOf "\n")
  string "ORIGIN"
  many1 space
  newline
  origin <- many1 parseOriginSlice
  string "//"
  eof  
  return $ Genbank locus length moleculeType circular division creationDate definition accession version geneIdentifier dblink keywords source organism lineage references comment features contig origin 

-- | Parse the input as Reference datatype
genParseReference :: GenParser Char st Reference
genParseReference = do
  string "REFERENCE"
  many1 space
  index <- many1 (noneOf " ")
  many1 space
  string "(bases"
  many1 space 
  baseFrom <- many1 (noneOf " ")
  many1 space
  string "to"
  many1 space
  baseTo  <- many1 (noneOf " ")
  string ")"
  newline
  many1 space
  string "AUTHORS"
  many1 space
  authors <- manyTill (string "TITLE")
  string "TITLE"
  many1 space
  title <- manyTill (string "JOURNAL")
  string "JOURNAL"
  journal <- choice [manyTill (string "REFERENCE")),(manyTill (string "FEATURES")]
  return $ Reference (readInt index) (readInt baseFrom) (readInt baseTo) authors title journal Nothing Nothing --pubmedId remark 

genParseFeatures :: GenParser Char st Features
genParseFeatures = do
  string "FEATURES"
  many1 space
  string "Location/Qualifiers"
  many1 space
  string "source"
  many1 space
  sourceCoordinates <- genParseCoordinates
  many1 space
  string "/organism=\""
  organism <- many1 (noneOf "\"")
  string "\""
  newline
  many1 space
  string "/mol_type=\""
  organism <- many1 (noneOf "\"")
  string "\""
  newline
  many1 space
  string "/strain=\""
  organism <- many1 (noneOf "\"")
  string "\""
  newline
  many1 space
  string "/db_xref=\""
  organism <- many1 (noneOf "\"")
  string "\""
  newline
  genes <- many1 genParseGenes
  return Features $ sourceCoordinates sourceOrganism sourceMoleculeType sourceStrain sourceDbXref genes

genParseCoordinates :: GenParser Char st Coordinates
  complement <- optionMaybe (string "complement(")
  coordinateFrom <- many1 (noneOf ".")
  many1 (oneOf ".><")
  coordinateTo <- many1 (noneOf " )")
  optional string ")"
  newline
  return Coordinates $ (readInt coordinateFrom) (readInt coordinateTo) (isComplement complement)
  
isComplement :: Maybe String -> Bool
isComplement string
  | (isJust string) = True
  | otherwise = False

-- | 
parseGenbank input = parse genParserClustalw2Alignment "genParserClustalw2Alignment" input

-- |                      
readGenbank :: String -> IO (Either ParseError Genbank)          
readGenbank filePath = parseFromFile genParserGenbank filePath

-- auxiliary functions
readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

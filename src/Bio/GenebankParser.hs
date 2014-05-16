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
import Data.List.Split 
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
  references <- many1 genParserReference
  string "COMMENT"
  many1 space
  lineage <- manyTill (string "FEATURES")
  features <- genParserFeatures
  string "CONTIG"
  many1 space
  contig <- many1 (noneOf "\n")
  string "ORIGIN"
  many1 space
  newline
  origin <- many1 parserOriginSlice
  string "//"
  eof  
  return $ Genbank locus length moleculeType circular division creationDate definition accession version geneIdentifier dblink keywords source organism lineage references comment features contig origin 

-- | Parse the input as Reference datatype
genParserReference :: GenParser Char st Reference
genParserReference = do
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

genParserFeatures :: GenParser Char st Features
genParserFeatures = do
  string "FEATURES"
  many1 space
  string "Location/Qualifiers"
  many1 space
  string "source"
  many1 space
  sourceCoordinates <- genParseCoordinates
  sourceOrganism <- parseStringField "organism"
  sourceMoleculeType <- parseStringField "mol_type"
  sourceStrain <- parseStringField "strain"
  geneDbXref <- many1 genParseDbXRef
  newline
  genes <- many1 genParseFeature
  return Features $ sourceCoordinates sourceOrganism sourceMoleculeType sourceStrain sourceDbXref genes

genParserFeature :: GenParser Char st Feature
genParserFeature = do
  feature <- choice [genParseRepeatRegion,genParseGene]
  return feature

genParserRepeatRegion :: GenParser Char st RepeatRegion
genParserRepeatRegion = do
  many1 space
  string "repeat_region"
  many1 space
  repeatCoordinates <- genParseCoordinates
  repeatNote <- parseStringField "note"
  return RepeatRegion repeatCoordinates repeatNote

genParserGene :: GenParser Char st Gene
genParserGene = do
  many1 space
  string "gene"
  many1 space
  geneCoordinates <- genParseCoordinates
  geneName <- parseStringField "gene"
  locusTag <- parseStringField "locus_tag"
  geneSynonym <- parseStringField "gene_synonym"
  geneDbXref <- many1 genParseDbXRef
  subFeatures <- many1 genParseSubFeature
  return Gene geneCoordinates geneName locusTag (splitOn ";" geneSynonym) geneDbXref subFeatures

genParserSubFeature :: GenParser Char st SubFeature
genParserSubFeature = do
  subFeature <- choice [genParseCDS,genParseMiscFeature,genParseNcRNA,genParseMobileElement]
  return subFeature

genParserCDS :: GenParser Char st CDS
genParserCDS = do
  many1 space
  string "CDS"
  many1 space
  cdsCoordinates <- genParseCoordinates
  cdsGeneName <- parseStringField "gene"
  cdsLocusTag <- parseStringField "locus_tag"
  cdsGeneSynonym <- many1 parseStringField "gene_synonym"
  ecNumber <- many1 (parseStringField "EC_number")
  cdsFunction  <- many1 (parseStringField "function")
  experiment <- many1 (parseStringField "experiment")
  cdsGOterms <- many1 genParseGOterm
  cdsNote <- parseStringField "note"
  codonStart <- parseIntField "codon_start"
  translationTable <- parseIntField "transl_table"
  cdsProduct <- parseStringField "product"
  proteinId <- parseStringField "protein_id"
  geneDbXref <- many1 genParseDbXRef
  translation <- parseStringField "translation"
  return CDS cdsCoordinates cdsGeneName cdsLocusTag (splitOn ";" cdsGeneSynonym) (splitOn ";" cdsFunction) 

genParseGOterm :: GenParser Char st GOterm
genParseGOterm = do
  many1 space
  string "/GO_=\""
  goType <- many1 (noneOf "=")
  string "=\""
  goId <- many1 (noneOf "-")
  string "-"
  goName <- many1 (noneOf "\"")
  string "\""
  newline
  return GOterm $ goType goId goName

genParseDbXRef :: GenParser Char st DbXRef
genParseDbXRef = do
  many1 space
  string "/db_xref=\""
  db <- many1 (noneOf ":")
  string ":"
  ref <- many1 (noneOf "\"")
  string "\""
  newline
  return DbXRef $ db ref

genParseCoordinates :: GenParser Char st Coordinates
genParseCoordinates = do
  complement <- optionMaybe (string "complement(")
  coordinateFrom <- many1 (noneOf ".")
  many1 (oneOf ".><")
  coordinateTo <- many1 (noneOf " )")
  optional string ")"
  newline
  return Coordinates $ (readInt coordinateFrom) (readInt coordinateTo) (isComplement complement)
  
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

-- | Parse a field containing a String         
parseStringField :: String -> GenParser Char st String
parseStringField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=\"")
  string <- many1 (noneOf "\"")
  string "\""
  newline
  return $ string    

-- | Parse a field containing a Int          
parseIntField :: String -> GenParser Char st Int
parseIntField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=")
  int <- many1 (noneOf "\n")
  newline
  return $ (readInt int)

isComplement :: Maybe String -> Bool
isComplement string
  | (isJust string) = True
  | otherwise = False

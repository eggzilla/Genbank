-- | Parse Genebank format

module Bio.GenbankParser (
                       parseGenebank,
                       readGenebankParser,
                       module Bio.GenbankParserData
                      ) where

import Bio.GenbankParserData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
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
  origin <- many1 genParserOriginSlice
  string "//"
  eof  
  return $ Genbank locus length moleculeType circular division creationDate definition accession version geneIdentifier dblink keywords source organism lineage references comment features contig origin 

-- | Parse the input as OriginSlice datatype
genParserOriginSlice :: GenParser Char st OriginSlice
genParserOriginSlice = do
  many1 space
  index <- many1 (noneOf " ")
  many1 space
  journal <- many1 (noneOf "\n")
  newline
  return $ OriginSlice 

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
  journal <- choice [(manyTill (string "REFERENCE")),(manyTill (string "FEATURES"))]
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
  genes <- many1 genParserFeature
  return Features $ sourceCoordinates sourceOrganism sourceMoleculeType sourceStrain sourceDbXref genes

genParserFeature :: GenParser Char st Feature
genParserFeature = do
  feature <- choice [genParserRepeatRegion,genParserGene]
  return feature

genParserRepeatRegion :: GenParser Char st Feature
genParserRepeatRegion = do
  many1 space
  string "repeat_region"
  many1 space
  repeatCoordinates <- genParseCoordinates
  repeatNote <- parseStringField "note"
  return RepeatRegion repeatCoordinates repeatNote

genParserGene :: GenParser Char st Feature
genParserGene = do
  many1 space
  string "gene"
  many1 space
  geneCoordinates <- genParseCoordinates
  geneName <- parseStringField "gene"
  locusTag <- parseStringField "locus_tag"
  geneSynonym <- parseStringField "gene_synonym"
  geneDbXref <- many1 genParseDbXRef
  subFeatures <- many1 genParserSubFeature
  return Gene geneCoordinates geneName locusTag (splitOn ";" geneSynonym) geneDbXref subFeatures

genParserSubFeature :: GenParser Char st SubFeature
genParserSubFeature = do
  subFeature <- choice [genParserCDS,genParserMiscFeature,genParserNcRNA,genParserMobileElement]
  return subFeature

genParserCDS :: GenParser Char st SubFeature
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

genParserMiscFeature :: GenParser Char st SubFeature
genParserMiscFeature = do
  many1 space
  string "misc_feature"
  many1 space
  miscCoordinates <- genParseCoordinatesSet
  miscGeneName <- parseStringField "gene"
  miscLocusTag <- parseStringField "locus_tag"
  miscGeneSynonym <- many1 parseStringField "gene_synonym"
  miscNote <- parseStringField "note"
  miscDbXref <- many1 genParseDbXRef
  return MiscFeature miscCoordinates miscGeneName miscLocusTag miscGeneSynonym miscNote miscDbXref

genParseCoordinatesSet :: GenParser Char st [Coordinates]
genParseCoordinatesSet = do
  complement <- optionMaybe (string "complement(")
  optional string "order("
  coordinates <- many1 genParseCoordinates
  optional string ")"
  optional string ")"
  newline
  return [Coordinates] $ (setComplement complement coordinates)

setComplement :: String -> [Coordinates] -> [Coordinates]
setComplement complementString coordinates = coordinatesWithComplement
  where complementBool = isComplement complementString
        updateCoordinate complementBool coordinate= coordinate { complement = complementBool }
        coordinatesWithComplement = map (updateCoordinate complementBool) coordinates

genParserNcRNA  :: GenParser Char st SubFeature
genParserNcRNA = do
  many1 space
  string "ncRNA"
  many1 space
  ncRNACoordinates <- genParseCoordinates
  ncRNAGeneName <- parseStringField "gene"
  ncRNALocusTag <- parseStringField "locus_tag"
  ncRNAGeneSynonym <- many1 parseStringField "gene_synonym"
  ncRNAClass <- parseStringField "ncRNA_class"
  ncRNAProduct <- parseStringField "product"
  ncRNANote <- parseStringField "note"
  ncRNADbXref <- many1 genParseDbXRef
  return NcRNA ncRNACoordinates ncRNAGene ncRNALocusTag ncRNAGeneSynonym ncRNAClass ncRNAProduct ncRNANote ncRNADbXref

genParserMobileElement:: GenParser Char st SubFeature
genParserMobileElement = do
  many1 space
  string "mobile_element"
  many1 space
  mobileElementCoordinates <- genParseCoordinates
  mobileType <- parseStringField "mobile_element_type"
  return MobileElement 

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
  optional string ","
  optional string ")"
  newline
  return Coordinates $ (readInt coordinateFrom) (readInt coordinateTo) (isComplement complement)
  
-- | 
parseGenbank input = parse genParserGenbank "genParserGenbank" input

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

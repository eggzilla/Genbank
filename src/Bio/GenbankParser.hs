-- | Parse Genebank format

module Bio.GenbankParser (
                       parseGenbank,
                       readGenbank,
                       parseGenbankExplicit,
                       readGenbankExplicit,
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
import Bio.Core.Sequence
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------
--Generic parsing functions:

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
  definition <- genParserField "DEFINITION" "ACCESSION"
  accession <- genParserField "ACCESSION" "VERSION"
  string "VERSION"
  many1 space
  version <- many1 (noneOf " ")
  many1 space
  geneIdentifier <- many1 (noneOf "\n")
  newline
  dblink <- genParserField "DBLINK" "KEYWORDS"
  keywords <- genParserField "KEYWORDS" "SOURCE"
  source <- genParserField "SOURCE" "ORGANISM"
  organism <- genParserField "ORGANISM" "REFERENCE"
  references <- many1 genParserReference
  comment <- genParserField "COMMENT" "FEATURES"
  string "FEATURES"
  many1 space
  string "Location/Qualifiers"
  newline
  features <- many genParserFeature
  contig <- optionMaybe (try (genParserField "CONTIG" "ORIGIN"))
  string "ORIGIN"
  many (string " ")
  newline
  origin <- many1 genParserOriginSequence
  string "//"
  newline
  return $ Genbank (L.pack locus) (readInt length) (L.pack moleculeType) (L.pack circular) (L.pack division) (L.pack creationDate) (L.pack definition) (L.pack accession) (L.pack version) (L.pack geneIdentifier) (L.pack dblink) (L.pack keywords) (L.pack source)  (L.pack organism) references (L.pack comment) features contig (origintoSeqData origin) 

genParserFeature :: GenParser Char st Feature
genParserFeature = do
  string "     "
  featureType <- choice [(try (string "gene")) , (try (string "repeat_region")), (try (string "source"))]
  many1 space
  genericFeatureCoordinates <- genParserCoordinates
  attibutes <- many (try genParserAttributes)
  geneDbXref <- many (try genParseDbXRef)
  subFeatures <- many (try genParserSubFeature) 
  (choice [(try geneAhead), (try repeatAhead), (try (lookAhead (string "CONTIG"))), (try (lookAhead (string "ORIGIN")))])
  return $ Feature (L.pack featureType) genericFeatureCoordinates attibutes geneDbXref subFeatures

genParserAttributes :: GenParser Char st Attribute
genParserAttributes = choice [(try genParserAttribute), (try genParseGOattribute), (try genParserFlagAttribute)]

genParserAttribute :: GenParser Char st Attribute
genParserAttribute = do
  many1 space
  string "/"
  fieldName <- many1 (noneOf "=")
  string "=\""
  stringField <- many1 (noneOf "\"")
  string "\""
  newline
  return $ Field (L.pack fieldName) (L.pack stringField)

genParserSubFeature :: GenParser Char st SubFeature
genParserSubFeature = do
  string "     "
  subFeatureType <- many1 (noneOf " ")
  many1 space
  subFeatureCoordinates <- choice [(genParserCoordinatesSet "join"), (genParserCoordinatesSet "order")]
  attibutes <- many (try genParserAttributes)
  geneDbXref <- many (try genParseDbXRef)
  subFeatureTranslation <- optionMaybe (try (parseStringField "translation"))
  return $ SubFeature (L.pack subFeatureType) subFeatureCoordinates attibutes geneDbXref (translationtoSeqData subFeatureTranslation)

genParseGOattribute :: GenParser Char st Attribute
genParseGOattribute = do
  many1 space
  string "/GO_"
  goType <- many1 (noneOf "=")
  string "=\""
  goId <- many1 (noneOf "-")
  string "-"
  goName <- many1 (noneOf "\"")
  string "\""
  newline
  return $ GOattribute (L.pack goType) (L.pack goId) (L.pack goName)

genParserFlagAttribute :: GenParser Char st Attribute
genParserFlagAttribute = do
  many1 space
  string "/"
  flagType <- many1 (noneOf "\n")
  newline
  return $ Flag (L.pack flagType)

-- | 
parseGenbank input = parse genParserGenbank "genParserGenbank" input

-- |                      
readGenbank :: String -> IO (Either ParseError Genbank)          
readGenbank filePath = parseFromFile genParserGenbank filePath


--------------------------------------------------
--Explicit parsing functions:

-- | Parse the input as Genbank datatype
genParserGenbankExplicit :: GenParser Char st GenbankExplicit
genParserGenbankExplicit = do
  string "LOCUS"
  many1 space
  elocus <- many1 (noneOf " ")
  many1 space
  elength <- many1 (noneOf " ")
  string " bp"
  many1 space
  emoleculeType <- many1 (noneOf " ")
  many1 space
  ecircular <- many1 (noneOf " ")
  many1 space
  edivision <- many1 (noneOf " ")
  many1 space
  ecreationDate <- many1 (noneOf "\n")
  newline
  edefinition <- genParserField "DEFINITION" "ACCESSION"
  eaccession <- genParserField "ACCESSION" "VERSION"
  string "VERSION"
  many1 space
  eversion <- many1 (noneOf " ")
  many1 space
  egeneIdentifier <- many1 (noneOf "\n")
  newline
  edblink <- genParserField "DBLINK" "KEYWORDS"
  ekeywords <- genParserField "KEYWORDS" "SOURCE"
  esource <- genParserField "SOURCE" "ORGANISM"
  eorganism <- genParserField "ORGANISM" "REFERENCE"
  ereferences <- many1 genParserReference
  ecomment <- genParserField "COMMENT" "FEATURES"
  efeatures <- genParserFeaturesExplicit
  econtig <- optionMaybe (try (genParserField "CONTIG" "ORIGIN"))
  string "ORIGIN"
  many (string " ")
  newline
  eorigin <- many1 genParserOriginSequence
  string "//"
  newline
  return $ GenbankExplicit elocus (readInt elength) emoleculeType ecircular edivision ecreationDate edefinition eaccession eversion egeneIdentifier edblink ekeywords esource eorganism ereferences ecomment efeatures econtig (origintoSeqData eorigin) 

genParserField :: String -> String -> GenParser Char st String
genParserField fieldStart fieldEnd = do 
  string fieldStart
  many1 space
  manyTill anyChar (try (lookAhead (string fieldEnd)))
                 
-- | Parse the input as OriginSlice datatype
genParserOriginSequence :: GenParser Char st String
genParserOriginSequence = do
  many1 space
  many1 (noneOf " ")
  space
  originSequence <- many1 (noneOf "\n")
  newline
  return $ originSequence
 
-- | Parse the input as OriginSlice datatype
genParserOriginSlice :: GenParser Char st OriginSlice
genParserOriginSlice = do
  many1 space
  originIndex <- many1 (noneOf " ")
  space
  originSequence <- many1 (noneOf "\n")
  newline
  return $ OriginSlice (readInt originIndex) originSequence

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
  baseTo  <- many1 (noneOf ")")
  string ")"
  newline
  many1 space
  authors <- choice [(genParserField "AUTHORS" "TITLE"), (genParserField "CONSRTM" "TITLE")]
  title <- genParserField "TITLE" "JOURNAL"
  journal <- choice [(try (genParserField "JOURNAL" "REFERENCE")), (genParserField "JOURNAL" "COMMENT")]
  return $ Reference (readInt index) (readInt baseFrom) (readInt baseTo) authors title journal Nothing Nothing --pubmedId remark 

genParserFeaturesExplicit :: GenParser Char st FeaturesExplicit
genParserFeaturesExplicit = do
  string "FEATURES"
  many1 space
  string "Location/Qualifiers"
  newline
  many1 space
  string "source"
  many1 space
  sourceCoordinates <- genParserCoordinates
  sourceOrganism <- parseStringField "organism"
  sourceMoleculeType <- parseStringField "mol_type"
  sourceStrain <- optionMaybe (try (parseStringField "strain"))
  sourceSubStrain <- optionMaybe (try (parseStringField "sub_strain"))
  sourceSeroVar <- optionMaybe (try (parseStringField "serovar"))
  sourceIsolationSource <- optionMaybe (try (parseStringField "isolation_source"))
  sourceSubSpecies  <- optionMaybe (try (parseStringField "sub_species"))
  sourceDbXref <- many1 (try genParseDbXRef)
  sourceCollectionDate <- optionMaybe (try (parseStringField "collection_date"))
  egenes <- many (try genParserFeatureExplicit)
  return $ FeaturesExplicit sourceCoordinates sourceOrganism sourceMoleculeType sourceStrain sourceSubStrain sourceSeroVar sourceIsolationSource sourceSubSpecies sourceDbXref sourceCollectionDate egenes

genParserFeatureExplicit :: GenParser Char st FeatureExplicit
genParserFeatureExplicit = do
  feature <- choice [(try genParserGene), (try genParserRepeatRegion)]
  return feature

genParserRepeatRegion :: GenParser Char st FeatureExplicit
genParserRepeatRegion = do
  string "     repeat_region"
  many1 space
  repeatCoordinates <- genParserCoordinates
  repeatNote <- optionMaybe (parseStringField "note")
  return $ RepeatRegion repeatCoordinates repeatNote

genParserGene :: GenParser Char st FeatureExplicit
genParserGene = do
  string "     gene"
  many1 space
  geneCoordinates <- (genParserCoordinatesSet "join")
  geneName <- optionMaybe (try (parseStringField "gene"))
  locusTag <- optionMaybe (try (parseStringField "locus_tag"))
  oldLocusTag <- optionMaybe (try (parseStringField "old_locus_tag"))
  geneSynonym <-  optionMaybe (try (parseStringField "gene_synonym"))
  geneNote <- optionMaybe (try (parseStringField "note"))
  genePseudo <- optionMaybe (try (parseFlag "pseudo"))
  geneDbXref <- many (try genParseDbXRef)
  subFeatures <- many (genParserSubFeatureExplicit) 
  (choice [(try geneAhead), (try repeatAhead), (try (lookAhead (string "CONTIG"))), (try (lookAhead (string "ORIGIN")))])
  return $ Gene geneCoordinates geneName locusTag oldLocusTag geneSynonym geneNote (isJust genePseudo) geneDbXref subFeatures

parseFlag :: String -> GenParser Char st Char
parseFlag flagString = do
  many1 space
  flag <- string ("/" ++ flagString)
  newline

geneAhead = do
  lookAhead (string "     gene")

repeatAhead= do
  lookAhead (string "     repeat")

genParserSubFeatureExplicit :: GenParser Char st SubFeatureExplicit
genParserSubFeatureExplicit = do
  subFeature <- choice [(try genParserMiscFeature),(try genParserNcRNA),(try genParserMobileElement),(try genParserCDS),(try genParserSTS), (try genParsertRNA), (try genParserRRNA), (try genParsertmRNA), (try genParserRepOrigin)]
  return subFeature

genParserSTS :: GenParser Char st SubFeatureExplicit
genParserSTS = do
  string "     STS"
  many1 space
  stsCoordinates <- genParserCoordinates
  stsGeneName <- optionMaybe (try (parseStringField "gene"))
  stsLocusTag <- optionMaybe (try (parseStringField "locus_tag"))
  stsGeneSynonym <- optionMaybe (try (parseStringField "gene_synonym"))
  standardName <- optionMaybe (try (parseStringField "standard_name"))
  stsDbXref <- many1 (try genParseDbXRef)
  return $ STS stsCoordinates stsGeneName stsLocusTag stsGeneSynonym standardName stsDbXref

genParsertRNA :: GenParser Char st SubFeatureExplicit
genParsertRNA = do
  string "     tRNA"
  many1 space
  tRNACoordinates <- genParserCoordinates
  tRNAGeneName <- parseStringField "gene"
  tRNALocusTag <- parseStringField "locus_tag"
  tRNAGeneSynonym <- parseStringField "gene_synonym"
  tRNAProduct <- parseStringField "product"
  tRNAPseudo <- optionMaybe (try (parseFlag "pseudo"))
  tRNANote <- optionMaybe (try (parseStringField "note"))
  tRNADbXref <- many1 (try genParseDbXRef)
  return $ TRNA tRNACoordinates tRNAGeneName tRNALocusTag tRNAGeneSynonym tRNAProduct tRNANote (isJust tRNAPseudo) tRNADbXref 

genParsertmRNA :: GenParser Char st SubFeatureExplicit
genParsertmRNA = do
  string "     tmRNA"
  many1 space
  tmRNACoordinates <- genParserCoordinates
  tmRNAGeneName <- parseStringField "gene"
  tmRNALocusTag <- parseStringField "locus_tag"
  tmRNAGeneSynonym <- parseStringField "gene_synonym"
  tmRNAProduct <- optionMaybe (try (parseStringField "product"))
  tmRNANote <- optionMaybe (try (parseStringField "note"))
  tmRNAFunction  <- many (try (parseStringField "function"))
  tmRNAPseudo <- optionMaybe (try (parseFlag "pseudo"))
  tmRNADbXref <- many1 (try genParseDbXRef)
  return $ TMRNA tmRNACoordinates tmRNAGeneName tmRNALocusTag tmRNAGeneSynonym tmRNAProduct tmRNANote tmRNAFunction (isJust tmRNAPseudo) tmRNADbXref 

genParserRRNA :: GenParser Char st SubFeatureExplicit
genParserRRNA = do
  string "     rRNA"
  many1 space
  rRNACoordinates <- genParserCoordinates
  rRNAGeneName <- parseStringField "gene"
  rRNALocusTag <- parseStringField "locus_tag"
  rRNAGeneSynonym <- parseStringField "gene_synonym"
  rRNAProduct <- parseStringField "product"
  rRNADbXref <- many1 (try genParseDbXRef)
  return $ RRNA rRNACoordinates rRNAGeneName rRNALocusTag rRNAGeneSynonym rRNAProduct rRNADbXref 

genParserCDS :: GenParser Char st SubFeatureExplicit
genParserCDS = do
  string "     CDS"
  many1 space
  cdsCoordinates <- (genParserCoordinatesSet "join")
  cdsGeneName <- parseStringField "gene"
  cdsLocusTag <- parseStringField "locus_tag"
  cdsOldLocusTag <- optionMaybe (try (parseStringField "old_locus_tag"))
  cdsGeneSynonym <- parseStringField "gene_synonym"
  ecNumber <- many (try (parseStringField "EC_number"))
  cdsFunction  <- many (try (parseStringField "function"))
  experiment <- many (try (parseStringField "experiment"))
  cdsRibosomalSlippage <- optionMaybe (try (parseFlag "ribosomal_slippage"))
  cdsGOterms <- many (try genParseGOterm)
  cdsNote <- optionMaybe (try (parseStringField "note"))
  cdsPseudo <- optionMaybe (try (parseFlag "pseudo"))
  codonStart <- parseIntField "codon_start"
  translationExcept <- optionMaybe (try (parseStringBracketField "transl_except"))
  translationTable <- parseIntField "transl_table"
  cdsProduct <- optionMaybe (try (parseStringField "product"))
  proteinId <- optionMaybe (try (parseStringField "protein_id"))
  geneDbXref <- many1 (try genParseDbXRef)
  translation <- optionMaybe (try (parseStringField "translation"))
  return $ CDS cdsCoordinates cdsGeneName cdsLocusTag cdsOldLocusTag (splitOn ";" cdsGeneSynonym) ecNumber cdsFunction experiment (isJust cdsRibosomalSlippage) cdsGOterms cdsNote (isJust cdsPseudo) codonStart translationExcept translationTable cdsProduct proteinId geneDbXref (translationtoSeqData translation)

origintoSeqData :: [String] -> SeqData
origintoSeqData originInput = SeqData $ (L.pack (filter (\nuc -> (nuc /= ('\n') && (nuc /= (' ')))) (concat originInput)))

translationtoSeqData :: Maybe String -> Maybe SeqData
translationtoSeqData translationInput 
  | (isJust translationInput) = Just (SeqData $ (L.pack (filter (\aminoacid -> (aminoacid /=  '\n') && (aminoacid /=  ' ') ) (fromJust translationInput))))
  | otherwise = Nothing 

genParserMiscFeature :: GenParser Char st SubFeatureExplicit
genParserMiscFeature = do
  string "     misc_feature"
  many1 space
  miscCoordinates <- choice [(try (genParserCoordinatesSet "order")), (try (genParserCoordinatesSet "join"))]
  miscGeneName <- optionMaybe (try (parseStringField "gene"))
  miscLocusTag <- optionMaybe (try (parseStringField "locus_tag"))
  miscGeneSynonym <- optionMaybe (try (parseStringField "gene_synonym"))
  miscNote <- optionMaybe (try (parseStringField "note"))
  miscDbXref <- many (try genParseDbXRef)
  return $ MiscFeature miscCoordinates miscGeneName miscLocusTag miscGeneSynonym miscNote miscDbXref

genParserNcRNA  :: GenParser Char st SubFeatureExplicit
genParserNcRNA = do
  string "     ncRNA"
  many1 space
  ncRNACoordinates <- genParserCoordinates
  ncRNAGeneName <- parseStringField "gene"
  ncRNALocusTag <- parseStringField "locus_tag"
  ncRNAGeneSynonym <- many1 (try (parseStringField "gene_synonym"))
  ncRNAClass <- parseStringField "ncRNA_class"
  ncRNAProduct <- optionMaybe (try (parseStringField "product"))
  ncRNANote <- optionMaybe (try (parseStringField "note"))
  ncRNAPseudo <- optionMaybe (try (parseFlag "pseudo"))
  ncRNAFunction <- many (try (parseStringField "function"))
  ncRNADbXref <- many1 (try genParseDbXRef)
  return $ NcRNA ncRNACoordinates ncRNAGeneName ncRNALocusTag ncRNAGeneSynonym ncRNAClass ncRNAProduct ncRNANote (isJust ncRNAPseudo) ncRNAFunction ncRNADbXref

genParserMobileElement :: GenParser Char st SubFeatureExplicit
genParserMobileElement = do
  string "     mobile_element"
  many1 space
  mobileElementCoordinates <- genParserCoordinates
  mobileType <- parseStringField "mobile_element_type"
  return $ MobileElement mobileElementCoordinates mobileType

genParserRepOrigin :: GenParser Char st SubFeatureExplicit
genParserRepOrigin = do
  string "     rep_origin"
  many1 space
  repOriginCoordinates <- genParserCoordinates
  repOriginNote <- optionMaybe (try (parseStringField "note"))
  repOriginDbXref <- many1 (try genParseDbXRef)
  return $ REPORIGIN repOriginCoordinates repOriginNote repOriginDbXref

genParserCoordinates :: GenParser Char st Coordinates
genParserCoordinates = do
  coordinates <- choice [(try genParserForwardCoordinates),(try genParserComplementCoordinates)]
  return $ coordinates

genParserCoordinatesSet :: String -> GenParser Char st CoordinateSet
genParserCoordinatesSet prefix = do
  coordinates <- choice [(try (many1 genParserForwardCoordinates)),(try (many1 genParserComplementCoordinates)),(try (genParserForwardPrefix prefix)),(try (genParserComplementPrefix prefix))]
  return $ CoordinateSet coordinates (Just prefix)

-- | Parsing of coordinate lists with prefix e.g. order, join
genParserForwardPrefix :: String -> GenParser Char st [Coordinates]
genParserForwardPrefix prefix = do
  string (prefix ++ "(")
  coordinates <- many1 genParserForwardPrefixCoordinates
  string ")"
  return $ coordinates

genParserForwardPrefixCoordinates :: GenParser Char st Coordinates
genParserForwardPrefixCoordinates = do
  optional (oneOf "><")  
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  optional (oneOf "><")
  coordinateTo <- many1 digit
  optional (choice [(try (string ",\n")),(try (string ","))])
  optional (many1 (string " "))
  return $ Coordinates (readInt coordinateFrom) (readInt coordinateTo) True

-- | Parseing of coordinate complement coordinate lists with prefix
genParserComplementPrefix :: String -> GenParser Char st [Coordinates]
genParserComplementPrefix prefix = do
  string "complement("
  string (prefix ++ "(")
  coordinates <- many1 genParserForwardPrefixCoordinates
  string ")"
  string ")"
  newline
  return $ (setComplement False coordinates)

genParserForwardCoordinates :: GenParser Char st Coordinates
genParserForwardCoordinates = do
  optional (oneOf "><")
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  optional (oneOf "><")
  coordinateTo <- many1 digit
  newline
  return $ Coordinates (readInt coordinateFrom) (readInt coordinateTo) False

genParserComplementCoordinates :: GenParser Char st Coordinates
genParserComplementCoordinates = do
  string "complement("
  optional (oneOf "><")
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  optional (oneOf "><")
  coordinateTo <- many1 digit
  string ")"
  newline
  return $ Coordinates (readInt coordinateFrom) (readInt coordinateTo) True

setComplement :: Bool -> [Coordinates] -> [Coordinates]
setComplement complementBool coordinates = coordinatesWithComplement
  where updateCoordinate complementBool coordinate = coordinate { complement = complementBool }
        coordinatesWithComplement = map (updateCoordinate complementBool) coordinates

genParseGOterm :: GenParser Char st GOterm
genParseGOterm = do
  many1 space
  string "/GO_"
  goType <- many1 (noneOf "=")
  string "=\""
  goId <- many1 (noneOf "-")
  string "-"
  goName <- many1 (noneOf "\"")
  string "\""
  newline
  return $ GOterm goType goId goName

genParseDbXRef :: GenParser Char st DbXRef
genParseDbXRef = do
  many1 space
  string "/db_xref=\""
  db <- many1 (noneOf ":")
  string ":"
  ref <- many1 (noneOf "\"")
  string "\""
  newline
  return $ DbXRef (L.pack db) (L.pack ref)
  
-- | 
parseGenbankExplicit input = parse genParserGenbankExplicit "genParserGenbank" input

-- |                      
readGenbankExplicit :: String -> IO (Either ParseError GenbankExplicit)          
readGenbankExplicit filePath = parseFromFile genParserGenbankExplicit filePath

-- auxiliary functions
readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

parseStringBracketField :: String -> GenParser Char st String
parseStringBracketField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=(")
  stringBracketField <- manyTill anyChar (try (string ")\n"))
  return $ stringBracketField
  
-- | Parse a field containing a String         
parseStringField :: String -> GenParser Char st String
parseStringField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=\"")
  stringField <- many1( noneOf "\"")
  string "\""
  newline
  return $ stringField

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


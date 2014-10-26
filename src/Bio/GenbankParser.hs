-- | Functions for parsing genebank format
module Bio.GenbankParser (
                       parseGenbank,
                       readGenbank,
                       module Bio.GenbankData
                      ) where

import Bio.GenbankData
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
--Parsing functions:

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
  circular <- optionMaybe (try (choice [string "linear",string "circular",string "LINEAR",string "CIRCULAR"]))
  many space
  division <-  many1 (noneOf " ")
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
  dblink <- optionMaybe (try (genParserField "DBLINK" "KEYWORDS"))
  keywords <- genParserField "KEYWORDS" "SOURCE"
  source <- genParserField "SOURCE" "ORGANISM"
  organism <- genParserField "ORGANISM" "REFERENCE"
  references <- many1 genParserReference
  comment <- optionMaybe (try (genParserField "COMMENT" "FEATURES"))
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
  return $ Genbank (L.pack locus) (readInt length) (L.pack moleculeType) (liftM L.pack circular) (L.pack division) (L.pack creationDate) (L.pack definition) (L.pack accession) (L.pack version) (L.pack geneIdentifier) (liftM L.pack dblink) (L.pack keywords) (L.pack source)  (L.pack organism) references (liftM L.pack comment) features contig (origintoSeqData origin) 

-- | Parse a feature
genParserFeature :: GenParser Char st Feature
genParserFeature = do
  string "     "
  featureType <- choice [try (string "gene") , try (string "repeat_region"), try (string "source")]
  many1 space
  genericFeatureCoordinates <- choice [genParserCoordinatesSet "join", genParserCoordinatesSet "order"]
  attibutes <- many (try genParserAttributes)
  subFeatures <- many (try genParserSubFeature) 
  choice [try geneAhead, try repeatAhead, try (lookAhead (string "CONTIG")), try (lookAhead (string "ORIGIN"))]
  return $ Feature (L.pack featureType) genericFeatureCoordinates attibutes subFeatures

-- | Parse a attribute, a GO attribute or a Flag
genParserAttributes :: GenParser Char st Attribute
genParserAttributes = choice [try genParserAttribute, try genParseGOattribute, try genParserFlagAttribute]

-- | Parse a attribute, consisting of attribute designation and value
genParserAttribute :: GenParser Char st Attribute
genParserAttribute = do
  many1 space
  string "/"
  notFollowedBy (string "translation")
  fieldName <- many1 (noneOf "=")
  string "=\""
  stringField <- many1 (noneOf "\"")
  string "\""
  newline
  return $ Field (L.pack fieldName) (L.pack stringField)

-- | Parse a Subfeature
genParserSubFeature :: GenParser Char st SubFeature
genParserSubFeature = do
  string "     "
  notFollowedBy (choice [string "gene", string "repeat_region", string "source"])
  subFeatureType <- many1 (noneOf " ")
  many1 space
  subFeatureCoordinates <- choice [genParserCoordinatesSet "join", genParserCoordinatesSet "order"]
  attibutes <- many (try genParserAttributes)
  subFeatureTranslation <- optionMaybe (try (parseStringField "translation"))
  return $ SubFeature (L.pack subFeatureType) subFeatureCoordinates attibutes (translationtoSeqData subFeatureTranslation)

-- | Parse GO attribute 
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

-- | Parse flag attribute
genParserFlagAttribute :: GenParser Char st Attribute
genParserFlagAttribute = do
  many1 space
  string "/"
  notFollowedBy (string "translation")
  flagType <- many1 (noneOf "\n")
  newline
  return $ Flag (L.pack flagType)

-- | Parse the input as Genbank datatype
parseGenbank :: String -> Either ParseError Genbank
parseGenbank = parse genParserGenbank "genParserGenbank" 

-- | Read the file as Genbank datatype                     
readGenbank :: String -> IO (Either ParseError Genbank)          
readGenbank  = parseFromFile genParserGenbank 

-- | Parse a Field 
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
  return originSequence
 
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
  index <- many1 digit
  many (string " ")
  optional (try (string "(bases"))
  many (string " ")
  baseFrom <- optionMaybe (try (many1 digit))
  many (string " ")
  optional (try (string "to"))
  many (string " ")
  baseTo  <- optionMaybe (try (many1 digit))
  optional (try (string ")"))
  newline
  many1 space
  authors <- choice [genParserField "AUTHORS" "TITLE", genParserField "CONSRTM" "TITLE"]
  title <- genParserField "TITLE" "JOURNAL"
  journal <- choice [try (genParserField "JOURNAL" "REFERENCE"), try (genParserField "JOURNAL" "COMMENT"), try (genParserField "JOURNAL" "FEATURES")]
  return $ Reference (readInt index) (liftM readInt baseFrom) (liftM readInt baseTo) authors title journal Nothing Nothing --pubmedId remark 

parseFlag :: String -> GenParser Char st Char
parseFlag flagString = do
  many1 space
  flag <- string ('/' : flagString)
  newline

geneAhead = lookAhead (string "     gene")

repeatAhead = lookAhead (string "     repeat")

origintoSeqData :: [String] -> SeqData
origintoSeqData originInput = SeqData (L.pack (filter (\nuc -> nuc /= '\n' && (nuc /= ' ')) (concat originInput)))

translationtoSeqData :: Maybe String -> Maybe SeqData
translationtoSeqData translationInput 
  | isJust translationInput = Just (SeqData (L.pack (filter (\aminoacid -> (aminoacid /=  '\n') && (aminoacid /=  ' ') ) (fromJust translationInput))))
  | otherwise = Nothing 

genParserCoordinates :: GenParser Char st Coordinates
genParserCoordinates = do
  coordinates <- choice [try genParserForwardCoordinates, try genParserComplementCoordinates]
  return coordinates

genParserCoordinatesSet :: String -> GenParser Char st CoordinateSet
genParserCoordinatesSet prefix = do
  coordinates <- choice [try (many1 genParserForwardCoordinates), try (many1 genParserComplementCoordinates), try (genParserForwardPrefix prefix), try (genParserComplementPrefix prefix)]
  return $ CoordinateSet coordinates (Just prefix)

-- | Parsing of coordinate lists with prefix e.g. order, join
genParserForwardPrefix :: String -> GenParser Char st [Coordinates]
genParserForwardPrefix prefix = do
  string (prefix ++ "(")
  coordinates <- many1 genParserForwardPrefixCoordinates
  string ")"
  return coordinates

genParserForwardPrefixCoordinates :: GenParser Char st Coordinates
genParserForwardPrefixCoordinates = do
  coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><"))  
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  optional (choice [try (string ",\n"),try (string ",")])
  optional (many1 (string " "))
  return $ Coordinates (readInt coordinateFrom) coordinateFromEqualitySymbol (readInt coordinateTo) coordinateToEqualitySymbol True

-- | Parseing of coordinate complement coordinate lists with prefix
genParserComplementPrefix :: String -> GenParser Char st [Coordinates]
genParserComplementPrefix prefix = do
  string "complement("
  string (prefix ++ "(")
  coordinates <- many1 genParserForwardPrefixCoordinates
  string ")"
  string ")"
  newline
  return (setComplement False coordinates)

genParserForwardCoordinates :: GenParser Char st Coordinates
genParserForwardCoordinates = do
  coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><"))  
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  newline
  return $ Coordinates (readInt coordinateFrom) coordinateFromEqualitySymbol (readInt coordinateTo) coordinateToEqualitySymbol False

genParserComplementCoordinates :: GenParser Char st Coordinates
genParserComplementCoordinates = do
  string "complement("
  coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><")) 
  coordinateFrom <- many1 digit
  optional (oneOf "><")
  string "."
  string "."
  coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  string ")"
  newline
  return $ Coordinates (readInt coordinateFrom) coordinateFromEqualitySymbol (readInt coordinateTo) coordinateToEqualitySymbol True

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
  
---------------------------
-- Auxiliary functions

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

readChar :: String -> Char
readChar = read

parseStringBracketField :: String -> GenParser Char st String
parseStringBracketField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=(")
  stringBracketField <- manyTill anyChar (try (string ")\n"))
  return stringBracketField
  
-- | Parse a field containing a String         
parseStringField :: String -> GenParser Char st String
parseStringField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=\"")
  stringField <- many1( noneOf "\"")
  string "\""
  newline
  return stringField

-- | Parse a field containing a Int          
parseIntField :: String -> GenParser Char st Int
parseIntField fieldname = do
  many1 space
  string ("/" ++ fieldname ++ "=")
  int <- many1 (noneOf "\n")
  newline
  return (readInt int)

isComplement :: Maybe String -> Bool
isComplement string
  | isJust string = True
  | otherwise = False


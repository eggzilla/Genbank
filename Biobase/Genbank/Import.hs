-- | Functions for parsing genebank format
module Biobase.Genbank.Import (
                       parseGenbank,
                       readGenbank,
                       parseGenbankFeatures,
                       readGenbankFeatures,
                       module Biobase.Genbank.Types
                      ) where

import Biobase.Genbank.Types
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as TPP
import Control.Monad
import Data.Maybe
import qualified Biobase.Types.BioSequence as BS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Functor.Identity as DFI

--------------------------------------------------
--Parsing functions:

-- | Parse the input as Genbank datatype
genParserGenbank :: GenParser Char st Genbank
genParserGenbank = do
  _ <- string "LOCUS"
  _ <- many1 space
  _locus <- many1 (noneOf " ")
  _ <- many1 space
  _length <- many1 (noneOf " ")
  _ <- string " bp"
  _ <- many1 space
  _moleculeType <- many1 (noneOf " ")
  _ <- many1 space
  _circular <- optionMaybe (try (choice [string "linear",string "circular",string "LINEAR",string "CIRCULAR"]))
  _ <- many space
  _division <-  many1 (noneOf " ")
  _ <- many1 space
  _creationDate <- many1 (noneOf "\n")
  --newline
  _definition <- genParserField "\nDEFINITION" "\nACCESSION"
  _accession <- genParserField "\nACCESSION" "\nVERSION"
  _ <- string "\nVERSION"
  _ <- many1 space
  _version <- many1 (noneOf " ")
  _ <- many1 space
  _geneIdentifier <- many1 (noneOf "\n")
  --newline
  _dblink <- optionMaybe (try (genParserField "\nDBLINK" "\nKEYWORDS"))
  _keywords <- genParserField "\nKEYWORDS" "\nSOURCE"
  _source <- genParserField "\nSOURCE" "\n  ORGANISM"
  _organism <- genParserField "\n  ORGANISM" "\nREFERENCE"
  _ <- newline
  _references <- many genParserReference
  _comment <- optionMaybe (try (genParserField "COMMENT" "FEATURES"))
  _ <- string "FEATURES"
  _ <- many1 space
  _ <- string "Location/Qualifiers"
  _ <- newline
  _features <- many genParserFeature
  _contig <- optionMaybe (try (genParserField "CONTIG" "ORIGIN"))
  _ <- string "ORIGIN"
  _ <- many (string " ")
  _ <- newline
  _origin <- many1 genParserOriginSequence
  _ <- string "//"
  _ <- newline
  return $ Genbank (L.pack _locus) (readInt _length) (L.pack _moleculeType) (liftM L.pack _circular) (L.pack _division) (L.pack _creationDate) (L.pack _definition) (L.pack _accession) (L.pack _version) (L.pack _geneIdentifier) (liftM L.pack _dblink) (L.pack _keywords) (L.pack _source)  (L.pack _organism) _references (liftM L.pack _comment) _features _contig (origintoSeqData _origin) 

-- | Parse the input as list of GenbankFeature datatype
genParserGenbankFeatures :: GenParser Char st [Feature]
genParserGenbankFeatures = do
  _ <- manyTill anyChar (try (string "FEATURES"))
  _ <- many1 space
  _ <- string "Location/Qualifiers\n"
  _ <- newline
  _features <- many genParserFeature
  _ <- optionMaybe (try (genParserField "CONTIG" "ORIGIN"))
  _ <- string "ORIGIN"
  _ <- many (string " ")
  _ <- newline
  _ <- many1 genParserOriginSequence
  _ <- string "//"
  _ <- newline
  return $ _features

-- | Parse a feature
genParserFeature :: GenParser Char st Feature
genParserFeature = do
  _ <- string "     "
  _featureType <- choice [try (string "gene") , try (string "repeat_region"), try (string "source")]
  _ <- many1 space
  _genericFeatureCoordinates <- choice [genParserCoordinatesSet "join", genParserCoordinatesSet "order"]
  _attibutes <- many (try genParserAttributes)
  _subFeatures <- many (try genParserSubFeature) 
  _ <- choice [try geneAhead, try repeatAhead, try (lookAhead (string "CONTIG")), try (lookAhead (string "ORIGIN"))]
  return $ Feature (L.pack _featureType) _genericFeatureCoordinates _attibutes _subFeatures

-- | Parse a attribute, a GO attribute or a Flag
genParserAttributes :: GenParser Char st Attribute
genParserAttributes = choice [try genParserAttribute, try genParseGOattribute, try genParserFlagAttribute]

-- | Parse a attribute, consisting of attribute designation and value
genParserAttribute :: GenParser Char st Attribute
genParserAttribute = do
  _ <- many1 space
  _ <- string "/"
  _ <- notFollowedBy (string "translation")
  _fieldName <- many1 (noneOf "=")
  _ <- string "=\""
  _stringField <- many1 (noneOf "\"")
  _ <- string "\""
  _ <- newline
  return $ Field (L.pack _fieldName) (L.pack (unwords. words $ (map replaceSeparationChar (filter (\ic -> ic /='\n') _stringField))))

--remove linebreaks and separation chars from attributes
replaceSeparationChar :: Char -> Char
replaceSeparationChar inchar
  | inchar == ';' = ','
  | otherwise = inchar

-- | Parse a Subfeature
genParserSubFeature :: GenParser Char st SubFeature
genParserSubFeature = do
  _ <- string "     "
  _ <- notFollowedBy (choice [string "gene", string "repeat_region", string "source"])
  _subFeatureType <- many1 (noneOf " ")
  _ <- many1 space
  _subFeatureCoordinates <- choice [genParserCoordinatesSet "join", genParserCoordinatesSet "order"]
  _attibutes <- many (try genParserAttributes)
  _subFeatureTranslation <- optionMaybe (try (parseStringField "translation"))
  return $ SubFeature (L.pack _subFeatureType) _subFeatureCoordinates _attibutes (translationtoSeqData _subFeatureTranslation)

-- | Parse GO attribute 
genParseGOattribute :: GenParser Char st Attribute
genParseGOattribute = do
  _ <- many1 space
  _ <- string "/GO_"
  _goType <- many1 (noneOf "=")
  _ <- string "=\""
  _goId <- many1 (noneOf "-")
  _ <- string "-"
  _goName <- many1 (noneOf "\"")
  _ <- string "\""
  _ <- newline
  return $ GOattribute (L.pack _goType) (L.pack _goId) (L.pack _goName)

-- | Parse flag attribute
genParserFlagAttribute :: GenParser Char st Attribute
genParserFlagAttribute = do
  _ <- many1 space
  _ <- string "/"
  _ <- notFollowedBy (string "translation")
  _flagType <- many1 (noneOf "\n")
  _ <- newline
  return $ Flag (L.pack _flagType)

-- | Parse the input as Genbank datatype
parseGenbank :: String -> Either ParseError Genbank
parseGenbank = parse genParserGenbank "genParserGenbank" 

-- | Read the file as Genbank datatype                     
readGenbank :: String -> IO (Either ParseError Genbank)          
readGenbank  = parseFromFile genParserGenbank

-- | Parse the input as Genbank datatype
parseGenbankFeatures :: String -> Either ParseError [Feature]
parseGenbankFeatures = parse genParserGenbankFeatures "genParserGenbank" 

-- | Read the file as Genbank datatype                     
readGenbankFeatures :: String -> IO (Either ParseError [Feature])          
readGenbankFeatures = parseFromFile genParserGenbankFeatures

-- | Parse a Field 
genParserField :: String -> String -> GenParser Char st String
genParserField fieldStart fieldEnd = do 
  _ <- string fieldStart
  _ <- many1 space
  manyTill anyChar (try (lookAhead (string fieldEnd)))
                 
-- | Parse the input as OriginSlice datatype
genParserOriginSequence :: GenParser Char st String
genParserOriginSequence = do
  _ <- many1 space
  _ <- many1 (noneOf " ")
  _ <- space
  _originSequence <- many1 (noneOf "\n")
  _ <- newline
  return _originSequence
 
-- -- | Parse the input as OriginSlice datatype
--genParserOriginSlice :: GenParser Char st OriginSlice
--genParserOriginSlice = do
--  _ <- many1 space
--  _originIndex <- many1 (noneOf " ")
--  _ <- space
--  _originSequence <- many1 (noneOf "\n")
--  _ <- newline
--  return $ OriginSlice (readInt _originIndex) _originSequence

-- | Parse the input as Reference datatype
genParserReference :: GenParser Char st Reference
genParserReference = do
  _ <- string "REFERENCE"
  _ <- many1 space
  _index <- many1 digit
  _ <- many (string " ")
  _ <- optional (try (string "(bases"))
  _ <- many (string " ")
  _baseFrom <- optionMaybe (try (many1 digit))
  _ <- many (string " ")
  _ <- optional (try (string "to"))
  _ <- many (string " ")
  _baseTo  <- optionMaybe (try (many1 digit))
  _ <- optional (try (string ")"))
  _ <- newline
  _ <- many1 space
  _authors <- choice [genParserField "AUTHORS" "TITLE", genParserField "CONSRTM" "TITLE"]
  _title <- genParserField "TITLE" "JOURNAL"
  _journal <- choice [try (genParserField "JOURNAL" "REFERENCE"), try (genParserField "JOURNAL" "COMMENT"), try (genParserField "JOURNAL" "FEATURES")]
  return $ Reference (readInt _index) (liftM readInt _baseFrom) (liftM readInt _baseTo) _authors _title _journal Nothing Nothing --pubmedId remark 

--parseFlag :: String -> GenParser Char st Char
--parseFlag flagString = do
--  _ <- many1 space
--  _flag <- string ('/' : flagString)
--  newline

geneAhead :: TPP.ParsecT [Char] u DFI.Identity String
geneAhead = lookAhead (string "     gene")

repeatAhead :: TPP.ParsecT [Char] u DFI.Identity String
repeatAhead = lookAhead (string "     repeat")

origintoSeqData :: [String] -> BS.BioSequence BS.DNA
origintoSeqData originInput = BS.mkDNAseq (B.pack (filter (\nuc -> nuc /= '\n' && (nuc /= ' ')) (concat originInput)))

translationtoSeqData :: Maybe String -> Maybe (BS.BioSequence BS.AA)
translationtoSeqData translationInput 
  | isJust translationInput = Just (BS.mkAAseq (B.pack (filter (\aminoacid -> (aminoacid /=  '\n') && (aminoacid /=  ' ') ) (fromJust translationInput))))
  | otherwise = Nothing 

--genParserCoordinates :: GenParser Char st Coordinates
--genParserCoordinates = do
--  _coordinates <- choice [try genParserForwardCoordinates, try genParserComplementCoordinates]
--  return _coordinates

genParserCoordinatesSet :: String -> GenParser Char st CoordinateSet
genParserCoordinatesSet prefix = do
  _coordinates <- choice [try (many1 genParserForwardCoordinates), try (many1 genParserComplementCoordinates), try (genParserForwardPrefix prefix), try (genParserComplementPrefix prefix)]
  return $ CoordinateSet _coordinates (Just prefix)

-- | Parsing of coordinate lists with prefix e.g. order, join
genParserForwardPrefix :: String -> GenParser Char st [Coordinates]
genParserForwardPrefix prefix = do
  _ <- string (prefix ++ "(")
  _coordinates <- many1 genParserForwardPrefixCoordinates
  _ <- string ")"
  return _coordinates

genParserForwardPrefixCoordinates :: GenParser Char st Coordinates
genParserForwardPrefixCoordinates = do
  _coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><"))  
  coordinateFrom <- many1 digit
  _ <- optional (oneOf "><")
  _ <- string "."
  _ <- string "."
  _coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  _ <- optional (choice [try (string ",\n"),try (string ",")])
  _ <- optional (many1 (string " "))
  return $ Coordinates (readInt coordinateFrom) _coordinateFromEqualitySymbol (readInt coordinateTo) _coordinateToEqualitySymbol True

-- | Parseing of coordinate complement coordinate lists with prefix
genParserComplementPrefix :: String -> GenParser Char st [Coordinates]
genParserComplementPrefix prefix = do
  _ <- string "complement("
  _ <- string (prefix ++ "(")
  _coordinates <- many1 genParserForwardPrefixCoordinates
  _ <- string ")"
  _ <- string ")"
  _ <- newline
  return (setComplement False _coordinates)

genParserForwardCoordinates :: GenParser Char st Coordinates
genParserForwardCoordinates = do
  _coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><"))  
  coordinateFrom <- many1 digit
  _ <- optional (oneOf "><")
  _ <- string "."
  _ <- string "."
  _coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  _ <- newline
  return $ Coordinates (readInt coordinateFrom) _coordinateFromEqualitySymbol (readInt coordinateTo) _coordinateToEqualitySymbol False

genParserComplementCoordinates :: GenParser Char st Coordinates
genParserComplementCoordinates = do
  _ <- string "complement("
  _coordinateFromEqualitySymbol <- optionMaybe (try (oneOf "><")) 
  coordinateFrom <- many1 digit
  _ <- optional (oneOf "><")
  _ <- string "."
  _ <- string "."
  _coordinateToEqualitySymbol <- optionMaybe (try (oneOf "><"))
  coordinateTo <- many1 digit
  _ <- string ")"
  _ <- newline
  return $ Coordinates (readInt coordinateFrom) _coordinateFromEqualitySymbol (readInt coordinateTo) _coordinateToEqualitySymbol True

setComplement :: Bool -> [Coordinates] -> [Coordinates]
setComplement _complementBool coordinates = coordinatesWithComplement
  where updateCoordinate _complementBool coordinate = coordinate { complement = _complementBool }
        coordinatesWithComplement = map (updateCoordinate _complementBool) coordinates

--genParseGOterm :: GenParser Char st GOterm
--genParseGOterm = do
--  _ <- many1 space
--  _ <- string "/GO_"
--  _goType <- many1 (noneOf "=")
--  _ <- string "=\""
--  _goId <- many1 (noneOf "-")
--  _ <- string "-"
--  _goName <- many1 (noneOf "\"")
--  _ <- string "\""
--  _ <- newline
--  return $ GOterm _goType _goId _goName

--genParseDbXRef :: GenParser Char st DbXRef
--genParseDbXRef = do
--  _ <- many1 space
--  _ <- string "/db_xref=\""
--  db <- many1 (noneOf ":")
--  _ <- string ":"
--  ref <- many1 (noneOf "\"")
--  _ <- string "\""
--  _ <- newline
--  return $ DbXRef (L.pack db) (L.pack ref)
  
---------------------------
-- Auxiliary functions

--readDouble :: String -> Double
--readDouble = read              

readInt :: String -> Int
readInt = read

--readChar :: String -> Char
--readChar = read

--parseStringBracketField :: String -> GenParser Char st String
--parseStringBracketField fieldname = do
--  _ <- many1 space
--  _ <- string ("/" ++ fieldname ++ "=(")
--  stringBracketField <- manyTill anyChar (try (string ")\n"))
--  return stringBracketField
  
-- | Parse a field containing a String         
parseStringField :: String -> GenParser Char st String
parseStringField fieldname = do
  _ <- many1 space
  _ <- string ("/" ++ fieldname ++ "=\"")
  stringField <- many1( noneOf "\"")
  _ <- string "\""
  _ <- newline
  return stringField

-- -- | Parse a field containing a Int          
--parseIntField :: String -> GenParser Char st Int
--parseIntField fieldname = do
--  _ <- many1 space
--  string ("/" ++ fieldname ++ "=")
--  int <- many1 (noneOf "\n")
--  _ <- newline
--  return (readInt int)

--isComplement :: Maybe String -> Bool
--isComplement string
--  | isJust string = True
--  | otherwise = False


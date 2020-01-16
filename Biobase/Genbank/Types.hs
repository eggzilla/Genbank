-- | This module contains data structures for genbank format
--   For more information on genbank consult: <http://www.ncbi.nlm.nih.gov/genbank/>
--   Genbank record sample: <http://www.ncbi.nlm.nih.gov/Sitemap/samplerecord.html>

module Biobase.Genbank.Types where
--import Biobase.Fasta.Strict
import Biobase.Types.BioSequence
import qualified Data.ByteString.Lazy.Char8 as L
--------------------------------------------------
--Generic parser types

-- | Genbank type representing the content of a genbank record
data Genbank = Genbank
  {
    locus :: L.ByteString,
    genbankLength :: Int,
    -- DNA/RNA/Protein
    moleculeType :: L.ByteString,
    circular :: Maybe L.ByteString,
    division :: L.ByteString,
    creationDate:: L.ByteString,
    definition :: L.ByteString,
    accession :: L.ByteString,
    version :: L.ByteString,
    geneIdentifier :: L.ByteString,
    dblink :: Maybe L.ByteString,
    keywords :: L.ByteString,
    source :: L.ByteString,
    organism :: L.ByteString,
    references :: [Reference],
    comment :: Maybe L.ByteString,
    features :: [Feature],
    contig :: Maybe String,
    origin :: BioSequence DNA
  }
  deriving (Show, Eq)

data GenericFeature = GenericFeature {
     gFeatureType :: L.ByteString,
     gFeatureCoordinates :: CoordinateSet,
     gAttributes :: [Attribute],
     gFeatureTranslation :: Maybe (BioSequence AA),
     gSubFeatures :: [SubFeature]
  }
  deriving (Show, Eq)


-- | Genbank Feature - e.g gene, repeat region
data Feature = Feature {
     featureType :: L.ByteString,
     featureCoordinates :: CoordinateSet,
     attributes :: [Attribute],
     subFeatures :: [SubFeature]
  }
  deriving (Show, Eq)

-- | Genbank attribute of feature or subfeature, either a flag field or a GO attribute
data Attribute = Flag {
      flagType :: L.ByteString
  }
  |
  Field {
    fieldType :: L.ByteString,
    fieldValue :: L.ByteString
  }
  |
  GOattribute {
     gotype :: L.ByteString,
     goid :: L.ByteString,
     goname :: L.ByteString
  }
  deriving (Show, Eq)

-- | Genbank subfeature, e.g. CDS, MiscFeature, NcRNA, Mobile Element, STS rRNA, tRNA, tmRNA, reporigin
data SubFeature = SubFeature 
  {  
     subFeatureType :: L.ByteString,
     subFeatureCoordinates :: CoordinateSet,
     subFeatureAttributes :: [Attribute],
     subFeatureTranslation :: Maybe (BioSequence AA)
  }
  deriving (Show, Eq)

-- | Genbank reference associating record with publication
data Reference = Reference
  {
     index :: Int,
     baseFrom :: Maybe Int,
     baseTo :: Maybe Int,
     authors :: String,
     title :: String,
     journal :: String,
     pubmedId :: Maybe String,
     remark :: Maybe String
  }
  deriving (Show, Eq)

-- | Coordinate pair for a nucleotide sequence
data Coordinates = Coordinates
  {
    coordinatesFrom :: Int,
    -- leading smaller or greater than
    coordinateFromEqualitySymbol :: Maybe Char,
    coordinatesTo :: Int,
    -- leading smaller or greater than
    coordinateToEqualitySymbol :: Maybe Char,
    complement :: Bool
  }
  deriving (Show, Eq)

data DbXRef = DbXRef
  {
     db :: L.ByteString,
     ref :: L.ByteString
  }
  deriving (Show, Eq)

-- | Set of coordinates, with type order, join
data CoordinateSet = CoordinateSet
  {
    setCoordinates :: [Coordinates], 
    setType :: Maybe String
  }
  deriving (Show, Eq)

-- | Slices of the nucleotide sequence contained in the Genbank record
data OriginSlice = OriginSlice
  {
     originIndex :: Int,
     originSequence :: String
  }
  deriving (Show, Eq)

-- | Gene Onthology term
data GOterm = GOterm
  {
     goType :: String,
     goId :: String,
     goName :: String
  }
  deriving (Show, Eq)

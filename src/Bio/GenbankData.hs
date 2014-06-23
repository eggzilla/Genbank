-- | This module contains data structures for genbank format
--   For more information on genbank consult: <http://www.ncbi.nlm.nih.gov/genbank/>
--   Genbank record sample: <http://www.ncbi.nlm.nih.gov/Sitemap/samplerecord.html>

module Bio.GenbankData where
import Bio.Core.Sequence
import qualified Data.ByteString.Lazy.Char8 as L
--------------------------------------------------
--Generic parser types

data Genbank = Genbank
  {
    locus :: L.ByteString,
    genbankLength :: Int,
    -- DNA/RNA/Protein
    moleculeType :: L.ByteString,
    circular :: L.ByteString,
    division :: L.ByteString,
    creationDate:: L.ByteString,
    definition :: L.ByteString,
    accession :: L.ByteString,
    version :: L.ByteString,
    geneIdentifier :: L.ByteString,
    dblink :: L.ByteString,
    keywords :: L.ByteString,
    source :: L.ByteString,
    organism :: L.ByteString,
    references :: [Reference],
    comment :: L.ByteString,
    features :: [Feature],
    contig :: Maybe String,
    origin :: SeqData
  }
  deriving (Show, Eq)

data Feature = Feature {
     featureType :: L.ByteString,
     featureCoordinates :: CoordinateSet,
     attributes :: [Attribute],
     subFeatures :: [SubFeature]
  }
  deriving (Show, Eq)

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

data SubFeature = SubFeature 
  {  
     subFeatureType :: L.ByteString,
     subFeatureCoordinates :: CoordinateSet,
     subFeatureAttributes :: [Attribute],
     subFeatureTranslation :: Maybe SeqData
  }
  deriving (Show, Eq)

--------------------------------------------------
--Explicit parser types

-- | 
data GenbankExplicit = GenbankExplicit
  {
    elocus :: String,
    egenbankLength :: Int,
    -- DNA/RNA/Protein
    emoleculeType :: String,
    ecircular :: String,
    edivision :: String,
    ecreationDate:: String,
    edefinition :: String,
    eaccession :: String,
    eversion :: String,
    egeneIdentifier :: String,
    edblink :: String,
    ekeywords :: String,
    esource :: String,
    eorganism :: String,
    ereferences :: [Reference],
    ecomment :: String,
    efeatures :: FeaturesExplicit,
    econtig :: Maybe String,
    eorigin :: SeqData
  }
  deriving (Show, Eq)

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

data FeaturesExplicit = FeaturesExplicit
  {
     sourceCoordinates :: Coordinates,
     sourceOrganism :: String,
     sourceMoleculeType :: String,
     sourceStrain :: Maybe String,
     sourceSubStrain :: Maybe String,
     sourceSerovar :: Maybe String,
     sourceIsolationSource :: Maybe String,
     sourceSubSpecies :: Maybe String,
     sourceDbXref :: [DbXRef],
     sourceCollectionDate :: Maybe String,
     egenes :: [FeatureExplicit] 
  }
  deriving (Show, Eq)

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

data FeatureExplicit = Gene {
     geneCoordinates :: CoordinateSet,
     geneName :: Maybe String,
     locusTag :: Maybe String,
     oldLocusTag :: Maybe String,
     geneSynonym :: Maybe String,
     geneNote :: Maybe String,
     genePseudo :: Bool,
     geneDbXref :: [DbXRef],
     explicitSubFeatures :: [SubFeatureExplicit]
  }
  | RepeatRegion
  {
     repeatCoordinates :: Coordinates,
     repeatNote :: Maybe String
  }
  deriving (Show, Eq)

data DbXRef = DbXRef
  {
     db :: L.ByteString,
     ref :: L.ByteString
  }
  deriving (Show, Eq)

data CoordinateSet = CoordinateSet
  {
    setCoordinates :: [Coordinates], 
    setType :: Maybe String
  }
  deriving (Show, Eq)

data SubFeatureExplicit = CDS 
  {  
     cdsCoordinates :: CoordinateSet,
     cdsGeneName :: String,
     cdsLocusTag :: String,
     cdsOldLocusTag :: Maybe String,
     cdsGeneSynonym :: [String],
     ecNumber :: [String], 
     cdsFunction :: [String],
     experiment :: [String],
     cdsRibosomalSlippage :: Bool,
     cdsGOterms :: [GOterm],
     cdsNote :: Maybe String,
     cdsPseudo :: Bool,
     codonStart :: Int,
     translationExcept :: Maybe String,
     translationTable :: Int,
     cdsProduct :: Maybe String,
     proteinId :: Maybe String,
     cdsDbXref :: [DbXRef],
     translation :: Maybe SeqData
  }
  | MiscFeature
  {
     miscCoordinates :: CoordinateSet,
     miscGeneName :: Maybe String,
     miscLocusTag :: Maybe String,
     miscGeneSynonym :: Maybe String,
     miscNote :: Maybe String,
     miscDbXref :: [DbXRef]
  }
  | NcRNA
  {
     ncRNACoordinates :: Coordinates,
     ncRNAGeneName :: String,
     ncRNALocusTag :: String,
     ncRNAGeneSynonym :: [String],
     ncRNAClass :: String,
     ncRNAProduct :: Maybe String,
     ncRNANote :: Maybe String,
     ncRNAPseudo :: Bool,
     ncRNAFunction :: [String],
     ncRNADbXref :: [DbXRef]
  }
  | MobileElement
  {
     mobileCoordinates :: Coordinates,
     mobileType :: String
  }
  | STS
  {
    stsCoordinates :: Coordinates,
    stsGeneName :: Maybe String,
    stsLocusTag :: Maybe String,
    stsGeneSynonym :: Maybe String,
    standardName :: Maybe String,
    stsDbXref :: [DbXRef]
  }
  | RRNA
  {
    rRNACoordinates :: Coordinates,
    rRNAGeneName :: String,
    rRNALocusTag :: String,
    rRNAGeneSynonym :: String,
    tRNAProduct :: String,  
    rRNADbXref :: [DbXRef]
  }
  | TRNA
  {
    tRNACoordinates :: Coordinates,
    tRNAGeneName :: String,
    tRNALocusTag :: String,
    tRNAGeneSynonym :: String,
    tRNAProduct :: String,  
    tRNANote :: Maybe String,
    tRNAPseudo :: Bool,
    tRNADbXref :: [DbXRef]
  }
  | TMRNA
  {
    tmRNACoordinates :: Coordinates,
    tmRNAGeneName :: String,
    tmRNALocusTag :: String,
    tmRNAGeneSynonym :: String,
    tmRNAProduct :: Maybe String,  
    tmRNANote :: Maybe String,
    tmRNAFunction :: [String],
    tmRNAPseudo :: Bool,
    tmRNADbXref :: [DbXRef]
  }
  | REPORIGIN
  {
    tmRNACoordinates :: Coordinates,
    tmRNANote :: Maybe String,
    tmRNADbXref :: [DbXRef]
  }
  deriving (Show, Eq)

data OriginSlice = OriginSlice
  {
     originIndex :: Int,
     originSequence :: String
  }
  deriving (Show, Eq)

data GOterm = GOterm
  {
     goType :: String,
     goId :: String,
     goName :: String
  }
  deriving (Show, Eq)

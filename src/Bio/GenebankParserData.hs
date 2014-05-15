-- | This module contains data structures for the genbankParser 
--   For more information on genbank consult: <http://www.ncbi.nlm.nih.gov/genbank/>
--   Genbank record sample: <http://www.ncbi.nlm.nih.gov/Sitemap/samplerecord.html>

module Bio.GenbankParserData where
    
-- | 
data Genbank = Genbank
  {
    locus :: String,
    length :: Int,
    -- DNA/RNA/Protein
    moleculeType :: String,
    circular :: String,
    division :: String,
    creationDate:: String,
    definition :: String,
    accession :: String,
    version :: String,
    geneIdentifier :: Int,
    dblink :: String,
    keywords :: String,
    source :: String,
    organism :: String,
    lineage :: String,
    references :: [Reference],
    comment :: String,
    features :: Features,
    contig :: String,
    origin :: [OriginSlice]
  }
  deriving (Show, Eq)

data Reference = Reference
  {
     index :: Int,
     baseFrom :: Int,
     baseTo :: Int,
     authors :: String,
     title :: String,
     journal :: String,
     pubmedId :: Maybe String,
     remark :: Maybe String
  }
  deriving (Show, Eq)

data Features = Features
  {
     sourceCoordinates :: Coordinates,
     sourceOrganism :: String,
     sourceMoleculeType :: String,
     sourceStrain :: String,
     sourceDbXref :: String,
     genes :: [Gene] 
  }
  deriving (Show, Eq)

data Coordinates = Coordinates
  {
    coordinatesFrom :: Int,
    coordinatesTo :: Int,
    complement :: Bool
  }
  deriving (Show, Eq)

data Feature  = Gene | RepeatRegion
  deriving (Show, Eq)

data RepeatRegion = RepeatRegion
  {
     repeatCoordinates :: Coordinates,
     repeatNote :: String
  }
  deriving (Show, Eq)

data DbXRef = DbXRef
  {
     db :: String,
     ref :: String
  }
  deriving (Show, Eq)

data Gene = Gene
  {
     geneCoordinates :: Coordinates,
     geneName :: String,
     locusTag :: String,
     geneSynonym :: [String],
     geneDbXref :: [String],
     subFeatures :: [SubFeature]
  }
  deriving (Show, Eq)

data SubFeature  = CDS | MiscFeature | NcRNA | MobileElement 
  deriving (Show, Eq)

data CDS = CDS
  {
     cdsFrom :: Int,
     cdsTo :: Int,
     cdsComplement :: Bool,
     cdsGene :: String,
     cdsLocusTag :: String,
     cdsGeneSynonym :: [String],
     ecNumber :: Maybe String, 
     cdsFunction :: [String],
     cdsGOterms :: [GOterm]
     cdsNote :: String,
     codonStart :: Int,
     translationTable :: Int,
     cdsProduct :: String,
     proteinId :: String,
     cdsDbXref :: [String],
     translation :: String
  }
  deriving (Show, Eq)

data GOterm = GOterm
  {
     goType :: String,
     goId :: Int,
     goName :: String
  }
  deriving (Show, Eq)

data MiscFeature = MiscFeature
  {
     miscOrder :: [MiscFeatureOrder],
     miscComplement :: Bool,
     miscGene :: String,
     miscLocusTag :: String,
     miscGeneSynonym :: [String],
     miscNote :: String,
     miscDbXref :: [String]
  }
  deriving (Show, Eq)

data MiscFeatureOrder = MiscFeatureOrder
  {
      miscFrom :: Int,
      miscTo :: Int
  }

data NcRNA = NcRNA
  {
     ncRNAFrom :: Int,
     ncRNATo :: Int,
     ncRNAComplement :: Bool,
     ncRNAGene :: String,
     ncRNALocusTag :: String,
     ncRNAGeneSynonym :: [String],
     ncRNAClass :: String,
     ncRNAProduct :: String,
     ncRNADbXref :: [String]
  }
  deriving (Show, Eq)

data MobileElement = MobileElement
  {
     mobileFrom :: Int,
     mobileTo :: Int,
     complement :: Bool,
     mobileType :: String
  }
  deriving (Show, Eq)

data OriginSlice = OriginSlice
  {
     originIndex :: Int,
     originSequence :: String
  }
  deriving (Show, Eq)



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
    geneIdentifier :: String,
    dblink :: String,
    keywords :: String,
    source :: String,
    organism :: String,
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
     sourceStrain :: Maybe String,
     sourceSubStrain :: Maybe String,
     sourceDbXref :: [DbXRef],
     genes :: [Feature] 
  }
  deriving (Show, Eq)

data Coordinates = Coordinates
  {
    coordinatesFrom :: Int,
    coordinatesTo :: Int,
    complement :: Bool
  }
  deriving (Show, Eq)

data Feature = Gene {
     geneCoordinates :: CoordinateSet,
     geneName :: String,
     locusTag :: String,
     geneSynonym :: [String],
     geneNote :: Maybe String,
     genePseudo :: Bool,
     geneDbXref :: [DbXRef],
     subFeatures :: [SubFeature]
  }
  | RepeatRegion
  {
     repeatCoordinates :: Coordinates,
     repeatNote :: Maybe String
  }
  deriving (Show, Eq)

data DbXRef = DbXRef
  {
     db :: String,
     ref :: String
  }
  deriving (Show, Eq)

data CoordinateSet = CoordinateSet
  {
    setCoordinates :: [Coordinates], 
    setType :: Maybe String
  }
  deriving (Show, Eq)

data SubFeature = CDS 
  {  
     cdsCoordinates :: CoordinateSet,
     cdsGeneName :: String,
     cdsLocusTag :: String,
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
     translation :: Maybe String
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

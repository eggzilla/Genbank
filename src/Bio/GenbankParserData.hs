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

data Feature  = Gene {
     geneCoordinates :: Coordinates,
     geneName :: String,
     locusTag :: String,
     geneSynonym :: [String],
     geneDbXref :: [DbXRef],
     subFeatures :: [SubFeature]
  }
  | RepeatRegion
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

data SubFeature  = CDS 
  {
     cdsCoordinates :: Coordinates,
     cdsGeneName :: String,
     cdsLocusTag :: String,
     cdsGeneSynonym :: [String],
     ecNumber :: [String], 
     cdsFunction :: [String],
     experiment :: [String],
     cdsGOterms :: [GOterm],
     cdsNote :: String,
     codonStart :: Int,
     translationTable :: Int,
     cdsProduct :: String,
     proteinId :: String,
     cdsDbXref :: [DbXRef],
     translation :: String
  }
  | MiscFeature
  {
     -- multiple misc features can be annotated in one entry, therefore the coordinate field is a list
     miscCoordinates :: [Coordinates],
     miscGeneName :: String,
     miscLocusTag :: String,
     miscGeneSynonym :: [String],
     miscNote :: String,
     miscDbXref :: [DbXRef]
  }
  | NcRNA
  {
     ncRNACoordinates :: Coordinates,
     ncRNAGene :: String,
     ncRNALocusTag :: String,
     ncRNAGeneSynonym :: [String],
     ncRNAClass :: String,
     ncRNAProduct :: String,
     ncRNADbXref :: [DbXRef]
  }
  | MobileElement
  {
     mobileCoordinates :: Coordinates,
     mobileType :: String
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
     goId :: Int,
     goName :: String
  }
  deriving (Show, Eq)

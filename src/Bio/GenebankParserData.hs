-- | This module contains data structures for the genbankParser 
--   For more information on genbank consult: <>

module Bio.GenbankParserData where
    
-- | 
data Genbank = Genbank
  {
    locus :: String,
    length :: Int,
    -- DNA/RNA/Protein
    moleculeType :: String,
    circular :: String,
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
    features :: [Feature],
    contig :: String,
    origin :: Origin
  }
  deriving (Show, Eq)

data Reference = Reference
  {
     reference :: String
  }
  deriving (Show, Eq)

data Feature = Feature
  {
     feature :: String
  }
  deriving (Show, Eq)

data Origin = Origin
  {
     origin :: String
  }
  deriving (Show, Eq)



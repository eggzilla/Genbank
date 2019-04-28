-- | Functions for exporting of genbank data 
module Biobase.Genbank.Export (
                       genbankToGFF3
                      ) where

import Biobase.Genbank.Types
import Biobase.GFF3.Types
import Data.Maybe
import Bio.Core.Sequence
import Data.Int
import Bio.Sequence.Fasta
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

genbankToGFF3 :: Genbank -> GFF3
genbankToGFF3 genbank = GFF3 (V.fromList convertedGFF3Entries) L.empty
   where convertedGFF3Entries = concatMap (featureToGFF3Entry (accession genbank))(features genbank)

featureToGFF3Entry :: L.ByteString -> Feature -> [GFF3Entry]
featureToGFF3Entry gbkAccession feature = featureGFF3:subFeatureGFF3
  where featureGFF3 = GFF3Entry seqId source (featureType feature) start stop score strand phase fAttributes
        seqId = gbkAccession
        source = L.pack "."
        start = coordinatesFrom . head . setCoordinates . featureCoordinates $ feature
        stop = coordinatesTo . head . setCoordinates . featureCoordinates $ feature
        score = L.pack "."
        strand = if (complement . head . setCoordinates . featureCoordinates $ feature) then '+' else '-'
        phase = L.pack "."
        fAttributes = V.fromList []
        subFeatureGFF3 = map (subFeatureToGFF3Entry gbkAccession) (subFeatures feature)

subFeatureToGFF3Entry :: L.ByteString -> SubFeature -> GFF3Entry
subFeatureToGFF3Entry gbkAccession subFeature = subFeatureGFF3
  where subFeatureGFF3 = GFF3Entry seqId source (subFeatureType subFeature) start stop score strand phase sfAttributes
        seqId = gbkAccession
        source = L.pack "."                 
        start = coordinatesFrom . head . setCoordinates . subFeatureCoordinates $ subFeature
	stop = coordinatesTo . head . setCoordinates . subFeatureCoordinates $ subFeature
        score = L.pack "." 
	strand = if (complement . head . setCoordinates . subFeatureCoordinates $ subFeature) then '+' else '-'
        phase = L.pack "."
        sfAttributes = V.fromList []

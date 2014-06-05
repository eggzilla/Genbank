-- | Parse Genebank format
module Bio.GenbankTools (
                       extractAllFeatureSequences,
                       extractSpecificFeatureSequences,
                       extractSubSequence,
                       module Bio.GenbankData
                      ) where

import Bio.GenbankData
import Control.Monad
import Data.List
import Data.Maybe
import Bio.Core.Sequence
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L

-- |
extractAllFeatureSequences :: Genbank -> [SeqData]
extractAllFeatureSequences genbank = sequences
  where coordinates = map featureCoordinates (features genbank)
        fullSequence = origin genbank
        sequences = map (extractSubSequence fullSequence) coordinates

-- |
extractSpecificFeatureSequences :: String -> Genbank -> [SeqData]
extractSpecificFeatureSequences specificFeature genbank = sequences
  where coordinates = map featureCoordinates (filter (\x -> ((featureType x) == (L.pack specificFeature)))(features genbank))
        fullSequence = origin genbank
        sequences = map (extractSubSequence fullSequence) coordinates
        
extractSubSequence :: SeqData -> Coordinates -> SeqData
extractSubSequence genbankSeq seqCoordinates
  | complement seqCoordinates = (SeqData (revcompl' subsequence))
  | otherwise = SeqData subsequence
  where endTruncatedSequence = L.take (fromIntegral ((coordinatesFrom seqCoordinates) + (coordinatesTo seqCoordinates)):: Int64) (unSD genbankSeq)
        subsequence = L.drop (fromIntegral (coordinatesFrom seqCoordinates) :: Int64) endTruncatedSequence


--The following two functions are copied from Ketil Maldes hackage bio package. -- The same functionality has not been reincluded into the biocore package      
-- | Calculate the reverse complement for SeqData only.
revcompl' :: SeqData -> SeqData
revcompl' = L.map compl . L.reverse

-- | Complement a single character.  I.e. identify the nucleotide it 
--   can hybridize with.  Note that for multiple nucleotides, you usually
--   want the reverse complement (see 'revcompl' for that).
compl :: Char -> Char
compl 'A' = 'T'
compl 'T' = 'A'
compl 'C' = 'G'
compl 'G' = 'C'
compl 'a' = 't'
compl 't' = 'a'
compl 'c' = 'g'
compl 'g' = 'c'
compl  x  =  x

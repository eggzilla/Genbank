-- | Parse Genebank format
module Bio.GenbankTools (
                       extractFeatureSequences,
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

-- | Parse the input as Genbank datatype
extractFeatureSequences :: Genbank -> [SeqData]
extractFeatureSequences genbank = sequences
  where coordinates = map featureCoordinates (features genbank)
        fullSequence = origin genbank
        sequences = map (extractSubSequence fullSequence) coordinates
        
extractSubSequence :: SeqData -> Coordinates -> SeqData
extractSubSequence genbankSeq seqCoordinates
  | complement seqCoordinates = (SeqData (revcompl' subsequence))
  | otherwise = SeqData subsequence
  where endTruncatedSequence = L.take (fromIntegral ((coordinatesFrom seqCoordinates) + (coordinatesTo seqCoordinates)):: Int64) (unSD genbankSeq)
        subsequence = L.drop (fromIntegral (coordinatesFrom seqCoordinates) :: Int64) endTruncatedSequence
        
-- | Calculate the reverse complent for SeqData only.
--revcompl' :: SeqData -> SeqData
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

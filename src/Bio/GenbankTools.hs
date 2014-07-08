-- | Functions for processing of genbank data 
--   Extraction of feature sequences (header,sequencedata) or sequence data
module Bio.GenbankTools (
                       extractAllFeatureSeqData,
                       extractSpecificFeatureSeqData,
                       extractAllFeatureSequence,
                       extractSpecificFeatureSequence,
                       module Bio.GenbankData
                      ) where

import Bio.GenbankData
import Control.Monad
import Data.List
import Data.Maybe
import Bio.Core.Sequence
import Data.Int
import Bio.Sequence.Fasta
import qualified Data.ByteString.Lazy.Char8 as L

-- | Extracts the sequence data of all features in genbank data
extractAllFeatureSeqData :: Genbank -> [SeqData]
extractAllFeatureSeqData genbank = seqdatas
  where coordinates = map featureCoordinates (features genbank)
        fullSequence = origin genbank
        seqdatas = concat (map (extractSeqDataList fullSequence) coordinates)

-- | Extracts the sequence data of the specified feature type from genbank data
extractSpecificFeatureSeqData :: String -> Genbank -> [SeqData]
extractSpecificFeatureSeqData specificFeature genbank = seqdatas
  where coordinates = map featureCoordinates (filter (\x -> ((featureType x) == (L.pack specificFeature)))(features genbank))
        fullSequence = origin genbank
        seqdatas = concat (map (extractSeqDataList fullSequence) coordinates)

-- | Extract sequnce header (locus tag, Genbank ) and sequence data for all features in genbank data
extractAllFeatureSequence :: Genbank -> [Sequence]
extractAllFeatureSequence genbank = sequences
  where currentAccession = L.unpack (locus genbank)
        currentFeatures = features genbank
        fields = [ x | x@(Field {}) <- concat (map attributes currentFeatures)]
        locusTags = map fieldValue (filter (\field -> ((fieldType field) == (L.pack "locus_tag"))) fields)
        currentHeaders = map (\locus_tag -> L.pack ((L.unpack locus_tag) ++ " " ++ currentAccession)) locusTags
        coordinates = map featureCoordinates (features genbank)
        fullSequence = origin genbank
        seqdata = concat (map (extractSeqDataList fullSequence) coordinates)
        sequences = map (\(header,seqdata) -> Seq (SeqLabel header) seqdata Nothing) $ zip currentHeaders seqdata

-- | Extract sequence header (locus identifier, locus tag) and sequence data for a specific feature type in genbank data
extractSpecificFeatureSequence :: String -> Genbank -> [Sequence]
extractSpecificFeatureSequence specificFeature genbank = sequences
  where currentAccession = L.unpack (locus genbank)
        currentFeatures = filter (\x -> ((featureType x) == (L.pack specificFeature)))(features genbank)
        fields = [ x | x@(Field {}) <- concat (map attributes currentFeatures)]
        locusTags = map fieldValue (filter (\field -> ((fieldType field) == (L.pack "locus_tag"))) fields)
        currentHeaders = map (\locus_tag -> L.pack ((L.unpack locus_tag) ++ " " ++ currentAccession)) locusTags
        coordinates = map featureCoordinates currentFeatures
        fullSequence = origin genbank
        seqdata = concat (map (extractSeqDataList fullSequence) coordinates)
        sequences = map (\(header,seqdata) -> Seq (SeqLabel header) seqdata Nothing) $ zip currentHeaders seqdata
                
---------------------------
-- | Extract the seqdata for a coordinateSet from the ORIGIN section of genbank data  
extractSeqDataList :: SeqData -> CoordinateSet -> [SeqData]
extractSeqDataList genbankSeq seqCoordinates
  | isNothing (setType seqCoordinates) = [extractSeqData genbankSeq (head (setCoordinates seqCoordinates))]
  | fromJust (setType seqCoordinates) == "join" = extractJoinSeqData genbankSeq seqCoordinates
  | fromJust (setType seqCoordinates) == "order" = extractOrderSeqData genbankSeq seqCoordinates

-- | Extract sequence data for CoordinateSets of type "join" 
extractJoinSeqData :: SeqData -> CoordinateSet -> [SeqData]
extractJoinSeqData genbankSeq seqCoordinates = joinSequence
  where coordinateList = (setCoordinates seqCoordinates)
        partialSequences = map (extractByteStringFromSeqData genbankSeq) coordinateList
        joinSequence = [SeqData (L.concat partialSequences)]

-- | Extract sequence data for CoordinateSets of type "order"      
extractOrderSeqData :: SeqData -> CoordinateSet -> [SeqData]
extractOrderSeqData fullSeq seqCoordinates = orderSequences
  where coordinateList = (setCoordinates seqCoordinates)
        orderSequences = map (extractSeqData fullSeq) coordinateList 

-- | Extract sequence data for coordinates
extractSeqData :: SeqData -> Coordinates -> SeqData
extractSeqData fullSequence seqCoordinates
  | complement seqCoordinates = (SeqData (revcompl' subsequence))
  | otherwise = SeqData subsequence
  where subsequence = extractByteStringFromSeqData fullSequence seqCoordinates

-- | Extract partial sequence from ORIGIN seqction of genbank data according to provided coordinates
extractByteStringFromSeqData :: SeqData -> Coordinates -> L.ByteString
extractByteStringFromSeqData fullSequence seqCoordinates = substring
  where endTruncatedSequence = L.take (fromIntegral ((coordinatesFrom seqCoordinates) + (coordinatesTo seqCoordinates)):: Int64) (unSD fullSequence)
        substring = L.drop (fromIntegral (coordinatesFrom seqCoordinates) :: Int64) endTruncatedSequence

--The following two functions are copied from Ketil Maldes hackage bio package. -- The same functionality has not been reincluded into the biocore package      
-- | Calculate the reverse complement for SeqData only.
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

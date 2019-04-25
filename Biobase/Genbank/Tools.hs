-- | Functions for processing of genbank data 
-- Extraction of feature sequences (header,sequencedata) or sequence data
module Biobase.Genbank.Tools (
                       extractSpecificFeatureSeqData,
                       extractSpecificFeatureSequence,
                       module Biobase.Genbank.Data
                      ) where

import Biobase.Genbank.Data
import Data.Maybe
import Bio.Core.Sequence
import Data.Int
import Bio.Sequence.Fasta
import qualified Data.ByteString.Lazy.Char8 as L

-- | Extract nucleotide sequence data for all features of specified type, Nothing as specific feature extracts all feature sequence seqdatas
extractSpecificFeatureSeqData :: Maybe String -> Genbank -> [SeqData]
extractSpecificFeatureSeqData specificFeature genbank = seqdatas
  where currentFeatures = getCurrentFeatures specificFeature genbank 
        coordinates = map featureCoordinates currentFeatures
        fullSequence = origin genbank
        seqdatas = concatMap (extractSeqDataList fullSequence) coordinates

-- | Extract header (locus identifier, locus tag) and nucleotide sequence data for all features of specified type, Nothing as specific feature extracts all feature sequences
extractSpecificFeatureSequence :: Maybe String -> Genbank -> [Sequence]
extractSpecificFeatureSequence specificFeature genbank = sequences
  where currentAccession = L.unpack (locus genbank)
        currentFeatures = getCurrentFeatures specificFeature genbank 
        fields = [ x | x@(Field {}) <- concatMap attributes currentFeatures]
        locusTags = map fieldValue (filter (\field -> fieldType field == L.pack "locus_tag") fields)
        currentHeaders = map (\locus_tag -> L.pack (L.unpack locus_tag ++ " " ++ currentAccession)) locusTags
        coordinates = map featureCoordinates currentFeatures
        fullSequence = origin genbank
        seqdata = concatMap (extractSeqDataList fullSequence) coordinates
        sequences = map (\(header,seqdata) -> Seq (SeqLabel header) seqdata Nothing) $ zip currentHeaders seqdata
                
---------------------------
--Auxiliary functions:

-- | Extracts features dependent on provided Feature type, Nothing as specific feature extracts all features
getCurrentFeatures :: Maybe String -> Genbank -> [Feature]  
getCurrentFeatures specificFeature genbank 
  | isNothing specificFeature = features genbank
  | otherwise = filter (\x -> featureType x == L.pack (fromJust specificFeature))(features genbank)

-- | Extract the seqdata for a coordinateSet from the ORIGIN section of genbank data  
extractSeqDataList :: SeqData -> CoordinateSet -> [SeqData]
extractSeqDataList genbankSeq seqCoordinates
  | isNothing (setType seqCoordinates) = [extractSeqData genbankSeq (head (setCoordinates seqCoordinates))]
  | fromJust (setType seqCoordinates) == "join" = extractJoinSeqData genbankSeq seqCoordinates
  | fromJust (setType seqCoordinates) == "order" = extractOrderSeqData genbankSeq seqCoordinates
  | otherwise = []                                                   

-- | Extract sequence data for CoordinateSets of type "join" 
extractJoinSeqData :: SeqData -> CoordinateSet -> [SeqData]
extractJoinSeqData genbankSeq seqCoordinates = joinSequence
  where coordinateList = setCoordinates seqCoordinates
        partialSequences = map (extractByteStringFromSeqData genbankSeq) coordinateList
        joinSequence = [SeqData (L.concat partialSequences)]

-- | Extract sequence data for CoordinateSets of type "order"      
extractOrderSeqData :: SeqData -> CoordinateSet -> [SeqData]
extractOrderSeqData fullSeq seqCoordinates = orderSequences
  where coordinateList = setCoordinates seqCoordinates
        orderSequences = map (extractSeqData fullSeq) coordinateList 

-- | Extracts sequence according to a Coordinates pair
extractSeqData :: SeqData -> Coordinates -> SeqData
extractSeqData fullSequence seqCoordinates
  | complement seqCoordinates = SeqData (revcomplement' subsequence)
  | otherwise = SeqData subsequence
  where subsequence = extractByteStringFromSeqData fullSequence seqCoordinates

-- | Extract partial ByteString from ORIGIN section of genbank data according to provided coordinates
extractByteStringFromSeqData :: SeqData -> Coordinates -> L.ByteString
extractByteStringFromSeqData fullSequence seqCoordinates = substring
  where endTruncatedSequence = L.take (fromIntegral (coordinatesFrom seqCoordinates + coordinatesTo seqCoordinates):: Int64) (unSD fullSequence)
        substring = L.drop (fromIntegral (coordinatesFrom seqCoordinates) :: Int64) endTruncatedSequence

--The following two functions are copied from Ketil Maldes hackage bio package. -- The same functionality has not been reincluded into the biocore package      
-- | Calculate the reverse complement for SeqData only.
revcomplement' :: L.ByteString -> L.ByteString
revcomplement' = L.map complement' . L.reverse

-- | Complement a single character.  I.e. identify the nucleotide it 
--   can hybridize with.  Note that for multiple nucleotides, you usually
--   want the reverse complement (see 'revcompl' for that).
complement' :: Char -> Char
complement' 'A' = 'T'
complement' 'T' = 'A'
complement' 'C' = 'G'
complement' 'G' = 'C'
complement' 'a' = 't'
complement' 't' = 'a'
complement' 'c' = 'g'
complement' 'g' = 'c'
complement'  x  =  x

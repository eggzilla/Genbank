-- | Functions for exporting of genbank data 
module Biobase.Genbank.Export (
                       genbankToGFF3,
                       genbankFeaturesToGFF3,
                       genbankToGTF
                      ) where

import Biobase.Genbank.Types
import Biobase.GFF3.Types
import Biobase.GTF.Types
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector as V
import Data.Maybe
import Data.List
import qualified Data.Char as C


--GTF

genbankToGTF :: Genbank -> GTF
genbankToGTF genbank = GTF (V.fromList convertedGTFEntries) L.empty
   where convertedGTFEntries = concatMap (featureToGTFEntry (accession genbank))(features genbank)

featureToGTFEntry :: L.ByteString -> Feature -> [GTFEntry]
featureToGTFEntry gbkAccession _feature = featureGTF:subFeatureGTF
  where featureGTF = GTFEntry _seqId _source (featureType _feature) _start _stop _score _strand _phase _fAttributes
        _seqId = gbkAccession
        _source = L.pack "."
        _start = coordinatesFrom . head . setCoordinates . featureCoordinates $ _feature
        _stop = coordinatesTo . head . setCoordinates . featureCoordinates $ _feature
        _score = L.pack "."
        _strand = if (complement . head . setCoordinates . featureCoordinates $ _feature) then '-' else '+'
        _phase = L.pack "."
        _fAttributes = V.fromList(map attributeToGTFAttribute (attributes _feature))
        subFeatureGTF = map (subFeatureToGTFEntry gbkAccession) (subFeatures _feature)

attributeToGTFAttribute :: Attribute -> L.ByteString
attributeToGTFAttribute (Flag _flagType) = _flagType
attributeToGTFAttribute (Field _fieldType _fieldValue) = L.concat [(setGeneId _fieldType),L.pack " \"",_fieldValue,L.pack "\""]
attributeToGTFAttribute (GOattribute _gotype _go_id _goname) = L.concat [L.pack "GO-Term \"",_gotype,L.pack ",",_go_id,L.pack ",",_goname,L.pack "\""]

setGeneId :: L.ByteString -> L.ByteString
setGeneId _fieldType
  | (L.unpack _fieldType) == "gene" = L.pack ("gene_id")
  | otherwise = _fieldType

subFeatureToGTFEntry :: L.ByteString -> SubFeature -> GTFEntry
subFeatureToGTFEntry gbkAccession _subFeature = subFeatureGTF
  where subFeatureGTF = GTFEntry _seqId _source (subFeatureType _subFeature) _start _stop _score _strand _phase sfAttributes
        _seqId = gbkAccession
        _source = L.pack "."                 
        _start = coordinatesFrom . head . setCoordinates . subFeatureCoordinates $ _subFeature
        _stop = coordinatesTo . head . setCoordinates . subFeatureCoordinates $ _subFeature
        _score = L.pack "." 
        _strand = if (complement . head . setCoordinates . subFeatureCoordinates $ _subFeature) then '-' else '+'
        _phase = L.pack "."
        sfAttributes = V.fromList(map attributeToGTFAttribute (subFeatureAttributes _subFeature))

-- GFF3

genbankToGFF3 :: Genbank -> GFF3
genbankToGFF3 genbank = GFF3 (V.fromList convertedGFF3Entries) L.empty
   where convertedGFF3Entries = concatMap (featureToGFF3Entry (accession genbank))(features genbank)

genbankFeaturesToGFF3 :: String -> [Feature] -> GFF3
genbankFeaturesToGFF3 inputAccession gfeatures = GFF3 (V.fromList convertedGFF3Entries) L.empty
   where convertedGFF3Entries = concatMap (featureToGFF3Entry (L.pack inputAccession)) gfeatures

featureToGFF3Entry :: L.ByteString -> Feature -> [GFF3Entry]
featureToGFF3Entry gbkAccession _feature = featureGFF3:subFeatureGFF3
  where featureGFF3 = GFF3Entry _seqId _source (featureType _feature) _start _stop _score _strand _phase fAttributes
        _seqId = gbkAccession
        _source = L.pack "."
        (_start,_stop,_strand) = genbankFeatureCoordinatesToGFF3FeatureCoordinates (featureCoordinates _feature)
        _score = L.pack "."
        _phase = L.pack "."
        idAttribute = setId (attributes _feature) (featureType _feature) _start _stop
        fAttributes = V.fromList(map attributeToGFF3Attribute (idAttribute:attributes _feature))
        subFeatureGFF3 = concatMap (subFeatureToGFF3Entries gbkAccession) (subFeatures _feature)

setId :: [Attribute] -> L.ByteString -> Int -> Int -> Attribute
setId fAttributes fType fStart fStop
  | isJust idField = fromJust idField
  | isJust geneField = Field (L.pack "ID") (fieldValue (fromJust geneField))
  | isJust geneIdField = Field (L.pack "ID") (fieldValue (fromJust geneIdField))
  | isJust locusTagField = Field (L.pack "ID") (fieldValue (fromJust locusTagField))
  | isJust labelField = Field (L.pack "ID") (fieldValue (fromJust labelField))
  | otherwise = Field (L.pack "ID") (L.pack ((L.unpack fType) ++ "_" ++ show fStart ++ "_" ++ show fStop ++ "_notSet"))
  where idField = find (\f -> fieldType f == L.pack "ID") (filter isField fAttributes)
        geneIdField = find (\f -> fieldType f == L.pack "gene_id") (filter isField fAttributes)
        locusTagField = find (\f -> fieldType f == L.pack "locus_tag") (filter isField fAttributes)
        geneField = find (\f -> fieldType f == L.pack "gene") (filter isField fAttributes)
        labelField = find (\f -> fieldType f == L.pack "label") (filter isField fAttributes)


-- Genbank coordinates can be combined with operators defining the strand and connection between subfeatures
-- Complement indicats reverse strand
-- join indicates a set of subfeatures whose sequence should be concatenated
-- order indicates a set of subfeatures who share some connection but the sequences can be treated independently
-- more info: http://www.insdc.org/files/feature_table.html - 3.4.3 Location examples
-- Convertion to gff3 for the features uses the min and max coordinates of all coordinates in the set
-- Subfeatures are created for every entry in the coordinate list
genbankFeatureCoordinatesToGFF3FeatureCoordinates :: CoordinateSet -> (Int,Int,Char)
genbankFeatureCoordinatesToGFF3FeatureCoordinates cSet
  | null (setCoordinates cSet) = error "Corrdinates have you must"
  | isNothing (setType cSet) = (singleStart,singleStop,singleStrand)
  | "join" == justcSet = (joinStart,joinStop,joinStrand)
  | "order" == justcSet = (joinStart,joinStop,joinStrand)
  | otherwise = error ("Illegal coordinate set type: " ++ justcSet)
    where justcSet = fromJust (setType cSet)
          singleCoordinates = head (setCoordinates cSet)
          singleStart = coordinatesFrom singleCoordinates
          singleStop = coordinatesTo singleCoordinates
          singleStrand = if (complement singleCoordinates) then '-' else '+'
          joinStart = minimum (map coordinatesFrom (setCoordinates cSet))
          joinStop =  maximum (map coordinatesTo (setCoordinates cSet))
          joinStrand = if (complement singleCoordinates) then '-' else '+'

attributeToGFF3Attribute :: Attribute -> L.ByteString
attributeToGFF3Attribute (Flag _flagType) = _flagType
attributeToGFF3Attribute (Field _fieldType _fieldValue) = L.concat [(setGeneId _fieldType),L.pack "=",_fieldValue]
attributeToGFF3Attribute (GOattribute _gotype _go_id _goname) = L.concat [L.pack "GO-Term=",_gotype,L.pack ",",_go_id,L.pack ",",_goname]

--setId :: L.ByteString -> L.ByteString
--setId _fieldType
--  | (L.unpack _fieldType) == "gene" = L.pack ("ID")
--  | otherwise = _fieldType

subFeatureToGFF3Entries :: L.ByteString -> SubFeature -> [GFF3Entry]
subFeatureToGFF3Entries gbkAccession _subFeature
  | null (setCoordinates cSet) = error "Corrdinates have you must"
  | isNothing (setType cSet) = [subFeatureToGFF3Entry gbkAccession  _subFeature (1,head (setCoordinates cSet))]
  | "join" == justcSet = map (subFeatureToGFF3Entry gbkAccession _subFeature) (indexList (setCoordinates cSet))
  | "order" == justcSet = map (subFeatureToGFF3Entry gbkAccession _subFeature) (indexList (setCoordinates cSet))
  | otherwise = error ("Illegal coordinate set type: " ++ justcSet)
    where justcSet = fromJust (setType cSet)
          cSet = subFeatureCoordinates $ _subFeature

indexList :: [a] -> [(Int,a)]
indexList inlist = zip [1..(length inlist)] inlist

subFeatureToGFF3Entry :: L.ByteString -> SubFeature -> (Int,Coordinates) -> GFF3Entry
subFeatureToGFF3Entry gbkAccession _subFeature (sfIndex,sfCoordinates) = subFeatureGFF3
  where subFeatureGFF3 = GFF3Entry _seqId _source (subFeatureType _subFeature) _start _stop _score _strand _phase sfAttributes
        _seqId = gbkAccession
        _source = L.pack "."                 
        _start = coordinatesFrom $ sfCoordinates
        _stop = coordinatesTo $ sfCoordinates
        _score = L.pack "." 
        _strand = if (complement $ sfCoordinates) then '-' else '+'
        _phase = L.pack "."
        -- attempt to find parent id, with gene_id, locus_tag, gene
        parentId = setParentId (subFeatureType _subFeature) sfIndex (subFeatureAttributes _subFeature)
        sfAttributes = V.fromList(map attributeToGFF3Attribute (parentId ++ (subFeatureAttributes _subFeature)))

setParentId :: L.ByteString -> Int -> [Attribute] -> [Attribute]
setParentId sfType sfIndex sfAttributes
  | isJust idField = [Field (L.pack "ID") ((L.map C.toLower sfType) `L.append` L.pack "_" `L.append` fieldValue (fromJust idField) `L.append` L.pack "_" `L.append` sfIndexBS),Field (L.pack "Parent") (fieldValue (fromJust idField))]
  | isJust geneField = [Field (L.pack "ID") ((L.map C.toLower sfType) `L.append` L.pack "_" `L.append` fieldValue (fromJust geneField) `L.append` L.pack "_" `L.append` sfIndexBS),Field (L.pack "Parent") (fieldValue (fromJust geneField))]
  | isJust geneIdField = [Field (L.pack "ID") ((L.map C.toLower sfType) `L.append` L.pack "_"   `L.append` fieldValue (fromJust geneIdField) `L.append` L.pack "_" `L.append` sfIndexBS),Field (L.pack "Parent") (fieldValue (fromJust geneIdField))]
  | isJust locusTagField = [Field (L.pack "ID") ((L.map C.toLower sfType) `L.append` L.pack "_"   `L.append` fieldValue (fromJust locusTagField) `L.append` L.pack "_" `L.append` sfIndexBS),Field (L.pack "Parent") (fieldValue (fromJust locusTagField))]
  | isJust labelField = [Field (L.pack "ID") ((L.map C.toLower sfType) `L.append` L.pack "_"   `L.append` fieldValue (fromJust labelField) `L.append` L.pack "_" `L.append` sfIndexBS),Field (L.pack "Parent") (fieldValue (fromJust labelField))]
  | otherwise = [Field (L.pack "ID") (((L.map C.toLower sfType) `L.append` L.pack "_"  `L.append`  (L.pack "Missing_in_gbk"))),Field (L.pack "Parent") (L.pack "Missing_in_gbk")]
  where idField = find (\f -> fieldType f == L.pack "ID") (filter isField sfAttributes)
        geneIdField = find (\f -> fieldType f == L.pack "gene_id") (filter isField sfAttributes)
        locusTagField = find (\f -> fieldType f == L.pack "locus_tag") (filter isField sfAttributes)
        geneField = find (\f -> fieldType f == L.pack "gene") (filter isField sfAttributes)
        labelField = find (\f -> fieldType f == L.pack "label") (filter isField sfAttributes)
        sfIndexBS = L.pack . show $ sfIndex

isField :: Attribute -> Bool
isField (Field _ _) = True
isField _ = False

--subFeatureToGFF3Entry :: L.ByteString -> SubFeature -> GFF3Entry
--subFeatureToGFF3Entry gbkAccession _subFeature = subFeatureGFF3
--  where subFeatureGFF3 = GFF3Entry _seqId _source (subFeatureType _subFeature) _start _stop _score _strand _phase sfAttributes
--        _seqId = gbkAccession
--        _source = L.pack "."                 
--        _start = coordinatesFrom . head . setCoordinates . subFeatureCoordinates $ _subFeature
--        _stop = coordinatesTo . head . setCoordinates . subFeatureCoordinates $ _subFeature
--        _score = L.pack "." 
--        _strand = if (complement . head . setCoordinates . subFeatureCoordinates $ _subFeature) then '-' else '+'
--        _phase = L.pack "."
--        sfAttributes = V.fromList(map attributeToGFF3Attribute (subFeatureAttributes _subFeature))

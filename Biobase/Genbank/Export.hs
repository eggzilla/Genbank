-- | Functions for exporting of genbank data 
module Biobase.Genbank.Export (
--                       genbankToGFF3
                      ) where

import Biobase.Genbank.Data
import Biobase.GFF3.Types
import Data.Maybe
import Bio.Core.Sequence
import Data.Int
import Bio.Sequence.Fasta
import qualified Data.ByteString.Lazy.Char8 as L

--genbankToGFF :: Genbank -> GFF3
--genbankToGFF genbank = gff3
--   where 

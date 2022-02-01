{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Primitives where

import           Control.Monad
import           Data.Map(Map)

import           Types
import           Utils

---- compiler ----------------------------------------------------------------

validate :: P Bool -> P ()
validate mp = if validatingParser
              then
                do
                b <- mp -- run 'monadic predicate'
                when b $ fail "fails validation"
              else
                return ()
              -- or maybe warn
  where
  validatingParser = False


---- Dictionaries ------------------------------------------------------------

-- validate the trailer dictionary:
dictToTrailerDict :: Dict -> P TrailerDict
dictToTrailerDict = stub

-- returns a "Dictionary" of unused/redundant keys
trailerDict_Extras :: TrailerDict -> Dict 
trailerDict_Extras = stub

trailerDict_getPrev :: TrailerDict -> Maybe Offset
trailerDict_getPrev = stub

getKeyOfType :: Name -> PdfType -> Dict -> P PdfValue
getKeyOfType _kname _pdftype _d = stub


---- unimplemented functions for XRef parsing/manipulation -------------------

data XRef    -- TBD

pUpdate      :: (XRefRaw, TrailerDict) -> P (XRef, TrailerDict)
pUpdate      = stub

createIndex  :: XRef -> P (Map ObjId (Offset :+: Type2Ref))
createIndex  = stub

mergeIndices :: Map ObjId (Offset :+: Type2Ref)
             -> Map ObjId (Offset :+: Type2Ref)
             -> P (Map ObjId (Offset :+: Type2Ref))
mergeIndices = stub


---- unimplemented utility functions -----------------------------------------

derefValue :: Map ObjId (TopLevelDef' a :+: b) -> PdfValue -> P PdfValue
derefValue _ (V_Ref _i _g) = stub "lookup ..." 
derefValue _ x             = return x

derefTLD :: Map ObjId (TopLevelDef' a :+: b) -> ObjId -> P (TopLevelDef' a)
derefTLD m oi = stub "derefTLD" m oi


---- unimplemented, lower level parser functions -----------------------------

-- | pTopLevelDef_UnDecStm off - parse TopLevelDef at 'off' but if a stream,
--     we parse the dictionary but we don't yet decode and get the bytestring.
--   NOTE
--     - this needs to be a primitive parser if want to be efficient:
--       - parses top level values but if a stream, just record the offset
--         of 'stream' keyword.
--       - this allows us to parse top-levels without requiring a DOM to lookup objects in
--     - only the first two constructors of TopLevelDef' will be returned.

pTopLevelDef_UnDecStm :: P TopLevelDef_UnDecStm
pTopLevelDef_UnDecStm = stub


-- XRef functions
pAllUpdates jmp = stub jmp

pXrefRaw = stub

verifyXrefRaw :: XRefRaw -> P Bool
verifyXrefRaw _ = stub

-- lower level "parsers":

decodeStream :: PdfValue -> a -> Offset -> P ByteString
decodeStream _len _etc _off = stub "stream-data"

findPDFHeader = stub

findStartxrefThenParseToEOF = stub

-- basic parsers:
pSimpleWhiteSpace = stub

pDictionary = stub

pValue = stub

-- parser primitives:

parseString :: P a -> ByteString -> P a
parseString _p _s = stub

setInputAt :: Offset -> P ()
setInputAt offset = stub offset

readTo :: Offset -> P String
readTo offset = stub offset

keyword s = stub s

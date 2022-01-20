{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Spec where

import           Control.Monad
import           Data.Char
import qualified Data.Map as M
import           Data.Map(Map)

---- types: common -----------------------------------------------------------

type P a = IO a -- P is monadic parser, let's just use IO for the moment

-- PdfValue - per pdf-hs-driver ddl spec
data PdfValue = V_Ref ObjNum GenNum
              | V_Int Int
              -- FIXME: add more
              
data PdfType = T_Int   -- types of PDF values
               -- FIXME: add others
               
type ObjNum = Int
type GenNum = Int
type ObjId  = (ObjNum,GenNum) -- object identifier

type Offset = Int    -- offset from start of "PDF file" (not nec. file offset)
type Len    = Int    -- used as a length

type a :+: b = Either a b

type ByteString = String -- FIXME: update

                  
---- Dictionaries ------------------------------------------------------------

-- FIXME: whole scheme of going from PdfTypes to Haskell Types needs work!

type Name   = String -- dictionary keys
type Dict   = [(Name,PdfValue)] -- dups may exist in NCBUR PDFs?


-- type DictType = Map Name PdfType
-- 
-- trailerDictType :: DictType
-- trailerDictType = stub

-- A Trailer Dictionary:
type TrailerDict = Dict
data TrailerDict2
  -- FIXME: LATER, this would be a correct, typed version of TrailerDict

-- validate the trailer dictionary:
dictToTrailerDict :: Dict -> P TrailerDict
dictToTrailerDict d = return d

-- returns a "Dictionary" of unused/redundant keys
trailerDict_Extras :: TrailerDict -> Dict 
trailerDict_Extras = stub

trailerDict_getPrev :: TrailerDict -> Maybe Offset
trailerDict_getPrev = stub

getKeyOfType :: Name -> PdfType -> Dict -> P PdfValue
getKeyOfType kname pdftype d = stub


---- types for xref and lower level ------------------------------------------

type XRefRaw       = [SubSectionRaw]
type SubSectionRaw = (ObjId,Len,Offset)
                      -- the xref entries are not parsed yet
                      -- the Offset is the file offset of first xref entry
                      -- we assume valid PDF, with length(xrefEntry) = 20
                      -- TODO: add Free xref entries


---- parsing -----------------------------------------------------------------

pPDF =
  do
  (version,off0) <- findPDFHeader
                    -- find %PDF-x.y at start of file, skipping up to 1K bytes
  let jmp n = setInputAt (off0+n)
  (startxrefOff,xrefOff) <- findStartxrefThenParseToEOF
                            -- starts searching backwards from EOF
                            -- gives up after 1000 bytes
  jmp xrefOff
  (xrefRaw, xrefEndOff) <- pXrefRaw
                           :: P (XRefRaw,Offset)
                           
  -- At this point (without parsing sections)
  --  - we know the list of objects
  --  - we can find xref entry for each object
  --  - we know the end of xref table
  -- but the above is predicated on
  --  - enforcing full standard compliance with 20 byte (only) xref entries.
  --    - currently 19,21 byte xref entries seem to be considered NCBUR!

  validateXrefRaw xrefRaw
    -- - this ensures no duplicate objectIds
    -- - we could, but don't need to (yet)
    --   - parse xref entries
    --   - validate xref entries (without parsing the object)

  jmp xrefEndOff
     -- Because pXrefSubSections doesn't need to parse contents of
     -- the subsections.

  trailerDict <- pSimpleWhiteSpace >> keyword "trailer" >> pDictionary
  validate $
    do
    cs <- readTo startxrefOff
    return (all isSpace cs)
  trailerDict' <- dictToTrailerDict trailerDict

  let mPrev = trailerDict_getPrev trailerDict' :: Maybe Offset
      etc = trailerDict_Extras trailerDict'    :: Dict
        -- etc is a list of unknown key-value pairs
        -- we could be even lazier to allow
        --  - Xref & DOM analysis even when errors in dictionary

  -- FIXME: don't really know version till we have created 'dom'!
  if version > (2,0) then
    warn "PDF file has version greater than 2.0"
  else
    -- version <= (2,0)
    when (not (null etc)) $
      warn "trailer dictionary has unknown keys (per PDF 2.0)"

  updates' <- pAllUpdates mPrev :: P [(XRefRaw, TrailerDict)]
     -- we've followed the 'Prev's and for each
     --   - pXrefRaw     -- processed xref subsections (at raw level)
     --   - pTrailerDict -- similar to above, but
     --                     only reads/validates Prev key

  -- at this point
  --  - we have
  --    - parsed/validated minimally
  --    - rejected *some* invalid PDFs
  --    - no PDF 'values' parsed except trailer dictionaries
  --  - we can (without further 'parsing' or reading of input)
  --    - output trailer dictionaries
  --    - output high level info wrt incremental updates
  --  - we've detected
  --    - overlapping ObjIds in an xref table (and ...?)
  --  - we have NOT
  --    - parsed anything inessential to creating DOM
  --    - parsed the contents of xref entries

  let updates = (xrefRaw,trailerDict) : updates'
  dom <- pDOM jmp updates
  return dom
  

---- types for DOM creation --------------------------------------------------

data TopLevelDef' a = TLD_Value  PdfValue
                    | TLD_Stream Dict a -- ^ multiple possibilities for
                                        --   'state' of the Stream
                    | TLD_ObjStm ObjStm -- ^ Stream with "Type ObjStm" is
                                        --   treated specially

data ObjStm = ObjStm [ByteString]
  -- pre-processed ObjStm, can access source of objects efficiently, objects
  -- not parsed.

type TopLevelDef_UnDecStm = TopLevelDef' Offset  -- Undecoded Streams
type TopLevelDef          = TopLevelDef' ByteString -- Streams decoded

type DOM = Map ObjId TopLevelDef

-- | Type2Ref refers to a "compressed object" inside an ObjStm.
--   These only occur in 1.5+ PDFs that use Xref Streams.
data Type2Ref =
  Type2Ref
    { t2_objstm :: ObjNum -- ^ indexes an ObjStm in the DOM (gen == 0)
    , t2_offset :: Int    -- ^ the offset of object in the above ObjStm
    }


---- creating the DOM from the list of updates ---------------------------

pDOM :: (Offset -> P ()) -> [(XRefRaw, TrailerDict)] -> P DOM
pDOM jmp updates =
  do
  -- combine all the updates to get a single map to offsets:
  --  - i.e., merge multiple xref tables into one
  xrefs <- combineAllXrefTables updates
           :: P (Map ObjId (Offset :+: Type2Ref))

    --  - parses xref entries
    --  - merges all inc. updates into a single mapping.
    --  - we've lost information:
    --    - which update an object is part of
    --    - all object history
    --    - object definitions that are no longer reachable
    --  - fails on
    --    - malformed xref entries
    --    - mixture of xref table and xref streams [PW?]
    --  - should detect
    --    - trailer dicts that aren't consistent between updates
    --    - incremental updates that are "weird/nonsensical"
    --      - free-ing dead objects
    --      - unconventional use of generation numbers
    --  - we know ALL the object ids in PDF
                
    --  - IF updates are defined by xref STREAMS
    --    - no problem: as we can fully parse xref stream (w/ dict) as
    --      there is no dependence of xref STREAMS on DOM
    --      - NOTE: clarificaton to PDF working group regarding this.
    --    - we'll have Type2Ref's in addition to Offset's
    --       
    --  - NOTE 
    --    - when the latter, the ObjectId -> Offset must be available
    --        - in current or previous (or next!) xref stream
    --          - BTW, pervasive design issue: must partial updates be valid?

  domPass1 <- mapM
                (mMapLeft pTopLevelDef_UnDecStm)
                xrefs
              :: P (Map ObjId (TopLevelDef_UnDecStm :+: Type2Ref))

  -- at this point
  --   - might know the PDF version, if Root object is not in an ObjStm.
  -- and we can NOW (but not before this)
  --   - verify/read toplevel stream data
  --     - b/c now indirect /Length and ... is defined in domPass1
  --   - decode ObjStm streams (if 1.5+)

  -- extract stream data into ByteStrings, also pre-processes ObjStm streams
  domPass2 <- mapM
                (mMapLeft (extractStreamData domPass1))
                domPass1
              :: P (Map ObjId (TopLevelDef :+: Type2Ref))

  -- at this point
  --  - can compute body cavities
  --  - ObjStm's have been pre-processed
  --    - but objects inside not parsed

  domFinal <- mapM
               (return `either` derefType2Ref domPass2)
                domPass2
              :: P (Map ObjId TopLevelDef)

  -- at this point
  --  - every object referenced via xref has been parsed, but
  --    - extraneous object defs in body are never parsed
  --    - unreferenced objects (per xref) in ObjStm's are never parsed
  --  - we positively know the PDF version (only now!)
  --    - catalog dictionary might have been in an ObjStm

  return domFinal


-- | extractStreamData - since we now know all Lengths:
--   - 1st, with the file offset, read into a bytestring
--   - 2nd, if an ObjStm, decodes/validates the stream
--     - NOTE: this processing done just once, not each time
--       that we "index" into the ObjStm.

extractStreamData ::
     Map ObjId (TopLevelDef' Offset :+: a)
  -> TopLevelDef' Offset
  -> P (TopLevelDef' ByteString)
extractStreamData dom' (TLD_ObjStm _)     = error "unexpeced ObjStm"
extractStreamData dom' (TLD_Value v)      = return $ TLD_Value v
extractStreamData dom' (TLD_Stream d off) =
  do
  len  <- getKeyOfType "Length" T_Int d  -- indirect OR direct
  len' <- derefValue dom' len            -- now an integer direct
  etc  <- stub "exract (from dict) info needed to decode stream" d
  bs   <- decodeStream len' etc off
  return $ TLD_Stream d bs

derefType2Ref ::
     Map ObjId (TopLevelDef :+: Type2Ref)
  -> Type2Ref
  -> P TopLevelDef
derefType2Ref dom' (Type2Ref oi j) =
  do
  tld       <- derefTLD dom' (oi,0)
  ObjStm ss <- getObjStm tld      -- make sure the object is ObjStm
  s         <- case safeIndex ss j of
                 Just s  -> return s
                 Nothing -> error "bad type2 reference: index out of bounds"
  v <- parseString pValue s
       -- note that streams cannot be inside ObjStm
  return $ TLD_Value v


getObjStm :: TopLevelDef' ByteString -> P ObjStm
getObjStm (TLD_ObjStm x) = return x
getObjStm _              = error "expected ObjStm"

---- unimplemented utility functions -----------------------------------------

derefValue :: Map ObjId (TopLevelDef' a :+: b) -> PdfValue -> P PdfValue
derefValue map (V_Ref i g) = stub "lookup ..." 
derefValue _   x           = return x

derefTLD :: Map ObjId (TopLevelDef' a :+: b) -> ObjId -> P (TopLevelDef' a)
derefTLD m oi = stub "derefTLD"


---- unimplemented, lower level parser functions -----------------------------

-- | pTopLevelDef_UnDecStm off - parse TopLevelDef at 'off' but if a stream,
--     we parse the dictionary but we don't yet decode and get the bytestring.
--   NOTE
--     - this needs to be a primitive parser if want to be efficient:
--       - parses top level values but if a stream, just record the offset
--         of 'stream' keyword.
--       - this allows us to parse top-levels without requiring a DOM to lookup objects in
--     - only the first two constructors of TopLevelDef' will be returned.

pTopLevelDef_UnDecStm :: Offset -> P TopLevelDef_UnDecStm
pTopLevelDef_UnDecStm off = stub


-- XRef functions
pAllUpdates offset = stub

pXrefRaw = stub

combineAllXrefTables = stub

validateXrefRaw = stub

-- lower level "parsers":

decodeStream :: PdfValue -> a -> Offset -> P ByteString
decodeStream len etc off = stub "stream-data"

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
setInputAt int = stub

readTo :: Offset -> P String
readTo offset = stub

keyword s = stub


---- utils -------------------------------------------------------------------

mMapLeft :: Monad m => (a-> m c) -> a :+: b -> m (c :+: b)
mMapLeft f = (\a-> Left <$> f a) `either` (return . Right)

mMapRight :: Monad m => (b-> m c) -> a :+: b -> m (a :+: c)
mMapRight f = (return . Left) `either` (\b-> Right <$> f b) 

validate :: P Bool -> P ()
validate mp = if validatingParser
              then
                do
                b <- mp -- run 'monadic predicate'
                when b $ fail "fails validation"
              else
                return ()
  where
  validatingParser = False

-- sub - a safe version of (!!)
sub :: [a] -> Int -> Maybe a
sub (x:_)  0       = Just x
sub (_:xs) n | n>0 = sub xs (n-1)
sub _      _       = Nothing

safeIndex = sub

stub = error "stub"

warn :: [Char] -> IO ()
warn s = error s

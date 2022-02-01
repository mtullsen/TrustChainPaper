{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Spec where

import           Control.Monad
import           Data.Char
import           Data.Foldable(foldlM)
import qualified Data.Map as M
import           Data.Map(Map)

import           Types
import           Utils
import           Primitives
  -- put type sigs in appendix: ?


---- parsing -----------------------------------------------------------------

pPDFDom =
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
  --    - currently 19,21 byte xref entries are considered NCBUR!
  -- if we were to allow 19-21 byte xref entries:
  --   - we would be nothing essential would 
  --   - nothing essential would 
  
  validate $
    verifyXrefRaw xrefRaw
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
  

---- creating the DOM from the list of updates ---------------------------

pDOM :: (Offset -> P ()) -> [(XRefRaw, TrailerDict)] -> P DOM
pDOM jmp updates =
  do
    
  -- combine all the updates to get a single map to offsets:
  xrefs <- combineAllXrefTables updates
           :: P (Map ObjId (Offset :+: Type2Ref))

  -- at this point
  --  - we know ALL the object ids in PDF

  -- parse all uncompressed objects (but leave streams undecoded):
  domPass1 <- mapM
                (mMapLeft (\o-> do {jmp o; pTopLevelDef_UnDecStm}))
                xrefs
              :: P (Map ObjId (TopLevelDef_UnDecStm :+: Type2Ref))

  -- at this point
  --   - might know the PDF version, if Root object is not in an ObjStm
  -- we are only NOW able to
  --   - verify/read toplevel stream data
  --     - b/c now indirect /Length and ... is defined in domPass1
  --   - decode ObjStm streams (if 1.5+)

  -- decode streams into ByteStrings, also pre-processes ObjStm streams
  domPass2 <- mapM
                (mMapLeft (extractStreamData domPass1))
                domPass1
              :: P (Map ObjId (TopLevelDef :+: Type2Ref))

  -- at this point
  --  - can compute body cavities
  --  - ObjStm's have been pre-processed
  --    - but objects inside them not parsed

  domFinal <- mapM
               (return `either` derefType2Ref domPass2)
                domPass2
              :: P (Map ObjId TopLevelDef)

  -- at this point
  --  - every object referenced via xref has been parsed
  --  - However,
  --    - extraneous object defs in body are never parsed
  --    - unreferenced objects (per xref) in ObjStm's are never parsed
  --  - we positively know the PDF version (only now)
  --    - catalog dictionary might have been in an ObjStm
  --    - Q. is this intentional? this precludes lots of checks.

  return domFinal

-- | combineAllXrefTables updates - 
--   - for each update
--     - parses each xref subsection into a list of xref entries
--   - merges all the xref tables into a single mapping
--     - when no errors/inconsistencies

combineAllXrefTables
  :: [(XRefRaw, TrailerDict)] -> P (Map ObjId (Offset :+: Type2Ref))
combineAllXrefTables updates =
  do
  updates' <- mapM pUpdate updates  
  indices' <- mapM (createIndex . fst) updates' 
  index    <- foldlM mergeIndices M.empty indices'
  return index

  -- NOTE
  --  - we've lost information:
  --    - which update an object is part of
  --    - object history
  --    - object definitions that are no longer reachable
  --  - fails on
  --    - malformed xref entries
  --    - mixture of xref table and xref streams [PW?]
  --  - should detect (or fail) on
  --    - trailer dicts that aren't consistent between updates
  --    - incremental updates that are "weird/nonsensical"
  --      - free-ing dead objects
  --      - unconventional use of generation numbers
              
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



-- | extractStreamData - since we now know all Lengths:
--   - 1st, with the file offset, read into a bytestring
--   - 2nd, if an ObjStm, decodes/validates the stream
--     - NOTE: this processing done just once, not each time
--       that we "index" into the ObjStm.

extractStreamData ::
     Map ObjId (TopLevelDef' Offset :+: a)
  -> TopLevelDef' Offset
  -> P (TopLevelDef' ByteString)
extractStreamData _dom' (TLD_ObjStm _)    = error "unexpeced ObjStm"
extractStreamData _dom' (TLD_Value v)     = return $ TLD_Value v
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


module IPTC.Tag
  ( IPTCTag(..)
  , iptcTagFindByName
  ) where

import IPTC.Record
import Control.Monad(forM_)
import Data.ByteString.Char8(ByteString, useAsCString)
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C
import System.IO.Unsafe(unsafePerformIO)

foreign import ccall "libiptcdata/iptc-tag.h iptc_tag_find_by_name"
    c_iptcTagFindByName :: Foreign.C.CString -> Ptr CInt -> Ptr CInt -> IO CInt

newtype IPTCTag = IPTCTag CInt deriving (Show)

iptcTagFindByName :: ByteString -> Maybe (IPTCRecord, IPTCTag)
iptcTagFindByName bs = unsafePerformIO $
    useAsCString bs $ \cString ->
    alloca $ \recordIntPtr ->
    alloca $ \tagIntPtr-> do
        success <- c_iptcTagFindByName cString recordIntPtr tagIntPtr
        if success == 0 
            then do
                recordInt <- peek recordIntPtr
                tagInt <- peek tagIntPtr
                return $ Just (IPTCRecord recordInt, IPTCTag tagInt)
            else return Nothing



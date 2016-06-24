module IPTC.DataSet
  ( IPTCDataSet(..)
  , wrapIPTCDataSetPtr
  ) where

import Foreign hiding (newForeignPtr)
import Foreign.C
import Foreign.Concurrent
import System.IO.Unsafe(unsafePerformIO)

newtype IPTCDataSet = IPTCDataSet (ForeignPtr ())

instance Show IPTCDataSet where
    show (IPTCDataSet fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
      let allocatedSize = 1024
      allocaBytes allocatedSize $ \cString -> do
        c_iptcDataSetGetAsStr ptr cString $ fromIntegral allocatedSize
        str <- peekCString cString
        return $ "IPTCDataset " ++ show str


foreign import ccall "libiptcdata/iptc-dataset.h iptc_dataset_get_as_str"
    c_iptcDataSetGetAsStr :: Ptr () -> CString -> CUInt -> IO CString


foreign import ccall "libiptcdata/iptc-dataset.h iptc_dataset_unref"
    c_iptcDataSetUnref :: Ptr () -> IO ()


wrapIPTCDataSetPtr :: Ptr () -> IO IPTCDataSet
wrapIPTCDataSetPtr iptcDataSetPtr = IPTCDataSet
    <$> newForeignPtr iptcDataSetPtr garbageCollectorFunction
  where
    garbageCollectorFunction = c_iptcDataSetUnref iptcDataSetPtr



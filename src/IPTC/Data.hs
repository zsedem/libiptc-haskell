module IPTC.Data
  ( iptcDataNewFromData
  , iptcDataGetDataset
  , iptcDataGetNextDataset
  ) where
import IPTC.DataSet
import IPTC.Record
import IPTC.Tag
import Control.Monad(forM_)
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C
import System.IO.Unsafe(unsafePerformIO)

newtype IPTCData = IPTCData (ForeignPtr ()) deriving (Show)

iptcDataNewFromData :: [Word8] -> Maybe IPTCData
iptcDataNewFromData characters = if nullPtr == iptcDataPtr
                            then Nothing
                            else Just $ unsafePerformIO $ wrapIptcDataPtr iptcDataPtr
    where
      iptcDataPtr = unsafePerformIO $ allocaBytes sizeOfData $ \array -> do
                        forM_ (zip [0..] characters) $ \(offset, character) ->
                            pokeElemOff array offset (CUChar character)
                        return $ c_iptcDataNewFromData array $ fromIntegral sizeOfData

      sizeOfData = length characters

iptcDataGetDataset :: IPTCData -> IPTCRecord -> IPTCTag -> Maybe IPTCDataSet
iptcDataGetDataset (IPTCData iptcDataFPtr) (IPTCRecord iptcRecord) (IPTCTag iptcTag) =
    unsafePerformIO $ withForeignPtr iptcDataFPtr $ \iptcDataPtr -> do
        let iptcDataSetPtr = c_iptcDataGetDataset iptcDataPtr iptcRecord iptcTag
        if nullPtr == iptcDataSetPtr
            then return Nothing
            else Just <$> wrapIPTCDataSetPtr iptcDataSetPtr

iptcDataGetNextDataset :: IPTCData -> Maybe IPTCDataSet -> IPTCRecord -> IPTCTag -> Maybe IPTCDataSet
iptcDataGetNextDataset (IPTCData iptcDataFPtr) iptcDataSetFPtr (IPTCRecord iptcRecord) (IPTCTag iptcTag) =
    unsafePerformIO $ withForeignPtr iptcDataFPtr $ \iptcDataPtr ->
                      withMaybeForeignPtr iptcDataSetFPtr $ \iptcDataSetPtr -> do
        let nextIPTCDataSetPtr = c_iptcDataGetNextDataset iptcDataPtr iptcDataSetPtr iptcRecord iptcTag
        if nullPtr == nextIPTCDataSetPtr
            then return Nothing
            else Just <$> wrapIPTCDataSetPtr nextIPTCDataSetPtr
  where
    withMaybeForeignPtr (Just (IPTCDataSet fptr)) f = withForeignPtr fptr f
    withMaybeForeignPtr _ f = f nullPtr

foreign import ccall "libiptcdata/iptc-data.h iptc_data_new_from_data"
    c_iptcDataNewFromData :: Ptr Foreign.C.CUChar -> CUInt -> Ptr ()

foreign import ccall "libiptcdata/iptc-data.h iptc_data_free"
    c_iptcDataFree :: Ptr () -> IO ()

foreign import ccall "libiptcdata/iptc-data.h iptc_data_get_dataset"
    c_iptcDataGetDataset :: Ptr () -> CInt -> CInt -> Ptr ()

foreign import ccall "libiptcdata/iptc-data.h iptc_data_get_next_dataset"
    c_iptcDataGetNextDataset :: Ptr () -> Ptr () -> CInt -> CInt -> Ptr ()

wrapIptcDataPtr :: Ptr () -> IO IPTCData
wrapIptcDataPtr iptcDataPtr = IPTCData 
    <$> newForeignPtr iptcDataPtr garbageCollectorFunction
  where
    garbageCollectorFunction = c_iptcDataFree iptcDataPtr

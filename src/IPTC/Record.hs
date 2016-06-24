module IPTC.Record
  where
import Foreign.C

newtype IPTCRecord = IPTCRecord CInt deriving (Show)

iptcRecordObjectEnv = IPTCRecord 1
iptcRecordApp_2 = IPTCRecord 2
iptcRecordApp_3 = IPTCRecord 3
iptcRecordApp_4 = IPTCRecord 4
iptcRecordApp_5 = IPTCRecord 5
iptcRecordApp_6 = IPTCRecord 6
iptcRecordPreobjData = IPTCRecord 7
iptcRecordObjData = IPTCRecord 8
iptcRecordPostobjData = IPTCRecord 9



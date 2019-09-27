{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
import Clash.Prelude
import qualified Prelude

import Data.Maybe

import Clash.Container.FIFO
import Clash.Network.Types
import Clash.Stream.Stream
import Clash.ErrorControl.CRC
import Clash.Container.CuckooPipeline
import Clash.Container.FIFO

networkHeaders :: Vec 84 (BitVector 4)
networkHeaders = concatMap func $ bitCoerce (ethernetHeader, ipHeader, udpHeader)
    where
    func :: BitVector 8 -> Vec 2 (BitVector 4)
    func = reverse . unpack

    ethernetHeader = EthernetHeader {
            destMac   = repeat 0xff,
            sourceMac = 0x12 :> 0x34 :> 0x56 :> 0x78 :> 0x90 :> 0xab :> Nil,
            etherType = 0x0800
        }

    ipHeader = IPv4Header {
            version        = 4,
            ihl            = 5,
            dscp           = 0,
            ecn            = 0,
            ipLen          = 0x002c,
            identification = 0,
            flags          = 0,
            fragmentOffset = 0,
            ttl            = 0x80,
            protocol       = 0x11,
            headerChecksum = 0xaf6d,
            sourceIP       = 192 :> 168 :> 5 :> 2 :> Nil,
            destIP         = 192 :> 168 :> 5 :> 1 :> Nil
        }

    udpHeader = UDPHeader {
            sourcePort  = 0x1234,
            destPort    = 0x1234,
            udpLen      = 0x0018,
            udpChecksum = 0x0
        }

hashtable
    :: HiddenClockResetEnable dom
    => Signal dom (BitVector 64)
    -> Signal dom (Maybe (Maybe (BitVector 64)))
    -> (
        Signal dom (Maybe (BitVector 64)),
        Signal dom Bool
        )
hashtable lu cmd = cuckooPipelineInsert hashFunctions lu cmd
    where
    hashFunctions 
        =  (\x -> unpack (crc0 x))
        :> (\x -> unpack (crc1 x))
        :> (\x -> unpack (crc2 x))
        :> (\x -> unpack (crc3 x))
        :> Nil
        where
        table0 = $(lift $ (makeCRCTable (pack . crcSteps 0x5db (repeat 0)) :: Vec 64 (BitVector 11))) 
        crc0  = crcTable table0
        table1 = $(lift $ (makeCRCTable (pack . crcSteps 0x64d (repeat 0)) :: Vec 64 (BitVector 11))) 
        crc1  = crcTable table1 
        table2 = $(lift $ (makeCRCTable (pack . crcSteps 0x62b (repeat 0)) :: Vec 64 (BitVector 11))) 
        crc2  = crcTable table2 
        table3 = $(lift $ (makeCRCTable (pack . crcSteps 0x473 (repeat 0)) :: Vec 64 (BitVector 11))) 
        crc3  = crcTable table3 

data Command = Command {
    command      :: BitVector 32,
    commandKey   :: BitVector 64,
    commandValue :: BitVector 64
} deriving (Generic, NFDataX, Eq, BitPack)

decodeComand :: Command -> (Bool, BitVector 64, Maybe (Maybe (BitVector 64)))
decodeComand Command{..} 
    | command == 0 = (True,  commandKey, Nothing)
    | command == 1 = (False, commandKey, Just (Just commandValue))
    | command == 2 = (False, commandKey, Just Nothing)
    | otherwise    = (False, commandKey, Nothing)

topEntity' 
    :: forall dom
    .  HiddenClockResetEnable dom 
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (BitVector 4)
    -> Signal dom Bool
    -> (
            Signal dom Bool,
            Signal dom Bool,
            Signal dom (BitVector 4)
       )
topEntity' vld eof dat ready = unbundle headeredStream
    where
    noHeaderStream     = dropStream (SNat @ 84) $ bundle (vld, eof, dat)
    (wideVld, wideDat) = unbundle $ widenStream noHeaderStream

    (bufferedDat, bufferedFull, _, _) = blockRamFIFO (SNat @ 10) (not <$> busy) wideDat wideVld

    bufferedVld = not <$> bufferedFull

    cmd :: Signal dom Command
    cmd =  bitCoerce . swapNibbles <$> bufferedDat

    (lookup, key, cmd') = unbundle $ decodeComand <$> cmd

    (result, busy) = hashtable key $ mux bufferedVld cmd' (pure Nothing)

    nextCycle = register False (lookup .&&. bufferedVld)

    packResult :: Maybe (BitVector 64) -> BitVector 176
    packResult x = pack . swapNibbles . (unpack :: BitVector 176 -> Vec 44 (BitVector 4)) $ (0 :: BitVector 31) ++# pack x ++# (0 :: BitVector 80)

    bufferedStream  = packetize @8 (pure 0) (bundle (nextCycle, unpack . packResult <$> result)) narrowedReady
    (narrowedStream, narrowedReady) = narrowStream bufferedStream headeredReady
    (headeredStream, headeredReady) = prependHeader (pure networkHeaders) narrowedStream ready

{-# ANN topEntity
  (Synthesize
    { t_name     = "kvs"
    , t_inputs   = [
            PortName "clk", 
            PortName "rst", 
            PortName "clkEn",
            PortName "iVld",
            PortName "iEof",
            PortName "iDat",
            PortName "iReady"
        ]
    , t_output   = PortProduct "" [
            PortName "oVld",
            PortName "oEof",
            PortName "oDat"
        ]
    }) #-}
topEntity = exposeClockResetEnable @System topEntity'


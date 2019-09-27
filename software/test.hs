{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (lookup)

import Data.Word
import Control.Monad

import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket
import Network.Socket.ByteString (sendAllTo, recv)
import Data.Serialize

--Insert/delete/lookup command to send to FPGA
data Command = Command {
    command :: Word32,
    key     :: Word64,
    value   :: Word64
} deriving (Show)

putCommand :: Putter Command
putCommand Command{..}
    =  putWord32be command
    <> putWord64be key
    <> putWord64be value

--Result of a lookup
data LookupResult = LookupResult {
    present :: Word32,
    result  :: Word64
} deriving (Show)

getResult :: Get LookupResult
getResult
    =   LookupResult
    <$> getWord32be
    <*> getWord64be

--Send commands to FPGA
insert :: Socket -> SockAddr -> Word64 -> Word64 -> IO ()
insert socket fpgaAddress key value = do
    let cmd = Command 1 key value
    sendAllTo socket (runPut $ putCommand cmd) fpgaAddress

delete :: Socket -> SockAddr -> Word64 -> IO ()
delete socket fpgaAddress key = do
    let cmd = Command 2 key 0
    sendAllTo socket (runPut $ putCommand cmd) fpgaAddress

lookup :: Socket -> SockAddr -> Word64 -> IO (Either String LookupResult)
lookup socket fpgaAddress key = do
    let cmd = Command 0 key 0
    sendAllTo socket (runPut $ putCommand cmd) fpgaAddress
    res <- recv socket 16
    return $ runGet getResult res

--How many items to insert
maxIdx = 7500

--Derive keys and values from index
keyFunc1 :: Int -> Word64
keyFunc1 x = fromIntegral $ x ^ 2

keyFunc2 :: Int -> Word64
keyFunc2 x = fromIntegral $ x ^ 3

valueFunc :: Int -> Word64
valueFunc x = fromIntegral $ x ^ 5

--Test which fills up the hashtable, reads everything back, deletes everything, and checks its empty
fillAndEmpty :: Socket -> SockAddr -> IO ()
fillAndEmpty socket fpgaAddress = do

    forM_ [0..maxIdx] $ \idx -> insert socket fpgaAddress (keyFunc1 idx) (valueFunc idx)

    forM_ [0..maxIdx] $ \idx -> do
        res <- lookup socket fpgaAddress (keyFunc1 idx)
        case res of
            Left err -> putStrLn $ "Parsing error" ++ err
            Right res -> when ((present res /= 1) || (result res /= valueFunc idx)) $ putStrLn "Error after insertion"

    forM_ [0..maxIdx] $ \idx -> delete socket fpgaAddress (keyFunc1 idx)

    forM_ [0..maxIdx] $ \idx -> do
        res <- lookup socket fpgaAddress (keyFunc1 idx)
        case res of
            Left err -> putStrLn $ "Parsing error" ++ err
            Right res -> when (present res /= 0) $ putStrLn "Error after deletion"

    putStrLn "Success!"

--Test which fills up the hashtable, immediately reading back inserted keys as they are inserted. Then does the same with deletes.
alternateInsertRead :: Socket -> SockAddr -> IO ()
alternateInsertRead socket fpgaAddress = do

    forM_ [0..maxIdx] $ \idx -> do

        insert socket fpgaAddress (keyFunc2 idx) (valueFunc idx + 1)
        insert socket fpgaAddress (keyFunc2 idx) (valueFunc idx)

        res <- lookup socket fpgaAddress (keyFunc2 idx)
        case res of
            Left err -> putStrLn $ "Parsing error" ++ err
            Right res -> when ((present res /= 1) || (result res /= valueFunc idx)) $ putStrLn "Error after insertion"

    forM_ [0..maxIdx] $ \idx -> do

        delete socket fpgaAddress (keyFunc2 idx)

        res <- lookup socket fpgaAddress (keyFunc2 idx)
        case res of
            Left err -> putStrLn $ "Parsing error" ++ err
            Right res -> when (present res /= 0) $ putStrLn "Error after insertion"

    putStrLn "Success!"

main :: IO ()
main = do

    let hostAddress = Socket.SockAddrInet 0x1234 0
        fpgaAddress = Socket.SockAddrInet 0x1234 (Socket.tupleToHostAddress (192, 168, 5, 2))
    
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    Socket.bind socket hostAddress

    --Repeat both tests forever in a loop
    forever $ do
        alternateInsertRead socket fpgaAddress
        fillAndEmpty socket fpgaAddress


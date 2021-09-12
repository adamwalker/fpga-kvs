{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (lookup)

import Data.Word
import Control.Monad

import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket
import Network.Socket.ByteString (sendAllTo, recv)
import Data.Serialize
import System.IO (hFlush, stdout)

--Insert/delete/lookup command to send to FPGA
data Command = Command {
    command :: Word32,
    key     :: Word64,
    value   :: Word64
} deriving (Show)

--Encode the command
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

--Decode the result
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

--How many items to insert. Based on the number of RAMs available on the FPGA
maxIdx = 7500

--Derive keys and values from indices to get a "random" distribution of keys
keyFunc1 :: Int -> Word64
keyFunc1 x = fromIntegral $ x ^ 2

keyFunc2 :: Int -> Word64
keyFunc2 x = fromIntegral $ x ^ 3

valueFunc :: Int -> Word64
valueFunc x = fromIntegral $ x ^ 5

--Test which fills up the hashtable, reads everything back, deletes everything, and checks its empty
fillAndEmpty :: Socket -> SockAddr -> IO ()
fillAndEmpty socket fpgaAddress = do

    putStr "Testing filling the hashtable up, reading everything back, deleting everything, then making sure its empty... "
    hFlush stdout

    --Insert the values
    forM_ [0..maxIdx] $ \idx -> insert socket fpgaAddress (keyFunc1 idx) (valueFunc idx)

    --Readback the values
    forM_ [0..maxIdx] $ \idx -> do
        res <- lookup socket fpgaAddress (keyFunc1 idx)
        case res of
            Left err 
                -> putStrLn $ "Parsing error after insert" ++ err
            Right res -> do
                when (present res /= 1) $ putStrLn "Error: inserted item is not present"
                when (result res /= valueFunc idx) $ putStrLn $ "Error: inserted item has the wrong value. Expected: " ++ show (valueFunc idx) ++ ". Got: " ++ show (result res)

    --Delete the values
    forM_ [0..maxIdx] $ \idx -> delete socket fpgaAddress (keyFunc1 idx)

    forM_ [0..maxIdx] $ \idx -> do
        res <- lookup socket fpgaAddress (keyFunc1 idx)
        case res of
            Left err -> putStrLn $ "Parsing error after delete" ++ err
            Right res -> when (present res /= 0) $ putStrLn "Error: item is still present after deletion"

    putStrLn "Success!"

--Test which fills up the hashtable, immediately reading back inserted keys as they are inserted. Then does the same with deletes.
alternateInsertRead :: Socket -> SockAddr -> IO ()
alternateInsertRead socket fpgaAddress = do

    putStr "Testing filling the hashtable up while reading back each inserted value... "
    hFlush stdout

    --Alternate inserting and reading back
    forM_ [0..maxIdx] $ \idx -> do

        --Insert a value
        insert socket fpgaAddress (keyFunc2 idx) (valueFunc idx + 1)
        --Overwrite it 
        insert socket fpgaAddress (keyFunc2 idx) (valueFunc idx)

        --Lookup the value
        res <- lookup socket fpgaAddress (keyFunc2 idx)
        --Check the correct value is returned
        case res of
            Left err 
                -> putStrLn $ "Parsing error after insert" ++ err
            Right res -> do
                when (present res /= 1) $ putStrLn "Error: inserted item is not present"
                when (result res /= valueFunc idx) $ putStrLn $ "Error: inserted item has the wrong value. Expected: " ++ show (valueFunc idx) ++ ". Got: " ++ show (result res)

    --Alternate deleting and reading back
    forM_ [0..maxIdx] $ \idx -> do

        --Delete it
        delete socket fpgaAddress (keyFunc2 idx)

        --Check it's not there
        res <- lookup socket fpgaAddress (keyFunc2 idx)
        case res of
            Left err 
                -> putStrLn $ "Parsing error after delete" ++ err
            Right res 
                -> when (present res /= 0) $ putStrLn "Error: item is still present after deletion"

    putStrLn "Success!"

main :: IO ()
main = do

    let hostAddress = Socket.SockAddrInet 0x1234 0
        fpgaAddress = Socket.SockAddrInet 0x1234 (Socket.tupleToHostAddress (192, 168, 5, 2))
    
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    Socket.bind socket hostAddress

    --Repeat both tests forever in a loop
    putStrLn "Looping tests forever..."
    forever $ do
        alternateInsertRead socket fpgaAddress
        fillAndEmpty socket fpgaAddress


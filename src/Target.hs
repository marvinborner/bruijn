-- MIT License, Copyright (c) 2024 Marvin Borner
-- BLoC format is used in combination with BLoCade the BLoC-aid.
module Target
  ( toTarget
  ) where

import           Control.Exception
import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy.Char8    as Byte
import           System.IO
import           System.Process

import           Binary
import           Config
import           Error
import           Helper

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

compile :: Expression -> String -> IO (Failable String)
compile e target = do
  res <- tryIO $ createProcess
    (shell $ "bloc -v -i - --from-blc | blocade -v -i - -t " <> target)
      { std_in  = CreatePipe
      , std_out = CreatePipe
      }
  case res of
    Right (Just inH, Just outH, _, _) -> do
      let binary = toBinary e
      hSetBuffering inH  NoBuffering
      hSetBuffering outH NoBuffering
      hSetBinaryMode outH True
      hIsOpen inH >>= print
      hIsWritable inH >>= print
      putStrLn "sending binary"
      hPutStrLn inH binary
      hPutStr inH "\0"
      hFlush inH
      content <- hGetContents' outH
      hClose outH
      hClose inH
      return $ case content of
        "" -> Left $ OptimizerError "blocade returned empty string"
        _  -> Right content
    _ -> return $ Left $ OptimizerError "can't read from blocade"

-- TODO: add more targets (including other PL compilation)
toExpression :: String -> String -> Expression
toExpression "blc"   = fromBinary
toExpression "unblc" = fromBinary
toExpression "bblc" =
  fromBinary . fromBitString . Bit.bitStringLazy . Byte.pack
toExpression "unbblc" =
  fromBinary . fromBitString . Bit.bitStringLazy . Byte.pack
toExpression _ = invalidProgramState

toTarget :: EvalConf -> Expression -> IO Expression
toTarget conf e = do
  let target = _toTarget conf
  case target of
    "" -> return e -- No target, fallback to unoptimized expression
    _  -> do
      maybeRes <- compile e target
      case maybeRes of
        Left err -> do
          print err
          return e -- Fallback to unoptimized expression
        Right res -> return $ toExpression target res

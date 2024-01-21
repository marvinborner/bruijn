-- MIT License, Copyright (c) 2024 Marvin Borner
-- Ultimately, the optimizer should not be outsourced but handled directly in Haskell.
-- For now, my BLoC format is used in combination with BLoCade the BLoC-aid.

module Optimizer
  ( optimizeToTarget
  , optimizedReduce
  ) where

import           Binary
import           Control.Exception
import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy.Char8    as Byte
import           Helper
import           Reducer
import           System.IO
import           System.Process

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

toBloc :: Expression -> IO (Failable Handle)
toBloc e = do
  let binary = toBinary e
  tryBloc <- tryIO $ createProcess (proc "bloc" ["-i", "-", "--from-blc"])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    }
  let bloc = case tryBloc of
        Right (Just i, Just o, _, _) -> Just (i, o)
        _                            -> Nothing
  case bloc of
    Just (inH, outH) -> do
      hPutStrLn inH binary
      return $ Right outH
    Nothing -> return $ Left $ OptimizerError "can't read/write to bloc"

toTarget :: Expression -> String -> IO (Failable String)
toTarget e target = do
  maybeBloc <- toBloc e
  case maybeBloc of
    Left  err  -> return $ Left err
    Right bloc -> do
      res <- tryIO $ createProcess (proc "blocade" ["-i", "-", "-t", target])
        { std_in  = UseHandle bloc
        , std_out = CreatePipe
        }
      let out = case res of
            Left  _            -> Nothing
            Right (_, o, _, _) -> o
      case out of
        Just h -> do
          hSetBinaryMode h True
          content <- hGetContents h
          return $ case content of
            "" -> Left $ OptimizerError "blocade returned empty string"
            _  -> Right content
        Nothing -> return $ Left $ OptimizerError "can't read from blocade"

-- TODO: add more targets (including other PL compilation)
toExpression :: String -> String -> Expression
toExpression "blc"   = fromBinary
toExpression "unblc" = fromBinary
toExpression "bblc" =
  fromBinary . fromBitString . Bit.bitStringLazy . Byte.pack
toExpression "unbblc" =
  fromBinary . fromBitString . Bit.bitStringLazy . Byte.pack
toExpression _ = invalidProgramState

optimizeToTarget :: EvalConf -> Expression -> IO Expression
optimizeToTarget conf e = do
  let target = _optimizeTarget conf
  case target of
    "" -> return e -- No target, fallback to unoptimized expression
    _  -> do
      maybeRes <- toTarget e target
      case maybeRes of
        Left err -> do
          print err
          return e -- Fallback to unoptimized expression
        Right res -> return $ toExpression target res

optimizedReduce :: EvalConf -> Expression -> IO Expression
optimizedReduce conf e = optimizeToTarget conf e >>= reduce


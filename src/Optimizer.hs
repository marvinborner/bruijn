-- MIT License, Copyright (c) 2024 Marvin Borner
-- Ultimately, the optimizer should not be outsourced but handled directly in Haskell.
-- For now, my BLoC format is used in combination with BLoCade the BLoC-aid.

module Optimizer
  ( optimizedReduce
  ) where

import           Binary
import           Control.Exception
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
      blc <- tryIO $ createProcess (proc "blocade" ["-i", "-", "-t", target])
        { std_in  = UseHandle bloc
        , std_out = CreatePipe
        }
      let out = case blc of
            Left  _            -> Nothing
            Right (_, o, _, _) -> o
      case out of
        Just h -> do
          content <- hGetContents h
          return $ case content of
            "" -> Left $ OptimizerError "blocade returned empty string"
            _  -> Right content
        Nothing -> return $ Left $ OptimizerError "can't read from blocade"

optimizeToTarget :: Expression -> String -> IO Expression
optimizeToTarget e target = do
  maybeBlc <- toTarget e target
  case maybeBlc of
    Left err -> do
      print err
      reduce e -- Fallback to default reducer
    Right blc -> reduce $ fromBinary blc

-- TODO: add more targets (including other PL compilation)
optimizedReduce :: EvalConf -> Expression -> IO Expression
optimizedReduce conf e = do
  let target = _optimizeTarget conf
  case target of
    "" -> reduce e -- No target, fallback to default reducer
    _  -> optimizeToTarget e target

-- MIT License, Copyright (c) 2022 Marvin Borner
module Config where

import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )

data ArgMode = ArgEval | ArgEvalBblc | ArgEvalBlc | ArgDumpBblc | ArgDumpBlc

data Args = Args
  { _argMode     :: ArgMode
  , _argNoTests  :: Bool
  , _argVerbose  :: Bool
  , _argOptimize :: Bool
  , _argToTarget :: String
  , _argReducer  :: String
  , _argPath     :: Maybe String
  }

data EvalConf = EvalConf
  { _isRepl    :: Bool
  , _isVerbose :: Bool
  , _evalTests :: Bool
  , _optimize  :: Bool
  , _nicePath  :: String
  , _path      :: String
  , _evalPaths :: [String]
  , _toTarget  :: String
  , _reducer   :: String
  , _hasArg    :: Bool
  }

argsToConf :: Args -> EvalConf
argsToConf args = EvalConf { _isRepl    = isNothing $ _argPath args
                           , _isVerbose = _argVerbose args
                           , _evalTests = not $ _argNoTests args
                           , _optimize  = _argOptimize args
                           , _path      = path
                           , _nicePath  = path
                           , _evalPaths = []
                           , _toTarget  = _argToTarget args
                           , _reducer   = _argReducer args
                           , _hasArg    = False
                           }
  where path = fromMaybe "" (_argPath args)

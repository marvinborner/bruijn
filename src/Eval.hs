-- MIT License, Copyright (c) 2022 Marvin Borner
module Eval
  ( evalMain
  ) where

import           Binary
import           Control.Concurrent
import           Control.DeepSeq                ( deepseq )
import           Control.Exception
import           Control.Monad.State
import qualified Control.Monad.State.Strict    as StrictState
import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy          as Byte
import           Data.Function                  ( on )
import           Data.Functor
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Time.Clock
import           Helper
import           Optimizer
import           Parser
import           Paths_bruijn
import           Reducer
import           System.Clock
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath.Posix          ( takeBaseName )
import           System.Mem
import           Text.Megaparsec         hiding ( State
                                                , try
                                                )

data EnvState = EnvState
  { _env   :: Environment
  , _conf  :: EvalConf
  , _cache :: EnvCache
  }

type M = StrictState.StateT EnvState IO

entryFunction :: Identifier
entryFunction = NormalFunction "main"

-- why isn't this in Prelude??
split :: (Eq a) => [a] -> [a] -> [[a]]
split _  [] = []
split [] x  = map (: []) x
split a@(_ : _) b@(c : _)
  | Just suffix <- a `stripPrefix` b = [] : split a suffix
  | null rest                        = [[c]]
  | otherwise                        = (c : head rest) : tail rest
  where rest = split a $ tail b

-- TODO: Force naming convention for namespaces/files
loadFile :: EvalConf -> EnvCache -> IO EnvState
loadFile conf cache = do
  f <- try $ readFile (_path conf) :: IO (Either IOError String)
  case f of
    Left exception ->
      print
          (ContextualError (ImportError $ show (exception :: IOError))
                           (Context "" $ _nicePath conf)
          )
        >> pure (EnvState (Environment M.empty) conf cache)
    Right f' -> eval
      (filter (not . null) $ split "\n\n" f')
      (EnvState
        (Environment M.empty)
        (conf { _isRepl = False, _evalPaths = _path conf : _evalPaths conf })
        cache
      )

evalFun :: Identifier -> Environment -> EvalState (Failable Expression)
evalFun fun (Environment sub) = state $ \env@(Environment e) ->
  let lookup' env' = case M.lookup fun env' of
        Nothing                    -> Left $ UndefinedIdentifier fun
        Just (EnvDef { _exp = x }) -> Right x
      matching n
        | null e = "<no idea>"
        | otherwise = snd $ minimumBy (compare `on` fst) $ map
          (\f -> (levenshtein (functionName f) n, show f))
          (M.keys e)
      suggest (Left u@(UndefinedIdentifier n)) =
        Left $ SuggestSolution u (matching $ functionName n)
      suggest x = x
  in  case lookup' sub of -- search in sub env
        s@(Right _) -> (s, env)
        _           -> (suggest $ lookup' e, env) -- search in global env

evalAbs :: Expression -> Environment -> EvalState (Failable Expression)
evalAbs e sub = evalExp e sub <&> fmap Abstraction

evalApp
  :: Expression -> Expression -> Environment -> EvalState (Failable Expression)
evalApp f g sub = evalExp f sub >>= \case
  Left  e  -> pure $ Left e
  Right f' -> fmap (Application f') <$> evalExp g sub

-- TODO: This could be nicer and more performant (current is redundantly recursive)
evalMixfix :: [Mixfix] -> Environment -> EvalState (Failable Expression)
evalMixfix m sub = resolve (mixfixKind m) mixfixArgs
 where
  longestMatching [] = pure $ Left $ UnmatchedMixfix (mixfixKind m) m
  longestMatching x  = evalFun (MixfixFunction x) sub >>= \case
    Left  _ -> longestMatching $ init x
    Right _ -> pure $ Right $ Function $ MixfixFunction x
  holeCount f = length [ h | h@MixfixNone <- f ]
  resolve f args
    | null [ s | s@(MixfixSome _) <- f ] = evalExp (foldl1 Application args) sub
    | otherwise = longestMatching f >>= \case
      e@(Left _) -> pure e
      Right l@(Function (MixfixFunction l')) ->
        let splitted = take (holeCount l') args
            chainRst = drop (length l') m
        in  case chainRst of
              [] -> evalExp (foldl1 Application $ l : splitted) sub
              _  -> evalExp
                ( MixfixChain
                $ MixfixExpression (foldl1 Application $ l : splitted)
                : chainRst
                )
                sub
      _ -> invalidProgramState
  mixfixArgs = [ a | (MixfixExpression a) <- m ]
  mixfixKind = map $ \case
    MixfixOperator i -> MixfixSome $ functionName i
    _                -> MixfixNone

evalPrefix
  :: Identifier -> Expression -> Environment -> EvalState (Failable Expression)
evalPrefix p e = evalExp $ Application (Function p) e

evalQuote :: Expression -> Environment -> EvalState (Failable Expression)
evalQuote f sub = evalExp f sub >>= \case
  Left  e  -> pure $ Left e
  Right f' -> pure $ Right $ quotify 0 f'
   where
    base l r = Abstraction $ Abstraction $ Abstraction $ Application l r
    quotify n (Abstraction e) = base (Bruijn 0) (quotify (n + 1) e)
    quotify n (Application l r) =
      base (Application (Bruijn 1) (quotify (n + 1) l)) (quotify (n + 1) r)
    quotify _ (Bruijn i) = base (Bruijn 2) (decimalToUnary $ fromIntegral i)
    quotify n (Unquote (Bruijn i)) = Bruijn $ n * 3 + i
    quotify n (Unquote e         ) = quotify n e
    quotify _ _                    = invalidProgramState

evalUnquote :: Expression -> Environment -> EvalState (Failable Expression)
evalUnquote f sub = evalExp f sub >>= \case
  Left  e  -> pure $ Left e
  Right f' -> pure $ Right $ Unquote $ unsafeReduce f' -- TODO: REMOVE UNSAFE

evalExp :: Expression -> Environment -> EvalState (Failable Expression)
evalExp idx@(Bruijn      _  ) = const $ pure $ Right idx
evalExp (    Function    fun) = evalFun fun
evalExp (    Abstraction e  ) = evalAbs e
evalExp (    Application f g) = evalApp f g
evalExp (    MixfixChain es ) = evalMixfix es
evalExp (    Prefix p e     ) = evalPrefix p e
evalExp (    Quote   e      ) = evalQuote e
evalExp (    Unquote e      ) = evalUnquote e

evalDefinition
  :: Identifier -> Expression -> Environment -> EvalState (Failable Expression)
evalDefinition i e sub = evalExp e sub >>= \case
  Left e' -> pure $ Left e'
  Right f ->
    modify
        (\(Environment s) -> Environment
          $ M.insert i (EnvDef f (Environment M.empty) defaultFlags) s
        )
      >> pure (Right f)

evalTest
  :: Expression -> Expression -> Environment -> EvalState (Failable Command)
evalTest e1 e2 sub = evalExp e1 sub >>= \case
  Left  err -> pure $ Left err
  Right e1' -> fmap (Test e1') <$> evalExp e2 sub

evalSubEnv :: [Instruction] -> EnvState -> IO EnvState
evalSubEnv [] s = return s
evalSubEnv (instr : is) s =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ evalInstruction instr s (evalSubEnv is)

fullPath :: String -> IO String
fullPath path = do
  lib    <- getDataFileName path -- TODO: Use actual lib directory
  exists <- doesFileExist lib
  pure $ if exists then lib else path

evalCommand :: String -> EnvState -> Command -> IO EnvState
evalCommand inp s@(EnvState env@(Environment envDefs) conf cache) = \case
  Input path -> do
    full <- fullPath path
    if full `elem` _evalPaths conf
      then
        print
            (ContextualError (ImportError path) (Context inp $ _nicePath conf))
          >> pure s
      else if M.member path (_imported cache)
        then
          let (Environment env') = fromJust $ M.lookup path (_imported cache)
          in  pure $ s { _env = Environment $ M.union env' envDefs }
        else do
          EnvState (Environment env') _ cache' <- loadFile
            (conf { _nicePath = path, _path = full })
            cache -- TODO: Fix wrong `within` in import error
          let cache'' = cache
                { _imported = M.insert path (Environment env')
                                $ M.union (_imported cache) (_imported cache')
                }
          pure $ EnvState (Environment $ M.union env' envDefs) conf cache'' -- import => _isRepl = False
  Watch path ->
    let
      monitor mtime = do
        threadDelay 100000
        full <- fullPath path
        t    <- getModificationTime full
        if t > mtime
          then
            putStrLn "\ESC[2Jreload"
            >> evalCommand inp s (Input full)
            >> monitor t
          else monitor t
    in  getCurrentTime >>= monitor
  Import path namespace -> do
    -- TODO: Merge with Input (very similar)
    full <- fullPath path
    if full `elem` _evalPaths conf
      then
        print
            (ContextualError (ImportError path) (Context inp $ _nicePath conf))
          >> pure s
      else if M.member path (_imported cache)
        then -- load from cache

          let
            (Environment env') = fromJust $ M.lookup path (_imported cache)
            prefix | null namespace   = takeBaseName path ++ "."
                   | namespace == "." = ""
                   | otherwise        = namespace ++ "."
            rewriteKeys "" = id
            rewriteKeys p  = M.mapKeys $ \f -> NamespacedFunction p f
            rewriteFuns =
              M.map $ \d -> d { _flags = (_flags d) { _isImported = True } }
            filterImported =
              M.filter $ \(EnvDef { _flags = f }) -> not $ _isImported f
            env'' = rewriteFuns $ rewriteKeys prefix $ filterImported env'
          in
            pure $ s { _env = Environment $ M.union env'' envDefs }
        else do
          EnvState (Environment env') _ cache' <- loadFile
            (conf { _nicePath = path, _path = full })
            cache -- TODO: Fix wrong `within` in import error
          let
            cache'' = cache
              { _imported = M.insert path (Environment env')
                              $ M.union (_imported cache) (_imported cache')
              }
            prefix | null namespace   = takeBaseName path ++ "."
                   | namespace == "." = ""
                   | otherwise        = namespace ++ "."
            rewriteKeys "" = id
            rewriteKeys p  = M.mapKeys $ \f -> NamespacedFunction p f
            rewriteFuns =
              M.map $ \d -> d { _flags = (_flags d) { _isImported = True } }
            filterImported =
              M.filter $ \(EnvDef { _flags = f }) -> not $ _isImported f
            env'' = rewriteFuns $ rewriteKeys prefix $ filterImported env'
          pure $ EnvState (Environment $ M.union env'' envDefs) conf cache'' -- import => _isRepl = False
  Test e1 e2
    | _evalTests conf
    -> let (res, _) = evalTest e1 e2 (Environment M.empty) `runState` env
       in  case res of
             Left err ->
               print (ContextualError err $ Context inp $ _nicePath conf)
                 >> pure s
             Right (Test e1' e2') -> do
               lhs <- reduce e1'
               rhs <- reduce e2'
               when (lhs /= rhs) (print $ FailedTest e1 e2 lhs rhs) >> pure s
             _ -> pure s
    | otherwise
    -> pure s
  ClearState -> do
    -- TODO: actually free memory :/
    putStr "Currently allocated: "
    getAllocationCounter >>= putStr . show . (0 -)
    putStrLn " Byte"
    performGC
    pure $ EnvState (Environment M.empty) conf (EnvCache M.empty)
  Length e -> do
    let (res, _) = evalExp e (Environment M.empty) `runState` env
    case res of
      Left  err -> print err
      Right e'  -> do
        print $ length $ toBinary e'
        red <- optimizedReduce conf e'
        print $ length $ toBinary red
    pure s
  Blc e -> do
    let (res, _) = evalExp e (Environment M.empty) `runState` env
    case res of
      Left  err -> print err
      Right e'  -> do
        putStrLn $ toBinary e'
        red <- optimizedReduce conf e'
        putStrLn $ toBinary red
    pure s
  Jot str -> do
    let e        = fromJot str
    let (res, _) = evalExp e (Environment M.empty) `runState` env
    case res of
      Left  err -> print err
      Right e'  -> do
        print e
        print e'
        print $ length $ toBinary e'
        red <- optimizedReduce conf e'
        print red
        print $ length $ toBinary red
    pure s
  Time e -> do
    start <- getTime Monotonic
    let (res, _) = evalExp e (Environment M.empty) `runState` env
    end <- case res of
      Left  err -> print err >> getTime Monotonic
      Right e'  -> do -- TODO: should timing not include optimization time?
        red <- optimizedReduce conf e'
        deepseq red (getTime Monotonic)
    let roundSecs x = fromIntegral (round $ x * 1e6 :: Integer) / 1e6 :: Double
    putStr
      $ show
      $ roundSecs
      $ (fromIntegral $ toNanoSecs $ diffTimeSpec start end :: Double)
      / 1e9
    putStrLn " seconds"
    pure s

showResult :: Expression -> Environment -> IO ()
showResult reduced env =
  let matching   = matchingFunctions reduced env
      humanified = humanifyExpression reduced
  in  putStrLn
        $  "*> "
        <> show reduced
        <> (if null humanified then "" else "\n?> " <> humanified)
        <> (if null matching then "" else "\n#> " <> matching)

evalInstruction
  :: Instruction -> EnvState -> (EnvState -> IO EnvState) -> IO EnvState
evalInstruction (ContextualInstruction instr inp) s@(EnvState env conf _) rec =
  case instr of
    Define i e sub -> do
      EnvState subEnv _     _ <- evalSubEnv sub s
      (        res    , env') <- pure $ evalDefinition i e subEnv `runState` env
      case res of
        Left err ->
          print (ContextualError err $ Context inp $ _nicePath conf) >> pure s -- don't continue
        Right _
          | _isRepl conf -> putStrLn (show i <> " = " <> show e)
          >> return s { _env = env' }
          | otherwise -> rec s { _env = env' }
    Evaluate e ->
      let (res, _) = evalExp e (Environment M.empty) `runState` env
      in  (case res of
            Left  err -> print err >> rec s
            Right e'  -> do
              red <- optimizedReduce conf e'
              showResult red env
              rec s
          )
    Commands cs -> yeet (pure s) cs >>= rec
     where
        -- TODO: sus
      yeet s' []        = s'
      yeet s' (c : cs') = do
        s'' <- s'
        yeet (evalCommand inp s'' c) cs'
    _ -> rec s
evalInstruction instr s rec =
  evalInstruction (ContextualInstruction instr "<unknown>") s rec

eval :: [String] -> EnvState -> IO EnvState
eval []   s = return s
eval [""] s = return s
eval (block : bs) s@(EnvState _ conf _) =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ case parse blockParser "" block of
        Left err ->
          print
              (ContextualError (SyntaxError $ printBundle err)
                               (Context "" $ _nicePath conf)
              )
            >> eval bs s
        Right instr -> evalInstruction instr s (eval bs)
 where
  blockParser | _isRepl conf = parseReplLine
              | otherwise    = parseBlock 0

dumpFile :: EvalConf -> (a -> IO ()) -> (Expression -> a) -> IO ()
dumpFile conf wr conv = do
  EnvState (Environment env) _ _ <- loadFile conf (EnvCache M.empty)
  case M.lookup entryFunction env of
    Nothing -> print $ ContextualError (UndefinedIdentifier entryFunction)
                                       (Context "" (_nicePath conf))
    Just EnvDef { _exp = e } -> optimizeToTarget conf e >>= wr . conv

evalFileConf :: EvalConf -> IO ()
evalFileConf conf = do
  EnvState (Environment env) _ _ <- loadFile conf (EnvCache M.empty)
  arg                            <- encodeStdin
  case M.lookup entryFunction env of
    Nothing -> print $ ContextualError (UndefinedIdentifier entryFunction)
                                       (Context "" (_nicePath conf))
    Just EnvDef { _exp = e } -> do
      red <- optimizedReduce conf (Application e arg)
      showResult red (Environment env)

exec :: EvalConf -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec conf rd conv = do
  f   <- rd (_path conf)
  arg <- encodeStdin
  case f of
    Left  exception -> print (exception :: IOError)
    Right f'        -> do
      red <- optimizedReduce conf (Application e arg)
      showResult red (Environment M.empty)
      where e = fromBinary $ conv f'

repl :: EnvState -> InputT M ()
repl (EnvState env conf cache) =
  handleInterrupt (return $ Just "")
                  (withInterrupt $ getInputLine "\ESC[36mλ\ESC[0m ")
    >>= \case
      -- TODO: Add non-parser error support for REPL
          Nothing   -> return ()
          Just line -> do
            -- setting imported [] for better debugging
            s' <- liftIO
              $ eval [line] (EnvState env conf cache { _imported = M.empty })
            lift $ StrictState.put s'
            repl s'

lookupCompletion :: String -> M [Completion]
lookupCompletion str = do
  (EnvState (Environment env) _ _) <- StrictState.get
  return $ map (\s -> Completion s s False) $ filter
    (\s -> str `isPrefixOf` s)
    (map functionName (M.keys env))

completionSettings :: String -> Settings M
completionSettings history = Settings
  { complete       = completeWord Nothing " \n" lookupCompletion
  , historyFile    = Just history
  , autoAddHistory = True
  }

runRepl :: EvalConf -> IO ()
runRepl conf = do
  config  <- getDataFileName "config"
  history <- getDataFileName "history"
  prefs   <- readPrefs config
  let looper = runInputTWithPrefs
        prefs
        (completionSettings history)
        (withInterrupt $ repl $ EnvState (Environment M.empty)
                                         conf
                                         (EnvCache M.empty)
        )
  StrictState.evalStateT
    looper
    (EnvState (Environment M.empty) conf (EnvCache M.empty))

evalMain :: Args -> IO ()
evalMain args = do
  let conf = argsToConf args
  case _argMode args of
    ArgEval | _isRepl conf -> runRepl conf
    ArgEval | otherwise    -> evalFileConf conf
    ArgDumpBblc            -> dumpFile conf
                                       (Byte.putStr . Bit.realizeBitStringLazy)
                                       (toBitString . toBinary)
    ArgDumpBlc -> dumpFile conf putStrLn toBinary
    ArgEvalBblc ->
      exec conf (try . Byte.readFile) (fromBitString . Bit.bitStringLazy)
    ArgEvalBlc -> exec conf (try . readFile) id

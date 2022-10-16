module Eval
  ( evalMain
  ) where

import           Binary
import           Control.Exception
import           Control.Monad.State
import qualified Control.Monad.State.Strict    as StrictState
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import           Data.Function                  ( on )
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Helper
import           Parser
import           Paths_bruijn
import           Reducer
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.FilePath.Posix          ( takeBaseName )
import           Text.Megaparsec         hiding ( State
                                                , try
                                                )
import           Typer

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
loadFile :: String -> EvalConf -> EnvCache -> IO EnvState
loadFile path conf cache = do
  f <- try $ readFile path :: IO (Either IOError String)
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
        (conf { _isRepl = False, _evalPaths = (path : (_evalPaths conf)) })
        cache
      )

evalFun :: Identifier -> Environment -> EvalState (Failable Expression)
evalFun fun (Environment sub) = state $ \env@(Environment e) ->
  let lookup' env' = case M.lookup fun env' of
        Nothing                               -> Left $ UndefinedIdentifier fun
        Just (EnvDef { _exp = x, _type = t }) -> Right $ TypedExpression t x
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
evalAbs e sub = evalExp e sub >>= pure . fmap Abstraction

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
  holeCount f = length [ h | h@(MixfixNone) <- f ]
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
                $ (MixfixExpression $ foldl1 Application $ l : splitted)
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
-- IDEA: typing and reduce if all arguments are fulfilled
-- evalPrefix (PrefixFunction "⊩") e sub = evalExp e sub >>= \case
--   Left  e' -> pure $ Left e'
--   Right e' -> pure $ Right $ reduce e'
-- evalPrefix p e sub = evalExp (Application (Function p) e) sub
evalPrefix p e = evalExp $ Application (Function p) e

evalExp :: Expression -> Environment -> EvalState (Failable Expression)
evalExp idx@(Bruijn      _      ) = const $ pure $ Right idx
evalExp (    Function    fun    ) = evalFun fun
evalExp (    Abstraction e      ) = evalAbs e
evalExp (    Application f g    ) = evalApp f g
evalExp (    MixfixChain es     ) = evalMixfix es
evalExp (    Prefix          p e) = evalPrefix p e
evalExp (    TypedExpression t e) = error "invalid"

evalDefinition
  :: Identifier
  -> Expression
  -> Type
  -> Environment
  -> EvalState (Failable Expression)
evalDefinition i e t sub = evalExp e sub >>= \case
  Left  e' -> pure $ Left e'
  Right f  -> case typeCheck f t of
    err@(Left _) -> pure err
    Right f' ->
      modify
          (\(Environment s) -> Environment
            $ M.insert i (EnvDef f' t (Environment M.empty) defaultFlags) s
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
            full
            (conf { _nicePath = path })
            cache -- TODO: Fix wrong `within` in import error
          cache'' <- pure $ cache
            { _imported = M.insert path (Environment env')
                            $ M.union (_imported cache) (_imported cache')
            }
          pure $ EnvState (Environment $ M.union env' envDefs) conf cache'' -- import => _isRepl = False
  Import path namespace -> do -- TODO: Merge with Input (very similar)
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
              M.filter $ \(EnvDef { _flags = f }) -> _isImported f == False
            env'' = rewriteFuns $ rewriteKeys prefix $ filterImported env'
          in
            pure $ s { _env = Environment $ M.union env'' envDefs }
        else do
          EnvState (Environment env') _ cache' <- loadFile
            full
            (conf { _nicePath = path })
            cache -- TODO: Fix wrong `within` in import error
          cache'' <- pure $ cache
            { _imported = M.insert path (Environment env')
                            $ M.union (_imported cache) (_imported cache')
            }
          let
            prefix | null namespace   = takeBaseName path ++ "."
                   | namespace == "." = ""
                   | otherwise        = namespace ++ "."
            rewriteKeys "" = id
            rewriteKeys p  = M.mapKeys $ \f -> NamespacedFunction p f
            rewriteFuns =
              M.map $ \d -> d { _flags = (_flags d) { _isImported = True } }
            filterImported =
              M.filter $ \(EnvDef { _flags = f }) -> _isImported f == False
          env'' <- pure $ rewriteFuns $ rewriteKeys prefix $ filterImported env'
          pure $ EnvState (Environment $ M.union env'' envDefs) conf cache'' -- import => _isRepl = False
  Test e1 e2
    | _evalTests conf
    -> let (res, _) = evalTest e1 e2 (Environment M.empty) `runState` env
       in
         case res of
           Left err ->
             print (ContextualError err $ Context inp $ _nicePath conf)
               >> pure s
           Right (Test e1' e2') ->
             when (lhs /= rhs) (print $ FailedTest e1 e2 lhs rhs) >> pure s
            where
             lhs = reduce e1'
             rhs = reduce e2'
           _ -> pure s
    | otherwise
    -> pure s

-- TODO: Reduce redundancy
showResult :: Expression -> Expression -> Environment -> IO ()
showResult orig reduced env =
  putStrLn
    $  "<> "
    <> (show orig)
    <> "\n*> "
    <> (show reduced)
    <> "\n?> "
    <> (humanifyExpression reduced)
    <> "\n#> "
    <> (matchingFunctions reduced env)

evalInstruction
  :: Instruction -> EnvState -> (EnvState -> IO EnvState) -> IO EnvState
evalInstruction (ContextualInstruction instr inp) s@(EnvState env conf _) rec =
  case instr of
    Define i e t sub -> do
      EnvState subEnv _ _ <- evalSubEnv sub s
      (res, env') <- pure $ evalDefinition i e t subEnv `runState` env
      case res of
        Left err ->
          print (ContextualError err $ Context inp $ _nicePath conf) >> pure s -- don't continue
        Right _
          | _isRepl conf -> (putStrLn $ show i <> " = " <> show e)
          >> return s { _env = env' }
          | otherwise -> rec s { _env = env' }
    Evaluate e ->
      let (res, _) = evalExp e (Environment M.empty) `runState` env
      in  (case res of
            Left  err -> print err
            Right e'  -> showResult e' (reduce e') env
          )
            >> rec s
    Commands cs -> yeet (pure s) cs >>= rec
     where -- TODO: sus
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

dumpFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
dumpFile path wr conv = do
  EnvState (Environment env) _ _ <- loadFile path
                                             (defaultConf path)
                                             (EnvCache M.empty)
  case M.lookup entryFunction env of
    Nothing -> print
      $ ContextualError (UndefinedIdentifier entryFunction) (Context "" path)
    Just EnvDef { _exp = e } -> wr $ conv e

evalFileConf :: String -> EvalConf -> IO ()
evalFileConf path conf = do
  EnvState (Environment env) _ _ <- loadFile path conf (EnvCache M.empty)
  arg                            <- encodeStdin
  case M.lookup entryFunction env of
    Nothing -> print
      $ ContextualError (UndefinedIdentifier entryFunction) (Context "" path)
    Just EnvDef { _exp = e } ->
      showResult e (reduce $ Application e arg) (Environment env)

evalFile :: String -> IO ()
evalFile path = evalFileConf path (defaultConf path)

-- TODO: Merge with evalFile
evalYolo :: String -> IO ()
evalYolo path = evalFileConf path (defaultConf path) { _evalTests = False }

exec :: String -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec path rd conv = do
  f   <- rd path
  arg <- encodeStdin
  case f of
    Left exception -> print (exception :: IOError)
    Right f' -> showResult e (reduce $ Application e arg) (Environment M.empty)
      where e = fromBinary $ conv f'

repl :: EnvState -> InputT M ()
repl (EnvState env conf cache) =
  (handleInterrupt (return $ Just "") $ withInterrupt $ getInputLine
      "\ESC[36mλ\ESC[0m "
    )
    >>= \case -- TODO: Add non-parser error support for REPL
          Nothing   -> return ()
          Just line -> do -- setting imported [] for better debugging
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

runRepl :: IO ()
runRepl = do
  config  <- getDataFileName "config"
  history <- getDataFileName "history"
  prefs   <- readPrefs config
  let -- TODO: Use -y in repl for YOLO lifestyle
      conf = EvalConf { _isRepl    = True
                      , _evalTests = True
                      , _nicePath  = "<repl>"
                      , _evalPaths = []
                      }
      looper = runInputTWithPrefs
        prefs
        (completionSettings history)
        (withInterrupt $ repl $ EnvState (Environment M.empty)
                                         conf
                                         (EnvCache M.empty)
        )
  code <- StrictState.evalStateT
    looper
    (EnvState (Environment M.empty) conf (EnvCache M.empty))
  return code

usage :: IO ()
usage = do
  putStrLn "Invalid arguments. Use 'bruijn [option] path' instead"
  putStrLn "-o\toptimize path"
  putStrLn "-c\tcompress path to binary-BLC"
  putStrLn "-C\tcompress path to ASCII-BLC"
  putStrLn "-b\tcompile path to binary-BLC"
  putStrLn "-B\tcompile path to ASCII-BLC"
  putStrLn "-e\texecute path as binary-BLC"
  putStrLn "-E\texecute path as ASCII-BLC"
  putStrLn "-y\tdisable execution of tests - YOLO"
  putStrLn "-*\tshow this help"
  putStrLn "<default>\texecute path as text-bruijn"

evalMain :: IO ()
evalMain = do
  -- TODO: use actual args parser
  args <- getArgs
  case args of
    []           -> runRepl
    ["-b", path] -> dumpFile path
                             (Byte.putStr . Bit.realizeBitStringStrict)
                             (toBitString . toBinary)
    ["-B", path] -> dumpFile path putStrLn toBinary
    ["-e", path] ->
      exec path (try . Byte.readFile) (fromBitString . Bit.bitString)
    ["-E", path] -> exec path (try . readFile) id
    ["-y", path] -> evalYolo path
    ['-' : _]    -> usage
    [path   ]    -> evalFile path
    _            -> usage

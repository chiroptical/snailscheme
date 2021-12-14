{-# LANGUAGE ScopedTypeVariables #-}

module Evaluation where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LispVal
import Parser (readExpr, readExprFile)
import System.Directory (doesFileExist)
import Text.Parsec (ParseError (..))

import Control.Exception (
    Exception (fromException),
    SomeException,
    throw,
    try,
 )
import Control.Monad.Reader (
    MonadIO (liftIO),
    MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
 )
import Prim

evalText :: T.Text -> IO () --REPL
evalText textExpr = do
    stdlib <- getFileContents sTDLIB
    res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
    print res

textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show) evalBody $ parseWithLib std input

parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
    stdlib <- readExprFile sTDLIB std
    expr <- readExpr inp
    return $ endOfList stdlib expr

getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
    exists <- doesFileExist fname
    if exists then TIO.readFile fname else return "File does not exist."

sTDLIB :: FilePath
sTDLIB = "lib/stdlib.scm"

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _ = throw $ TypeMismatch "failure to get variable: " n

safeExec :: IO a -> IO (Either String a)
safeExec m = do
    result <- Control.Exception.try m
    case result of
        Left (eTop :: SomeException) ->
            case fromException eTop of
                Just (enclosed :: LispException) -> return $ Left (show enclosed)
                Nothing -> return $ Left (show eTop)
        Right val -> return $ Right val

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val = throw $ TypeMismatch "read expects string, instead got: " val

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) eval $ readExpr input

basicEnv :: Map.Map T.Text LispVal
basicEnv =
    Map.fromList $
        primEnv
            <> [("read", Fun . IFunc $ unop readFn)]

evalFile :: FilePath -> T.Text -> IO () --program file
evalFile filePath fileExpr = runASTinEnv basicEnv (fileToEvalForm filePath fileExpr) >>= print

fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input = either (throw . PError . show) evalBody $ readExprFile filePath input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input =
    either
        (T.pack . show)
        (T.pack . show)
        $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    ctx <- ask
    local (const $ updateEnv var evalVal ctx) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    ctx <- ask
    local (const $ updateEnv var evalVal ctx) $ evalBody $ List rest
evalBody x = eval x

updateEnv :: T.Text -> LispVal -> EnvCtx -> EnvCtx
updateEnv var e@(Fun _) env = Map.insert var e env
updateEnv var e@(Lambda _ _) env = Map.insert var e env
updateEnv var e env = Map.insert var e env

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = pure val
-- antiquote matches
eval (Number i) = pure $ Number i
eval (String s) = pure $ String s
eval (Bool b) = pure $ Bool b
eval (List []) = pure Nil
eval Nil = pure Nil
-- printing ast
eval (List [Atom "write", rest]) =
    pure . String . T.pack $ show rest
eval (List (Atom "write" : rest)) =
    pure . String . T.pack . show $ List rest
-- An atom looks up its' definition within the environment
eval n@(Atom _) = getVar n
-- If statement
eval (List [Atom "if", pred, truExpr, flsExpr]) = do
    ifRes <- eval pred
    case ifRes of
        (Bool True) -> eval truExpr
        (Bool False) -> eval flsExpr
        _ -> throw $ BadSpecialForm "if"

-- (let ((a 1) (b 2) (c 3)) (expr...))
eval (List [Atom "let", List pairs, expr]) = do
    env <- ask
    atoms <- mapM ensureAtom $ getEven pairs
    vals <- mapM eval $ getOdd pairs
    let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals)
    -- Combine the let environment with the overall environment to evaluate the expression
    local (env' <>) $ evalBody expr
eval (List [Atom "begin", rest]) = evalBody rest
eval (List (Atom "begin" : rest)) = evalBody $ List rest
eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    env <- ask
    let envFn = const $ Map.insert (extractVar varAtom) evalVal env
    local envFn $ return varExpr
eval (List [Atom "lambda", List params, expr]) = asks (Lambda (IFunc $ applyLambda expr params))
eval (List (Atom "lambda" : _)) = throw $ BadSpecialForm "lambda"
-- (delay x) => (lambda () x)
eval (List [Atom "delay", expr]) = asks (Lambda (IFunc $ applyLambda expr []))
eval (List (Atom "delay" : _)) = throw $ BadSpecialForm "delay"
eval (List (x : xs)) = do
    funVar <- eval x
    xVal <- mapM eval xs
    case funVar of
        (Fun (IFunc internalFn)) -> internalFn xVal
        (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) $ internalfn xVal
        _ -> throw $ NotFunction funVar

-- catch all
eval val = throw $ Default val

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env <- ask
    argEval <- mapM eval args
    let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) params argEval) <> env
    local (const env') $ eval expr

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
    env <- ask
    case Map.lookup atom env of
        Just x -> return x
        Nothing -> throw $ UnboundVar atom
getVar val = throw $ EnvironmentLookupOn val

getEven :: [t] -> [t]
getEven [] = []
getEven (x : xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x : xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar val = throw $ EnvironmentLookupOn val

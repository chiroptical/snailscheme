{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

data LispVal
    = Atom Text
    | List [LispVal]
    | Number Integer
    | String Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
    case val of
        (Atom atom) -> atom
        (String str) -> T.concat ["\"", str, "\""]
        (Number num) -> T.pack $ show num
        (Bool True) -> "#t"
        (Bool False) -> "#f"
        Nil -> "Nil"
        (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
        (Fun _) -> "(internal function)"
        (Lambda _ _) -> "(lambda function)"

instance Eq LispVal where
    Atom x == Atom y = x == y
    String x == String y = x == y
    Number x == Number y = x == y
    Bool x == Bool y = x == y
    Nil == Nil = True
    List x == List y = x == y
    _ == _ = False

-- Interface function?
newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
    deriving
        ( Monad
        , Functor
        , Applicative
        , MonadReader EnvCtx
        , MonadIO
        )

module Syntax where

import           Data.List
import           Lexer

type Index = Int

type Name = String

type FileInfo = AlexPosn

type TypeContext = [(Name, Type)]

type NameContext = [Name]

type BindingContext = [Binding]

data Binding
  = TyVarBind {getName :: Name}
  | TmVarBind {getName :: Name, getType :: Type}
  | TmVarNoBind {getName :: Name}
  deriving (Eq, Show)

data TermNode = TermNode
  { getFI :: FileInfo,
    getTm :: Term
  }
  deriving (Eq, Show)

data Term
  = TmVarRaw Name
  | TmVar Index Index Name
  | TmAbs [Binding] [Binding] TermNode
  | TmApp TermNode [Type] [TermNode]
  | TmAppInfer TermNode [TermNode]
  | TmError String
  deriving (Eq, Show)

data Type
  = TyForAll [Binding] [Type] Type
  | TyVarRaw Name
  | TyVar Index Index Name
  | TyBot
  | TyTop
  | TyError String
  deriving (Eq, Show)

getNames :: [Binding] -> [Name]
getNames = map getName

getTypes :: [Binding] -> [Type]
getTypes x = map getType x

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing  = error "fromMaybe, in Syntax.hs"

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

sameLength :: [a] -> [b] -> Bool
sameLength xs ys = length xs == length ys

areAnnotated :: [Binding] -> Bool
areAnnotated bs = and $ map isAnnotated bs

isAnnotated :: Binding -> Bool
isAnnotated b =
  case b of
    TmVarBind _ _ -> True
    _             -> False

getOtherArgs :: [Binding] -> Name -> [Name]
getOtherArgs bs x = getNames bs \\ [x]

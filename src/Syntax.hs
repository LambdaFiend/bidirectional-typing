module Syntax where

import Lexer

type Index = Int
type Name = String
type FileInfo = AlexPosn
type TypeContext = [(Name, Type)]
type NameContext = [Name]

data TermNode = TermNode
  { getFI :: FileInfo
  , getTm :: Term
  } 
  deriving (Eq, Show)

data Term
  = TmVarRaw Name
  | TmVar Index Index Name
  | TmAbs Name TermNode
  | TmApp TermNode TermNode
  | TmUnit
  | TmAnno TermNode Type
  deriving (Eq, Show)

data Type
  = TyUnit
  | TyArrow Type Type
  | TyError String
  deriving (Eq, Show)

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "Oops??? Nothing???"

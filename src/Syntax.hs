module Syntax where

import           Lexer

type Index = Int

type Name = String

type FileInfo = AlexPosn

type TypeContext = [(Name, Type)]

type NameContext = [Name]

type BindingContext = [Binding]

data Binding
  = TyVarBind Name
  | TyVarExistsBind Name
  | TmVarBind Name Type
  | ConstraintBind Name Type
  | MarkerBind Name
  deriving (Eq, Show)

data TermNode = TermNode
  { getFI :: FileInfo,
    getTm :: Term
  }
  deriving (Eq, Show)

data AppliedFunction = AppliedFunction Type TermNode
  deriving (Eq, Show)

data Term
  = TmVarRaw Name
  | TmVar Index Index Name
  | TmAbs Name TermNode
  | TmApp TermNode TermNode
  | TmUnit
  | TmAnno TermNode Type
  | TmError String
  deriving (Eq, Show)

data Type
  = TyUnit
  | TyArrow Type Type
  | TyError String
  | TyVar Index Index Name
  | TyForAll Name Type
  | TyVarExists Name
  | TyVarRaw Name
  deriving (Eq, Show)

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing  = error "fromMaybe, in Syntax.hs"

fromTyVarBind :: Binding -> Name
fromTyVarBind (TyVarBind x) = x

fromTmVarBind :: Binding -> (Name, Type)
fromTmVarBind (TmVarBind name ty) = (name, ty)

fromConstraintBind :: Binding -> (Name, Type)
fromConstraintBind (ConstraintBind x ty1) = (x, ty1)

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

getOuterBindings :: Type -> NameContext
getOuterBindings (TyForAll x ty1) = x : getOuterBindings ty1
getOuterBindings _ = []

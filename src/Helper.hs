module Helper where

import Syntax

import Data.List

newtype UpdatedTmArrTm = UpdatedTmArrTm
  { updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm, TermNode -> UpdatedTmArrTm, Type -> Type) }

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar k l x -> tm
    TmVarRaw x -> tm
    TmAbs x t1 -> TmAbs x (traverseTm' t1)
    TmApp t1 t2 -> TmApp (traverseTm' t1) (traverseTm' t2)
    TmUnit -> tm
    TmAnno t1 ty -> TmAnno (traverseTm' t1) (fTy ty)
  where tm = getTm t'
        fi = getFI t'
        traverseTm'  = traverseDownTm f'
        traverseTm'' = traverseDownTm f''
        (t', f', f'', fTy) = updateTmArrTm $ f t

shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t = let tm = getTm t; fi = getFI t; shift' = shift c d in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d) x, id', id', id :: Type -> Type)
    TmAbs x t1 -> (t, shift (c + 1) d, shift', id :: Type -> Type)
    _ -> (t, shift', shift', id :: Type -> Type)

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t = let tm = getTm t; subst' = subst c j s in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (if k == j + c then shift' 0 (j + c) s else t, id', id', id :: Type -> Type)
    TmAbs x t1 -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
    _ -> (t, subst', subst', id :: Type -> Type)


genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: [Name] -> TermNode -> UpdatedTmArrTm
genIndex ctx t = let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx in
  UpdatedTmArrTm $
  case tm of
    TmVarRaw x | elem x ctx -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx) x, genIndex', genIndex', id :: Type -> Type)
    TmVarRaw x -> errorWithoutStackTrace "The given term has free variables, but they are not allowed in VSBDT"
    TmAbs x t1 -> (TermNode fi $ TmAbs x t1, genIndex (x:ctx), genIndex', id :: Type -> Type)
    _ -> (t, genIndex', genIndex', id :: Type -> Type)

id' :: TermNode -> UpdatedTmArrTm
id' t = UpdatedTmArrTm (t, id', id', id :: Type -> Type)

findDisplayErrors' :: String -> Either String String
findDisplayErrors' s =
  case findDisplayErrors s [] of
    [] -> Right s
    s' -> Left $ intercalate "\n" s'

findDisplayErrors :: String -> String -> [String]
findDisplayErrors [] [] = []
findDisplayErrors [] ys = error "Someone needs to fix this, there's an error message which does not have its identifiers matching."
findDisplayErrors ('#':xs) [] = findDisplayErrors xs ['#']
findDisplayErrors ('#':xs) ys = ((\s -> case s of [] -> []; (x:xs) -> xs) $ reverse ys):findDisplayErrors xs []
findDisplayErrors (x:xs) [] = findDisplayErrors xs []
findDisplayErrors (x:xs) ys = findDisplayErrors xs (x:ys)

findTypeErrors' :: Type -> String
findTypeErrors' t = intercalate "\n" $ findTypeErrors t

findTypeErrors :: Type -> [String]
findTypeErrors t =
  case t of
    TyUnit -> []
    TyArrow ty1 ty2 -> findTypeErrors ty1 ++ findTypeErrors ty2
    TyError e -> [e]

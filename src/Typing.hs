module Typing where

import Syntax
import Display

synth' :: TermNode -> Type
synth' t = synth [] t

synth :: TypeContext -> TermNode -> Type
synth ctx t = let tm = getTm t in
  case tm of
    TmVar l k x | l >= 0 && l < length ctx -> snd $ ctx !! l
    TmApp t1 t2 ->
      case synth ctx t1 of
        TyArrow ty1 ty2 | check ctx t2 ty1 -> ty2
    TmAnno t1 ty1 | check ctx t1 ty1 -> ty1
    _ -> TyError $ showTerm (map fst ctx) t

check :: TypeContext -> TermNode -> Type -> Bool
check ctx t ty = let tm = getTm t in
  case (tm, ty) of
    (TmAbs x t1, TyArrow ty1 ty2) -> check ((x, ty1):ctx) t1 ty2
    (TmUnit, ty1) -> ty1 == TyUnit
    (_, _) -> ty == synth ctx t


module Typing where

import           Data.List
import           Debug.Trace
import           Display
import           Helper
import           Syntax

synth' :: TermNode -> Type
synth' t = synth [] t

-- disgrace! I should have planned how I would handle the scoping. the uncurried form terrorized me in ways I thought I had imagined beforehand (in fact, I hadn't - it was far worse)

synth :: BindingContext -> TermNode -> Type
synth ctx t =
  case tm of
    TmVar k _ _ -> getTypeFromContext ctx k
    TmAbs tyXs tmXs t1 -> tyShift (negate $ length tyXs) (negate $ length tmXs) $ TyForAll tyXs (map getType tmXs) (synth (tmXs ++ tyXs ++ map (applyToBindType $ tyShift' $ length (tyXs ++ tmXs)) ctx) t1)
    TmApp t1 tys ts ->
      case synth ctx t1 of
        TyForAll tyXs tys' ty1'
          | length tys == length tyXs && length ts == length tys' ->
              if and $ map (\(tm', ty') -> check ctx tm' ty') $ zip ts $ map (\ty -> foldr typingEvalSubst ty tys) tys'
                then foldr typingEvalSubst ty1' $ map (\(x, k) -> tyShift' k x) $ zip (reverse tys) [0 ..]
                else TyError "synth TmApp: failed to check"
        _ -> TyError "synth TmApp: not a function type or type arguments mismatch"
  where
    tm = getTm t

check :: BindingContext -> TermNode -> Type -> Bool
check ctx t ty = True

subtype :: BindingContext -> Type -> Type -> (Bool, undefined)
subtype ctx t = undefined

applyToBindType :: (Type -> Type) -> Binding -> Binding
applyToBindType f b =
  case b of
    TmVarBind x ty -> TmVarBind x $ f ty
    _              -> b

getTypeFromContext :: BindingContext -> Index -> Type
getTypeFromContext ctx ind
  | ind >= 0 && ind < length ctx =
      case ctx !! ind of
        TmVarBind _ ty -> ty
        _ -> TyError "\n(TmVar: possibly wrong binding for variable)"
  | otherwise = TyError "\n(TmVar: no type context for variable)"

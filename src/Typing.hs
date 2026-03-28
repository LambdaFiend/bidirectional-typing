module Typing where

import           Data.List
import           Display
import           Helper
import           Syntax

synth' :: TermNode -> Type
synth' t = synth [] t

-- What a disgrace! I should have planned how I would handle the scoping. The uncurried form terrorized me in ways unimaginable.

synth :: BindingContext -> TermNode -> Type
synth ctx t =
  case getTm t of
    TmVar k _ _ -> getTypeFromContext ctx k
    TmAbs tyXs tmXs t1
      | areAnnotated tmXs ->
          let tyXsLen = length tyXs
              tmXsLen = length tmXs
              shiftDown = tyShift (-tyXsLen) (-tmXsLen)
              shiftUp = tyShift 0 (tyXsLen + tmXsLen)
              ctx' = map (applyToBindType shiftUp) ctx
              ctx'' = tmXs ++ tyXs ++ ctx'
           in shiftDown $ TyForAll tyXs (getTypes tmXs) (synth ctx'' t1)
    TmApp t1 tys ts ->
      case synth ctx t1 of
        TyForAll tyXs tys' ty1'
          | sameLength tys tyXs && sameLength ts tys' ->
              let tysZipRange = zip (reverse tys) [0 ..]
                  fixedIndexTys = map (\(x, k) -> tyShift' k x) tysZipRange
                  tsZipSubstTys' = zip ts $ map (\ty -> foldr typingEvalSubst ty fixedIndexTys) tys'
               in if and $ map (\(tm, ty) -> check ctx tm ty) tsZipSubstTys'
                    then foldr typingEvalSubst ty1' fixedIndexTys
                    else TyError "synth TmApp: failed to check"
        _ -> TyError "synth TmApp: not a function type or type arguments mismatch"
    _ -> TyError "synth: not a valid term (maybe missing annotations?)"

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

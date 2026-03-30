module Typing where

import           Data.List
import           Debug.Trace
import           Helper
import           Syntax

synth' :: TermNode -> Type
synth' t = let x = synth [] t in trace (show x) x

zipBindings :: [Binding] -> BindingContext
zipBindings bs = let bLen = length bs in zip bs [bLen, bLen - 1, 1]

synth :: BindingContext -> TermNode -> Type
synth ctx t =
  case getTm t of
    TmVar k _ _ ->
      let (ty', info) = getInfoFromContext ctx k
       in tyShift' (k + info) ty'
    TmAbs tyXs tmXs t1
      | areAnnotated tmXs ->
          let shiftDown = tyShift' (negate $ length tmXs)
              ctx' = zipBindings tmXs ++ zipBindings tyXs ++ ctx
              synthT1 = synth ctx' t1
           in case synthT1 of
                TyError e -> TyError ("synth TmAbs: function body failed to synthesize(\n" ++ e ++ "\n)")
                _ -> TyForAll tyXs (getTypes tmXs) (shiftDown synthT1)
    TmAbs _ _ _ -> TyError "synth TmAbs: missing annotations"
    TmApp t1 tys ts ->
      case synth ctx t1 of
        TyForAll tyXs tys' ty1'
          | sameLength tys tyXs && sameLength ts tys' ->
              let tysZipRange = zip (reverse tys) [0 ..]
                  fixedIndexTys = map (\(x, k) -> tyShift' k x) tysZipRange
                  tsZipSubstTys' = zip ts $ map (\ty -> foldr typingEvalSubst ty fixedIndexTys) tys'
               in if and $ map (\(tm, ty) -> check ctx tm ty) tsZipSubstTys'
                    then foldr typingEvalSubst ty1' fixedIndexTys
                    else TyError "synth TmApp TyForAll: failed to check arguments"
        TyForAll _ _ _ -> TyError "synth TmApp TyForAll: argument length mismatch"
        TyBot | all (not . isTyError) $ map (synth ctx) ts -> TyBot
        TyBot -> TyError "synth TmApp TyBot: failed to synthesize arguments"
        TyError e -> TyError e
        _ -> TyError "synth TmApp _: not a valid function type"
    TmAppInfer t1 ts ->
      case synth ctx t1 of
        TyForAll tyXs tys ty1
          | length tyXs > 0 ->
              let tys' = map (synth ctx) ts
                  tyXsLen = length tyXs
                  tys'' = map (tyShift (-tyXsLen) (-tyXsLen)) tys
                  ds = map (\(x, y) -> constraintGen [] tyXs x y) $ zip tys' tys''
                  c =
                    case ds of
                      (d' : ds') -> foldr (meetConstraintLists) d' ds'
                      _          -> []
                  minimalSigma = calculateSubst c ty1
                  cond = all ((not . isTyError) . snd) minimalSigma && tyXs == [x1 | x1 <- tyXs, (x2, _) <- minimalSigma, x1 == x2]
                  orderedSigma = [ty | (TyVarBind x1) <- tyXs, (TyVarBind x2, ty) <- minimalSigma, x1 == x2]
                  tysZipRange = zip (reverse orderedSigma) [0 ..]
                  fixedIndexTys = map (\(x, k) -> tyShift' k x) tysZipRange
               in trace (show minimalSigma ++ "\n" ++ show c ++ "\n" ++ show ds ++ "\n" ++ show tys' ++ "\n" ++ show tys) $
                    if cond
                      then foldr typingEvalSubst ty1 fixedIndexTys
                      else TyError "synth TmAppInfer TyForAll: couldn't find a minimal substitution"
        TyForAll _ _ _ -> synth ctx (TermNode (getFI t) $ TmApp t1 [] ts)
        _ -> TyError "synth TmAppInfer _: not a valid function type"
    _ -> TyError "synth _: not a valid term"

check :: BindingContext -> TermNode -> Type -> Bool
check ctx t ty =
  case (getTm t, ty) of
    (_, TyTop) -> not $ isTyError $ synth ctx t
    (TmVar k _ _, _) ->
      let (ty', info) = getInfoFromContext ctx k
          ty'' = tyShift' (k + info) ty'
       in subtype ty'' ty
    (TmAbs tyXs1 tmXs t1, TyForAll tyXs2 tys ty1)
      | sameLength tyXs1 tyXs2 && areAnnotated tmXs ->
          let ctx' = zipBindings tmXs ++ zipBindings tyXs1 ++ ctx
              tysZipTmTys = zip tys (getTypes tmXs)
           in if all (\(x, y) -> subtype x y) tysZipTmTys
                then check ctx' t1 ty1
                else False
    (TmAbs tyXs1 tmXs t1, TyForAll tyXs2 tys ty1)
      | sameLength tyXs1 tyXs2 ->
          let typedTmBinds = map (\(x, y) -> addTypeToBind x y) (zip tmXs tys)
              ctx' = zipBindings typedTmBinds ++ zipBindings tyXs1 ++ ctx
           in check ctx' t1 ty1
    (TmApp t1 tys ts, _) ->
      case synth ctx t1 of
        TyForAll tyXs tys' ty1'
          | sameLength tys tyXs && sameLength ts tys' ->
              let tysZipRange = zip (reverse tys) [0 ..]
                  fixedIndexTys = map (\(x, k) -> tyShift' k x) tysZipRange
                  tsZipSubstTys' = zip ts $ map (\ty' -> foldr typingEvalSubst ty' fixedIndexTys) tys'
                  cond1 = and $ map (\(tm', ty') -> check ctx tm' ty') tsZipSubstTys'
                  ty1'' = foldr typingEvalSubst ty1' fixedIndexTys
                  cond2 = subtype ty1'' ty
               in cond1 && cond2
        TyBot -> all (not . isTyError) $ map (synth ctx) ts
        _ -> False
    (TmAppInfer t1 ts, _) ->
      case synth ctx t1 of
        TyForAll tyXs tys ty1
          | length tyXs > 0 ->
              let tys' = map (synth ctx) ts
                  tyXsLen = length tyXs
                  tys'' = map (tyShift (-tyXsLen) (-tyXsLen)) tys
                  cs = map (\(x, y) -> constraintGen [] tyXs x y) $ zip tys' tys''
                  d = constraintGen [] tyXs ty1 ty
                  sigma = foldr (meetConstraintLists) d cs
               in all (\(Constraint s' _ t') -> subtype s' t') sigma
        TyForAll _ _ _ -> check ctx (TermNode (getFI t) $ TmApp t1 [] ts) ty
        _ -> False
    _ -> False

subtype :: Type -> Type -> Bool
subtype ty1 ty2 =
  case (ty1, ty2) of
    (TyVar k1 _ _, TyVar k2 _ _) -> k1 == k2
    (_, TyTop) -> True
    (TyBot, _) -> True
    (TyForAll tyXs1 tys1 ty1', TyForAll tyXs2 tys2 ty2')
      | sameLength tyXs1 tyXs2 ->
          let cond1 = all (\(x, y) -> subtype x y) $ zip tys2 tys1
              cond2 = subtype ty1' ty2'
           in cond1 && cond2
    _ -> False

getFreeTyVars' :: Type -> [Binding]
getFreeTyVars' ty = getFreeTyVars 0 ty

getFreeTyVars :: Index -> Type -> [Binding]
getFreeTyVars n ty =
  case ty of
    TyVar k _ x | k >= n -> [TyVarBind x]
    TyForAll tyXs tys ty1 ->
      let n' = n + length tyXs
       in concat (map (getFreeTyVars n') tys) ++ getFreeTyVars n' ty1
    _ -> []

constraintGen :: [Binding] -> [Binding] -> Type -> Type -> ConstraintList
constraintGen vars unks ty1 ty2 =
  case (ty1, ty2) of
    (_, TyTop) -> [Constraint TyBot x TyTop | x <- unks]
    (TyBot, _) -> [Constraint TyBot x TyTop | x <- unks]
    (TyVar _ _ x, _)
      | elem (TyVarBind x) unks && (getFreeTyVars' ty2 `intersect` unks) == [] ->
          let t = demote vars ty2
           in union' [Constraint TyBot (TyVarBind x) t] [Constraint TyBot x' TyTop | x' <- unks]
    (_, TyVar _ _ x)
      | elem (TyVarBind x) unks && (getFreeTyVars' ty1 `intersect` unks) == [] ->
          let t = promote vars ty1
           in union' [Constraint t (TyVarBind x) TyTop] [Constraint TyBot x' TyTop | x' <- unks]
    (TyVar k1 _ x1, TyVar k2 _ _)
      | k1 == k2 && not (elem (TyVarBind x1) unks) -> [Constraint TyBot x TyTop | x <- unks]
    (TyForAll tyXs1 tys1 ty1', TyForAll tyXs2 tys2 ty2')
      | sameLength tyXs1 tyXs2 && (tyXs1 `intersect` (vars `union` unks)) == [] ->
          let vars' = tyXs1 `union` vars
              cs = map (\(x, y) -> constraintGen vars' unks x y) $ zip tys2 tys1
              d = constraintGen vars' unks ty1' ty2'
           in union (foldr (meetConstraintLists) d cs) [Constraint TyBot x TyTop | x <- unks]
    _ -> []
  where
    union' = unionBy (\(Constraint _ (TyVarBind x) _) (Constraint _ (TyVarBind y) _) -> x == y)

calculateSubst :: ConstraintList -> Type -> [(Binding, Type)]
calculateSubst cs r =
  [ result
  | (Constraint s x t) <- cs,
    let result = minimizeConstraint (getVariance 0 (getName x) r) (s, x, t)
  ]

minimizeConstraint :: Variance -> (Type, Binding, Type) -> (Binding, Type)
minimizeConstraint variance (s, x, _) | elem variance [Constant, Covariant] = (x, s)
minimizeConstraint Contravariant (_, x, t) = (x, t)
minimizeConstraint Invariant (s, x, t) | s == t = (x, s)
minimizeConstraint _ (_, x, _) = (x, TyError "minimizeConstraint: either varianceError or s /= t when invariant")

tightenVariance :: Variance -> Variance -> Variance
tightenVariance Covariant Contravariant = Invariant
tightenVariance Contravariant Covariant = Invariant
tightenVariance v1 v2                   = max v1 v2

getVariance :: Index -> Name -> Type -> Variance
getVariance n x ty =
  case ty of
    TyTop -> Constant
    TyBot -> Constant
    TyVar k _ x' | k >= n && x' == x -> Covariant
    TyVar _ _ _ -> Constant
    TyForAll tyXs tys ty1 ->
      let n' = n + length tyXs
          v1 =
            case ty1 of
              TyVar k _ x' | k >= n' && x' == x -> Covariant
              _                                 -> getVariance n' x ty1
          v2 =
            if elem (TyVarBind x) (concat $ map getFreeTyVars' tys)
              then Contravariant
              else Constant
          v3 = tightenVariance v1 v2
       in foldr tightenVariance v3 (map (getVariance n' x) tys)
    _ -> VarianceError

meetConstraintLists :: ConstraintList -> ConstraintList -> ConstraintList
meetConstraintLists cs1 cs2 =
  [ Constraint (calculateJoin s1 s2) x1 (calculateMeet t1 t2)
  | (Constraint s1 x1 t1) <- cs1,
    (Constraint s2 x2 t2) <- cs2,
    x1 == x2
  ]

calculateJoin :: Type -> Type -> Type
calculateJoin ty1 ty2 =
  if subtype ty1 ty2
    then ty2
    else
      if subtype ty2 ty1
        then ty1
        else case (ty1, ty2) of
          (TyForAll tyXs1 tys1 ty1', TyForAll tyXs2 tys2 ty2')
            | sameLength tyXs1 tyXs2 ->
                let ms = map (\(x, y) -> calculateMeet x y) $ zip tys1 tys2
                    j = calculateJoin ty1' ty2'
                 in TyForAll tyXs1 ms j
          _ -> TyTop

calculateMeet :: Type -> Type -> Type
calculateMeet ty1 ty2 =
  if subtype ty1 ty2
    then ty1
    else
      if subtype ty2 ty1
        then ty2
        else case (ty1, ty2) of
          (TyForAll tyXs1 tys1 ty1', TyForAll tyXs2 tys2 ty2')
            | sameLength tyXs1 tyXs2 ->
                let js = map (\(x, y) -> calculateJoin x y) $ zip tys1 tys2
                    m = calculateMeet ty1' ty2'
                 in TyForAll tyXs1 js m
          _ -> TyBot

promote :: [Binding] -> Type -> Type
promote vars ty =
  case ty of
    TyTop -> TyTop
    TyBot -> TyBot
    TyVar _ _ x | elem (TyVarBind x) vars -> TyTop
    TyVar _ _ _ -> ty
    TyForAll tyXs tys ty1
      | intersect tyXs vars == [] ->
          let us = map (demote vars) tys
              r = promote vars ty1
           in TyForAll tyXs us r
    _ -> TyError "promote: no cases matched"

demote :: [Binding] -> Type -> Type
demote vars ty =
  case ty of
    TyTop -> TyTop
    TyBot -> TyBot
    TyVar _ _ x | elem (TyVarBind x) vars -> TyBot
    TyVar _ _ _ -> ty
    TyForAll tyXs tys ty1
      | intersect tyXs vars == [] ->
          let us = map (promote vars) tys
              r = demote vars ty1
           in TyForAll tyXs us r
    _ -> TyError "demote: no cases matched"

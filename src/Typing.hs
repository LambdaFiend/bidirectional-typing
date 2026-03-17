module Typing where

import           Data.List
import           Display
import           Helper
import           Syntax

synth' :: TermNode -> Type
synth' t =
  let (ty, ctx, _) = synth [] 0 t
   in freshenVarExists $ substCtxToTy ctx ty

synth :: BindingContext -> Index -> TermNode -> (Type, BindingContext, Index)
synth ctx n t =
  let tm = getTm t
   in case tm of
        TmVar _ _ x ->
          case find (isTmVarBind x) ctx of
            Just b -> (snd $ fromTmVarBind $ b, ctx, n)
            _      -> errorResult
        TmAbs x t1 ->
          let x' = generateName n
              x'' = generateName (n + 1)
              (alpha, beta) = (TyVarExists x', TyVarExists x'')
              bindings = [TmVarBind x alpha, TyVarExistsBind x'', TyVarExistsBind x']
              (b, ctx', n') = check (bindings ++ ctx) (n + 2) t1 beta
              ctx'' = cutContext ctx' $ TmVarBind x alpha
           in isError b (TyArrow alpha beta, ctx'', n')
        TmApp t1 t2 ->
          let (ty', ctx', n') = synth ctx n t1
              appliedFun = AppliedFunction (substCtxToTy ctx' ty') t2
           in apply ctx' n' appliedFun
        TmAnno t1 ty1
          | isTyWellFormed ctx ty1 ->
              let (b, ctx', n') = check ctx n t1 ty1
               in isError b (ty1, ctx', n')
        TmUnit -> (TyUnit, ctx, n)
        _ -> errorResult
  where
    errorResult =
      let ctx' = filter (\x -> case x of TmVarBind _ _ -> True; _ -> False) ctx
          ctx'' = map (fst . fromTmVarBind) ctx'
       in (TyError $ "{\nCouldn't synthesize: \n" ++ (removeOuterParens $ showTerm ctx'' t) ++ "\n" ++ show ctx ++ "\n}", ctx, n)
    isError b r = if b then r else errorResult
    isTmVarBind = (\x y -> case y of TmVarBind z _ -> x == z; _ -> False)

check :: BindingContext -> Index -> TermNode -> Type -> (Bool, BindingContext, Index)
check ctx n t ty =
  let tm = getTm t
   in case (tm, ty) of
        (TmAbs x t1, TyArrow ty1 ty2) ->
          let (b, ctx', n') = check (TmVarBind x ty1 : ctx) n t1 ty2
              ctx'' = cutContext ctx' $ TmVarBind x ty1
           in (b, ctx'', n')
        (TmUnit, TyUnit) -> (True, ctx, n)
        (_, TyForAll x ty1) ->
          let (b, ctx', n') = check (TyVarBind x : ctx) n t ty1
              ctx'' = cutContext ctx' $ TyVarBind x
           in (b, ctx'', n')
        (_, _) ->
          let (ty', ctx', n') = synth ctx n t
           in subtype ctx' n' (substCtxToTy ctx' ty') (substCtxToTy ctx' ty)

apply :: BindingContext -> Index -> AppliedFunction -> (Type, BindingContext, Index)
apply ctx n (AppliedFunction ty t) =
  case ty of
    TyArrow ty1 ty2 ->
      let (b, ctx', n') = check ctx n t ty1
       in isError b (ty2, ctx', n')
    TyVarExists x
      | elem (TyVarExistsBind x) ctx ->
          let x' = generateName n
              x'' = generateName (n + 1)
              (ctx1, ctx2) = splitContext ctx (TyVarExistsBind x)
              ctx' = ctx1 ++ [ConstraintBind x (TyArrow (TyVarExists x') (TyVarExists x'')), TyVarExistsBind x', TyVarExistsBind x''] ++ ctx2
              (b, ctx'', n') = check ctx' (n + 2) t (TyVarExists x')
           in isError b (TyVarExists x'', ctx'', n')
    TyForAll _ ty1 ->
      let x' = generateName n
          appliedFun = AppliedFunction (typingEvalSubst (TyVarExists x') ty1) t
       in apply (TyVarExistsBind x' : ctx) (n + 1) appliedFun
    _ -> errorResult
  where
    errorResult = (TyError $ "{\nCouldn't apply: \n" ++ showType' ty ++ "\n" ++ show ctx ++ "\n}", ctx, n)
    isError b r = if b then r else errorResult

subtype :: BindingContext -> Index -> Type -> Type -> (Bool, BindingContext, Index)
subtype ctx n ty1 ty2 =
  case (ty1, ty2) of
    (TyVar _ _ x1, TyVar _ _ x2) | x1 == x2 && elem (TyVarBind x1) ctx -> (True, ctx, n)
    (TyUnit, TyUnit) -> (True, ctx, n)
    (TyVarExists x1, TyVarExists x2) | x1 == x2 && elem (TyVarExistsBind x1) ctx -> (True, ctx, n)
    (TyArrow ty11 ty12, TyArrow ty21 ty22) ->
      let (b, ctx', n') = subtype ctx n ty21 ty11
          (b', ctx'', n'') = subtype ctx' n' (substCtxToTy ctx' ty12) (substCtxToTy ctx' ty22)
       in (b && b', ctx'', n'')
    (TyForAll _ ty11, _) ->
      let x' = generateName n
          ctx' = TyVarExistsBind x' : MarkerBind x' : ctx
          (b, ctx'', n') = subtype ctx' (n + 1) (typingEvalSubst (TyVarExists x') ty11) ty2
       in (b, cutContext ctx'' $ MarkerBind x', n')
    (_, TyForAll x ty21) ->
      let (b, ctx', n') = subtype (TyVarBind x : ctx) n ty1 ty21
       in (b, cutContext ctx' $ TyVarBind x, n')
    (TyVarExists x1, _)
      | not (elem ty1 (freeVarsTy' ty2)) && elem (TyVarExistsBind x1) ctx -> instantiateL ctx n ty1 ty2
    (_, TyVarExists x2)
      | not (elem ty2 (freeVarsTy' ty1)) && elem (TyVarExistsBind x2) ctx -> instantiateR ctx n ty1 ty2
    _ -> (False, ctx, n)

instantiateL :: BindingContext -> Index -> Type -> Type -> (Bool, BindingContext, Index)
instantiateL ctx n ty1 ty2 =
  case (ty1, ty2) of
    (TyVarExists x1, TyVarExists x2)
      | elem (TyVarExistsBind x1) ctx && elem (TyVarExistsBind x2) ctx && elem (TyVarExistsBind x1) (snd $ splitContext ctx $ TyVarExistsBind x2) ->
          let (ctx1, ctx2) = splitContext ctx $ TyVarExistsBind x2
           in (True, ctx1 ++ [ConstraintBind x2 ty1] ++ ctx2, n)
    (TyVarExists x1, TyArrow ty21 ty22)
      | elem (TyVarExistsBind x1) ctx ->
          let x' = generateName n
              x'' = generateName (n + 1)
              (ctx1, ctx2) = splitContext ctx (TyVarExistsBind x1)
              ctx' = ctx1 ++ [ConstraintBind x1 (TyArrow (TyVarExists x') (TyVarExists x'')), TyVarExistsBind x', TyVarExistsBind x''] ++ ctx2
              (b, ctx'', n') = instantiateR ctx' (n + 2) ty21 (TyVarExists x')
              (b', ctx''', n'') = instantiateL ctx'' n' (TyVarExists x'') (substCtxToTy ctx'' ty22)
           in (b && b', ctx''', n'')
    (TyVarExists x1, TyForAll x2 ty21)
      | elem (TyVarExistsBind x1) ctx ->
          let (b, ctx', n') = instantiateL (TyVarBind x2 : ctx) n ty1 ty21
           in (b, cutContext ctx' $ TyVarBind x2, n')
    (TyVarExists x1, _)
      | isMonotype ty2 && elem (TyVarExistsBind x1) ctx ->
          let (ctx1, ctx2) = splitContext ctx $ TyVarExistsBind x1
           in (isTyWellFormed ctx2 ty2, ctx1 ++ [ConstraintBind x1 ty2] ++ ctx2, n)
    _ -> (False, ctx, n)

instantiateR :: BindingContext -> Index -> Type -> Type -> (Bool, BindingContext, Index)
instantiateR ctx n ty1 ty2 =
  case (ty1, ty2) of
    (TyVarExists x1, TyVarExists x2)
      | elem (TyVarExistsBind x1) ctx && elem (TyVarExistsBind x2) ctx && elem (TyVarExistsBind x2) (snd $ splitContext ctx $ TyVarExistsBind x1) ->
          let (ctx1, ctx2) = splitContext ctx $ TyVarExistsBind x1
           in (True, ctx1 ++ [ConstraintBind x1 ty2] ++ ctx2, n)
    (TyArrow ty11 ty12, TyVarExists x2)
      | elem (TyVarExistsBind x2) ctx ->
          let x' = generateName n
              x'' = generateName (n + 1)
              (ctx1, ctx2) = splitContext ctx (TyVarExistsBind x2)
              ctx' = ctx1 ++ [ConstraintBind x2 (TyArrow (TyVarExists x') (TyVarExists x'')), TyVarExistsBind x', TyVarExistsBind x''] ++ ctx2
              (b, ctx'', n') = instantiateL ctx' (n + 2) (TyVarExists x') ty11
              (b', ctx''', n'') = instantiateR ctx'' n' (substCtxToTy ctx'' ty12) (TyVarExists x'')
           in (b && b', ctx''', n'')
    (TyForAll _ ty11, TyVarExists x2)
      | elem (TyVarExistsBind x2) ctx ->
          let x' = generateName n
              ctx' = TyVarExistsBind x' : MarkerBind x' : ctx
              (b, ctx'', n') = instantiateR ctx' (n + 1) (typingEvalSubst (TyVarExists x') ty11) ty2
           in (b, cutContext ctx'' $ MarkerBind x', n')
    (_, TyVarExists x2)
      | isMonotype ty1 && elem (TyVarExistsBind x2) ctx ->
          let (ctx1, ctx2) = splitContext ctx $ TyVarExistsBind x2
           in (isTyWellFormed ctx2 ty1, ctx1 ++ [ConstraintBind x2 ty1] ++ ctx2, n)
    _ -> (False, ctx, n)

-- REFACTOR!!!
-- refactor so that, for example, types can be directly converted into binds
-- refactor so that errors are caught properly, so that they do not propagate further unlikely errors

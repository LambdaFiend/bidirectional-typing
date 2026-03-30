module Helper where

import           Data.List
import           Syntax

newtype UpdatedTmArrTm = UpdatedTmArrTm
  {updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm, TermNode -> UpdatedTmArrTm, Type -> Type)}

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar _ _ _ -> tm
    TmVarRaw _ -> tm
    TmAbs tyXs tmXs t1 ->
      let tmXs' = map (\b -> case b of TmVarBind x ty -> TmVarBind x $ fTy ty; _ -> b) tmXs
       in TmAbs tyXs tmXs' (traverseTm' t1)
    TmApp t1 tys tms -> TmApp (traverseTm' t1) (map fTy tys) (map traverseTm' tms)
    TmAppInfer t1 tms -> TmAppInfer (traverseTm' t1) (map traverseTm' tms)
    TmLet x t1 t2 -> TmLet x (traverseTm' t1) (traverseTm'' t2)
    _ -> tm
  where
    tm = getTm t'
    fi = getFI t'
    traverseTm' = traverseDownTm f'
    traverseTm'' = traverseDownTm f''
    (t', f', f'', fTy) = updateTmArrTm $ f t

isVal :: TermNode -> Bool
isVal t =
  let tm = getTm t
   in case tm of
        _ -> False

{--
evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift' 0 (-1) (subst' 0 (shift' 0 1 s) t)
--}

typingEvalSubst :: Type -> Type -> Type
typingEvalSubst s t = tyShift' (-1) (tySubst' 0 (tyShift' 1 s) t)

tyShift' :: Index -> Type -> Type
tyShift' d t = tyShift 0 d t

tyShift :: Index -> Index -> Type -> Type
tyShift c d t =
  case t of
    TyVar k l x -> TyVar (if k < c then k else k + d) (l + d) x
    TyForAll tyXs tys ty1 ->
      let tyShift'' = tyShift (c + length tyXs) d
       in TyForAll tyXs (map tyShift'' tys) $ tyShift'' ty1
    _ -> t
  where
    -- _ -> TyError $ "Helper, tyShift: type is not applicable: " ++ show t
    tyShift' = tyShift c d

tySubst' :: Index -> Type -> Type -> Type
tySubst' j s t = tySubst 0 j s t

tySubst :: Index -> Index -> Type -> Type -> Type
tySubst c j s t =
  case t of
    TyVar k _ _ -> if k == j + c then tyShift' (j + c) s else t
    TyForAll tyXs tys ty1 ->
      let tySubst'' = tySubst (c + length tyXs) j s
       in TyForAll tyXs (map tySubst'' tys) $ tySubst'' ty1
    _ -> t
  where
    tySubst' = tySubst c j s

shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t =
  let tm = getTm t; fi = getFI t; shift' = shift c d
   in UpdatedTmArrTm $
        case tm of
          TmVar k l x -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d) x, id', id', tyShift c d)
          TmAbs tyXs tmXs _ -> (t, shift (c + length tyXs + length tmXs) d, shift', tyShift (c + length tyXs) d)
          TmLet _ _ _ -> (t, shift', shift (c + 1) d, tyShift c d)
          _ -> (t, shift', shift', tyShift c d)

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t =
  let tm = getTm t; subst' = subst c j s
   in UpdatedTmArrTm $
        case tm of
          TmVar k _ _ -> (if k == j + c then shift' 0 (j + c) s else t, id', id', id :: Type -> Type)
          TmAbs tyXs tmXs _ -> (t, subst (c + length tyXs + length tmXs) j s, subst', id :: Type -> Type)
          TmLet _ _ _ -> (t, subst', subst (c + 1) j s, id :: Type -> Type)
          _ -> (t, subst', subst', id :: Type -> Type)

genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: NameContext -> TermNode -> UpdatedTmArrTm
genIndex ctx t =
  let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx
   in UpdatedTmArrTm $
        case tm of
          TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx) x, genIndex', genIndex', id :: Type -> Type)
          TmAbs tyXs tmXs _ ->
            let ctx' = getNames tyXs ++ ctx
                ctx'' = getNames tmXs ++ ctx'
             in (t, genIndex ctx'', genIndex', genIndexType ctx')
          TmLet x _ _ -> (t, genIndex', genIndex (x : ctx), id :: Type -> Type)
          _ -> (t, genIndex', genIndex', genIndexType ctx)
  where
    genIndexType :: NameContext -> Type -> Type
    genIndexType ctx ty =
      case ty of
        TyForAll tyXs tys ty1 ->
          let ctx' = map getName tyXs ++ ctx
              genIndexType' = genIndexType ctx'
           in TyForAll tyXs (map genIndexType' tys) $ genIndexType' ty1
        TyVarRaw x -> TyVar (length $ takeWhile (/= x) ctx) (length ctx) x
        _ -> ty

fixTermNames' :: TermNode -> TermNode
fixTermNames' t = traverseDownTm (fixTermsNames []) t

fixTermsNames :: NameContext -> TermNode -> UpdatedTmArrTm
fixTermsNames ctx t =
  UpdatedTmArrTm $
    case getTm t of
      TmVar k l x -> (TermNode fi $ if k >= 0 && k < length ctx then TmVar k l (ctx !! k) else TmVar k l (fixName ctx x), fixTermsNames', fixTermsNames', fixTypesNames ctx)
      TmAbs tyXs tmXs t1 ->
        let tyXs' = fixBindingNames ctx tyXs tyXs
            tmXs' = fixBindingNames ctx tmXs tmXs
            ctx' = getNames tyXs' ++ ctx
            ctx'' = getNames tmXs' ++ ctx'
         in (TermNode fi $ TmAbs tyXs' tmXs' t1, fixTermsNames ctx'', fixTermsNames ctx'', fixTypesNames ctx')
      TmLet x t1 t2 ->
        let x' = fixName ctx x
         in (TermNode fi $ TmLet x' t1 t2, fixTermsNames', fixTermsNames (x' : ctx), fixTypesNames ctx)
      _ -> (t, fixTermsNames', fixTermsNames', fixTypesNames ctx)
  where
    fi = getFI t
    fixTermsNames' = fixTermsNames ctx
    fixTypesNames :: NameContext -> Type -> Type
    fixTypesNames ctx ty =
      case ty of
        TyForAll tyXs tys ty1 ->
          let tyXs' = fixBindingNames ctx tyXs tyXs
              ctx' = getNames tyXs' ++ ctx
           in TyForAll tyXs' (map (fixTypesNames ctx') tys) (fixTypesNames ctx' ty1)
        TyVar k l x -> if k >= 0 && k < length ctx then TyVar k l (ctx !! k) else TyVar k l (fixName ctx x)
        _ -> ty

fixName :: NameContext -> Name -> Name
fixName ctx x
  | (length $ filter ((==) x) ctx) < 1 = x
  | otherwise = fixName ctx (x ++ "\'")

fixBindingNames :: NameContext -> [Binding] -> [Binding] -> [Binding]
fixBindingNames _ [] _ = []
fixBindingNames ctx (x : xs) ys =
  case x of
    TmVarBind x' tm -> TmVarBind (fun x') tm : fixBindingNames ctx xs ys
    TmVarNoBind x'  -> TmVarNoBind (fun x') : fixBindingNames ctx xs ys
    TyVarBind x'    -> TyVarBind (fun x') : fixBindingNames ctx xs ys
  where
    fun = (\x' -> fixName ((getNames ys \\ [x']) ++ ctx) x')

findConflicts' :: TermNode -> Either String ()
findConflicts' t =
  case findDisplayErrors' $ findConflicts t of
    Left e -> Left e
    _      -> Right ()

findConflicts :: TermNode -> String
findConflicts t =
  case getTm t of
    TmAbs tyXs tmXs t1 ->
      let b1 = nub (map getName tyXs \\ nub (map getName tyXs))
          b2 = nub (map getName tmXs \\ (nub $ map getName tmXs))
          b3 =
            if and $ map (\b -> case b of TmVarBind _ _ -> True; _ -> False) tmXs
              then concat $ map findConflictsType $ map getType tmXs
              else []
       in if b1 == [] && b2 == [] && b3 == []
            then findConflicts t1
            else "#Conflicting variable names:\n" ++ show b1 ++ "\n" ++ show b2 ++ "\n" ++ "#" ++ b3 ++ findConflicts t1
    TmApp t1 _ ts -> findConflicts t1 ++ concat (map findConflicts ts)
    TmAppInfer t1 ts -> findConflicts t1 ++ concat (map findConflicts ts)
    TmLet _ t1 t2 -> findConflicts t1 ++ findConflicts t2
    _ -> ""
  where
    findConflictsType :: Type -> String
    findConflictsType ty =
      case ty of
        TyForAll tyXs tys ty1 ->
          let b = nub (map getName tyXs \\ nub (map getName tyXs))
           in if b == []
                then findConflictsType ty1
                else "#Conflicting variable names:\n" ++ show b ++ "#" ++ concat (map findConflictsType tys) ++ findConflictsType ty1
        _ -> ""

getFreeVars' :: TermNode -> [Binding]
getFreeVars' t = getFreeVars 0 t

getFreeVars :: Index -> TermNode -> [Binding]
getFreeVars n t =
  case getTm t of
    TmVarRaw x -> [TmVarNoBind x]
    TmVar k _ x -> if k < n then [] else [TmVarNoBind x]
    TmAbs tyXs tmXs t1 ->
      let n' = length tyXs + n
          n'' = length tmXs + n'
          tys =
            if all isAnnotated tmXs
              then concat (map (getFreeTyVars n') (getTypes tmXs))
              else []
       in tys ++ getFreeVars n'' t1
    TmApp t1 tys ts -> getFreeVars n t1 ++ concat (map (getFreeTyVars n) tys) ++ concat (map (getFreeVars n) ts)
    TmAppInfer t1 ts -> getFreeVars n t1 ++ concat (map (getFreeVars n) ts)
    TmLet _ t1 t2 -> getFreeVars n t1 ++ getFreeVars (n + 1) t2
    _ -> []

id' :: TermNode -> UpdatedTmArrTm
id' t = UpdatedTmArrTm (t, id', id', id :: Type -> Type)

findDisplayErrors' :: String -> Either String String
findDisplayErrors' s =
  case findDisplayErrors s [] of
    [] -> Right s
    s' -> Left $ intercalate "\n" s'

findDisplayErrors :: String -> String -> [String]
findDisplayErrors [] [] = []
findDisplayErrors [] _ = error "Someone needs to fix this, there's an error message which does not have its identifiers matching."
findDisplayErrors ('#' : xs) [] = findDisplayErrors xs ['#']
findDisplayErrors ('#' : xs) ys = ((\s -> case s of [] -> []; (_ : xs') -> xs') $ reverse ys) : findDisplayErrors xs []
findDisplayErrors (_ : xs) [] = findDisplayErrors xs []
findDisplayErrors (x : xs) ys = findDisplayErrors xs (x : ys)

findTypeErrors' :: Type -> String
findTypeErrors' t = intercalate "\n" $ findTypeErrors t

findTypeErrors :: Type -> [String]
findTypeErrors t =
  case t of
    TyError e          -> [e]
    TyForAll _ tys ty1 -> concat (map findTypeErrors tys) ++ findTypeErrors ty1
    _                  -> []

findTermErrors' :: TermNode -> String
findTermErrors' t = intercalate "\n" $ findTermErrors t

findTermErrors :: TermNode -> [String]
findTermErrors t =
  let tm = getTm t
   in case tm of
        TmError e -> [e]
        TmAbs _ tmXs t1 -> concat (map getAnnoErrors tmXs) ++ findTermErrors t1
        TmApp t1 tys ts -> findTermErrors t1 ++ concat (map findTypeErrors tys) ++ (concat $ map findTermErrors ts)
        TmAppInfer t1 ts -> findTermErrors t1 ++ (concat $ map findTermErrors ts)
        TmLet _ t1 t2 -> findTermErrors t1 ++ findTermErrors t2
        _ -> []
  where
    getAnnoErrors :: Binding -> [String]
    getAnnoErrors b =
      case b of
        TmVarBind _ ty -> findTypeErrors ty
        _              -> []

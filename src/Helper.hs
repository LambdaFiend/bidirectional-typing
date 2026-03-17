module Helper where

import           Data.List
import           Syntax

newtype UpdatedTmArrTm = UpdatedTmArrTm
  {updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm, TermNode -> UpdatedTmArrTm, Type -> Type)}

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar _ _ _  -> tm
    TmVarRaw _   -> tm
    TmAbs x t1   -> TmAbs x (traverseTm' t1)
    TmApp t1 t2  -> TmApp (traverseTm' t1) (traverseTm' t2)
    TmUnit       -> tm
    TmAnno t1 ty -> TmAnno (traverseTm' t1) (fTy ty)
  where
    tm = getTm t'
    fi = getFI t'
    traverseTm' = traverseDownTm f'
    traverseTm'' = traverseDownTm f''
    (t', f', f'', fTy) = updateTmArrTm $ f t

isVal :: TermNode -> Bool
isVal t = let tm = getTm t in
  case tm of
    TmAbs _ _ -> True
    TmUnit -> True
    _ -> False

evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift' 0 (-1) (subst' 0 (shift' 0 1 s) t)

typingEvalSubst :: Type -> Type -> Type
typingEvalSubst s t = tyShift' (-1) (tySubst' 0 (tyShift' 1 s) t)

tyShift' :: Index -> Type -> Type
tyShift' d t = tyShift 0 d t

tyShift :: Index -> Index -> Type -> Type
tyShift c d t =
  case t of
    TyUnit        -> t
    TyArrow t1 t2 -> TyArrow (tyShift' t1) (tyShift' t2)
    TyVar k l x   -> TyVar (if k < c then k else k + d) (l + d) x
    TyForAll x t1 -> TyForAll x $ tyShift (c + 1) d t1
    TyVarExists _ -> t
    TyError _     -> t
  where
    -- _ -> TyError $ "Helper, tyShift: type is not applicable: " ++ show t
    tyShift' = tyShift c d

tySubst' :: Index -> Type -> Type -> Type
tySubst' j s t = tySubst 0 j s t

tySubst :: Index -> Index -> Type -> Type -> Type
tySubst c j s t =
  case t of
    TyUnit -> TyUnit
    TyArrow t1 t2 -> TyArrow (tySubst' t1) (tySubst' t2)
    TyVar k _ _ -> if k == j + c then tyShift' (j + c) s else t
    TyForAll x t1 -> TyForAll x $ tySubst (c + 1) j s t1
    TyVarExists _ -> t
    TyError e -> TyError e
    _ -> TyError $ "Helper, tySubst: type is not applicable; " ++ show t
  where
    tySubst' = tySubst c j s


shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t =
  let tm = getTm t; fi = getFI t; shift' = shift c d
   in UpdatedTmArrTm $
        case tm of
          TmVar k l x -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d) x, id', id', tyShift' d)
          TmAbs _ _ -> (t, shift (c + 1) d, shift', tyShift c d)
          _ -> (t, shift', shift', tyShift c d)

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t =
  let tm = getTm t; subst' = subst c j s
   in UpdatedTmArrTm $
        case tm of
          TmVar k _ _ -> (if k == j + c then shift' 0 (j + c) s else t, id', id', id :: Type -> Type)
          TmAbs _ _ -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
          _ -> (t, subst', subst', id :: Type -> Type)

genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: NameContext -> TermNode -> UpdatedTmArrTm
genIndex ctx t =
  let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx
   in UpdatedTmArrTm $
        case tm of
          TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx) x, genIndex', genIndex', id :: Type -> Type)
          TmAbs x _ -> (t, genIndex (x : ctx), genIndex', id :: Type -> Type)
          _ -> (t, genIndex', genIndex', genIndexType [])
  where
    genIndexType :: [Name] -> Type -> Type
    genIndexType ctx ty =
      case ty of
        TyArrow ty1 ty2 -> TyArrow (genIndexType ctx ty1) (genIndexType ctx ty2)
        TyForAll x ty1  -> TyForAll x (genIndexType (x : ctx) ty1)
        TyVarRaw x      -> TyVar (length $ takeWhile (/= x) ctx) (length ctx) x
        _               -> ty

id' :: TermNode -> UpdatedTmArrTm
id' t = UpdatedTmArrTm (t, id', id', id :: Type -> Type)

freshenVarExists :: Type -> Type
freshenVarExists ty =
  let vars = zip (nub $ collectVarExists ty) ([1 ..])
   in updateVarExists vars ty

updateVarExists :: [(Type, Index)] -> Type -> Type
updateVarExists vars ty =
  case ty of
    TyVarExists x ->
      let y = lookup ty vars
       in case y of
            Just y' -> TyVarExists $ generateName y'
            _       -> ty
    TyForAll x ty1 -> TyForAll x $ updateVarExists vars ty1
    TyArrow ty1 ty2 -> TyArrow (updateVarExists vars ty1) (updateVarExists vars ty2)
    _ -> ty

collectVarExists :: Type -> [Type]
collectVarExists ty =
  case ty of
    TyVarExists _   -> [ty]
    TyArrow ty1 ty2 -> collectVarExists ty1 ++ collectVarExists ty2
    TyForAll _ ty1  -> collectVarExists ty1
    _               -> []


splitContext :: BindingContext -> Binding -> (BindingContext, BindingContext)
splitContext ctx b =
  let ctx1 = takeWhile ((/=) b) ctx
      ctx2 = dropWhile ((/=) b) ctx
      ctx2' = case ctx2 of [] -> []; (_ : xs) -> xs
   in (ctx1, ctx2')

cutContext :: BindingContext -> Binding -> BindingContext
cutContext ctx b =
  case dropWhile ((/=) b) ctx of
    []       -> []
    (_ : xs) -> xs

freeVarsTy' :: Type -> [Type]
freeVarsTy' ty = freeVarsTy [] ty

freeVarsTy :: NameContext -> Type -> [Type]
freeVarsTy ctx ty =
  case ty of
    TyVar k _ _     -> if k < length ctx then [] else [ty]
    TyVarExists _   -> [ty]
    TyForAll x ty1  -> freeVarsTy (x : ctx) ty1
    TyArrow ty1 ty2 -> freeVarsTy ctx ty1 ++ freeVarsTy ctx ty2
    TyUnit          -> []
    TyError _       -> []
    _               -> []

generateName :: Index -> Name
generateName n = "t" ++ show n

isMonotype :: Type -> Bool
isMonotype ty =
  case ty of
    TyForAll _ _    -> False
    TyArrow ty1 ty2 -> isMonotype ty1 && isMonotype ty2
    _               -> True

isTyWellFormed :: BindingContext -> Type -> Bool
isTyWellFormed ctx ty =
  case ty of
    TyArrow ty1 ty2 -> isTyWellFormed' ty1 && isTyWellFormed' ty2
    TyUnit -> True
    TyForAll x ty1 -> isTyWellFormed (TyVarBind x : ctx) ty1
    TyVar _ _ x -> elem (TyVarBind x) ctx
    TyVarExists x ->
      elem (TyVarExistsBind x) ctx
        || ( case find (isConstraintBind x) ctx of
               Just (ConstraintBind _ ty') -> isMonotype ty'
               _                           -> False
           )
    TyError _ -> False
    _ -> False
  where
    isTyWellFormed' = isTyWellFormed ctx
    isConstraintBind = (\x y -> case y of ConstraintBind z _ -> x == z; _ -> False)

isCtxWellFormed :: BindingContext -> Bool
isCtxWellFormed ctx =
  case ctx of
    [] -> True
    (b : bs) ->
      (safeEmpty (\x -> not $ elem x $ getCtxDomain bs) $ getBindingName b)
        && ( case b of
               TmVarBind _ ty      -> isTyWellFormed bs ty
               ConstraintBind _ ty -> isMonotype ty && isTyWellFormed bs ty
               _                   -> True
           )
        && isCtxWellFormed bs
  where
    safeEmpty f x = case x of [] -> True; (x' : _) -> f x'

getCtxDomain :: BindingContext -> NameContext
getCtxDomain ctx = concat $ map getBindingName ctx

getBindingName :: Binding -> [Name]
getBindingName b = singleton $
  case b of
    TyVarBind x        -> x
    TyVarExistsBind x  -> x
    TmVarBind x _      -> x
    ConstraintBind x _ -> x
    MarkerBind _       -> []

substCtxToTy :: BindingContext -> Type -> Type
substCtxToTy ctx ty =
  case ty of
    TyUnit -> ty
    TyVar _ _ _ -> ty
    TyArrow ty1 ty2 -> TyArrow (substCtxToTy' ty1) (substCtxToTy' ty2)
    TyForAll x ty1 -> TyForAll x $ substCtxToTy' ty1
    TyVarExists x | elem (TyVarExistsBind x) ctx -> ty
    TyVarExists x | find (isConstraintBind x) ctx /= Nothing ->
      case find (isConstraintBind x) ctx of
        Just (ConstraintBind _ ty') | isMonotype ty' -> substCtxToTy' ty'
        _                                            -> TyError errMsg1
    TyError _ -> ty
    _ -> TyError $ errMsg2 ++ show ty
  where
    substCtxToTy' = substCtxToTy ctx
    isConstraintBind = (\x y -> case y of ConstraintBind z _ -> x == z; _ -> False)
    errMsg1 = "substCtxToTy in Typing: Couldn't properly find ConstraintBind after having found it"
    errMsg2 = "substCtxToTy in Typing: Couldn't find a match for "

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
    TyUnit          -> []
    TyArrow ty1 ty2 -> findTypeErrors ty1 ++ findTypeErrors ty2
    TyError e       -> [e]
    TyForAll _ ty1  -> findTypeErrors ty1
    TyVar _ _ _     -> []
    TyVarExists _   -> []

findTermErrors' :: TermNode -> String
findTermErrors' t = intercalate "\n" $ findTermErrors t

findTermErrors :: TermNode -> [String]
findTermErrors t = let tm = getTm t in
  case tm of
    TmError e -> [e]
    TmApp t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmAbs _ t1 -> findTermErrors t1
    TmAnno t1 _ -> findTermErrors t1
    _ -> []

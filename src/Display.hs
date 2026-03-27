module Display where

import           Data.List
import           Lexer
import           Syntax

showTerm' :: TermNode -> String
showTerm' t =
  case showTerm [] t of
    "()" -> showTerm [] t
    _    -> removeOuterParens $ showTerm [] t

-- currently, much of the code needs to be abstracted better. I dirtied this so that I could quickly fix the issue of name instantiation colliding with same argument space name

showTerm :: NameContext -> TermNode -> String
showTerm ctx t =
  let tm = getTm t
   in case tm of
        TmVar k l x ->
          let ctxLength = length ctx
           in if l == ctxLength
                then getNameFromContext ctx k x
                else tmVarErr l ctxLength
        TmAbs tyXs tmXs t1 ->
          let tyXs' = "[" ++ (intercalate ", " $ map (\x -> fixName ((map getName tyXs \\ [x]) ++ ctx) x) $ map getName tyXs) ++ "]"
              tmXs' = map (\x -> fixName ((map getName tmXs \\ [x]) ++ ctx) x) $ map getName tmXs
              ctx' = map (\x -> fixName ((map getName tmXs \\ [x]) ++ ctx) x) (map getName tmXs) ++ map (\x -> fixName ((map getName tyXs \\ [x]) ++ ctx) x) (map getName tyXs) ++ ctx
              tmXs'' = "(" ++ (intercalate ", " $ map (\(x, y) -> x ++ y) $ zip tmXs' $ map (showAnno ctx') tmXs) ++ ")"
           in "(" ++ "fun" ++ tyXs' ++ tmXs'' ++ showTerm ctx' t1 ++ ")"
        TmApp t1 tys ts ->
          let tys' = "[" ++ (intercalate ", " $ map (showType ctx) tys) ++ "]"
              ts' = "(" ++ (intercalate ", " $ map (showTerm ctx) ts) ++ ")"
           in "(" ++ showTerm' t1 ++ tys' ++ ts' ++ ")"
        TmAppInfer t1 ts ->
          let ts' = "(" ++ (intercalate ", " $ map (showTerm ctx) ts) ++ ")"
           in "(" ++ showTerm' t1 ++ ts' ++ ")"
  where
    showTerm' = showTerm ctx
    fixName' = fixName ctx
    tmVarErr l ctxLength = "#TmVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"
    showAnno ctx' b =
      case b of
        TmVarBind x ty -> " : " ++ showType ctx' ty
        TmVarNoBind x -> ""
        _ -> "#showAnno: got a TyVarBind binding in an annotation, which is meant to be unacheavable#"

showType' :: Type -> String
showType' ty = removeOuterParens $ showType [] ty

showType :: NameContext -> Type -> String
showType ctx ty =
  let showType' = showType ctx
   in case ty of
        TyTop -> "Top"
        TyBot -> "Bot"
        TyForAll tyXs tys ty1 ->
          let tyXs' = "(" ++ (intercalate ", " $ map (\x -> fixName ((map getName tyXs \\ [x]) ++ ctx) x) $ map getName tyXs) ++ ")"
              ctx' = map (\x -> fixName ((map getName tyXs \\ [x]) ++ ctx) x) (map getName tyXs) ++ ctx
              tys' = "(" ++ (intercalate ", " $ map (removeOuterParens . showType ctx') tys) ++ ")"
           in "(" ++ "All" ++ tyXs' ++ tys' ++ " -> " ++ showType ctx' ty1 ++ ")"
        TyError e -> e
        TyVar k l x ->
          let ctxLength = length ctx
           in if l == ctxLength
                then getNameFromContext ctx k x
                else tyVarErr l ctxLength
  where
    tyVarErr l ctxLength = "#TyVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"

getNameFromContext :: NameContext -> Index -> Name -> Name
getNameFromContext ctx ind x
  | ind >= 0 && ind < length ctx = ctx !! ind
  | otherwise = x -- "#TmVar: no name context for var#"

fixName :: NameContext -> Name -> Name
fixName ctx x
  | (length $ filter ((==) x) ctx) < 1 = x
  | otherwise = fixName ctx (x ++ "\'")

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "\n"
    ++ "Absolute Offset: "
    ++ show p
    ++ "\n"
    ++ "Line: "
    ++ show l
    ++ "\n"
    ++ "Column: "
    ++ show c

removeOuterParens :: String -> String
removeOuterParens xs
  | length xs >= 2 =
      let xs' = reverse $ getTail xs
       in if getHead xs == '(' && getHead xs' == ')'
            then reverse $ getTail xs'
            else xs
  | otherwise = xs
  where
    getHead = (\ws -> case ws of (y : _) -> y; _ -> '\0')
    getTail = (\ws -> case ws of (_ : ys) -> ys; _ -> [])

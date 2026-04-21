module Display where

import           Lexer
import           Syntax

showTerm' :: TermNode -> String
showTerm' t =
  case showTerm [] t of
    "()" -> showTerm [] t
    _    -> removeOuterParens $ showTerm [] t

showTerm :: NameContext -> TermNode -> String
showTerm ctx t =
  let tm = getTm t
   in case tm of
        TmVar k l x ->
          let ctxLength = length ctx
           in if l == ctxLength
                then getNameFromContext ctx k x
                else tmVarErr l ctxLength
        TmAbs x t1 ->
          let x' = fixName' x
           in "(" ++ "λ" ++ x' ++ "." ++ showTerm (x' : ctx) t1 ++ ")"
        TmApp t1 t2 -> "(" ++ showTerm' t1 ++ " " ++ showTerm' t2 ++ ")"
        TmUnit -> "()"
        TmAnno t1 ty -> "(" ++ showTerm (getOuterBindings ty ++ ctx) t1 ++ " : " ++ showType ctx ty ++ ")"
  where
    showTerm' = showTerm ctx
    fixName' = fixName ctx
    tmVarErr l ctxLength = "#TmVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"

showType' :: Type -> String
showType' ty = removeOuterParens $ showType [] ty

showType :: NameContext -> Type -> String
showType ctx ty =
  let showType' = showType ctx
   in case ty of
        TyUnit -> "unit"
        TyArrow ty1 ty2 -> "(" ++ showType' ty1 ++ " → " ++ showType' ty2 ++ ")"
        TyForAll x ty1 ->
          let x' = fixName ctx x
           in "(" ++ "∀" ++ x' ++ "." ++ showType (x' : ctx) ty1 ++ ")"
        TyVarExists x -> x
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

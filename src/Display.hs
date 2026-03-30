module Display where

import           Data.List
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
        TmAbs tyXs tmXs t1 ->
          let tyXs' = fixBindingNames' tyXs
              tyXs'' = "[" ++ intercalate ", " tyXs' ++ "]"
              tmXs' = fixBindingNames' tmXs
              ctx' = tyXs' ++ ctx
              ctx'' = tmXs' ++ ctx'
              argsZipAnnos = zip tmXs' (map (showAnno ctx') tmXs)
              tmXs'' = "(" ++ intercalateArgs (\(x, y) -> x ++ y) argsZipAnnos ++ ")"
           in "(" ++ "fun" ++ tyXs'' ++ tmXs'' ++ showTerm ctx'' t1 ++ ")"
        TmApp t1 tys ts ->
          let tys' = "[" ++ intercalateArgs (showType ctx) tys ++ "]"
              ts' = "(" ++ intercalateArgs (showTerm ctx) ts ++ ")"
           in "(" ++ showTerm ctx t1 ++ tys' ++ ts' ++ ")"
        TmAppInfer t1 ts ->
          let ts' = "(" ++ intercalateArgs (showTerm ctx) ts ++ ")"
           in "(" ++ showTerm ctx t1 ++ ts' ++ ")"
        TmError e -> e
        _ -> "#Bad term for display:\n" ++ show tm ++ "#"
  where
    tmVarErr l ctxLength = "#TmVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"
    fixBindingNames' = fixBindingNames ctx

showType' :: Type -> String
showType' ty = removeOuterParens $ showType [] ty

showType :: NameContext -> Type -> String
showType ctx ty =
  case ty of
    TyTop -> "Top"
    TyBot -> "Bot"
    TyForAll tyXs tys ty1 ->
      let tyXs' = fixBindingNames ctx tyXs
          tyXs'' = "(" ++ intercalate ", " tyXs' ++ ")"
          ctx' = tyXs' ++ ctx
          tys' = "(" ++ intercalateArgs (removeOuterParens . showType ctx') tys ++ ")"
       in "(" ++ "All" ++ tyXs'' ++ tys' ++ " -> " ++ showType ctx' ty1 ++ ")"
    TyError e -> e
    TyVar k l x ->
      let ctxLength = length ctx
       in if l == ctxLength
            then getNameFromContext ctx k x
            else tyVarErr l ctxLength
    _ -> "#Bad type for display:\n" ++ show ty ++ "#"
  where
    tyVarErr l ctxLength = "#TyVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"

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

-- Display helper functions below this line

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

fixName :: NameContext -> Name -> Name
fixName ctx x
  | (length $ filter ((==) x) ctx) < 1 = x
  | otherwise = fixName ctx (x ++ "\'")

getNameFromContext :: NameContext -> Index -> Name -> Name
getNameFromContext ctx ind x
  | ind >= 0 && ind < length ctx = ctx !! ind
  | otherwise = x -- "#TmVar: no name context for var#"

showAnno :: NameContext -> Binding -> String
showAnno ctx b =
  case b of
    TmVarBind _ ty -> " : " ++ showType ctx ty
    TmVarNoBind _ -> ""
    _ -> "#showAnno: got a TyVarBind binding in an annotation, which is meant to be unacheavable#"

intercalateArgs :: (a -> String) -> [a] -> String
intercalateArgs f xs = intercalate ", " $ map f xs

fixBindingNames :: NameContext -> [Binding] -> [Name]
fixBindingNames ctx xs =
  let xs' = getNames xs
   in map (\x -> fixName ((xs' \\ [x]) ++ ctx) x) xs'

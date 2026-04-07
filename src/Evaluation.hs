module Evaluation where

import Syntax
import Helper
import Display

eval1 :: TermNode -> TermNode
eval1 t = let tm = getTm t; fi = getFI t in
  TermNode fi $
  case tm of
    TmApp (TermNode _ (TmAbs _ t11)) v2 | isVal v2 -> getTm $ evalSubst v2 t11
    TmApp v1 t2 | isVal v1 ->
      let result = eval1 t2
       in checkError result $ TmApp v1 result
    TmApp t1 t2 | not $ isVal t1 ->
      let result = eval1 t1
       in checkError result $ TmApp result t2
    TmAnno t1 ty | not $ isVal t1 ->
      let result = eval1 t1
       in checkError result $ TmAnno result ty
    TmAnno v1 ty -> getTm $ shift' 0 (negate $ length $ getOuterBindings ty) $ collapseAnnos v1
    _ -> TmError $ "No rule applies" ++ showFileInfo fi
  where
    checkError :: TermNode -> Term -> Term
    checkError term result =
      case getTm term of
        TmError e -> TmError e
        _ -> result
    -- Goodbye subject reduction... I promise we'll meet again, one day
    collapseAnnos :: TermNode -> TermNode
    collapseAnnos (TermNode fi tm) =
      TermNode fi $
        case tm of
          TmApp t1 t2 -> TmApp (collapseAnnos t1) (collapseAnnos t2)
          TmAbs x t1 -> TmAbs x (collapseAnnos t1)
          TmAnno t1 ty1 -> getTm $ shift' 0 (negate $ length $ getOuterBindings ty1) $ collapseAnnos t1
          _ -> tm

type Counter = Int

eval' :: TermNode -> (Counter, TermNode)
eval' t = eval 0 t

eval :: Counter -> TermNode -> (Counter, TermNode)
eval n t@(TermNode _ (TmError e)) = (n, t)
eval n t | isVal t   = (n, t)
         | otherwise = eval (n + 1) $ eval1 t

evalN :: Counter -> TermNode -> (Counter, TermNode)
evalN n t@(TermNode _ (TmError e)) = (n, t)
evalN n t | n <= 0 || isVal t = (n, t)
          | otherwise         = evalN (n - 1) $ eval1 t

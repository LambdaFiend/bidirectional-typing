{
module Parser where

import Lexer
import Syntax
}

%monad { Either String } { (>>=) } { return }
%name parser
%tokentype { Token }
%error { parseError }

%token

fun     { Token pos FUN }
","     { Token pos COMMA }
":"     { Token pos COLON }
"("     { Token pos LPAREN }
")"     { Token pos RPAREN }
"["     { Token pos LBRACK }
"]"     { Token pos RBRACK }
bot     { Token pos BOT }
top     { Token pos TOP }
forall  { Token pos FORALL }
"->"    { Token pos ARROW }
idLower { Token pos (IDLower s) }
idUpper { Token pos (IDUpper s) }

%%

Term
  : App { $1 }
  | Fun { $1 }

App
  : App "[" TypeMany "]" "(" AtomMany ")" { TermNode (getFI $1) $ TmApp $1 $3 $6 }
  | App "[" TypeMany "]" "(" ")"          { TermNode (getFI $1) $ TmApp $1 $3 [] }
  | App "[" "]" "(" AtomMany ")"          { TermNode (getFI $1) $ TmApp $1 [] $5 }
  | App "(" AtomMany ")"                  { TermNode (getFI $1) $ TmAppInfer $1 $3 }
  | App "(" ")"                           { TermNode (getFI $1) $ TmAppInfer $1 [] }
  | Atom                                  { $1 }

Atom
  : Value        { $1 }
  | "(" Term ")" { $2 }

Value
  : NameLower { TermNode (fst $1) $ TmVarRaw (snd $1) }

AtomMany
  : Atom "," AtomMany { $1 : $3 }
  | Atom              { $1 : [] }

TypeMany
  : Type "," TypeMany { $1 : $3 }
  | Type              { $1 : [] }

Fun
  : fun "[" NameUpperMany "]" "(" NameLowerMany ")" Term     { TermNode (tokenPos $1) $ TmAbs $3 $6 $8 }
  | fun "[" NameUpperMany "]" "(" NameLowerManyAnno ")" Term { TermNode (tokenPos $1) $ TmAbs $3 $6 $8 }
  | fun "[" NameUpperMany "]" "(" ")" Term                   { TermNode (tokenPos $1) $ TmAbs $3 [] $7 }
  | fun "[" "]" "(" NameLowerMany ")" Term                   { TermNode (tokenPos $1) $ TmAbs [] $5 $7 }
  | fun "[" "]" "(" NameLowerManyAnno ")" Term               { TermNode (tokenPos $1) $ TmAbs [] $5 $7 }

NameLower : idLower { (tokenPos $1, (\(IDLower s) -> s) $ tokenDat $1) }

NameLowerMany
  : NameLower "," NameLowerMany { TmVarNoBind (snd $1) : $3 }
  | NameLower                   { TmVarNoBind (snd $1) : [] }

NameLowerManyAnno
  : NameLower ":" Type "," NameLowerManyAnno { TmVarBind (snd $1) $3 : $5 }
  | NameLower ":" Type                       { TmVarBind (snd $1) $3 : [] }

NameUpper : idUpper { (tokenPos $1, (\(IDUpper s) -> s) $ tokenDat $1) }

NameUpperMany
  : NameUpper "," NameUpperMany { TyVarBind (snd $1) : $3 }
  | NameUpper                   { TyVarBind (snd $1) : [] }

Type : TypeForAll { $1 }

TypeForAll
  : forall "(" NameUpperMany ")" "(" TypeMany ")" "->" Type { TyForAll $3 $6 $9 }
  | forall "(" NameUpperMany ")" "(" ")" "->" Type          { TyForAll $3 [] $8 }
  | forall "(" ")" "(" TypeMany ")" "->" Type               { TyForAll [] $5 $8 }
  | TypeAtom                                                { $1 }

TypeAtom
  : NameUpper    { TyVarRaw (snd $1) }
  | bot          { TyBot }
  | top          { TyTop }
  | "(" Type ")" { $2 }

{
parseError :: [Token] -> Either String a
parseError [] = Left ("Parsing error near the end of the file")
parseError ((Token fi _):tokens) = Left ("Parsing error at:" ++ showFileInfoHappy fi)
parseError (x:xs) = Left "Parsing error"

showFileInfoHappy :: AlexPosn -> String
showFileInfoHappy (AlexPn p l c) =
  "\n" ++"Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c
}

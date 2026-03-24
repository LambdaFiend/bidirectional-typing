{
module Lexer where
}

%wrapper "posn"

$white = [\ \t\n\r\b]
$digit = [0-9]
$lower = [a-z]
$alpha = [a-zA-Z]

tokens :-

$white+  ;
"."            { \pos _ -> Token pos DOT }
":"            { \pos _ -> Token pos COLON }
"("            { \pos _ -> Token pos LPAREN }
")"            { \pos _ -> Token pos RPAREN }
(\\)|"λ"       { \pos _ -> Token pos LAMBDA }
All|forall|"∀" { \pos _ -> Token pos FORALL }
"->"|"→"       { \pos _ -> Token pos ARROW }
unit           { \pos _ -> Token pos TYUNIT }
$alpha+("\'"*) { \pos s -> Token pos $ ID s }
.              { \pos s -> Token pos $ ERROR ("Lexing error: " ++ s) }

{
data Token = Token
  { tokenPos :: AlexPosn
  , tokenDat :: TokenData
  }
  deriving (Eq, Show)

data TokenData
  = DOT
  | SEMI
  | LPAREN
  | RPAREN
  | LAMBDA
  | FORALL
  | ARROW
  | TYUNIT
  | ID String
  | ERROR String
  deriving (Eq, Show)
}

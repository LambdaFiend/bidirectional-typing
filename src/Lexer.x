{
module Lexer where
}

%wrapper "posn"

$white = [\ \t\n\r\b]
$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]

tokens :-

$white+              ;
fun                  { \pos _ -> Token pos FUN }
Bot                  { \pos _ -> Token pos BOT }
Top                  { \pos _ -> Token pos TOP }
","                  { \pos _ -> Token pos COMMA }
":"                  { \pos _ -> Token pos COLON }
"("                  { \pos _ -> Token pos LPAREN }
")"                  { \pos _ -> Token pos RPAREN }
"["                  { \pos _ -> Token pos LBRACK }
"]"                  { \pos _ -> Token pos RBRACK }
All                  { \pos _ -> Token pos FORALL }
"->"                 { \pos _ -> Token pos ARROW }
($lower+)(\')*       { \pos s -> Token pos $ IDLower s }
($upper$lower*)(\')* { \pos s -> Token pos $ IDUpper s }
.                    { \pos s -> Token pos $ ERROR ("Lexing error: " ++ s) }

{
data Token = Token
  { tokenPos :: AlexPosn
  , tokenDat :: TokenData
  }
  deriving (Eq, Show)

data TokenData
  = FUN
  | COMMA
  | COLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | FORALL
  | ARROW
  | TOP
  | BOT
  | IDLower String
  | IDUpper String
  | ERROR String
  deriving (Eq, Show)
}

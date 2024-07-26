{
    module Lexer where
}

%wrapper "basic"

$alpha = [a-z]
$alphanum = [a-zA-z0-9]

tokens :-
    $white+;
    "not"                           { \s -> Tnot }
    $alpha [$alpha $alphanum \_\']* { \s -> Tatom s }
    ":-"                            { \s -> Timpliedby }
    "."                             { \s -> Tdot }
    "*"                             { \s -> Tod }
    ","                             { \s -> Tcomma }
    "-"                             { \s -> Tdash }

{

data Token
    = Tatom String
    | Timpliedby
    | Tdot
    | Tod
    | Tcomma
    | Tnot
    | Tdash
    deriving (Eq,Show)

scanTokens = alexScanTokens

}
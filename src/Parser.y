{
module Parser where
import Lexer
import Types
}

%name parseLPOD
%tokentype { Token }
%error { parseError }

%token
    atom    { Tatom $$ }
    '.'     { Tdot }
    '*'     { Tod }
    ':-'    { Timpliedby }
    ','     { Tcomma }
    'not'   { Tnot }
    '-'     { Tdash }

%%

Program       : RuleList             { $1 }

RuleList      : Rule RuleList        { $1 : $2 }
              |                      { [] }

Rule          : ODList Body '.'      { ($1, $2) }

ODList        : Atom ODRest          { $1 : $2 }

ODRest        : '*' Atom ODRest      { $2 : $3 }
              |                      { [] }

Body          : ':-' LitList         { $2 }
              |                      { [] }

LitList       : Literal LitRest      { $1 : $2 }

LitRest       : ',' Literal LitRest  { $2 : $3 }
              |                      { [] }

Literal       : 'not' Atom           { NotL $2 }
              | Atom                 { PosL $1 }

Atom          : '-' atom             { NotA $2 }
              | atom                 { PosA $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
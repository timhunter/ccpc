%{
open Grammar
%}

%token ARROW NEWLINE EOF QUOTE LBRAC RBRAC COMMA INT CONCAT EPSILON
%token <string> CAT TERM
%token <int> INT
%start mcfgrule
%type <Grammar.t> mcfgrule
%%

mcfgrule:
   rule     {[$1]}
|  rule mcfgrule {$1::$2};

rule:
   CAT ARROW children stringyield NEWLINE {($1, $3, $4) }
|  CAT ARROW QUOTE TERM QUOTE NEWLINE {($1, [$4], [[Component(0,0)]])}
|  CAT ARROW QUOTE QUOTE NEWLINE {($1, [" "], [[]])};

children:
   CAT {[ $1 ]}
|  CAT children {$1::$2};

stringyield:
   LBRAC component RBRAC { [$2] }
|  LBRAC component RBRAC stringyield { $2 :: $4 };

component:
   tuple { [$1] }
|  EPSILON { [Epsilon] }
|  tuple CONCAT component { $1 :: $3 }
|  EPSILON CONCAT component { Epsilon :: $3 };

tuple:
   INT COMMA INT { Component($1,$3) };

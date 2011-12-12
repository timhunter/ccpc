%{
open Rule
open Util
open Rational
%}
%token ARROW NEWLINE EOF LBRAC RBRAC COMMA INT CONCAT EPSILON SLASH TERM_EMPTY
%token <string> CAT TERM
%token <int> INT
%start mcfgrule
%type <Rule.r list> mcfgrule
%%

mcfgrule:
   rule     {[$1]}
|  rule mcfgrule {$1::$2}
|  NEWLINE {[]}
|  NEWLINE mcfgrule {$2}
;

rule:
   weight CAT ARROW children stringyield NEWLINE {Rule.create_rule ($2, $4, $5, $1)}
|  weight CAT ARROW TERM NEWLINE {Rule.create_terminating ($2, $4, $1)}
|  weight CAT ARROW TERM_EMPTY NEWLINE {Rule.create_terminating ($2, " ", $1)}
;

weight:
   {None}
|  INT SLASH INT {Some ($1,$3)}
;

children:
   CAT {[ $1 ]}
|  CAT children {$1::$2};

stringyield:
   LBRAC component RBRAC {Rule.create_tuplerecipe $2  }
|  LBRAC component RBRAC stringyield { Rule.add_to_recipe $2 $4 };

component:
   tuple { [$1] }
|  EPSILON { [Rule.Epsilon] }
|  tuple CONCAT component { $1 :: $3 }
|  EPSILON CONCAT component { Rule.Epsilon :: $3};

tuple:
   INT COMMA INT { Rule.Component($1,$3) };

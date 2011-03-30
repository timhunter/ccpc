%{
open Rule
open Util
%}

%token ARROW NEWLINE EOF QUOTE LBRAC RBRAC COMMA INT CONCAT EPSILON
%token <string> CAT TERM
%token <int> INT
%start mcfgrule
%type <Rule.r list> mcfgrule
%%

mcfgrule:
   rule     {[$1]}
|  rule mcfgrule {$1::$2};

rule:
   CAT ARROW children stringyield NEWLINE {Rule.create_rule ($1, $3, $4)}
|  CAT ARROW QUOTE TERM QUOTE NEWLINE {Rule.create_terminating ($1, $4)}
|  CAT ARROW QUOTE QUOTE NEWLINE {Rule.create_terminating ($1, " ")};

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

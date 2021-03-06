%{
open Rule
open Util
%}
%token ARROW NEWLINE EOF LBRAC RBRAC COMMA DIGITS CONCAT SLASH TERM_EMPTY
%token <string> CAT TERM DIGITS
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
   weight CAT ARROW children stringyield NEWLINE {Rule.create_nonterminating ($2, $4, $5, $1)}
|  weight CAT ARROW TERM NEWLINE {Rule.create_terminating ($2, $4, $1)}
|  weight CAT ARROW TERM_EMPTY NEWLINE {Rule.create_terminating ($2, " ", $1)}
;

weight:
   {no_weight}
|  DIGITS SLASH DIGITS { try
			   make_weight (Big_int.big_int_of_string $1) (Big_int.big_int_of_string $3)
                          with
			  _ -> failwith ("could not read the weight: "^$1^" / "^$3^"\n")
                  }
;

children:
   CAT {[ $1 ]}
|  CAT children {$1::$2};

stringyield:
   LBRAC component RBRAC {Rule.create_tuplerecipe $2  }
|  LBRAC component RBRAC stringyield { Rule.add_to_recipe $2 $4 };

component:
   tuple { [$1] }
|  tuple CONCAT component { $1 :: $3 }

tuple:
   DIGITS COMMA DIGITS { Rule.create_component (int_of_string $1) (int_of_string $3) };

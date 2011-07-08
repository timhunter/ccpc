%{
open Rule
open Util
open Rational
%}

%token ARROW NEWLINE EOF QUOTE LBRAC RBRAC COMMA INT CONCAT EPSILON SLASH
%token <string> CAT TERM
%token <int> INT
%start mcfgrule
%type <Rule.r list> mcfgrule
%%

mcfgrule:
   rule     {[$1]}
|  rule mcfgrule {$1::$2};

rule:
	INT SLASH INT CAT ARROW children stringyield NEWLINE {Rule.create_rule ($4, $6, $7, Some ($1,$3))}
|  INT SLASH INT CAT ARROW QUOTE TERM QUOTE NEWLINE {Rule.create_terminating ($4, $7, Some ($1,$3))}
|  INT SLASH INT CAT ARROW QUOTE QUOTE NEWLINE {Rule.create_terminating ($4, " ", Some ($1,$3))}
|   CAT ARROW children stringyield NEWLINE {Rule.create_rule ($1, $3, $4, None)}
|  CAT ARROW QUOTE TERM QUOTE NEWLINE {Rule.create_terminating ($1, $4, None)}
|  CAT ARROW QUOTE QUOTE NEWLINE {Rule.create_terminating ($1, " ", None)};

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

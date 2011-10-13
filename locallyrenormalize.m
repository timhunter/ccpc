#!/Volumes/mechanical/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script
# on garvin
#   /usr/local/bin/MathematicaScript -script

Off[FindRoot::precw,General::compat];

Needs["Combinatorica`"]


MCFGFromTable[tbl_,startsymbolregexp_ :"S.*"] := Module[{nonterminals,terminals},
LhsRhsSymbol[line_] :=Switch[Length[line],
6, {Part[line,4],Part[line,6]},   (* terminal line *)
7,{Part[line,4],Part[line,6]},    (* unary with string rewriting recipe *)
8,{Part[line,4],Part[line,6],Part[line,7]}, (* binary *)
_,{}];

LhsSymbol[line_] :=Switch[Length[line],
6, {Part[line,4]},
7,{Part[line,4]},
8,{Part[line,4]},
_,{}];

kind[f_] := DeleteDuplicates[Flatten[f /@ tbl]];

nonterminals = kind[LhsSymbol];
terminals = Complement[kind[LhsRhsSymbol],kind[LhsSymbol]];

markup[line_] := Switch[Length[line], (* throw away recipe *)
6, {"Preterminal",Part[line,1]/Part[line,3],Part[line,4],Part[line,6]},
7,{"Unary",Part[line,1]/Part[line,3],Part[line,4],Part[line,6],Part[line,7]},
8,{"Binary",Part[line,1]/Part[line,3],Part[line,4],Part[line,6],Part[line,7],Part[line,8]}, 
_,{}];

Return[{ First[Select[nonterminals,StringMatchQ[#,RegularExpression[startsymbolregexp]]&]],(* assume it's the first one *)
              nonterminals,   (* second and third components constitute a disjoint set of vertex labels *)
              terminals,
	     markup/@ tbl 
	}]
];

LocallyNormalize[grammar_List]:= Module[{start,nonterminals,terminals,rules,sums,both,divide},
{start,nonterminals,terminals,rules}=grammar;
sums=Function[symbol,Fold[Plus,0,Part[#,2]&/@Select[rules,Part[#,3]==symbol&]]]/@nonterminals;

both=Join[nonterminals,terminals];

divide[wrule_]:=Module[{originalweight,nonterminalnumber,nonterminalsum},originalweight=Part[wrule,2];
nonterminalnumber=FromDigits[FromDigits[Position[both,Part[wrule,3]]]];
nonterminalsum=N[originalweight/(sums[[nonterminalnumber]]),MachinePrecision]; (* become machine-precision *)
ReplacePart[wrule,2->nonterminalsum]];

Return[{start,nonterminals,terminals,(divide/@rules)}]
];

(* for output *)
enquote[s_] := FromCharacterCode[Prepend[Append[ToCharacterCode[s],34],34]]

showprob[p_] := Module[{num,denom},
	If[p==1, (num=1;denom=1),
	If[Chop[p]==0,(num=0;denom=0),
		If[Rationalize[p]!= p,(num=Numerator[Rationalize[p]];denom=Denominator[Rationalize[p]]),
			denom=10^IntegerPart[Precision[p]];num=Round[denom*p]]]];
	StringJoin[ToString[num]," / ",ToString[denom]]
]
showrule[{kind_,prob_,lhs_,child_,recipe_}] :=StringJoin[showprob[prob],"     ",lhs," --> ",child," ", recipe]/;kind=="Unary"
showrule[{kind_,prob_,lhs_,child1_,child2_,recipe_}]:= StringJoin[showprob[prob],"     ",lhs," --> ",child1," ", child2," ",recipe]/; kind=="Binary"
showrule[{kind_, prob_, lhs_, child_}] := 
 StringJoin[showprob[prob], "     ", lhs, 
   " --> EMPTY"] (*no recipe for preterminals*)/; child == " "
showrule[{kind_, prob_, lhs_, child_}] := 
 StringJoin[showprob[prob], "     ", lhs, " --> ", 
   enquote[child]] (*no recipe for preterminals*)/; 
  kind == "Preterminal" && child != " "


ExtractComment[tbl_] := Module[{comments = {}, rules = {}},
  
  	DispatchLine[line_] := Module[{},
    	If[MatchQ["(*", First[line]] && MatchQ["*)", Last[line]],
     	AppendTo[comments, line],
     	AppendTo[rules, line]]
    ];
  
  	Scan[DispatchLine, tbl];
  	   Return[{comments, rules}]
  ];


(* actually do it *)
{metadata,tbl} = ExtractComment[Import[$ScriptCommandLine[[2]],"Table"]];
grammar = MCFGFromTable[tbl];
result = LocallyNormalize[grammar];
lose = SetPrecision[result,9]

Print[ExportString[metadata,"Table","FieldSeparators" -> " "]];
Scan[Print,showrule /@ lose[[4]]]

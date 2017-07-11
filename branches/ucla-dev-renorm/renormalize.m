#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script
# on linux /usr/local/bin/MathematicaScript -script
# on Mac OS X  /Applications/Mathematica.app/Contents/MacOS/MathematicaScript

Off[FindRoot::precw,FindRoot::bddir,General::compat];

Needs["Combinatorica`"]


MCFGFromTable[tbl_,startsymbolregexp_ :"S(_)?([0-9]+|-)*$"] := Module[{nonterminals,terminals},
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


GraphProjection[g_ ]:= Module[{start,nonterminals,terminals,vertices,rules,edges,i},
	{start,nonterminals,terminals,rules} =g;
	vertices = Join[nonterminals,terminals];
	edges ={};
	
	lookup[rulenum_,eltnum_] := FromDigits[FromDigits[Position[vertices,rules[[rulenum]][[eltnum]]]]];

	For[i=1,i<= Length[rules],i++,
	    Switch[First[rules[[i]]],
	 "Preterminal",AppendTo[edges,{lookup[i,3],lookup[i,4]}],
	"Unary",AppendTo[edges,{lookup[i,3],lookup[i,4]}],
	"Binary",AppendTo[edges,{lookup[i,3],lookup[i,4]}];
			   AppendTo[edges,{lookup[i,3],lookup[i,5]}]
	]
	];

Return[FromOrderedPairs[edges]]
];


ftimeDFSVisit[u_Integer] :=
 		
 Module [{}, (* recursive helper function for ftimeDFS, below *)
  		
  color[[u]] = Gray;
  			Scan[ (If[color[[#]] == White,
      				ftimeDFSVisit[#];
      				color[[u]] = Black];
     				   t++;
     			   ftime[[u]] = t;
     		) &, e[[u]]
   			]
  		]


ftimeDFS[g_Graph] := (* this function computes a list of finishing \
times in a DFS traversal of g. p541 of Cormen, Leisersohn & Rivest *)
 	Block[{color = Table[White, {V[g]}], t = 0, e = ToAdjacencyLists[g],
    ftime = Table[-1, {V[g]}], $RecursionLimit = Infinity},
  	(* for each vertex, check if we've been there before, 
  otherwise call ftimeDFSVisit *)
  	
  Do[If[color[[vert]] == White, ftimeDFSVisit[vert]], {vert, 1, V[g]}];
  	
  	Return[Sort[Range[1, V[g]], (ftime[[#1]] > ftime[[#2]]) &]] (* 
  Descending order order on times = Ascending whole numbers *)
  	]



myDFSVisit[u_Integer] := 
 Module [{}, (* recursive helper function for myDFS, below *)
  	
  AppendTo[lastfound, u];
  	color[[u]] = Gray;
  	Scan[ (If[color[[#]] == White,
      			myDFSVisit[#];
      			color[[u]] = Black]) &, e[[u]]
   		]
  ]


myDFS[graph_Graph, 
  orderedvertices_List] := (* same algorithm but keep track of all \
vertices in what CLR call the Depth-first Forest *)
 	
 Block[{color = Table[White, {V[graph]}], e = ToAdjacencyLists[graph],
    lastfound = {}, $RecursionLimit = Infinity},
  	(If[color[[#]] == White, myDFSVisit[#]; AppendTo[SCCs, lastfound]; 
      lastfound = {}]) & /@ orderedvertices
  	]


getSCCs[graph_Graph] := 
 Block[{SCCs = {}},
  	myDFS[ReverseEdges[graph], ftimeDFS[graph]];
  	Return[Reverse[SCCs]]
  	]


CalculateZ[grammar_,graphprojection_,sortedSCCs_] := Module[{myZ,start,nonterminals,terminals,rules,tbl,vertices,vertexNumber,vertexLabel,eqns,vars,zValues},
{start,nonterminals,terminals,rules} =grammar;
vertices = Join[nonterminals,terminals]; (* this is the ordering used in GraphProjection *)

(* lookup table maps from a given nonterminal number to all the rules sharing that nonterminal *)
tbl = Function[nt,Select[rules,Part[#,3]==nt&]]/@ nonterminals;

(* Initialize the list of Z values as 0 *)
myZ=Table[0,{V[graphprojection]}];

(* vertexNumber -> x_vertexNumber *)
XVariable[n_]:=Subscript[x,n];

vertexNumber[v_String] := FromDigits[FromDigits[Position[vertices,v]]]; (* just as in GraphProjection *)
vertexLabel[v_Integer] := vertices[[v]];

(* Recursive Function Falpha: Falpha[scc,rhs] calculates the value of f_a for the given scc, following the instruction in Nederhof & Satta (2008) p .146 *)
(* caller: Expr *)
Falpha[scc_,rhs_]:= Module[{},
If[rhs=={},
Return[1], (* (22) f_epsilon = 1 *)
If[MemberQ[terminals,vertexLabel[First[rhs]]],Return[Falpha[scc,Rest[rhs]]], (* (23) f_ab = f_b, if a is terminal *)
If[!MemberQ[scc,First[rhs]],
Return[myZ[[First[rhs]]]*Falpha[scc,Rest[rhs]]],(* (24) f_Bb = Z(B) * f_b, if B is not a member of the scc *)
Return[XVariable[First[rhs]]*Falpha[scc,Rest[rhs]]] (* (25) f_Bb = x_i * f_b, if B is a member of the scc *)
];
];
];
];

(* Function Expr: Expr[scc,vertex] gives a polynomial expression for each vertex in the scc, which eventually comprises Eqns *)
Expr[scc_,vertex_]:=Module[{myValue,weight,rhs,relevantRules},
If [MemberQ[terminals,vertexLabel[vertex]],Return[-XVariable[vertex]+1]];

(* otherwise... *)
myValue=-XVariable[vertex];

Scan[Function[r,weight = r[[2]];
	rhs = vertexNumber /@ Switch[First[r],"Binary",{r[[4]],r[[5]]},_,{r[[4]]}];
	myValue+=weight*Falpha[scc,rhs]],tbl[[vertex]]];

Return[myValue];
];

Vars[scc_] := Module[{poly},
	poly=Expr[scc,#]&/@ scc;
	Return[Variables[poly]]
];

Eqns[scc_]:= (Expr[scc,#]==0)&/@scc;

(* For each scc, calculate the Z value of all the vertices in it *)
Scan[Function[myBucket,If [Length[myBucket]==0, 
(* Case 0: the scc is empty -> something is wrong. *)
Print["Fail: The scc is empty",i], (* Jiwon: break here!! *)

(* Case 1: otherwise we can calculate the inside probability of each node in the scc! *)
eqns=Eqns[myBucket];
vars=Vars[myBucket];
zValues=vars/.FindRoot[eqns,({#,0}&/@vars),WorkingPrecision->16 (* MachinePrecision+2 *)]; (* motivated by sentence 6 *)
For[j=1,j<=Length[vars],j++, 
(* [vars[[j]][[2]]] = subscript number = vertex number *)
If[vars[[j]][[2]]==0,Print[eqns," got set to zero. increase WorkingPrecision?"];Abort[]];
(* Print["node ",vars[[j]][[2]]," Z=",zValues[[j]]];*)
myZ[[vars[[j]][[2]]]]=zValues[[j]]; 
]
]
],sortedSCCs];

Return[myZ]
];


TiltGrammar[g_,z_]:= Module[{start,nonterminals,terminals,rules,amount},
	{start,nonterminals,terminals,rules} =g;
	vertices = Join[nonterminals,terminals];
	vertexNumber[v_String] := FromDigits[FromDigits[Position[vertices,v]]]; (* just as in GraphProjection *)

	zParent[lhs_String] := z[[vertexNumber[lhs]]];
         zChildren[rhs_List] := Fold[Times,1,Function[sym,If[MemberQ[terminals,sym],1,z[[vertexNumber[sym]]]]]/@rhs];

	       TiltRule[wrule_] := Module[{originalprob,lhs,rhs},
	originalprob = Part[wrule,2];
	lhs =Part[wrule,3];
	rhs = Switch[Part[wrule,1],"Binary",{Part[wrule,4],Part[wrule,5]},_,{Part[wrule,4]}];
	If[zParent[lhs]==0,Print["couldn't find a z for ",lhs," number ",vertexNumber[lhs]];Abort[]];
	amount = zChildren[rhs]/zParent[lhs];
	(* If[Abs[1-amount]>10^-10,Print["tilting ",wrule," by ",amount]]; *)
	ReplacePart[wrule,2-> (originalprob*amount)]
	];

	Return[{start,
                      nonterminals,
                      terminals,
	             TiltRule/@rules}]
];


(* might as well compute the entropy of the start symbol while we're at it *)
littleh[g_] := Module[{start, nonterminals, terminals, rules, Eta},
   			{start, nonterminals, terminals, rules} = g;
   
   			Eta[p_] := -p*Log2[p];(* is there a better name? *)
   
   			Return[
    				Function[nt,
      					
      Fold[Plus, 0, 
       Eta[Part[#, 2]] & /@ Select[rules, Part[#, 3] == nt & ]]
      		                    ] /@ nonterminals
    				]
   			
   			];

fertilitymatrix[g_] := 
  Module[{start, nonterminals, terminals, rules, idx, a, updateA},
   			                    {start, nonterminals, terminals, rules} = 
    g;
   					idx[nt_String] := 
    FromDigits[FromDigits[Position[nonterminals, nt]]];
   					a = 
    ConstantArray[0, {Length[nonterminals], Length[nonterminals]}];
   
   					updateA[r_] := Module[{prob, lhs},
     	                                              
     prob = Part[r, 2];
     						lhs = Part[r, 3];
     						
     If[MemberQ[nonterminals, Part[r, 4]], 
      a[[idx[lhs]]][[idx[Part[r, 4]]]] += prob];
     
     						
     If [Part[r, 1] == "Binary" && MemberQ[nonterminals, Part[r, 5]], 
      a[[idx[lhs]]][[idx[Part[r, 5]]]] += prob] 
     					];
   					Scan[updateA, rules];
   					Return[a]
   ];

entropyOfGrammar[g_] := 
  Module[{start, nonterminals, terminals, rules, idx, fert, h,   spectralradius, everybody},
     {start, nonterminals, terminals, rules} =   g;
   					idx[nt_String] := 
    FromDigits[FromDigits[Position[nonterminals, nt]]];
   					h = littleh[g];
   					fert = fertilitymatrix[g];
   
   					spectralradius = Max[Abs /@ Eigenvalues[fert]];
   					If[spectralradius > 1, 
    Print["entropyOfGrammar: grammar not consistent"]];
   
   					everybody = (Inverse[
       IdentityMatrix[Length[nonterminals]] - fert]) . h;
   					(* Return[{everybody[[idx[start]]],everybody}] *)
   					
   everybody[[idx[start]]]
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


unquoteterminal[line_List] := Switch[Length[line],
  6, Join[line[[1 ;; 5]], {StringReplace[Part[line, 6],RegularExpression["\"([^\"]+)\""] -> "$1"]}],
  _, line
  ]

makeWeightsNumbers[line_List] := 
 Module[{numerator = ToExpression[line[[1]]], denominator = ToExpression[line[[3]]]},
	ReplacePart[line, {1 -> numerator, 3 -> denominator}]
  ]

(* actually do it *)

(* old way *)
(* {metadata,tbl} = ExtractComment[Import[$ScriptCommandLine[[2]],"Table"]];*)
{metadata,pretbl} = ExtractComment[StringSplit[#, Whitespace] & /@ StringSplit[Import[$ScriptCommandLine[[2]], "Text"], "\n"]];
tbl = Composition[makeWeightsNumbers, unquoteterminal] /@ pretbl;
grammar = MCFGFromTable[tbl];
graph = GraphProjection[grammar];
SCCs = getSCCs[graph];

startsymbolindex = First[First[Position[grammar[[2]], grammar[[1]]]]];
z = CalculateZ[grammar,graph,SCCs];

(* add some useful metadata *)
AppendTo[metadata, {"(*", ("probability = " <> ToString[CForm[z[[startsymbolindex]]]]), "*)"}]

tilted = TiltGrammar[grammar,z];

(*  add some useful metadata  *)
AppendTo[metadata, {"(*", ("entropy = " <> ToString[CForm[entropyOfGrammar[tilted]]]), "*)"}]

lose = tilted;  (* SetPrecision[tilted,9]; *\) (\* try keeping it all *)

Print[ExportString[metadata,"Table","FieldSeparators" -> " "]];
Scan[Print,showrule /@ lose[[4]]];
Exit[]

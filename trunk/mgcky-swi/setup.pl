% file: setup.pl
% origin author : E Stabler
% origin date: Jan 2001
% purpose: load files for CKY-like MG parser, swi version
% use: This is the top file for loading files and demos.
%      If this load prompts about redefining predicates, type p(roceed)
% updates: June 2001- Willemijn Vermaat 
% updates: May 2002 - Stabler - extension to standard transformational grammar
% updates: Jul 2010 - Stabler - update for SWI Prolog features
% todo:

% operator defs - don't touch these
:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features
:- op(500, xf, <=).		% for right incorporation
:- op(500, fx, =>). % for left incorporation
:- op(500, xf, ==>). % for right affix hop
:- op(500, fx, <==). % for left affix hop
:- op(500, xfy, <<). % for adjunction
:- op(500, xfy, >>). % for adjunction

% for tree display
:- ensure_loaded('tree_display/wish_treeSWI').
:- ensure_loaded('tree_display/latex_treeSWI').
:- ensure_loaded('tree_display/pptree').

% uncomment ONE of the recognizers and display tools
%:- ensure_loaded('parser/mgpx'),ensure_loaded('parser/lpx').  % basic MG parser and lexical sequence parser
:- ensure_loaded('parser/mghapx'),ensure_loaded('parser/lhapx').  % TG parser and lexical sequence parser

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% New code for putting multiple trees into a single tex file.
%%% Tim Hunter, October 2011
%%% Zhong Chen, August 2012

% Top-level function to be called from outside.
% - IntroLines are some lines of latex code which will be placed in the body of the document, 
%   before the trees.
% - DerivLists is a list of lists, each of which has a 'note' with which to annotate 
%   the tree as its head, and the derivation string to be passed to Stabler's code as its tail.
% - Filename is the name of the latex file to save to.
parse_and_display(IntroLines, Prefix, Entropy, DerivLists, Filename) :-
	strings_to_trees(DerivLists, Trees),
	length(DerivLists,NumDerivs), length(Trees,NumTrees), 
	format(user_output, 'Given ~w derivations; found trees for ~w of those~n', [NumDerivs,NumTrees]),
	latex_trees(IntroLines, Prefix, Entropy, Trees, Filename).

% Given a list like the DerivList argument of parse_and_display, turns it into a list of 
% pairs, each containing a 'note' and an x-bar tree.
strings_to_trees([], []).
strings_to_trees([FirstList|OtherLists], [[Note,X]|OtherTrees]) :-
	[Note|DerivationList] = FirstList,
	lparse(DerivationList,D,B,X,_,_), !,
	strings_to_trees(OtherLists, OtherTrees).
strings_to_trees([FirstList|OtherLists], OtherTrees) :-
	[_|DerivationList] = FirstList,
	write('*** WARNING: Couldn\'t parse this derivation, it is being left out of the output: '), write(DerivationList), nl,
	strings_to_trees(OtherLists, OtherTrees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% uncomment ONE grammar
   % GRAMMARS REQUIRING ONLY PHRASAL MOVEMENT (mgpx+lpx)
%:- ['grammars/anbncn']. % 
%:- ['grammars/g0']. % simple SOV 
%:- ['grammars/g-ne']. % "titus praise -s lavinia" inspired by Mahajan 2000
%:- ['grammars/g-nz']. % "praise -s titus lavinia" inspired by Mahajan 2000
%:- ['grammars/g-nza']. % "praise -s Beatrice Benedick" inspired by Mahajan 2000
%:- ['grammars/g-nt']. % "titus lavinia praise -s" inspired by Mahajan 2000
%:- ['grammars/g-nta']. % rigid "titus lavinia praise -s" inspired by Mahajan 2000
%:- ['grammars/g6']. % "the hammer which broke the window" à la Kayne, cf g6a, with head movement
%:- ['grammars/logic1']. % "- p & q"

   % GRAMMARS REQUIRING HEAD MOVEMENT (mghapx+lhapx)
%:- ['grammars/gh6']. % left-adjoining head movement + affix hop
%:- ['grammars/larsonian1']. % larson-ian inspired grammar by Hale (2003)

g0(a) :- showParse([the,king,the,pie,eat]).
g0(b) :- showParse([which,pie,the,king,eat]).

logic(1) :- showParse(['-',p,v,q]).
logic(2) :- showParse([p,q,x,r]).
logic(3) :- showParse([a,'R',b]).
logic(4) :- showParse([x,'V',a,'R']).
logic(5) :- showParse([x,'V','R',a]).
logic(6) :- showParse([x,'V',y,'E','R']).
logic(7) :- showParse(['V',x,'E',y,'R']).

gh1(0) :- showParse([the,king,will,'-s',eat]).
gh1(1) :- showParse([the,king,have,'-s',eat,'-en']).
gh1(2) :- showParse([the,king,be,'-s',eat,'-ing']).
gh1(3) :- showParse([have,'-s',the,king,eat,'-en']).
gh1(4) :- showParse([will,'-s',the,king,eat]).
gh1(5) :- showParse([have,'-s',the,king,been,eat,'-ing']).
gh1(6) :- showParse([which,pie,have,'-s',the,king,been,eat,'-ing']).
gh1(7) :- showParse([which,pie,have,'-s',the,king,eat,'-en']).
gh1(8) :- showParse([which,king,have,'-s',eat,'-en',the,pie]).
gh1(9) :- showParse([which,pie,have,'-s',the,king,been,eat,'-ing']).
gh1(a) :- showParse([the,king,have,'-s',eat,'-en',the,pie]).
gh1(b) :- showParse([the,king,be,'-s',eat,'-ing',the,pie]).
gh1(c) :- showParse([the,king,will,'-s',eat,the,pie]).
gh1(d) :- showParse([the,king,will,'-s',have,eat,'-en',the,pie]).
gh1(e) :- showParse([the,king,have,'-s',been,eat,'-ing',the,pie]).
gh1(f) :- showParse([the,king,will,'-s',have,been,eat,'-ing',the,pie]).
gh1(g) :- showParse([the,king,'-s',will,'-s',have,been,eat,'-ing',the,pie]).
gh1(x) :- showParse([the,king,laugh,'-s']).
gh1(y) :-  showParse([the,king,eat,'-s']).

gh3(X) :- gh1(X).
gh3(10) :- showParse(['Titus',know,'-s',that,'Lavinia',have,'-s',eat,'-en']).
gh3(x10) :- showParse(['Titus',wonders,'-s',that,'Lavinia',laugh,'-s']).
gh3(11) :- showParse(['Titus',think,'-s','Lavinia',laugh,'-s']).
gh3(12) :- showParse(['Titus',doubt,'-s',the,claim,that,'Lavinia',laugh,'-s']).
gh3(13) :- showParse(['Titus',know,'-s',what,'Lavinia',praise,'-s']).
gh3(14) :- showParse(['Titus',wonder,'-s',which,king,'Lavinia',praise,'-s']).
gh3(15) :- showParse(['Titus',seem,'-s',to,laugh]).
gh3(16) :- showParse(['Titus',know,'-s',that,'Lavinia',seem,'-s',to,laugh]).
gh3(17) :- showParse(['Titus',seem,'-s',to,praise,'Lavinia']).
gh3(18) :- showParse(['Titus',seem,'-s',to,be,laugh,'-ing']).
gh3(19) :- showParse(['Titus',seem,'-s',to,have,eat,'-en',the,pie]).
gh3(20) :- showParse(['Titus',seem,'-s',to,have,been,eat,'-ing',the,pie]).
gh3(21) :- showParse(['Titus',seem,'-s',happy]).
gh3(22) :- showParse(['Titus',be,'-s',happy]).
gh3(23) :- showParse(['Titus',will,'-s',be,happy]).
gh3(x23) :- showParse(['Titus',be,'-s',be,'ing',happy]).
gh3(x23) :- showParse(['Titus',be,'-s',have,'-ing',been,happy]).
gh3(24) :- showParse(['Titus',be,'-s',proud]).
gh3(25) :- showParse(['Titus',be,'-s',proud,of,'Lavinia']).
gh3(26) :- showParse(['Titus',prefer,'-s','Lavinia',happy]).
gh3(27) :- showParse(['Titus',prefer,'-s',his,coffee,black]).
gh3(28) :- showParse(['Titus',prefer,'-s',his,shirt,white]).
gh3(29) :- showParse(['Titus',prefer,'-s','Lavinia',proud]).
gh3(30) :- showParse(['Titus',prefer,'-s','Lavinia',proud,of,'Tamara']).
gh3(31) :- showParse(['Titus',prefer,'-s','Lavinia',proud,about,it]).
gh3(32) :- showParse([the,student,be,'-s',up,the,creek]).
gh3(33) :- showParse(['Titus',prefer,'-s','Lavinia',to,be,happy]).
gh3(34) :- showParse(['Titus',prefer,'-s','Lavinia',to,laugh]).
gh3(35) :- showParse(['Titus',prefer,'-s','Lavinia',to,be,laugh,'-ing']).
gh3(36) :- showParse(['Titus',prefer,'-s','Lavinia',to,have,been,eat,'-ing']).
gh3(37) :- showParse([the,student,try,'-s',to,laugh]).
gh3(38) :- showParse([the,student,want,'-s',to,laugh]).
gh3(39) :- showParse([the,student,want,'-s','Lavinia',to,be,happy]).
gh3(x39) :- showParse([the,student,try,'-s','Lavinia',to,be,happy]).
gh3(40) :- showParse([the,student,want,'-s','Lavinia',to,laugh]).
gh3(x40) :- showParse([the,student,try,'-s','Lavinia',to,laugh]).
gh3(41) :- showParse([the,student,consider,'-s','Lavinia',to,be,happy]).
gh3(x41) :- showParse([the,student,consider,'-s',to,be,happy]).

gh5lex :- showLexicon.
gh5(X) :- gh3(X).
gh5(101) :- showParse(['I',have,'-s',a,car]).
gh5(102) :- showParse(['Titus',break,'-s',the,car]).
gh5(103) :- showParse([the,car,break,'-s']).
gh5(104) :- showParse(['Titus',be,'-s',human]).
gh5(105) :- showParse([every,human,be,'-s',mortal]).
gh5(106) :- showParse(['Titus',be,'-s',mortal]).
% adjunction
gh5(107) :- showParse([the,happy,happy,student,laugh,'-s']).
gh5(x107) :- showParse([the,student,happy,laugh,'-s']).
gh5(108) :- showParse(['Titus',happily,happily,laugh,'-s']).
gh5(109) :- showParse(['Titus',laugh,'-s',happily,happily]).
gh5(110) :- showParse(['Titus',will,'-s',eat,the,pie,completely]).
gh5(112) :- showParse([the,king,of,the,coffee,laugh,'-s']).
gh5(113) :- showParse(['Lavinia',laugh,'-s',about,the,king,on,'Sunday']).
gh5(114) :- showParse(['Tamara',the,queen,of,the,'Goth','-s',laugh,'-s',very,happily]).
gh5(115) :- showParse([the,3,king,'-s',laugh,'-s']).
gh5(116) :- showParse([only,'Saturninus',the,king,laugh,'-s']).
gh5(117) :- showParse(['Saturninus',the,king,of,'Rome',consider,'-s',your,proposition,that,'Lavinia',completely,eat,'-s',only,the,3,pie,'-s']).
% coordination
gh6(X) :- gh5(X).
gh6(125) :- showParse(['Titus',and,'Lavinia',and,'Tamara',sing,'-s']).
gh6(126) :- showParse(['Titus',sing,'-s',and,'Titus',laugh,'-s']).
gh6(127) :- showParse(['Titus',sing,'-s',and,laugh,'-s']).

%adjunctionTests
larson(0):-showParse([the,boy,be,'-s',young]).
larson(a0):-showParse([the,boy,be,'-s',so,young]).
larson(b0):-showParse([the,boy,be,'-s',so,so,young]).
larson(x0):-showParse([the,boy,be,'-s',young,so]).
larson(c0):- showParse([the,right,answer,matter,'-s']).
larson(d0):- showParse([the,right,right,answer,matter,'-s']).
larson(e0):- showParse([the,so,right,answer,matter,'-s']).
larson(f0):- showParse([the,so,right,right,answer,matter,'-s']).
larson(c0):- showParse([the,answer,rright,matter,'-s']).
larson(d0):- showParse([the,answer,rright,rright,matter,'-s']).
larson(e0):- showParse([the,answer,rright,sso,matter,'-s']).
larson(f0):- showParse([the,answer,rright,rright,sso,matter,'-s']).

%(*subject*)
larson(1):-showParse([they,have,'-ed',forget,'-en',that,the,boy,who,tell,'-ed',the,story,be,'-s',so,young]).
larson(2):-showParse([the,fact,that,the,girl,who,pay,'-ed',for,the,ticket,'-s',be,'-s',very,poor,doesnt,matter]).
larson(3):-showParse(['I',know,that,the,girl,who,get,'-ed',the,right,answer,be,'-s',clever]).
larson(4):-showParse([he,remember,'-ed',that,the,man,who,sell,'-ed',the,house,leave,'-ed',the,town]).
%(*direct object*)
larson(5):-showParse([they,have,'-ed',forget,'-en',that,the,letter,which,'Dick',write,'-ed',yesterday,be,'-s',long]).
larson(6):-showParse([the,fact,that,the,cat,which,'David',show,'-ed',to,the,man,like,'-s',eggs,be,'-s',strange]).
larson(7):-showParse(['I',know,that,the,dog,which,'Penny',buy,'-ed',today,be,'-s',very,gentle]).
larson(8):-showParse([he,remember,'-ed',that,the,sweet,'-s',which,'David',give,'-ed','Sally',be,'-ed',a,treat]).
%(*indirect object*)
larson(9):-showParse([they,have,'-ed',forget,'-en',that,the,man,who,'Ann',give,'-ed',the,present,to,be,'-ed',old]).
larson(10):-showParse([the,fact,that,the,boy,who,'Paul',sell,'-ed',the,book,to,hate,'-s',reading,be,'-s',strange]).
larson(11):-showParse(['I',know,that,the,man,who,'Stephen',explain,'-ed',the,accident,to,be,'-s',kind]).
larson(12):-showParse([he,remember,'-ed',that,the,dog,which,'Mary',teach,'-ed',the,trick,to,be,'-s',clever]).
%(*oblique*)
larson(13):-showParse([they,have,'-ed',forget,'-en',that,the,box,which,'Pat',bring,'-ed',the,apple,'-s',in,be,'-ed',lost]).
larson(14):-showParse([the,fact,that,the,girl,who,'Sue',write,'-ed',the,story,with,be,'-s',proud,doesnt,matter]).
larson(15):-showParse(['I',know,that,the,ship,which,my,uncle,take,'-ed','Joe',on,be,'-ed',interesting]).
larson(16):-showParse([he,remember,'-ed',that,the,food,which,'Chris',pay,'-ed',the,bill,for,be,'-ed',cheap]).
%(*genitive subject*)
larson(17):-showParse([they,have,'-ed',forget,'-en',that,the,girl,who,s,friend,buy,'-ed',the,cake,be,'-ed',wait,'-ing']).
larson(18):-showParse([the,fact,that,the,boy,who,s,brother,tell,'-s',lies,be,'-s',always,honest,surprise,'-ed',us]).
larson(19):-showParse(['I',know,that,the,boy,who,s,father,sell,'-ed',the,dog,be,'-ed',very,sad]).
larson(20):-showParse([he,remember,'-ed',that,the,girl,who,s,mother,send,'-ed',the,clothe,'-s',come,'-ed',too,late]).
%(*genitive object*)
larson(21):-showParse([they,have,'-ed',forget,'-en',that,the,man,who,s,house,'Patrick',buy,'-ed',be,'-ed',so,ill]).
larson(22):-showParse([the,fact,that,the,sailor,who,s,ship,'Jim',take,'-ed',have,'-ed',one,leg,be,'-s',important]).
larson(23):-showParse(['I',know,that,the,woman,who,s,car,'Jenny',sell,'-ed',be,'-ed',very,angry]).
larson(24):-showParse([he,remember,'-ed',that,the,girl,who,s,picture,'Clare',show,'-ed',us,be,'-ed',pretty]).

larson1(1) :- showParse(['Mommy',like,'-s',cookies]).
larson2(1) :- showParse(['I',like,'-ed',the,dog,which,'Penny',buy,'-ed']).
larson2(2) :- showParse([the,picture,which,he,like,'-ed',fall,'-ed']).
larson2(4) :- showParse([he,admire,'-s',it]).
larson2(5) :- showParse([he,admire,'-s',the,picture,which,you,paint,'-ed']).
larson2(6) :- showParse([his,admiring,the,picture]).
larson2(7) :- showParse([his,admiring,the,picture,of,'John']).
larson2(8) :- showParse([they,laugh,in,that,way]).

%   File   : chineseabs_promotion2.pl
%   Author : Zhong Chen / modified by Jiwon Yun
%   Updated: September 14 2012
%   altered by John to differentiate between overt Nouns and covert Nouns
% It follows Den Dikken & Singhapreecha (2004)'s theory of de (cf. Ou (2007), pp.922). de is treated as a LINKER.

% Jiwon: This grammar is the same as chinesepromotion.pl,
% except that traces are distinguished from pros.

% Proper Nouns
 ['Pronoun']::['N',-case].

% Nouns
 ['Noun']::['N'].	% in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N-trace',-case,-wh].  % nouns that can be relativized

% Null argument (trace)
[]::['N-trace',-case,-wh]. % for null-head RCs

% Null argument (pro)
[]::['N-pro',-case]. 

% One-place predicates (intransitive verbs and adjectives)
 ['Vi']::['V'].	% had bad intensions

% Two-place predicates (transitive verbs)
 ['Vt']::[='N',+case,'V'].
 ['Vt']::[='N-pro',+case,'V'].
 ['Vt']::[='Poss',+case,'V'].	% take possessive as arguments

% ['Vt']::[='F',+case,'V'].

% little v
 []::[=>'V',='N','v'].
 []::[=>'V',='N-pro','v'].
 []::[=>'V',='Poss','v'].
% []::[=>'V',='F','v'].

% tense
 []::[='v',+case,'T'].

% complimentizer
 []::[='T','C'].


% Relative Clauses
% SR:   showParse([Vt,Noun,de,Noun,Vt,Noun]).
% OR:   showParse([Noun,Vt,de,Noun,Vt,Noun]).

% V (optionally taking obj)
['Vi']::['V-SR'].
['Vt']::[='N',+case,'V-SR'].
['Vt']::[='N-pro',+case,'V-SR'].
['Vt']::[='Poss',+case,'V-SR'].
['Vt']::[='N-trace',+case,'V-OR'].
['Vt']::[='N-trace',+case,'V-OR'].

% little v (taking sbj)
[]::[=>'V-SR',='N-trace','v-SR'].
[]::[=>'V-OR',='N','v-OR']. 
[]::[=>'V-OR',='N-pro','v-OR']. 
[]::[=>'V-OR',='Poss','v-OR']. 

% Tense
[]::[='v-SR',+case,'T-SR',-f].
[]::[='v-OR',+case,'T-OR',-f].

% Wh-hoisting complementizer
[]::[='T-SR',+wh,'C-SR'].
[]::[='T-OR',+wh,'C-OR'].

[de]::[='C-SR',+f,'F'].
[de]::[='C-OR',+f,'F'].

[]::[='F','N',-case].

% relativizer de hoist TRel to Spec CP
% [de]::[='TRel',+f,'CRel',-k].

% raise argument NP to head position
% []::[='CRel',+wh,'N'].

% need to move CP to Spec NP
% []::[='N',+k,'N',-case].

% complement clause

 []::[=>'V',='N','vComp'].   	%v
 []::[=>'V',='N-pro','vComp'].   	%v
 []::[=>'V',='Poss','vComp'].	%v
 []::[='vComp',+case,'TComp',-f]. %tense
 []::[='TComp','CComp'].
 [fact]::[='CComp','NDep'].
 [de]::[='NDep',+f,'F'].

% possessive de
 [de]::[='N',='N','Poss',-case].



% The root.
startCategory('C').

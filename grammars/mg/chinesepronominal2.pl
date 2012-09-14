%   File   : chinesepronominal2.pl
%   Author : Zhong Chen / modified by Jiwon Yun
%   Updated: September 14 2012
% This grammar assumes there is no DP in Chinese.
% It treats the empty category in the RC as a pro

% Jiwon: This grammar is the same as chinesepronominal.pl,
% except that traces are distinguished from pros.

% pronouns

['Pronoun']::['N',-case].

% Common Nouns
['Noun']::['N',-case].

% Common Nouns in PossP
['Noun']::['N'].


% Null argument (trace)
[]::['N-trace',-case]. 

% Null argument (pro)
[]::['N-pro',-case]. 

% One-place predicates (intransitive verbs and adjective predicates)
['Vi']::['V'].

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
['Vt']::[='N',+case,'V'].
['Vt']::[='N-pro',+case,'V'].
['Vt']::[='N-trace',+case,'V'].
['Vt']::[='Poss',+case,'V'].


[]::[=>'V',='N','v']. %v

[]::[=>'V',='N-pro','v']. %v


[]::[=>'V',='Poss','v']. %v



[]::[='v',+case,'T']. %tense


[]::[='T','C'].



% Relative Clauses: an Adjunction Analysis
% SSR showParse([Vt,Noun,de,Noun,Vt,Noun]).
% SOR showParse([Noun,Vt,de,Noun,Vt,Noun]).
% OSR showParse([Noun,Vt,Vt,Noun,de,Noun]).
% OOR showParse([Noun,Vt,Noun,Vt,de,Noun]).

[]::[=>'V',='N','vRel']. %v
[]::[=>'V',='N-trace','vRel']. %v
[]::[=>'V',='Poss','vRel']. %v

% Nothing special for T; 
 []::[='vRel',+case,'TRel',-f].

% Wh-hoisting complementizer
 []::[='TRel','CRel'].

 [de]::[='CRel',+f,'F'].

% Relative CP can left-adjoin onto the NP head
 ['F']>>['N'].
 ['F']>>['N-trace'].

% complement clause

 []::[=>'V',='N','vComp'].   	%v
 []::[=>'V',='N-pro','vComp'].   	%v
 []::[=>'V',='Poss','vComp'].	%v

 []::[='vComp',+case,'TComp',-f]. %tense
 []::[='TComp','CComp'].
 [fact]::[='CComp','NDep'].
 [de]::[='NDep',+f,'FDep'].
 []::[='FDep','N',-case].

% % genitive de
 [de]::[='N',='N','Poss',-case].


% The root.
startCategory('C').

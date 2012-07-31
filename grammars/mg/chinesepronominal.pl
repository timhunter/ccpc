%   File   : chinesepronominal.pl
%   Author : Zhong Chen
%   Updated: July 23rd 2012
% This grammar assumes there is no DP in Chinese.
% It treats the empty category in the RC as a pro

% pronouns

['Pronoun']::['N',-case].

% Common Nouns
['Noun']::['N',-case].

% Common Nouns in PossP
['Noun']::['N'].


% Null argument (pro)
[]::['N-null',-case]. 

% One-place predicates (intransitive verbs and adjective predicates)
['Vi']::['V'].

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
['Vt']::[='N',+case,'V'].

['Vt']::[='N-null',+case,'V'].

['Vt']::[='Poss',+case,'V'].


[]::[=>'V',='N','v']. %v

[]::[=>'V',='N-null','v']. %v


[]::[=>'V',='Poss','v']. %v



[]::[='v',+case,'T']. %tense


[]::[='T','C'].



% Relative Clauses: an Adjunction Analysis
% SSR showParse([Vt,Noun,de,Noun,Vt,Noun]).
% SOR showParse([Noun,Vt,de,Noun,Vt,Noun]).
% OSR showParse([Noun,Vt,Vt,Noun,de,Noun]).
% OOR showParse([Noun,Vt,Noun,Vt,de,Noun]).

[]::[=>'V',='N','vRel']. %v
[]::[=>'V',='N-null','vRel']. %v
[]::[=>'V',='Poss','vRel']. %v

% Nothing special for T; 
 []::[='vRel',+case,'TRel'].

% Wh-hoisting complementizer
 []::[='TRel','CRel',-f].

 [de]::[='CRel',+f,'F'].

% Relative CP can left-adjoin onto the NP head
 ['F']>>['N'].
 ['F']>>['N-null'].

% complement clause

 []::[=>'V',='N','vComp'].   	%v
 []::[=>'V',='N-null','vComp'].   	%v
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
